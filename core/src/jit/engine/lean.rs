//! Phase 2.2 / 2.4: the lean integer entry.
//!
//! A third entry kind alongside `Generic` and `IntSpecialized`, with the ABI the
//! 2.3 A/B selected:
//!
//! ```text
//! fn(i64 args.., i64 budget) -> i64
//! ```
//!
//! No `*mut VM`, no status, no operand stack, no `JitFrame`. Everything lives in
//! Cranelift SSA, and self-recursion is a direct call to this same entry with a
//! decremented budget.
//!
//! **Why this can skip VM state entirely (2.2).** `lean_entry_eligible`
//! (`compiler::slot_types`) admits only integer parameters, integer locals,
//! integer constants, wrapping arithmetic, integer comparisons, branches, a
//! self-recursive call, and a return. No upvalue, no capture, no heap value, no
//! fallible operation. Such a body has nothing to say to the VM, so the entry is
//! not given a way to reach it. That is stronger than declining to write the
//! stack — it makes writing it unrepresentable.
//!
//! **Why reconstruction is nearly vacuous (2.4).** The plan requires a
//! reconstruction description per bailout point, and a lean entry has exactly
//! one: budget exhaustion. Because the body never touched `stack_view.len`,
//! `jit_frame_view.len`, or any `Rc`, there is no VM state to unwind — the
//! boundary simply reports the overflow against the state it held before the
//! call. The reconstruction description is therefore "nothing to reconstruct",
//! and it is valid precisely because 2.2 holds.
//!
//! The cold exit uses the thread-local `longjmp` verified against real Cranelift
//! frames in `core/tests/jit_unwind_probe.rs`.

use std::collections::HashMap;

use cranelift_codegen::Context;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{AbiParam, Block, InstBuilder, types};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module};

use crate::compiler::opcode::OpCode;
use crate::vm::value::{Function, ValueRepr};

/// Upper bound on operand-stack depth we will model. The eligible subset is
/// simple expression code; anything deeper is refused rather than guessed at.
const MAX_LEAN_STACK: usize = 32;

fn read_u16(code: &[u8], off: usize) -> u16 {
    ((code[off] as u16) << 8) | (code[off + 1] as u16)
}

/// Compiles `func` as a lean entry. Returns `None` when the body uses anything
/// outside the modelled subset — refusing is always safe, since the caller then
/// keeps whatever entry it already had.
pub(crate) fn compile_lean_entry(
    module: &mut JITModule,
    ctx: &mut Context,
    fbc: &mut FunctionBuilderContext,
    func: &Function,
    cold_overflow: FuncId,
    seq: u32,
) -> Option<FuncId> {
    let arity = func.arity as usize;
    // Must not exceed what `invoke_lean` can dispatch.
    if arity == 0 || arity > 8 {
        return None;
    }

    let mut sig = module.make_signature();
    for _ in 0..arity {
        sig.params.push(AbiParam::new(types::I64));
    }
    sig.params.push(AbiParam::new(types::I64)); // budget
    sig.returns.push(AbiParam::new(types::I64));

    let name = format!("oxigen_jit_lean_{seq}");
    // Discarding the error is deliberate and cannot hide a failure: every
    // caller treats `None` as "no lean entry", and the function then keeps
    // whichever entry it already had. Refusing to compile is always a
    // performance outcome, never a correctness one.
    let func_id = module
        .declare_function(&name, Linkage::Local, &sig)
        .ok()?;

    ctx.func.signature = sig;
    let ok = {
        let mut b = FunctionBuilder::new(&mut ctx.func, fbc);
        let r = translate_lean_body(&mut b, module, func, func_id, cold_overflow, arity);
        if r {
            b.finalize(module.target_config());
        }
        r
    };
    if !ok {
        // Leave ctx clean for the next caller; the declaration is inert.
        module.clear_context(ctx);
        return None;
    }

    // OXIGEN_JIT_DISASM=LEAN dumps the lean body; 3.4 is an audit, so it needs
    // to be inspectable the same way the other entries are.
    let want_disasm = std::env::var("OXIGEN_JIT_DISASM")
        .map(|v| v == "LEAN" || v == "ALL")
        .unwrap_or(false);
    if want_disasm {
        ctx.want_disasm = true;
    }
    if module.define_function(func_id, ctx).is_err() {
        module.clear_context(ctx);
        return None;
    }
    if want_disasm
        && let Some(code) = ctx.compiled_code()
        && let Some(vcode) = code.vcode.as_ref()
    {
        eprintln!(
            "=== disasm fn={} kind=Lean ===\n{}",
            func.name.as_deref().unwrap_or("<anon>"),
            vcode
        );
    }
    module.clear_context(ctx);
    Some(func_id)
}

/// Emits the body. Returns false if an unmodelled shape is hit, in which case
/// the partially built function is discarded.
fn translate_lean_body(
    b: &mut FunctionBuilder<'_>,
    module: &mut JITModule,
    func: &Function,
    self_id: FuncId,
    cold_overflow: FuncId,
    arity: usize,
) -> bool {
    let code = &func.chunk.code;
    let chunk = &func.chunk;

    let entry = b.create_block();
    b.append_block_params_for_function_params(entry);
    b.switch_to_block(entry);

    let budget = b.block_params(entry)[arity];

    // Operand stack and locals both become Variables so Cranelift's SSA
    // construction builds the phis at merges; nothing needs a block-param
    // protocol of its own.
    let stack_vars: Vec<Variable> = (0..MAX_LEAN_STACK)
        .map(|_| b.declare_var(types::I64))
        .collect();
    let mut local_vars: HashMap<u16, Variable> = HashMap::new();
    let zero = b.ins().iconst(types::I64, 0);
    for &v in &stack_vars {
        b.def_var(v, zero);
    }
    // Slot 0 is the closure marker; parameters are 1..=arity.
    for slot in 1..=arity as u16 {
        let v = b.declare_var(types::I64);
        b.def_var(v, b.block_params(entry)[slot as usize - 1]);
        local_vars.insert(slot, v);
    }

    // One block per branch target, plus the depth the operand stack must have on
    // entry to it. Structured bytecode is consistent here; a mismatch means the
    // shape is outside what we model and we refuse.
    let mut blocks: HashMap<usize, Block> = HashMap::new();
    let mut block_depth: HashMap<usize, usize> = HashMap::new();
    let mut ip = 0usize;
    while ip < code.len() {
        let Some(op) = OpCode::from_byte(code[ip]) else {
            return false;
        };
        let Ok(len) = chunk.instruction_len(ip) else {
            return false;
        };
        match op {
            OpCode::Jump | OpCode::JumpIfFalse | OpCode::JumpIfTrue => {
                let off = read_u16(code, ip + 1) as usize;
                blocks.entry(ip + 3 + off).or_insert_with(|| b.create_block());
            }
            OpCode::Loop => {
                let off = read_u16(code, ip + 1) as usize;
                let after = ip + 3;
                if off > after {
                    return false;
                }
                blocks.entry(after - off).or_insert_with(|| b.create_block());
            }
            _ => {}
        }
        ip += len.max(1);
    }

    let mut depth: usize = 0;
    let mut terminated = false;
    // The overflow adapter is identical at every call site, so emit it once and
    // branch to it. Per-site copies also carried a dead epilogue each, since the
    // helper never returns.
    let mut cold_block: Option<Block> = None;
    ip = 0;

    macro_rules! push {
        ($v:expr) => {{
            if depth >= MAX_LEAN_STACK {
                return false;
            }
            b.def_var(stack_vars[depth], $v);
            depth += 1;
        }};
    }
    macro_rules! pop {
        () => {{
            if depth == 0 {
                return false;
            }
            depth -= 1;
            b.use_var(stack_vars[depth])
        }};
    }

    while ip < code.len() {
        // A branch target starts a new block; fall-through needs an explicit edge.
        if let Some(&blk) = blocks.get(&ip) {
            if !terminated {
                b.ins().jump(blk, &[]);
            }
            match block_depth.get(&ip) {
                Some(&d) if d != depth && !terminated => return false,
                Some(&d) => depth = d,
                None => {
                    block_depth.insert(ip, depth);
                }
            }
            b.switch_to_block(blk);
            terminated = false;
        }
        if terminated {
            // Unreachable tail: skip without emitting.
            let Ok(len) = chunk.instruction_len(ip) else {
                return false;
            };
            ip += len.max(1);
            continue;
        }

        let Some(op) = OpCode::from_byte(code[ip]) else {
            return false;
        };
        let Ok(len) = chunk.instruction_len(ip) else {
            return false;
        };

        match op {
            OpCode::Constant => {
                let idx = read_u16(code, ip + 1) as usize;
                match chunk.constants.get(idx).map(|v| v.repr()) {
                    Some(ValueRepr::Integer(n)) => {
                        let v = b.ins().iconst(types::I64, n);
                        push!(v);
                    }
                    _ => return false,
                }
            }
            OpCode::GetLocal => {
                let slot = read_u16(code, ip + 1);
                let Some(&var) = local_vars.get(&slot) else {
                    return false;
                };
                let v = b.use_var(var);
                push!(v);
            }
            OpCode::SetLocal => {
                // VM semantics: peek, do not pop. A following Pop drains it.
                let slot = read_u16(code, ip + 1);
                if depth == 0 {
                    return false;
                }
                let v = b.use_var(stack_vars[depth - 1]);
                let var = *local_vars
                    .entry(slot)
                    .or_insert_with(|| b.declare_var(types::I64));
                b.def_var(var, v);
            }
            OpCode::Pop => {
                let _ = pop!();
            }
            OpCode::Add | OpCode::Subtract | OpCode::Multiply => {
                let rhs = pop!();
                let lhs = pop!();
                let v = match op {
                    OpCode::Add => b.ins().iadd(lhs, rhs),
                    OpCode::Subtract => b.ins().isub(lhs, rhs),
                    _ => b.ins().imul(lhs, rhs),
                };
                push!(v);
            }
            OpCode::Less
            | OpCode::LessEqual
            | OpCode::Greater
            | OpCode::GreaterEqual
            | OpCode::Equal
            | OpCode::NotEqual => {
                let rhs = pop!();
                let lhs = pop!();
                let cc = match op {
                    OpCode::Less => IntCC::SignedLessThan,
                    OpCode::LessEqual => IntCC::SignedLessThanOrEqual,
                    OpCode::Greater => IntCC::SignedGreaterThan,
                    OpCode::GreaterEqual => IntCC::SignedGreaterThanOrEqual,
                    OpCode::Equal => IntCC::Equal,
                    _ => IntCC::NotEqual,
                };
                let c = b.ins().icmp(cc, lhs, rhs);
                let v = b.ins().uextend(types::I64, c);
                push!(v);
            }
            OpCode::Jump => {
                let off = read_u16(code, ip + 1) as usize;
                let target = ip + 3 + off;
                let Some(&blk) = blocks.get(&target) else {
                    return false;
                };
                record_depth(&mut block_depth, target, depth);
                b.ins().jump(blk, &[]);
                terminated = true;
            }
            OpCode::JumpIfFalse | OpCode::JumpIfTrue => {
                let off = read_u16(code, ip + 1) as usize;
                let target = ip + 3 + off;
                let Some(&blk) = blocks.get(&target) else {
                    return false;
                };
                // The condition is PEEKED, not popped: the compiler emits an
                // explicit `Pop` at the head of each arm. Popping here made the
                // depth wrong on both sides of every branch.
                if depth == 0 {
                    return false;
                }
                let cond = b.use_var(stack_vars[depth - 1]);
                let fall = b.create_block();
                record_depth(&mut block_depth, target, depth);
                if op == OpCode::JumpIfFalse {
                    b.ins().brif(cond, fall, &[], blk, &[]);
                } else {
                    b.ins().brif(cond, blk, &[], fall, &[]);
                }
                b.switch_to_block(fall);
                b.seal_block(fall);
            }
            OpCode::Loop => {
                let off = read_u16(code, ip + 1) as usize;
                let target = ip + 3 - off;
                let Some(&blk) = blocks.get(&target) else {
                    return false;
                };
                b.ins().jump(blk, &[]);
                terminated = true;
            }
            OpCode::GetGlobal => {
                // Pushes the callee for the self-call below. A lean entry knows
                // its own identity statically, so nothing is materialized —
                // just a placeholder the Call pops and discards.
                push!(zero);
            }
            OpCode::Call => {
                let argc = code[ip + 1] as usize;
                if argc != arity {
                    return false;
                }
                let mut args: Vec<cranelift_codegen::ir::Value> = Vec::with_capacity(argc + 1);
                for _ in 0..argc {
                    args.push(pop!());
                }
                args.reverse();
                let _callee_placeholder = pop!();

                // Budget check before recursing; exhaustion never returns.
                let one = b.ins().iconst(types::I64, 1);
                let next_budget = b.ins().isub(budget, one);
                let exhausted = b.ins().icmp(IntCC::SignedLessThanOrEqual, budget, one);
                let cold = match cold_block {
                    Some(c) => c,
                    None => {
                        let c = b.create_block();
                        cold_block = Some(c);
                        c
                    }
                };
                let go = b.create_block();
                b.ins().brif(exhausted, cold, &[], go, &[]);

                b.switch_to_block(go);
                b.seal_block(go);
                args.push(next_budget);
                let self_ref = module.declare_func_in_func(self_id, b.func);
                let call = b.ins().call(self_ref, &args);
                let r = b.inst_results(call)[0];
                push!(r);
            }
            OpCode::Return => {
                let v = pop!();
                b.ins().return_(&[v]);
                terminated = true;
            }
            _ => return false,
        }
        ip += len.max(1);
    }

    if !terminated {
        // Body fell off the end without returning — outside the modelled subset.
        return false;
    }
    // Emit the single shared overflow adapter, if any call site needed one.
    if let Some(c) = cold_block {
        b.switch_to_block(c);
        let cold_ref = module.declare_func_in_func(cold_overflow, b.func);
        b.ins().call(cold_ref, &[]);
        // Unreachable: the helper longjmps. Cranelift still needs a terminator.
        let z = b.ins().iconst(types::I64, 0);
        b.ins().return_(&[z]);
    }
    b.seal_all_blocks();
    true
}

fn record_depth(map: &mut HashMap<usize, usize>, target: usize, depth: usize) {
    map.entry(target).or_insert(depth);
}

#[cfg(test)]
mod tests {
    use super::*;
    use cranelift_codegen::settings::{self, Configurable};
    use cranelift_jit::JITBuilder;
    use cranelift_module::default_libcall_names;

    /// Compile `source`, pull out the function named `name`, and build a lean
    /// entry for it. Returns the callable pointer, or None if refused.
    fn lean_for(source: &str, name: &str) -> Option<(JITModule, *const u8, usize)> {
        use crate::compiler::Compiler;
        use crate::lexer::Lexer;
        use crate::parser::Parser;

        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer, source);
        let program = parser.parse_program();
        assert!(parser.errors().is_empty(), "{}", parser.format_errors());
        let top = Compiler::new().compile(&program).expect("compile");

        let mut target: Option<Function> = None;
        for c in &top.chunk.constants {
            if let Some(cl) = c.as_closure()
                && cl.function.name.as_deref() == Some(name)
            {
                target = Some((*cl.function).clone());
            }
        }
        let func = target.expect("named function in constant pool");
        let arity = func.arity as usize;

        // Mirror JitInner::new's ISA configuration so the frames match production.
        let mut fb = settings::builder();
        fb.set("opt_level", "speed").unwrap();
        let flags = settings::Flags::new(fb);
        let isa = cranelift_native::builder().unwrap().finish(flags).unwrap();
        let mut jb = JITBuilder::with_isa(isa, default_libcall_names());
        jb.symbol(
            "oxigen_lean_overflow",
            crate::jit::runtime::jit_lean_overflow as *const u8,
        );
        let mut module = JITModule::new(jb);

        let mut cold_sig = module.make_signature();
        cold_sig.returns.push(AbiParam::new(types::I64));
        let cold_id = module
            .declare_function("oxigen_lean_overflow", Linkage::Import, &cold_sig)
            .unwrap();

        let mut ctx = Context::new();
        let mut fbc = FunctionBuilderContext::new();
        let id = compile_lean_entry(&mut module, &mut ctx, &mut fbc, &func, cold_id, 1)?;
        module.finalize_definitions().unwrap();
        let code = module.get_finalized_function(id);
        Some((module, code, arity))
    }

    #[test]
    fn lean_entry_computes_recursive_arithmetic() {
        // bench_arith's body. The lean entry never touches the VM, so a correct
        // result here is evidence the whole computation ran in registers.
        let src = r#"
fun work(n <int>) {
    option {
        n < 2 -> { n }
        { work(n - 1) + work(n - 2) * 3 - n }
    }
}
work(1)
"#;
        let (_m, code, arity) = lean_for(src, "work").expect("work must compile lean");
        assert_eq!(arity, 1);
        // Reference computed here rather than hardcoded, so the expectation
        // cannot drift from the recurrence it is checking. Wrapping arithmetic
        // via u64 mirrors Oxigen's integer semantics.
        fn reference(n: i64) -> i64 {
            if n < 2 {
                return n;
            }
            let a = reference(n - 1) as u64;
            let b = reference(n - 2) as u64;
            (a.wrapping_add(b.wrapping_mul(3)).wrapping_sub(n as u64)) as i64
        }
        for n in [1i64, 2, 3, 7, 10, 20] {
            let v = unsafe {
                crate::jit::runtime::invoke_lean(code as *const (), &[n], 1_000_000)
            };
            assert_eq!(v, Ok(reference(n)), "work({n})");
        }
        // Cross-check the reference itself against the value the interpreter
        // produces, so a shared mistake cannot pass silently.
        assert_eq!(reference(20), -5_482_531);
    }

    #[test]
    fn lean_entry_cold_exit_reports_budget_exhaustion() {
        let src = r#"
fun work(n <int>) {
    option {
        n < 2 -> { n }
        { work(n - 1) + work(n - 2) * 3 - n }
    }
}
work(1)
"#;
        let (_m, code, _) = lean_for(src, "work").expect("compile");
        // A budget far below the required depth must unwind, not return garbage.
        let v = unsafe { crate::jit::runtime::invoke_lean(code as *const (), &[30], 4) };
        assert_eq!(
            v,
            Err(crate::jit::runtime::LeanExit::Overflow),
            "budget exhaustion must take the cold exit"
        );

        // And the entry stays usable afterwards.
        let again = unsafe { crate::jit::runtime::invoke_lean(code as *const (), &[10], 100_000) };
        assert!(again.is_ok(), "entry must be reusable after a cold exit");
    }

    /// Not an assertion — a measurement. Prints the lean entry's time for
    /// work(34), the same workload bench_arith runs, so it can be put beside
    /// the current JIT (~132 ms) and the native C floor (~11 ms).
    /// Run with: cargo test --release -p oxigen-core --lib lean_entry_speed -- --nocapture
    #[test]
    fn lean_entry_speed_work_34() {
        let src = "fun work(n <int>) {\n option {\n n < 2 -> { n }\n { work(n - 1) + work(n - 2) * 3 - n }\n }\n}\nwork(1)\n";
        let (_m, code, _) = lean_for(src, "work").expect("compile");
        let mut best = f64::MAX;
        let mut out = 0i64;
        for _ in 0..5 {
            let t0 = std::time::Instant::now();
            out = unsafe {
                crate::jit::runtime::invoke_lean(code as *const (), &[34], 10_000_000).unwrap()
            };
            let e = t0.elapsed().as_secs_f64() * 1000.0;
            if e < best {
                best = e;
            }
        }
        eprintln!("lean entry work(34): {best:.1} ms  result={out}");
        assert_eq!(out, -646_393_729_142, "must match bench_arith");
    }
}
