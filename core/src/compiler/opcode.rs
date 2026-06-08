/// Bytecode instruction set for the OxigenLang VM.
///
/// Each opcode is a single byte. Operands follow immediately in the bytecode
/// stream as u8 or u16 (big-endian) values depending on the opcode.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpCode {
    // --- Constants & Literals ---
    /// Push constant from pool. Operand: u16 constant index.
    Constant,
    /// Push None.
    None,
    /// Push True.
    True,
    /// Push False.
    False,

    // --- Stack Manipulation ---
    /// Discard top of stack.
    Pop,
    /// Duplicate top of stack.
    Dup,

    // --- Arithmetic ---
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    // --- Comparison ---
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // --- Logical ---
    Not,

    // --- Bitwise ---
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    ShiftLeft,
    ShiftRight,

    // --- Unary ---
    /// Unary minus.
    Negate,

    // --- Postfix ---
    /// Increment local. Operand: u16 stack slot.
    Increment,
    /// Decrement local. Operand: u16 stack slot.
    Decrement,

    // --- Variables ---
    /// Get local variable. Operand: u16 stack slot.
    GetLocal,
    /// Set local variable. Operand: u16 stack slot.
    SetLocal,
    /// Get global variable. Operand: u16 constant index (name string).
    GetGlobal,
    /// Set global variable. Operand: u16 constant index (name string).
    SetGlobal,
    /// Define global variable. Operand: u16 constant index (name string).
    DefineGlobal,
    /// Get upvalue. Operand: u16 upvalue index.
    GetUpvalue,
    /// Set upvalue. Operand: u16 upvalue index.
    SetUpvalue,
    /// Close the topmost open upvalue.
    CloseUpvalue,

    // --- Control Flow ---
    /// Unconditional forward jump. Operand: u16 offset.
    Jump,
    /// Jump if top of stack is falsy (does NOT pop). Operand: u16 offset.
    JumpIfFalse,
    /// Jump if top of stack is truthy (does NOT pop). Operand: u16 offset.
    JumpIfTrue,
    /// Backward jump (for loops). Operand: u16 offset.
    Loop,

    // --- Functions ---
    /// Create closure from function constant. Operand: u16 constant index.
    /// Followed by upvalue descriptors: (u8 is_local, u16 index) per upvalue.
    Closure,
    /// Call function. Operand: u8 argument count.
    Call,
    /// Call with named arguments. Operands: u8 positional_count, u8 named_count.
    /// Named args on stack as: [name_constant_string, value] pairs.
    CallNamed,
    /// Return value from top of stack.
    Return,

    // --- Collections ---
    /// Build array from N elements on stack. Operand: u16 element count.
    BuildArray,
    /// Build tuple from N elements on stack. Operand: u16 element count.
    BuildTuple,
    /// Build map from N key-value pairs on stack. Operand: u16 pair count.
    BuildMap,
    /// Build set from N elements on stack. Operand: u16 element count.
    BuildSet,
    /// Index into collection. Stack: [collection, index] -> [value].
    Index,
    /// Assign to index. Stack: [collection, index, value] -> [].
    IndexAssign,
    /// Slice collection. Operand: u8 flags (bit0=has_start, bit1=has_end).
    Slice,

    // --- Structs ---
    /// Define struct. Operand: u16 constant index (struct def info).
    StructDef,
    /// Create struct instance with named fields. Operands: u16 name constant, u16 field count.
    /// Stack has field name-value pairs.
    StructLiteral,
    /// Get field from struct/module. Operand: u16 field name constant.
    GetField,
    /// Set field on struct. Operand: u16 field name constant.
    SetField,
    /// Define methods on struct. Operands: u16 struct name constant, u16 method count.
    /// Stack has method name-closure pairs.
    DefineMethod,

    // --- Enums ---
    /// Define an enum. Operand: u16 constant index (ObjEnumDef value).
    EnumDef,
    /// Construct a unit enum variant. Operand: u16 variant name constant.
    /// Stack has the EnumDef value.
    MakeEnumVariantUnit,
    /// Construct a tuple enum variant. Operands: u16 variant name constant, u8 arg count.
    /// Stack: [EnumDef, arg1, ..., argN] -> [EnumInstance].
    MakeEnumVariantTuple,
    /// Construct a struct enum variant. Operands: u16 variant name constant, u8 field count.
    /// Stack: [EnumDef, name1, val1, ..., nameN, valN] -> [EnumInstance].
    MakeEnumVariantStruct,

    // --- Pattern Matching ---
    /// Define a pattern. Operand: u16 constant index (pattern info).
    DefinePattern,
    /// Test pattern against value. Operands: u16 pattern name, u16 jump offset if no match.
    TestPattern,

    // --- Error Handling ---
    /// Construct ErrorValue. Operand: u8 has_tag. Stack: [tag?, value] -> [ErrorValue].
    ErrorConstruct,
    /// Wrap top of stack in Value wrapper.
    ValueConstruct,
    /// Guard expression. Operands: u16 binding name constant, u16 fallback jump offset.
    Guard,
    /// Fail: convert ErrorValue to Error.
    Fail,

    // --- Modules ---
    /// Import module. Operands: u16 path constant, u8 has_selective.
    Import,
    /// Get field from module on stack. Operand: u16 field name constant.
    GetModuleField,

    // --- String Interpolation ---
    /// Concatenate N string parts. Operand: u16 part count.
    StringInterp,

    // --- Misc ---
    /// Log expression. Operand: u8 flags (bit0=has_tag, bit1=has_sub_tag, bit2=has_msg).
    Log,
    /// Unpack iterable into N variables. Operand: u8 name count.
    Unpack,
    /// Main block. Operand: u16 jump offset to skip if not main context.
    Main,
    /// Unless expression: condition already evaluated.
    /// Operand: u16 jump to alternative.
    Unless,
    /// Pop and check if truthy, jump if not. Used for option arms.
    /// Operand: u16 jump offset.
    PopJumpIfFalse,
    /// Get iterator length / iteration support.
    /// Operand: none. Stack: [iterable] -> [iterable, length].
    IterLen,
    /// Get element at iteration index.
    /// Stack: [iterable, index] -> [element].
    IterGet,
    /// Type wrap / convert. Operand: u16 type name constant.
    /// Stack: [value] -> [converted_value].
    /// Handles Error/Value union wrapping, type conversion, etc.
    TypeWrap,
    /// Call a method on a struct instance.
    /// Operands: u16 method_name constant, u8 arg_count.
    /// Stack: [instance, arg1, ..., argN] -> [result].
    MethodCall,
    /// Call a method with named arguments.
    /// Operands: u16 method_name constant, u8 positional_count, u8 named_count.
    /// Named args on stack as: [name_constant_string, value] pairs.
    MethodCallNamed,
    /// Check if a variable is mutable. Operand: u16 name constant.
    /// Push bool result. For locals, resolved at compile time.
    IsMut,
    /// Check if a value matches a type. Operand: u16 type_name constant.
    /// Stack: [value] -> [bool].
    IsType,
    /// Check if a variable's type is mutable (no type constraint).
    /// Operand: u16 name constant. Push bool result.
    IsTypeMut,
    /// Define a global with mutability/type metadata.
    /// Operands: u16 name constant, u8 mutable flag, u16 type_name constant (0xFFFF = no type).
    DefineGlobalTyped,
}

impl OpCode {
    /// Convert a raw byte to an OpCode. Returns None for invalid bytes.
    pub fn from_byte(byte: u8) -> Option<OpCode> {
        if byte <= OpCode::DefineGlobalTyped as u8 {
            // SAFETY: OpCode is repr(u8) and we verified the range.
            Some(unsafe { std::mem::transmute(byte) })
        } else {
            None
        }
    }
}

/// A chunk of bytecode with its associated constant pool and source location info.
#[derive(Debug, Clone)]
pub struct Chunk {
    /// Raw bytecode.
    pub code: Vec<u8>,
    /// Constant pool.
    pub constants: Vec<crate::vm::value::Value>,
    /// Line number for each byte of code (for error reporting).
    pub lines: Vec<u32>,
    /// Column for each byte of code, paired with `lines`. 0 means unknown
    /// (the writer never set a column before emitting).
    pub columns: Vec<u32>,
    /// The column to attach to the next byte written. Set by the compiler
    /// before each emit batch via `set_loc_column`. Lives on the chunk
    /// (rather than threaded through every `emit_*` signature) so the
    /// existing line-only emit API stays unchanged.
    cur_column: u32,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
            columns: Vec::new(),
            cur_column: 0,
        }
    }

    /// Set the column to attach to subsequently written bytes. Call before
    /// emitting each opcode whose source location you know; bytes written
    /// without a prior `set_loc_column` get column 0 (unknown).
    pub fn set_loc_column(&mut self, col: u32) {
        self.cur_column = col;
    }

    /// Write a single byte with line information. Column comes from the
    /// most recent `set_loc_column` call.
    pub fn write(&mut self, byte: u8, line: u32) {
        self.code.push(byte);
        self.lines.push(line);
        self.columns.push(self.cur_column);
    }

    /// Write a u16 value as two bytes (big-endian).
    pub fn write_u16(&mut self, value: u16, line: u32) {
        self.write((value >> 8) as u8, line);
        self.write(value as u8, line);
    }

    /// Add a constant to the pool and return its index.
    pub fn add_constant(&mut self, value: crate::vm::value::Value) -> u16 {
        self.constants.push(value);
        (self.constants.len() - 1) as u16
    }

    /// Read a u16 from the bytecode at the given offset.
    pub fn read_u16(&self, offset: usize) -> u16 {
        ((self.code[offset] as u16) << 8) | (self.code[offset + 1] as u16)
    }

    /// Current length of the bytecode.
    pub fn len(&self) -> usize {
        self.code.len()
    }

    /// Patch a u16 value at a previously emitted offset.
    pub fn patch_u16(&mut self, offset: usize, value: u16) {
        self.code[offset] = (value >> 8) as u8;
        self.code[offset + 1] = value as u8;
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}
