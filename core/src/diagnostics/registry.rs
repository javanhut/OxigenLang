//! The code registry: every code's identity and long-form explanation.
//!
//! Codes are allocated **sequentially as concepts are added**, deliberately not
//! partitioned by compiler stage. A stage partition looks tidy until a check
//! moves — argument-count checking wants to move from runtime to compile time —
//! and since codes are permanent and never renumbered, the partition would be
//! wrong forever the first time that happened.
//!
//! Rules: never reuse a code, never renumber. A retired code stays here marked
//! deprecated so old output remains explicable.

use super::Code;

// ── Lexical ─────────────────────────────────────────────────────────────────
pub const UNTERMINATED_STRING: Code = Code("E0001");
pub const ILLEGAL_TOKEN: Code = Code("E0002");

// ── Syntax ──────────────────────────────────────────────────────────────────
pub const UNEXPECTED_TOKEN: Code = Code("E0003");
pub const UNCLOSED_DELIMITER: Code = Code("E0004");
pub const EXPECTED_TYPE_NAME: Code = Code("E0005");
pub const EXPECTED_IDENTIFIER: Code = Code("E0006");
pub const EXPECTED_MODULE_PATH: Code = Code("E0007");
pub const UNKNOWN_ANGLE_EFFECT: Code = Code("E0008");
pub const UNSUPPORTED_EFFECT_FILTER: Code = Code("E0009");
pub const MISSING_ARROW_IN_OPTION_ARM: Code = Code("E0010");
pub const POSITIONAL_AFTER_NAMED: Code = Code("E0011");
pub const REQUIRED_AFTER_OPTIONAL: Code = Code("E0012");
pub const BAD_STRING_INTERPOLATION: Code = Code("E0013");
pub const EXPECTED_FUN_IN_INCLUDES: Code = Code("E0014");

// ── Compile ─────────────────────────────────────────────────────────────────
pub const IMMUTABLE_ASSIGN: Code = Code("E0015");
pub const SKIP_STOP_OUTSIDE_LOOP: Code = Code("E0016");
pub const SKIP_STOP_AS_VALUE: Code = Code("E0017");
pub const INTRINSIC_MISUSE: Code = Code("E0018");
pub const GENERIC_WITH_EQUALS: Code = Code("E0019");
pub const CHUNK_LIMIT: Code = Code("E0020");
pub const COMPILE_ERROR: Code = Code("E0021");
pub const INVALID_CHAR_LITERAL: Code = Code("E0022");
pub const UNKNOWN_OPERATOR: Code = Code("E0023");
pub const PATTERN_ARITY: Code = Code("E0024");
pub const LOOP_VAR_REBIND: Code = Code("E0025");
pub const TYPE_NAME_REBIND: Code = Code("E0026");

pub struct Entry {
    pub code: Code,
    pub title: &'static str,
    /// Long form, markdown, shown by `oxigen explain`.
    pub explanation: &'static str,
}

pub static REGISTRY: &[Entry] = &[
    Entry {
        code: UNTERMINATED_STRING,
        title: "unterminated string literal",
        explanation: "\
A string literal was opened but never closed.

    name := \"ada

A single-quoted or double-quoted string must close on the same line — write a
newline with the `\\n` escape rather than a real line break:

    name := \"ada\\nlovelace\"

A string that genuinely spans lines uses a triple-quoted literal:

    text := \"\"\"first line
    second line\"\"\"
",
    },
    Entry {
        code: ILLEGAL_TOKEN,
        title: "illegal token",
        explanation: "\
A character appeared that is not part of Oxigen's syntax.

The most common cause is a statement terminator carried over from another
language. Oxigen ends statements at the newline:

    x := 1;   // error
    x := 1    // correct
",
    },
    Entry {
        code: UNEXPECTED_TOKEN,
        title: "unexpected token",
        explanation: "\
The parser found a token that cannot appear in this position.

This is the general-purpose syntax error. The caret marks the token that could
not be used; the problem is often the construct just before it — an unclosed
bracket or a missing operator.
",
    },
    Entry {
        code: UNCLOSED_DELIMITER,
        title: "unclosed delimiter",
        explanation: "\
A block, bracket or parenthesis was opened and never closed.

    main {
        println(1)

The secondary label points at the delimiter that was left open, which is
usually further from the end of the file than it appears.
",
    },
    Entry {
        code: EXPECTED_TYPE_NAME,
        title: "expected a type name",
        explanation: "\
A type annotation must name a type:

    count <int> := 0
    name <str> := \"ada\"

Built-in names are `int`, `str`, `float`, `char`, `bool`, `array`, `byte`,
`uint`, `tuple`, `map`, `set`, `generic`, `None`, `Error`, `Value` and `Enum`;
a struct or enum name also works. Unions are written `<int || str>`.
",
    },
    Entry {
        code: EXPECTED_IDENTIFIER,
        title: "expected an identifier",
        explanation: "\
A name was required here — after `.` in a module path, in an unpack, or as a
guard binding — and something else appeared.
",
    },
    Entry {
        code: EXPECTED_MODULE_PATH,
        title: "expected a module path",
        explanation: "\
`introduce` takes a module path:

    introduce math          // a stdlib module
    introduce .utils        // a file next to this one
    introduce {upper} from strings   // selected names only
",
    },
    Entry {
        code: UNKNOWN_ANGLE_EFFECT,
        title: "unknown angle effect",
        explanation: "\
Angle syntax (`<...>`) names an effect. The valid effects are `guard`, `fail`,
`log`, `type`, `Error` and `Value`.
",
    },
    Entry {
        code: UNSUPPORTED_EFFECT_FILTER,
        title: "unsupported effect filter",
        explanation: "\
Only `Error` may be used as a filter for `guard` and `fail`:

    value <guard> err -> fallback
",
    },
    Entry {
        code: MISSING_ARROW_IN_OPTION_ARM,
        title: "missing `->` in an option arm",
        explanation: "\
Each `option` arm is `condition -> result`:

    option {
        n < 0  -> \"negative\",
        n == 0 -> \"zero\",
        \"positive\"
    }

The final arm with no condition is the default.
",
    },
    Entry {
        code: POSITIONAL_AFTER_NAMED,
        title: "positional argument after a named one",
        explanation: "\
Once a call uses named arguments, the rest must be named too:

    draw(10, 20, color: \"red\")     // fine
    draw(10, color: \"red\", 20)     // error
",
    },
    Entry {
        code: REQUIRED_AFTER_OPTIONAL,
        title: "required parameter after an optional one",
        explanation: "\
A parameter with no default cannot follow one that has a default, because a
caller supplying positional arguments could never reach it:

    fun f(a, b = 2, c)      // error — `c` is unreachable positionally
    fun f(a, c, b = 2)      // fine
",
    },
    Entry {
        code: BAD_STRING_INTERPOLATION,
        title: "invalid string interpolation",
        explanation: "\
The expression inside `{ }` could not be parsed, or the interpolation was never
closed. Any expression is allowed:

    println(\"{a == b}\")
    println(\"{x % 2}\")
    println(\"{ {\\\"k\\\": 7}[\\\"k\\\"] }\")

Write a literal brace with `\\{` and `\\}`.
",
    },
    Entry {
        code: EXPECTED_FUN_IN_INCLUDES,
        title: "expected `fun` inside an includes block",
        explanation: "\
An `includes` block contains only method definitions:

    Point includes {
        fun length() { self.x + self.y }
    }
",
    },
    Entry {
        code: IMMUTABLE_ASSIGN,
        title: "cannot assign to an immutable binding",
        explanation: "\
Oxigen has two binding operators:

    x <int> = 5      binds `x` immutably
    x <int> := 5     binds `x` mutably

Assigning with `=` to a name that was bound immutably is an error. Loop
variables and block-scoped type names are also immutable — for a loop variable
the assignment would have no effect anyway, because the next iteration
overwrites it.
",
    },
    Entry {
        code: SKIP_STOP_OUTSIDE_LOOP,
        title: "`skip` or `stop` outside a loop",
        explanation: "\
`skip` continues with the next iteration and `stop` leaves the loop, so both
require an enclosing `each` or `repeat`.
",
    },
    Entry {
        code: SKIP_STOP_AS_VALUE,
        title: "`skip` or `stop` used as a value",
        explanation: "\
`skip` and `stop` change control flow; they do not produce a value, so they
cannot appear where one is expected.
",
    },
    Entry {
        code: INTRINSIC_MISUSE,
        title: "intrinsic used incorrectly",
        explanation: "\
A compiler intrinsic such as `is_mut` was given the wrong number of arguments,
or an argument of the wrong shape — these take a bare variable or type name,
not an arbitrary expression.
",
    },
    Entry {
        code: GENERIC_WITH_EQUALS,
        title: "`<generic>` cannot be bound with `=`",
        explanation: "\
A `<generic>` binding must use `:=`, because its type is fixed by the value it
is first given.
",
    },
    Entry {
        code: CHUNK_LIMIT,
        title: "compiled chunk limit exceeded",
        explanation: "\
A single function exceeded a bytecode limit — too many constants, or a jump or
loop body too large to encode. Split the function into smaller ones.
",
    },
    Entry {
        code: INVALID_CHAR_LITERAL,
        title: "invalid character literal",
        explanation: "\
A backtick literal holds exactly one character:

    initial := `A`

For zero characters or more than one, use a string instead:

    word := \"Ada\"
",
    },
    Entry {
        code: UNKNOWN_OPERATOR,
        title: "unknown operator",
        explanation: "\
The compiler reached an operator it has no lowering for. This indicates the
parser accepted something the compiler does not implement — please report it
with the source that triggered it.
",
    },
    Entry {
        code: PATTERN_ARITY,
        title: "a pattern takes exactly one parameter",
        explanation: "\
A pattern is invoked with the single value being matched, so only its first
parameter is ever bound:

    pattern big(n) when n > 3

    choose value {
        big -> \"large\",
        else -> \"small\"
    }

Declaring more parameters used to parse, but nothing could ever supply them.
Referencing one in the condition failed at run time inside the pattern
(`cannot compare INTEGER > NONE`); referencing one in an arm body failed with
`undefined variable`. Compare against a captured value instead:

    limit := 3
    pattern big(n) when n > limit
",
    },
    Entry {
        code: LOOP_VAR_REBIND,
        title: "rebinding a loop variable with `:=`",
        explanation: "\
`:=` on a loop variable applies only to the rest of the current iteration; the
next iteration rebinds the name from the sequence, discarding it.

    each i in range(3) {
        i := i * 10     // visible below, gone next iteration
        println(i)      // 0, 10, 20 — but iteration is still 0, 1, 2
    }

It reads as though it changes the loop, and it does not. Use a separate name:

    each i in range(3) {
        scaled := i * 10
        println(scaled)
    }
",
    },
    Entry {
        code: TYPE_NAME_REBIND,
        title: "rebinding a type name",
        explanation: "\
A block-scoped `struct` or `enum` name cannot be reassigned or shadowed in the
same scope — later code in that block would silently refer to a different
thing than the declaration suggests. Choose a different name for the value.
",
    },
    Entry {
        code: COMPILE_ERROR,
        title: "compile error",
        explanation: "\
A general compilation failure that has not yet been given a more specific code.
",
    },
];

pub fn lookup(code: Code) -> Option<&'static Entry> {
    REGISTRY.iter().find(|e| e.code == code)
}

/// Every code that has an entry, in registry order.
pub fn all() -> impl Iterator<Item = &'static Entry> {
    REGISTRY.iter()
}
