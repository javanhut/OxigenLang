# OxigenLang Enums

OxigenLang supports enumerations — named types whose values are a fixed set of variants. Variants can be unit (tag-only or with an explicit discriminant), tuple (positional payload), or struct (named payload), in the Rust style.

## Defining an Enum

Use the `enum` keyword, a name, and a brace-delimited list of variants:

```oxi
enum Stoplight {
    Red: 0
    Yellow: 1
    Green: 2
}
```

Variants with explicit discriminants use `Name: expression`. Integer, float, string, boolean, and `None` literals are accepted for the VM backend. Omit the `:` to auto-assign sequential integers starting at 0:

```oxi
enum Direction {
    North
    East
    South
    West
}
// North=0, East=1, South=2, West=3
```

Explicit discriminants reset the auto-increment counter — the next bare variant after `Red: 5` becomes `6`.

### Tuple and Struct Variants

Variants may carry a payload:

```oxi
enum Shape {
    Circle(radius <float>)
    Rectangle { w <float>, h <float> }
    Unit
}
```

Tuple variants use `(param <type>, ...)`; struct variants use `{ field <type>, ... }`.

## Constructing Variants

```oxi
light := Stoplight.Green           // unit variant
c     := Shape.Circle(1.5)         // tuple variant
r     := Shape.Rectangle { w: 3.0, h: 4.0 }  // struct variant
u     := Shape.Unit                // unit-style, no discriminant
```

## Accessing Variants

- `.name` returns the variant's name as a string.
- `.value` returns the discriminant on unit variants (or `None` if the variant has no discriminant).
- On tuple variants, fields are accessible by numeric index (`.0`, `.1`, ...) or by declared parameter name (`.radius`).
- On struct variants, fields are accessible by their declared names (`.w`, `.h`).

```oxi
Stoplight.Green.value   // 2
Stoplight.Green.name    // "Green"
c.radius                // 1.5 (also c.0)
r.w                     // 3.0
```

## Type Annotations

Two forms of annotation are recognised:

- `<EnumName>` — constrain a variable to instances (or the definition) of a specific enum.
- `<Enum>` — generic, accepts any enum value.

```oxi
light <Stoplight> := Stoplight.Green
any   <Enum>      := Direction.East
```

## Equality

Enum instances compare equal only when their enum name, variant name, and payload all match. Two variants with the same discriminant but different enums are not equal.

```oxi
Stoplight.Red == Stoplight.Red       // True
Stoplight.Red == Direction.North     // False (different enums, both discriminant 0)
```

## Pattern Matching

Enum instances work with the existing `pattern`/`choose`/`when` system via equality:

```oxi
pattern is_red(x) when x == Stoplight.Red
pattern is_green(x) when x == Stoplight.Green

choose light {
    is_red   -> println("STOP")
    is_green -> println("go")
    else     -> println("caution")
}
```

Destructuring tuple and struct variants in patterns (e.g. `when Shape.Circle(r)`) is not yet supported — only equality against a constructed variant value works today.
