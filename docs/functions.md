# OxigenLang Functions

Functions in OxigenLang are first-class values — they can be assigned to variables, passed as arguments, and returned from other functions.

## Defining Functions

### Named Functions

Use the `fun` keyword followed by a name, parameters, and a body:

```oxi
fun add(a, b) {
    a + b
}

println(add(3, 4))
```

### Anonymous Functions

Functions can be created without a name and assigned to variables:

```oxi
double := fun(x) { x * 2 }
println(double(5))
```

Anonymous functions are expressions, so they can appear anywhere an expression is expected.

## Parameters

### Untyped Parameters

By default, parameters accept any type:

```oxi
fun greet(name) {
    println("Hello, " + name + "!")
}

greet("Alice")
```

### Typed Parameters

Add type annotations to enforce parameter types. The annotation uses the same `<type>` syntax as variable declarations:

```oxi
fun add(a <int>, b <int>) {
    a + b
}

add(3, 4)
```

If a typed parameter receives a value of the wrong type, an error is produced:

```oxi
fun add(a <int>, b <int>) { a + b }
add(3, "four")
```

You can mix typed and untyped parameters:

```oxi
fun format(label, value <int>) {
    label + ": " + str(value)
}
```

## Return Values

### Implicit Return

The last expression in a function body is automatically returned:

```oxi
fun square(x) { x * x }
println(square(5))
```

For multi-statement bodies, the last expression is the return value:

```oxi
fun process(x) {
    doubled := x * 2
    tripled := x * 3
    doubled + tripled
}
println(process(4))
```

### Explicit Return with `give`

Use `give` to return a value early:

```oxi
fun abs(x) {
    give -x when x < 0
    x
}
```

`give` immediately exits the function with the provided value. It works well with postfix guards for concise early returns:

```oxi
fun safe_divide(a, b) {
    give None when b == 0
    a / b
}
```

## Functions as Values

Functions are first-class — they can be stored in variables, passed to other functions, and returned:

### Storing in Variables

```oxi
add := fun(a, b) { a + b }
sub := fun(a, b) { a - b }
println(add(5, 3))
println(sub(5, 3))
```

### Passing as Arguments

```oxi
fun apply(f, x) {
    f(x)
}

double := fun(x) { x * 2 }
println(apply(double, 5))
```

### Returning Functions

```oxi
fun make_adder(n) {
    fun(x) { x + n }
}

add5 := make_adder(5)
println(add5(10))
```

## Closures

Functions capture variables from their enclosing scope. The captured variables remain accessible even after the outer function returns:

```oxi
fun counter() {
    count := 0
    fun() {
        count++
        count
    }
}

c := counter()
println(c())
println(c())
println(c())
```

## Scope

Each function call creates a new environment (scope). Variables defined inside a function are local to that call:

```oxi
x := "outer"

fun test() {
    x := "inner"
    println(x)
}

test()
println(x)
```

Functions can read variables from outer scopes but create new bindings with `:=`:

```oxi
multiplier := 10

fun scale(x) {
    x * multiplier
}

println(scale(5))
```

## Struct Methods

Functions can be attached to structs using `contains` blocks. Inside a method, struct fields are accessible directly by name (implicit self):

```oxi
struct Circle {
    radius <float>
}

Circle contains {
    fun area() { 3.14159 * radius * radius }
    fun scale(factor <float>) { radius = radius * factor }
}

c := Circle(5.0)
println(c.area())
c.scale(2.0)
println(c.area())
```

See the [Structs](structs.md) guide for full details on methods, implicit self, and inheritance.

## Recursion

Functions can call themselves:

```oxi
fun factorial(n) {
    give 1 when n <= 1
    n * factorial(n - 1)
}

println(factorial(5))
```

## Summary

| Feature          | Syntax                               |
|------------------|--------------------------------------|
| Named function   | `fun name(params) { body }`         |
| Anonymous function | `fun(params) { body }`             |
| Typed parameter  | `fun f(x <int>) { ... }`           |
| Implicit return  | Last expression in body              |
| Explicit return  | `give value`                         |
| Early return     | `give value when condition`          |
| Closure          | Functions capture enclosing variables |

See also:
- [Variables and Assignments](variables.md) — how function variables work
- [Structs](structs.md) — attaching functions as methods
- [Control Flow](control_flow.md) — `give` with postfix guards
