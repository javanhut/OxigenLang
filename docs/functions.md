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

### Default Values

Parameters can have default values using `= value` after the type annotation. If the caller omits the argument, the default is used:

```oxi
fun greet(name <str>, greeting <str> = "Hello") {
    println("{greeting}, {name}!")
}

greet("Alice")           // Hello, Alice!
greet("Bob", "Hey")      // Hey, Bob!
```

Default values work with untyped parameters too:

```oxi
fun add(a, b = 0) { a + b }
println(add(5))      // 5
println(add(5, 3))   // 8
```

### Optional Parameters

Mark a parameter as optional with `?` after the name. Optional parameters default to `None` when omitted and skip type checking for `None`:

```oxi
fun connect(host <str>, port? <int>) {
    option {
        port == None -> println("Connecting to {host} on default port"),
        println("Connecting to {host}:{port}")
    }
}

connect("localhost")         // port is None
connect("localhost", 8080)   // port is 8080
```

### Mixing Required, Default, and Optional

All three forms can be combined. Required parameters must come first — the parser enforces this:

```oxi
fun log(msg <str>, level <str> = "INFO", tag? <str>) {
    option {
        tag == None -> println("[{level}] {msg}"),
        println("[{level}] [{tag}] {msg}")
    }
}

log("server started")                       // [INFO] server started
log("request received", "DEBUG")            // [DEBUG] request received
log("auth failed", "ERROR", "security")     // [ERROR] [security] auth failed
```

Placing a required parameter after an optional or default parameter is an error:

```oxi
fun bad(x <str> = "hi", y <int>) { ... }
// error: required parameter 'y' cannot follow optional/default parameters
```

### Named Arguments

When calling a function, arguments can be passed by name using `name=value` syntax. This is especially useful with default and optional parameters, since it lets you skip parameters you don't need:

```oxi
fun connect(host <str>, port <int> = 8080, tls? <bool>) {
    println("host={host} port={port} tls={tls}")
}

connect("example.com")                          // positional only
connect("example.com", tls=True)                // skip port, set tls
connect("example.com", port=443, tls=True)      // named for both
```

Named arguments can be in any order:

```oxi
fun point(x <int>, y <int>) {
    println("({x}, {y})")
}

point(y=20, x=10)    // (10, 20)
```

You can mix positional and named arguments — positional arguments must come first:

```oxi
fun greet(name <str>, greeting <str> = "Hello") {
    println("{greeting}, {name}!")
}

greet("Alice", greeting="Hey")   // Hey, Alice!
```

A positional argument after a named argument is an error:

```oxi
greet(name="Alice", "Hey")
// error: positional argument cannot follow named arguments
```

Named arguments are validated against the function's parameter names:

```oxi
greet(unknown="Bob")
// error: unknown parameter name: 'unknown'
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

| Feature            | Syntax                               |
|--------------------|--------------------------------------|
| Named function     | `fun name(params) { body }`         |
| Anonymous function | `fun(params) { body }`              |
| Typed parameter    | `fun f(x <int>) { ... }`           |
| Default value      | `fun f(x <int> = 10) { ... }`      |
| Optional parameter | `fun f(x? <int>) { ... }`          |
| Named argument     | `f(name="Alice", age=30)`           |
| Implicit return    | Last expression in body              |
| Explicit return    | `give value`                         |
| Early return       | `give value when condition`          |
| Closure            | Functions capture enclosing variables |

See also:
- [Variables and Assignments](variables.md) — how function variables work
- [Structs](structs.md) — attaching functions as methods
- [Control Flow](control_flow.md) — `give` with postfix guards
