# OxigenLang Conventions

## Variable Declarations

### Prefer Type-Locked Variables Over Dynamic Variables

When initializing a default value, declare it as a type-locked variable rather than a dynamic variable where both type and value are mutable. Type-locked variables prevent accidental type changes that can introduce subtle bugs.

```oxi
// Good — type-locked, value-mutable, zero-initialized
counter <int>
names <array>

// Good — type-locked with initial value
limit <int> := 100

// Avoid — dynamic, type and value can both change
counter := 0
names := []
```

### Use Shadowing Over Dynamic Reassignment

If you need to change a variable's type, use a rescoped shadowed value rather than assigning it dynamically. This makes the type change explicit and intentional.

```oxi
// Good — shadowed in a new scope with a clear type change
x <int> := 42
x <str> := str(x)

// Avoid — silently changes type through dynamic assignment
x := 42
x := "now a string"
```

### When to Use Dynamic Variables

Dynamic variables (`:=` without a type annotation) should only be used when the value or the value's type cannot be determined from the basic primitives. In these cases, a `<generic>` type or `<None>` type is often a better choice.

```oxi
// Acceptable — type is truly unknown (generic callback result)
acc := initial

// Better — declare the expected type when you can
acc <generic> := initial
```

### Zero-Initialized Values

Zero-initialized values using `variable <type>` are more predictable and don't require explicit declaration of the initial value. Use them when the default zero value is what you need.

```oxi
// Clean — zero-initialized, no explicit value needed
count <int>          // 0
name <str>           // ""
items <array>        // []
active <bool>        // False
```

### Always Declare the Expected Type

In most cases, use a typed or type-locked variable when the variable's value will change. Use dynamic only when the type truly cannot be known, but even then it's better to declare the expected type than not.

```oxi
// Good — type is declared even with walrus
result <int> := compute()
buffer <array> := get_items()

// Avoid — type left implicit
result := compute()
buffer := get_items()
```

### Use Type Conversion with `<type>`

Use explicit type conversion with `<type>` when possible, as it makes the intent clear.

```oxi
// Explicit — clear conversion intent
value <int> := int("42")
ratio <float> := float(count) / float(total)

// Implicit — less clear
value := int("42")
```

## Functions

### Named Functions Over Anonymous Functions

Functions should use explicit named definitions (`fun name() {}`) where clarity is key. Use anonymous functions only when an explicit reusable function isn't needed, such as inline callbacks.

```oxi
// Good — named function, clear and reusable
fun calculate_area(width <int>, height <int>) {
    width * height
}

// Good — anonymous function as an inline callback
introduce array
doubled := array.map([1, 2, 3], fun(x <int>) { x * 2 })

// Avoid — anonymous function assigned to a variable when a named function is clearer
calculate_area := fun(width, height) { width * height }
```

### Type Your Parameters

Always use type annotations on function parameters. The function signature should communicate what types are expected.

```oxi
// Good — typed parameters
fun greet(name <str>) {
    println("Hello, {name}!")
}

// Good — union types for flexibility
fun abs(x <int> || <float>) {
    option { x >= 0 -> x, -x }
}

// Avoid — untyped parameters
fun greet(name) {
    println("Hello, {name}!")
}
```

### Use Default Values Over `None` Checks

When a parameter has a natural default, declare it in the signature rather than checking for `None` inside the body.

```oxi
// Good — default value in signature
fun connect(host <str>, port <int> = 8080) {
    println("Connecting to {host}:{port}")
}

// Avoid — checking None manually
fun connect(host <str>, port? <int>) {
    actual_port := option { port == None -> 8080, port }
    println("Connecting to {host}:{actual_port}")
}
```

Use `?` (optional parameters) when there is genuinely no sensible default and the caller may or may not provide a value:

```oxi
// Good — tag is truly optional, no sensible default
fun log(msg <str>, level <str> = "INFO", tag? <str>) {
    option {
        tag == None -> println("[{level}] {msg}"),
        println("[{level}] [{tag}] {msg}")
    }
}
```

### Use Named Arguments for Clarity at Call Sites

When a function has multiple parameters of the same type or boolean flags, use named arguments to make the call self-documenting:

```oxi
// Good — named arguments make intent clear
fun create_user(name <str>, email <str>, admin <bool> = False) { ... }
create_user("Alice", email="alice@example.com", admin=True)

// Avoid — positional args are ambiguous
create_user("Alice", "alice@example.com", True)
```

Named arguments are especially useful for skipping default parameters:

```oxi
// Good — skip port, only set tls
fun connect(host <str>, port <int> = 8080, tls? <bool>) { ... }
connect("example.com", tls=True)
```

## Conditionals and Pattern Matching

### Use `option` for Conditional Logic

Use `option` when you need conditional branching based on expressions. It's the right tool for inline decisions, ternary-style expressions, and multi-arm conditionals within a single file.

```oxi
// Good — option for conditional logic
status <str> := option {
    age < 13 -> "child"
    age < 20 -> "teenager"
    "adult"
}

// Good — ternary-style option
sign <int> := option { x >= 0 -> 1, -1 }
```

### Use `pattern` + `choose` for Reusable Matching

Patterns defined with the `pattern` keyword are meant for reuse across multiple files. They define named, reusable conditions that can be shared through the module system. Use `choose` to match against them.

Don't use `pattern` + `choose` for conditions that only matter in one place — that's what `option` is for.

```oxi
// Good — patterns for reusable conditions shared across files
pattern is_even(n) when n % 2 == 0
pattern is_odd(n) when n % 2 != 0

each n in range(10) {
    choose n {
        is_even -> println(n, "is even")
        is_odd -> println(n, "is odd")
    }
}

// Avoid — using pattern + choose for a one-off condition
pattern is_positive(x) when x > 0
choose value {
    is_positive -> println("positive")
    else -> println("not positive")
}

// Better — use option for one-off conditions
option {
    value > 0 -> println("positive")
    println("not positive")
}
```

## Structs

### Use `variable <Struct>` Over Constructor Functions

Instantiate structs using the `variable <Struct>` convention rather than writing explicit `new()` factory functions. The struct definition already declares the field types, so inference from the struct is more predictable and idiomatic.

```oxi
struct Point {
    x <int>
    y <int>
}

// Good — types are inferred from the struct definition
origin <Point>
p <Point> := Point(10, 20)

// Avoid — wrapping instantiation in a new() function
fun new_point(x <int>, y <int>) {
    Point(x, y)
}
p := new_point(10, 20)
```

The struct definition is the source of truth for types. Using `<Struct>` at the declaration site keeps the type visible and lets the struct enforce its own field constraints.

```oxi
struct Config {
    host <str>
    port <int>
    debug <bool>
}

// Good — struct type is explicit, fields are type-checked by the struct
settings <Config> := Config("localhost", 8080, False)

// Zero-initialized struct
defaults <Config>
```

### Use `<generic>` for Dynamic Types, Not Untyped Variables

When a variable's type is truly dynamic inside a function, use `<generic>` rather than leaving it untyped. Being explicit about dynamic behavior makes it easier for readers to understand how the variable will be used.

```oxi
// Good — explicit that the type is dynamic
fun identity(val <generic>) {
    result <generic> := val
    result
}

// Avoid — untyped, reader can't tell if this is intentional or lazy
fun identity(val) {
    result := val
    result
}
```

### Inherit Behavior, Don't Redeclare It

When a child struct inherits from a parent, use the parent's existing functionality. Only declare new methods or override methods that need different behavior. Don't redeclare methods that already work correctly from the parent.

```oxi
struct Animal {
    name <str>
    sound <str>
}

Animal contains {
    fun speak() { println(name, "says", sound) }
    fun describe() { println("Animal:", name) }
}

struct Dog(Animal) {
    breed <str>
}

// Good — only add new functionality and override what's different
Dog contains {
    fun fetch() { println(name, "fetches the ball!") }
    fun describe() { println("Dog:", name, "- Breed:", breed) }
}

// Avoid — redeclaring speak() when Animal's version already works
Dog contains {
    fun speak() { println(name, "says", sound) }  // identical to parent
    fun fetch() { println(name, "fetches the ball!") }
    fun describe() { println("Dog:", name, "- Breed:", breed) }
}
```

The parent struct's methods are automatically available to the child. Only write a method on the child if it needs to do something different.

### Structs Are for Shared Behavior, Not One-Off Logic

Structs should define expected behavior and group related functionality together. If something is a one-off function or a case that can be handled by a standalone function or found elsewhere, don't create a struct for it.

```oxi
// Good — struct groups related state and behavior
struct HttpResponse {
    status <int>
    body <str>
    headers <map>
}

HttpResponse contains {
    fun is_ok() { status >= 200 and status < 300 }
    fun is_error() { status >= 400 }
}

// Avoid — struct wrapping a single function with no real state
struct Greeter {
    name <str>
}

Greeter contains {
    fun greet() { println("Hello, {name}!") }
}

// Better — just a function
fun greet(name <str>) {
    println("Hello, {name}!")
}
```

### No Empty or Untyped Structs

Every struct should have typed fields. Don't create empty structs or leave fields untyped unless a parent struct needs to be generic.

```oxi
// Good — all fields typed
struct User {
    name <str>
    age <int>
    active <bool>
}

// Good — generic parent where children define the specifics
struct Container {
    value <generic>
}

struct IntBox(Container) {
    label <str>
}

// Avoid — empty struct with no fields
struct Utils {}

// Avoid — untyped fields
struct User {
    name
    age
}
```

### Encapsulation with `hide`

Oxigen is public by default. Everything on a struct is accessible unless you explicitly hide it with the `hide` keyword. Use `hide` on fields that are internal implementation details — things others don't need to see or touch.

```oxi
struct Account {
    name <str>
    hide balance <int>
    hide pin <int>
}

Account contains {
    fun deposit(amount <int>) { self.balance = self.balance + amount }
    fun get_balance() { self.balance }
}

a <Account> := Account("Alice", 1000, 1234)
println(a.name)        // works — public
println(a.get_balance()) // works — public method
println(a.balance)     // error — hidden field
println(a.pin)         // error — hidden field
```

If a field or behavior should be part of the struct's public interface, leave it visible. Only `hide` what's genuinely internal.

### Always Use `self.field` in Methods

Inside struct methods, always use `self.field` to access fields rather than the bare field name. Oxigen allows both, but explicit `self` makes it immediately clear that you're reading or writing struct state, not a local variable.

```oxi
struct Player {
    name <str>
    score <int>
}

// Good — explicit self
Player contains {
    fun add_score(points <int>) { self.score = self.score + points }
    fun display() { println(self.name, ":", self.score) }
}

// Avoid — implicit field access
Player contains {
    fun add_score(points <int>) { score = score + points }
    fun display() { println(name, ":", score) }
}
```

When a function parameter shares the same name as a struct field (like a setter), use `self.field = field` to distinguish between them. Both implicit and explicit `self` are acceptable here, but prefer explicit when the behavior isn't obvious.

```oxi
struct User {
    name <str>
    age <int>
}

// Good — self disambiguates the struct field from the parameter
User contains {
    fun set_name(name <str>) { self.name = name }
    fun set_age(age <int>) { self.age = age }
}
```

## Formatting

### Pick a Block Style and Stick With It

Braces `{}` are the default expected formatting. If you prefer `#[indent]` for indentation-based blocks, that's welcome — but commit to one style across all your code. Don't mix formats.

```oxi
// Good — braces throughout
fun greet(name <str>) {
    option {
        len(name) > 0 -> println("Hello, {name}!")
        println("Hello, stranger!")
    }
}
```

```oxi
// Good — indentation throughout
#[indent]

fun greet(name <str>):
    option:
        len(name) > 0 -> println("Hello, {name}!")
        println("Hello, stranger!")
```

```oxi
// Avoid — mixing braces and indentation in the same project
```

### Use String Interpolation Over Concatenation

Use string interpolation instead of concatenating strings with `+`. Both work, but interpolation is cleaner.

```oxi
name <str> := "Oxigen"
version <int> := 1

// Good — interpolation
println("Welcome to {name} v{version}!")

// Avoid — concatenation
println("Welcome to " + name + " v" + str(version) + "!")
```

### Use Snake Case

Use `snake_case` for variable names, function names, and field names. Avoid `camelCase` or `PascalCase` for these. Struct names are the exception — they use `PascalCase`.

```oxi
// Good
max_retries <int> := 3
fun get_user_name(user_id <int>) { ... }

struct HttpResponse {
    status_code <int>
    response_body <str>
}

// Avoid
maxRetries <int> := 3
fun getUserName(userId <int>) { ... }

struct HttpResponse {
    statusCode <int>
    responseBody <str>
}
```

### Constants

Oxigen has no `const` keyword. To create a constant, use a typed strict assignment (`=`). This makes the variable both type-locked and immutable.

```oxi
// Good — immutable, type-locked constant
PI <float> = 3.14159
MAX_RETRIES <int> = 5
APP_NAME <str> = "Oxigen"

PI = 3.0          // error — cannot reassign immutable variable
PI := "something"  // error — type is locked
```

If you find yourself needing to change a value later, it's probably best to use a type-locked mutable assignment (`<type> :=`) instead of a strict immutable one. You can technically get around immutability by shadowing or reassigning with the walrus operator, but be clear about your intent.

```oxi
// Good — type-locked but mutable, value is expected to change
retries <int> := 5
retries = retries - 1

// Acceptable — shadowing to override an immutable value with clear intent
MAX <int> = 100
MAX <int> := 200  // shadowed, new mutable binding

// Avoid — using walrus to silently override immutable behavior
MAX <int> = 100
MAX := 200  // works but unclear why it was immutable in the first place
```

## Loops

### `each` for Iterables, `repeat` for Continuous Conditions

Use `each` when iterating over a collection of values that don't need to be continuous. Use `repeat when` or `repeat unless` when the loop could run indefinitely and depends on a condition.

```oxi
// Good — each for iterating a known collection
each item in items {
    println(item)
}

each i in range(10) {
    println(i)
}

// Good — repeat for condition-based loops
repeat unless queue_empty() {
    process_next()
}

repeat when retries > 0 {
    retries = retries - 1
    attempt()
}
```

### Pick `when` or `unless` for Clarity

Use whichever reads more naturally for the condition. The goal is that the line reads like plain intent — pick the one where the condition is simpler and more obvious.

```oxi
// Good — unless reads cleaner here
repeat unless i >= size { ... }

// Less clear — double negative logic
repeat when i < size { ... }  // equivalent but the "unless" version is more direct

// Good — when reads cleaner here
repeat when retries > 0 { ... }

// Less clear — negated condition
repeat unless retries <= 0 { ... }  // equivalent but awkward
```

Choose based on which condition is easier to read at a glance, not on a rigid rule.
