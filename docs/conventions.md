# OxigenLang Conventions

## Philosophy

Oxigen optimizes for code that states its intent plainly. A few principles run
through every convention below:

- **Be explicit about types.** A declared type is locked, which turns whole
  classes of bugs into errors and documents what a value is. Reach for a
  dynamic variable only when a type genuinely can't be known.
- **Read like intent.** Prefer the construct that reads most naturally — `option`
  for a decision, `unless`/`when` for the clearer condition, named arguments at
  the call site, string interpolation over concatenation.
- **Returns are implicit.** The last expression is the result; `give` is a
  rarely-used escape hatch, not the norm.
- **Public by default.** Expose behavior by default and `hide` only what is
  genuinely internal.
- **Don't repeat yourself.** Inherit and reuse rather than redeclare; reach for
  a struct only when there's shared state and behavior.
- **One tool per job.** `option` for local decisions, `pattern` + `choose` for
  reusable matching; `each` for collections, `repeat` for conditions.
- **Pick a style and commit.** One block style and `snake_case` names
  (`PascalCase` for structs), consistently.

## Simplicity Over Verbosity

Oxigen code should never be verbose when it can be simplified. It should read
naturally and be reasoned about naturally — close to how you'd describe the
problem out loud. This is the language's central goal, and it's why Oxigen
deliberately removes redundant constructs and renames familiar ones: when there
is exactly one clear way to express something, intent is unambiguous.

These choices are purpose-built, not accidental. Some will feel unfamiliar coming
from other languages — that's intentional. Favor the simpler, more direct form:
if a line can be said more plainly, say it that way.

### One Construct per Job

Oxigen drops overlapping constructs so there's a single, obvious tool for each
job. Each has its own section below with the details.

| Instead of | Oxigen uses |
|------------|-------------|
| `if` / `else if` / `else` | `option` |
| `for` loops | `each` over an iterable |
| `while` loops | `repeat when` (loop while true) / `repeat unless` (loop while false) |
| `break` / `continue` | `stop` / `skip` |
| explicit `return` | implicit return — `give` only for a rare early exit |

### Declarations Encode Behavior

A variable's form states how it behaves — there's no `const`, `let`, or `mut`
keyword. Mutability comes from how it's declared; the type supplies the initial
value and drives automatic conversion where it's unambiguous, so values become
what the context intends without manual ceremony.

| Form | Behavior |
|------|----------|
| `x <int>` | `x` initialized to the type's zero value (`0`) |
| `x <int> := 0` | mutable, type-locked |
| `x <int> = 0` | constant — immutable and type-locked |

### Angle Forms Carry Meaning

The `<...>` angle forms are part of the same idea: one compact, consistent syntax
that marks a type or an effect inline instead of spelling it out. Types
(`<int>`, `<Config>`), the Error || Value system (`<Error<tag>>`, `<Value>`), and
effects like `<log>`, `<fail>`, `<guard>`, and `<test>` all read as a single
intentional mark rather than boilerplate.

## Variable Declarations

Oxigen has four declaration forms. Reach for the most explicit one that fits:

| Form | Meaning |
|------|---------|
| `x <type>` | Type-locked, zero-initialized, value-mutable |
| `x <type> := value` | Type-locked with an initial value, value-mutable |
| `x <type> = value` | Type-locked **and immutable** (a constant) |
| `x := value` | Dynamic — both type and value can change (avoid) |

### Always Declare the Expected Type

A type-locked variable prevents accidental type changes that cause subtle bugs,
and the declared type documents intent. Prefer a type even when initializing
with `:=`. Zero-initialized variables (`x <type>`) are the cleanest choice when
the default value is what you want.

```oxi
// Good — type-locked, zero-initialized (defaults: 0, "", [], False)
counter <int>
names <array>

// Good — type-locked with an initial value
limit <int> := 100
result <int> := compute()

// Avoid — dynamic; type and value can both drift
counter := 0
result := compute()
```

### Reserve Dynamic `:=` for Genuinely Unknown Types

Use a bare `:=` only when neither the type nor a sensible primitive can be known
ahead of time — and even then, prefer `<generic>` to say "this is intentionally
dynamic" rather than leaving a reader guessing whether it was deliberate.

```oxi
// Acceptable — a generic callback result whose type isn't known
acc := initial

// Better — explicit that the type is dynamic
acc <generic> := initial

fun identity(val <generic>) {
    result <generic> := val
    result
}
```

### Change a Type by Shadowing, Not Reassignment

To change a variable's type, shadow it with a new binding. This keeps the change
explicit instead of silently mutating the type in place.

```oxi
// Good — shadowed with a clear type change
x <int> := 42
x <str> := str(x)

// Avoid — silent type change
x := 42
x := "now a string"
```

### Convert with `<type>`

Use explicit `<type>` conversion so the intent is visible at a glance.

```oxi
value <int> := int("42")
ratio <float> := float(count) / float(total)
```

### Constants

Oxigen has no `const` keyword. A type-locked **strict** assignment (`=`) is both
type-locked and immutable.

```oxi
PI <float> = 3.14159
MAX_RETRIES <int> = 5
APP_NAME <str> = "Oxigen"

PI = 3.0          // error — cannot reassign an immutable variable
PI := "something" // error — type is locked
```

If a value is meant to change, declare it mutable with `<type> :=` instead. You
can override an immutable binding by shadowing, but only when the intent is clear.

```oxi
// Good — mutable because it's expected to change
retries <int> := 5
retries = retries - 1

// Acceptable — shadowing to override with clear intent
MAX <int> = 100
MAX <int> := 200

// Avoid — walrus silently overriding an immutable binding
MAX <int> = 100
MAX := 200
```

## Scripts and Modules

### Use `main` for Script Entry Points

Keep reusable definitions at the top level and put executable script logic inside
`main`. `main` is skipped when a file is brought in with `introduce`, so a file
can serve as both a runnable script and an importable module.

```oxi
introduce strings

fun banner(name <str>) {
    strings.upper("hello, {name}")
}

main {
    println(banner("Oxigen"))
}
```

Top-level statements still run when a file is executed directly, but `main` is
the preferred home for script logic.

## Functions

### Named Functions Over Anonymous Functions

Use explicit named definitions (`fun name() {}`) for anything reusable. Reserve
anonymous functions for inline callbacks.

```oxi
// Good — named function, clear and reusable
fun calculate_area(width <int>, height <int>) {
    width * height
}

// Good — anonymous function as an inline callback
introduce array
doubled <array> := array.map([1, 2, 3], fun(x <int>) { x * 2 })

// Avoid — anonymous function bound to a name when a named function is clearer
calculate_area := fun(width, height) { width * height }
```

### Type Your Parameters

Always annotate parameter types — the signature should communicate what's
expected. Use union types (`<a> || <b>`) for genuine flexibility.

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

When a parameter has a natural default, declare it in the signature instead of
checking for `None` in the body.

```oxi
// Good — default value in the signature
fun connect(host <str>, port <int> = 8080) {
    println("Connecting to {host}:{port}")
}

// Avoid — checking None manually
fun connect(host <str>, port? <int>) {
    actual_port <int> := option { port == None -> 8080, port }
    println("Connecting to {host}:{actual_port}")
}
```

Use `?` (optional parameters) only when there's genuinely no sensible default and
the caller may or may not provide a value.

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

When a call has several same-typed parameters or boolean flags, use named
arguments so the call documents itself. They also let you skip defaults.

```oxi
fun create_user(name <str>, email <str>, admin <bool> = False) { ... }

// Good — intent is clear
create_user("Alice", email="alice@example.com", admin=True)

// Avoid — positional args are ambiguous
create_user("Alice", "alice@example.com", True)

// Good — skip port, only set tls
fun connect(host <str>, port <int> = 8080, tls? <bool>) { ... }
connect("example.com", tls=True)
```

### Returns Are Implicit — `give` Is a Last Resort

A function returns the value of its last expression. Because Oxigen has no
semicolons, lean on this rather than an explicit return: write the value, don't
announce it. Explicit return exists as `give` — named differently from `return`
on purpose, so it isn't reached for out of habit. Use it only for the rare early
exit where it genuinely reads clearer; to break a loop, prefer `stop`.

```oxi
// Good — the option is the function body; its value is returned
fun is_even(val <int>) {
    option {
        val % 2 == 0 -> True,
        False
    }
}

// Not recommended — give adds a keyword and clutters with no benefit
fun is_even(val <int>) {
    option {
        val % 2 == 0 -> give True,
        give False
    }
}
```

## Conditionals and Pattern Matching

### Use `option` for Conditional Logic

`option` is the tool for conditional branching on expressions — inline decisions,
ternary-style expressions, and multi-arm conditionals within a single file.

```oxi
// Good — multi-arm option
status <str> := option {
    age < 13 -> "child"
    age < 20 -> "teenager"
    "adult"
}

// Good — ternary-style option
sign <int> := option { x >= 0 -> 1, -1 }
```

### Use `pattern` + `choose` for Reusable Matching

Patterns defined with `pattern` are named, reusable conditions meant to be shared
across files through the module system; match against them with `choose`. For a
condition that only matters in one place, use `option` instead.

```oxi
// Good — reusable patterns shared across files
pattern is_even(n) when n % 2 == 0
pattern is_odd(n) when n % 2 != 0

each n in range(10) {
    choose n {
        is_even -> println(n, "is even")
        is_odd -> println(n, "is odd")
    }
}

// Avoid — pattern + choose for a one-off condition
pattern is_positive(x) when x > 0
choose value {
    is_positive -> println("positive")
    else -> println("not positive")
}

// Better — option for one-off conditions
option {
    value > 0 -> println("positive")
    println("not positive")
}
```

## Structs

### Instantiate with `variable <Struct>`

Create struct instances with the `variable <Struct>` form rather than writing
`new()` factory functions. The struct definition is the source of truth for field
types, so declaring the type at the instantiation site keeps it visible and lets
the struct enforce its own constraints.

```oxi
struct Config {
    host <str>
    port <int>
    debug <bool>
}

// Good — type is explicit, fields are checked by the struct
settings <Config> := Config("localhost", 8080, False)

// Zero-initialized struct
defaults <Config>

// Avoid — wrapping instantiation in a new() function
fun new_config(host <str>, port <int>, debug <bool>) {
    Config(host, port, debug)
}
```

### Inherit Behavior, Don't Redeclare It

A child struct automatically has its parent's methods. Only add new methods or
override the ones that need to differ — never copy a parent method unchanged.

```oxi
struct Animal {
    name <str>
    sound <str>
}

Animal includes {
    fun speak() { println(self.name, "says", self.sound) }
    fun describe() { println("Animal:", self.name) }
}

struct Dog(Animal) {
    breed <str>
}

// Good — add new behavior, override only what's different
Dog includes {
    fun fetch() { println(self.name, "fetches the ball!") }
    fun describe() { println("Dog:", self.name, "- Breed:", self.breed) }
}

// Avoid — redeclaring speak() when the parent's version already works
Dog includes {
    fun speak() { println(self.name, "says", self.sound) }
    fun fetch() { println(self.name, "fetches the ball!") }
}
```

### Structs Are for Shared Behavior, Not One-Off Logic

Use a struct to group related state and behavior. If there's no real state — just
a single operation — a plain function is clearer.

```oxi
// Good — groups related state and behavior
struct HttpResponse {
    status <int>
    body <str>
    headers <map>
}

HttpResponse includes {
    fun is_ok() { self.status >= 200 and self.status < 300 }
    fun is_error() { self.status >= 400 }
}

// Avoid — a struct wrapping a single function with no real state
struct Greeter {
    name <str>
}
Greeter includes {
    fun greet() { println("Hello, {self.name}!") }
}

// Better — just a function
fun greet(name <str>) {
    println("Hello, {name}!")
}
```

### No Empty or Untyped Structs

Every struct field should be typed. Don't create empty structs or leave fields
untyped — the one exception is a `<generic>` field on a parent meant to be
specialized by its children.

```oxi
// Good — all fields typed
struct User {
    name <str>
    age <int>
    active <bool>
}

// Good — generic parent, children add specifics
struct Container {
    value <generic>
}
struct IntBox(Container) {
    label <str>
}

// Avoid — empty struct
struct Utils {}

// Avoid — untyped fields
struct User {
    name
    age
}
```

### Encapsulate with `hide`

Oxigen is public by default; everything on a struct is accessible unless marked
with `hide`. Hide only genuine implementation details, and keep anything that's
part of the public interface visible.

```oxi
struct Account {
    name <str>
    hide balance <int>
    hide pin <int>
}

Account includes {
    fun deposit(amount <int>) { self.balance = self.balance + amount }
    fun get_balance() { self.balance }
}

a <Account> := Account("Alice", 1000, 1234)
println(a.name)          // works — public
println(a.get_balance()) // works — public method
println(a.balance)       // error — hidden field
```

### Always Use `self.field` in Methods

Inside methods, access fields through `self.field`. Oxigen allows the bare name,
but explicit `self` makes it clear you're touching struct state, not a local.
This matters most in setters, where a parameter shares a field's name.

```oxi
struct Player {
    name <str>
    score <int>
}

// Good — explicit self
Player includes {
    fun add_score(points <int>) { self.score = self.score + points }
    fun set_name(name <str>) { self.name = name }
}

// Avoid — implicit field access
Player includes {
    fun add_score(points <int>) { score = score + points }
}
```

## Formatting

### Pick a Block Style and Stick With It

Braces `{}` are the default. Indentation blocks via `#[indent]` are welcome too —
but commit to one style across a project; don't mix them.

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

### Use String Interpolation Over Concatenation

Both work, but interpolation reads cleaner.

```oxi
name <str> := "Oxigen"
version <int> := 1

// Good — interpolation
println("Welcome to {name} v{version}!")

// Avoid — concatenation
println("Welcome to " + name + " v" + str(version) + "!")
```

### Use Snake Case

Use `snake_case` for variables, functions, and fields. Struct names are the
exception — they use `PascalCase`.

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
```

## Loops

### `each` for Iterables, `repeat` for Conditions

Use `each` to iterate a known collection. Use `repeat when` / `repeat unless` for
condition-based loops that could run indefinitely.

```oxi
// Good — each over a collection
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

### Pick `when` or `unless` for Readability

Choose whichever makes the condition read most directly — favor the form that
avoids a double negative. It's about readability, not a rigid rule.

```oxi
// Good — unless reads cleaner here
repeat unless i >= size { ... }

// Good — when reads cleaner here
repeat when retries > 0 { ... }
```
