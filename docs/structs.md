# OxigenLang Structs

OxigenLang supports structs — composite data types with typed fields, inheritance, and methods. Structs provide a way to group related data together and attach behavior to it.

## Defining a Struct

Use the `struct` keyword followed by a name and a list of typed fields:

```oxi
struct Person {
    name <str>
    age <int>
}
```

Each field requires a type annotation using the same `<type>` syntax as typed variables. See the [Type System](type_system.md) guide for available types.

## Creating Instances

### Positional Instantiation

Pass values in the order fields are declared:

```oxi
p := Person("Alice", 30)
```

### Named Instantiation

Specify fields by name using `Name { field: value }` syntax:

```oxi
p := Person { name: "Alice", age: 30 }
```

Both forms perform strict type checking — the values must match the declared field types.

## Accessing Fields

Use dot notation to access fields:

```oxi
p := Person("Alice", 30)
println(p.name)    # Alice
println(p.age)     # 30
```

## Mutating Fields

Assign to a field using dot notation with `=`:

```oxi
p := Person("Alice", 30)
p.age = 31
println(p.age)     # 31
```

Field mutation is type-checked — assigning a value of the wrong type produces an error:

```oxi
p := Person("Alice", 30)
p.age = "thirty"   # error: type mismatch: field 'age' expects INTEGER, got STRING
```

## Methods

Attach methods to a struct using a `contains` block:

```oxi
Person contains {
    fun greet() { println(name) }
    fun is_adult() { age >= 18 }
}
```

### Implicit Self

Inside a method, all fields of the instance are accessible directly by name — there is no explicit `self` parameter. When you call `p.greet()`, the method body can reference `name` and `age` as if they were local variables.

```oxi
struct Person {
    name <str>
    age <int>
}

Person contains {
    fun greet() { name + " is " + str(age) + " years old" }
}

p := Person("Alice", 30)
println(p.greet())   # Alice is 30 years old
```

### Methods with Parameters

Methods can take additional parameters beyond the implicit fields:

```oxi
Person contains {
    fun greet_with(greeting) { greeting + ", " + name }
}

p := Person("Alice", 30)
println(p.greet_with("Hello"))   # Hello, Alice
```

### Returning Values

Methods can use `give` for early returns, or implicitly return the last expression:

```oxi
Person contains {
    fun birth_year(current_year) { give current_year - age }
}

p := Person("Alice", 30)
println(p.birth_year(2026))   # 1996
```

## Inheritance

A struct can inherit from a parent by specifying the parent name in parentheses:

```oxi
struct Person {
    name <str>
    age <int>
}

struct American(Person) {
    nationality <str>
}
```

The child struct gets all parent fields first, followed by its own. Instantiation requires all fields in order — parent fields, then child fields:

```oxi
a := American("John", 25, "USA")
println(a.name)          # John
println(a.nationality)   # USA
```

Named instantiation works the same way:

```oxi
a := American { name: "John", age: 25, nationality: "USA" }
```

### Inheriting Methods

A child struct automatically inherits all methods defined on the parent:

```oxi
Person contains {
    fun greet() { name }
}

struct American(Person) {
    nationality <str>
}

a := American("John", 25, "USA")
println(a.greet())   # John — inherited from Person
```

### Overriding Methods

Define a `contains` block on the child struct to add or override methods:

```oxi
American contains {
    fun greet() { name + " from " + nationality }
}

a := American("John", 25, "USA")
println(a.greet())   # John from USA — overridden method
```

The child's `contains` block only affects the child. The parent's methods remain unchanged.

## Zero-Field Structs

Structs with no fields are valid:

```oxi
struct Empty {
}

e := Empty()
println(type(e))   # Empty
```

## Dot Chaining

If a method returns a struct instance, you can chain dot access:

```oxi
struct Inner {
    val <int>
}

struct Outer {
    name <str>
}

Outer contains {
    fun make_inner() { Inner(42) }
}

o := Outer("test")
println(o.make_inner().val)   # 42
```

## Introspection

The `type()` built-in returns the struct name for instances:

```oxi
struct Person {
    name <str>
    age <int>
}

p := Person("Alice", 30)
println(type(p))    # Person
```

## Error Handling

OxigenLang produces clear errors for common mistakes:

```oxi
# Wrong number of arguments
Person("Alice")              # error: struct Person has 2 fields, got 1 arguments

# Unknown field in named instantiation
Person { name: "Alice", height: 170 }   # error: unknown field 'height' for struct Person

# Type mismatch on field assignment
p := Person("Alice", 30)
p.age = "thirty"            # error: type mismatch: field 'age' expects INTEGER, got STRING

# Type mismatch on instantiation
Person("Alice", "thirty")   # error: type mismatch for field 'age': expected INTEGER, got STRING
```

## Indentation Mode

Structs and `contains` blocks work with indentation mode:

```oxi
#[indent]
struct Person:
    name <str>
    age <int>

Person contains:
    fun greet():
        name
```

## Complete Example

```oxi
# Define structs
struct Animal {
    name <str>
    sound <str>
}

struct Dog(Animal) {
    breed <str>
}

# Add methods
Animal contains {
    fun speak() { name + " says " + sound }
}

Dog contains {
    fun info() { name + " (" + breed + ")" }
}

# Create instances
dog := Dog("Rex", "Woof", "Labrador")
println(dog.speak())   # Rex says Woof — inherited from Animal
println(dog.info())    # Rex (Labrador) — Dog's own method

# Mutate fields
dog.sound = "Bark"
println(dog.speak())   # Rex says Bark
```
