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

### Zero-Value Instantiation

Declare a struct-typed variable without providing values. All fields are initialized to their type's zero value:

```oxi
p <Person>
println(p.name)
println(p.age)
```

See [Zero-Value Declaration](#structs-as-types) below for full details.

## Accessing Fields

Use dot notation to access fields:

```oxi
p := Person("Alice", 30)
println(p.name)
println(p.age)
```

## Mutating Fields

Assign to a field using dot notation with `=`:

```oxi
p := Person("Alice", 30)
p.age = 31
println(p.age)
```

Field mutation is type-checked — assigning a value of the wrong type produces an error:

```oxi
p := Person("Alice", 30)
p.age = "thirty"
```

Field mutation works with postfix guards:

```oxi
p.name = "Bob" when should_update
```

## Hidden Fields

The `hide` keyword marks a field as internal. Hidden fields are accessible inside methods but not from outside the struct:

```oxi
struct Person {
    hide fname <str>
    hide lname <str>
    hide age <int>
}

Person contains {
    fun set_name(fn <str>, ln <str>) {
        self.fname = fn
        self.lname = ln
    }
    fun get_name() { self.fname + " " + self.lname }
}

p <Person>
p.set_name("John", "Doe")
println(p.get_name())
```

Use `hide` to encapsulate implementation details and control access through methods.

## Methods

Attach methods to a struct using a `contains` block:

```oxi
Person contains {
    fun greet() { println(name) }
    fun is_adult() { age >= 18 }
}
```

### Implicit Self

Inside a method, all fields of the instance are accessible directly by name — there is no explicit `self` parameter. When you call `p.greet()`, the method body can reference `name` and `age` as if they were local variables:

```oxi
struct Person {
    name <str>
    age <int>
}

Person contains {
    fun greet() { name + " is " + str(age) + " years old" }
}

p := Person("Alice", 30)
println(p.greet())
```

### Explicit `self`

You can also use `self` to explicitly reference the instance. This is required when you want to assign to a field within a method:

```oxi
Person contains {
    fun set_name(new_name <str>) {
        self.name = new_name
    }
    fun set_age(new_age <int>) {
        self.age = new_age
    }
}

p <Person>
p.set_name("Alice")
p.set_age(30)
```

Use `self.field = value` to mutate the instance's field from within a method. Plain field access (`name`, `age`) reads the field value; `self.field = value` writes to it.

### Methods with Parameters

Methods can take additional parameters beyond the implicit fields:

```oxi
Person contains {
    fun greet_with(greeting) { greeting + ", " + name }
}

p := Person("Alice", 30)
println(p.greet_with("Hello"))
```

### Returning Values

Methods can use `give` for early returns, or implicitly return the last expression:

```oxi
Person contains {
    fun birth_year(current_year) { give current_year - age }
}

p := Person("Alice", 30)
println(p.birth_year(2026))
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
println(a.name)
println(a.nationality)
```

Named instantiation works the same way:

```oxi
a := American { name: "John", age: 25, nationality: "USA" }
```

### Multi-Level Inheritance

Inheritance can chain through multiple levels:

```oxi
struct Person {
    hide fname <str>
    hide lname <str>
    hide age <int>
}

struct Nationality(Person) {
    hide country <str>
    hide title <str>
}

struct American(Nationality) {
    state <str>
}
```

`American` inherits all fields from `Nationality`, which inherits all fields from `Person`. The full field order is: `fname`, `lname`, `age`, `country`, `title`, `state`.

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
println(a.greet())
```

### Overriding Methods

Define a `contains` block on the child struct to add or override methods:

```oxi
American contains {
    fun greet() { name + " from " + nationality }
}

a := American("John", 25, "USA")
println(a.greet())
```

The child's `contains` block only affects the child. The parent's methods remain unchanged for parent instances.

### Adding Methods at Each Level

Each level in an inheritance chain can define its own methods:

```oxi
Person contains {
    fun set_name(fn <str>, ln <str>) {
        self.fname = fn
        self.lname = ln
    }
}

Nationality contains {
    fun set_country(country <str>) {
        self.country = country
    }
}

American contains {
    fun print_info() {
        println("Name: ", self.fname, self.lname)
        println("Age: ", self.age)
        println("Country: ", self.country)
        println("State: ", self.state)
    }
}
```

## Zero-Field Structs

Structs with no fields are valid:

```oxi
struct Empty {
}

e := Empty()
println(type(e))
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
println(o.make_inner().val)
```

## Structs as Types

Struct names can be used as type annotations, just like built-in types. This locks a variable to a specific struct type.

### Strict Declaration

```oxi
p <Person> = Person("Alice", 30)
p = Person("Bob", 25)
```

### Walrus Declaration

```oxi
p <Person> := Person("Alice", 30)
p = Person("Bob", 25)
p = 42
```

### Zero-Value Declaration

Declare a typed variable with all fields set to their zero values. Both forms are equivalent:

```oxi
p <Person>
// or equivalently: p as <Person>

println(p.name)
println(p.age)

p.name = "Alice"
p.age = 30

p = Person("Bob", 25)
```

### Different Struct Types

A variable locked to one struct type cannot be reassigned to a different struct:

```oxi
struct Person { name <str> }
struct Dog { name <str> }

p <Person> := Person("Alice")
p := Dog("Rex")
```

### Struct-Typed Fields

Struct fields can use other structs as their type:

```oxi
struct Address {
    city <str>
}

struct Person {
    name <str>
    addr <Address>
}

a := Address("NYC")
p := Person("Alice", a)
println(p.addr.city)
```

## Introspection

The `type()` built-in returns the struct name for instances:

```oxi
struct Person {
    name <str>
    age <int>
}

p := Person("Alice", 30)
println(type(p))
```

Use `is_mut()` and `is_type_mut()` on fields:

```oxi
m <Math> = Math { num_one: 10, num_two: 20 }
println(is_mut(m.num_one))
println(is_type_mut(m.num_one))
```

## Error Handling

OxigenLang produces clear errors for common mistakes:

```oxi
Person("Alice")
Person { name: "Alice", height: 170 }
p.age = "thirty"
Person("Alice", "thirty")
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
struct Animal {
    name <str>
    sound <str>
}

struct Dog(Animal) {
    breed <str>
}

Animal contains {
    fun speak() { name + " says " + sound }
}

Dog contains {
    fun info() { name + " (" + breed + ")" }
}

dog := Dog("Rex", "Woof", "Labrador")
println(dog.speak())
println(dog.info())

dog.sound = "Bark"
println(dog.speak())
```

See also:
- [Type System](type_system.md) — type annotations and zero values
- [Functions](functions.md) — function syntax used in methods
- [Variables and Assignments](variables.md) — typed variable declarations
