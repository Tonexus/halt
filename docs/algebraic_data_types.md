## Algeraic Data types

Halt's data types are all compositions via products and sums. A product of types
`A` and `B` may be expressed as `(A, B)`, and values of type `(A, B)` are
constructed from a value of type `A` *and* a value of type `B`.  Similarly, a
sum of types `A` and `B` may be expressed as `[A, B]`, and values of type
`[A, B]` are constructed from a value of type `A` *or* a value of type `B`.
In addition to implicitly labeled compositions, Halt also allows for explicitly
labeled products and sums.

### Primitive and keyword data types

The only truly primitive data type in Halt is the reference type. Halt has
non-primitive keyword types, but it will be shown later that they may be treated
as compositions:

- `Bool` - taking on either the value "true" or the value "false"

- `N1`, `N2`, ... - representing enums of n choices

- `U8`, `U16`, `U32`, `U64` - representing unsigned integers of their respective size

- `S8`, `S16`, `S32`, `S64` - representing signed integers of their respective size

- `F32` - representing 32-bit IEEE floating point numbers

### Product types

Those familiar with tuples and structs will be familiar with product types. In
fact, in Halt, tuples are just implicilty labeled products, whereas structs are
explicitly labeled.

Product types are so named because the number of distinct values that may
inhabit a product type `(A, B)` is the product of the number values that may
inhabit `A` and the number of values that may inhabit `B`. Thus, the most
primitive product type is the empty product type, denoted as `()` and referred
to as the unit type. The unit type may be inhabited by exactly one value, the
unit value, which is also denoted `()`.

In Halt, general product types have the following forms:

```
// explicitly labeled
Foo := (label_1: Type1, label_2: Type2, label_3: Type3);

// implicitly labeled
Bar := (Type1, Type2, Type3);

// Bar is equivalent to (_1: Type1, _2: Type2, _3: Type3)

// instantiation
let foo: Foo = (label_1 = value_1, label_2 = value_2, label_3 = value_3);
let bar: Bar = (value_1, value_2, value_3);
```

TODO: outdated?

Product types are ordered by their fields, and can be implicitly cast to a tuple
and from a tuple with the same order (though their types are not the same).

```
let foo: (a: U32, b: U32, c: U32) = (a = 1, b = 2, c = 3);
let bar: (U32, U32, U32) = foo; // bar = (1, 2, 3)
let baz: (x: U32, y: U32, z: U32) = bar; // baz = (x = 1, y = 2, z = 3)
// cannot implicitly cast from foo to bar directly
```

TODO: move to special types section (also include coproduct, accessor)
An array is expressed as follows:

```
Foo := Arr{U32, N5};
```

The above array type is equivalent to the following product type with `length`
constituent types:

```
(
    _1: Type,
    _2: Type,
    ...
)
```

### Sum types

Those familiar with tagged unions may be familiar with sum types. Like their
product type cousins, sum types may also be explicitly labeled or implicitly
labeled. In Halt, an explicitly labeled sum is known as a choice. Someone give
me a good name for explicitly labeled sums.

As with product types, sum types are so named because the number of distinct
values that may inhabit a sum type `[A, B]` is the sum of the number values that
may inhabit `A` and the number of values that may inhabit `B`. Again, the most
primitive sum type is the empty product type, denoted as `[]` and referred to as
the bottom type. However, unlike the unit type, the bottom type cannot be
inhabited by any values. Thus, a statement containing a value of type bottom
must be unreachable.

In Halt, general sum types have the following forms:

```
// explicitly labeled
Foo := [label_1: Type1, label_2: Type2, label_3: Type3]

// implicitly labeled
Bar := [Type1, Type2, Type3]

// Bar is equivalent to [_1: Type1, _2: Type2, _3: Type3]

// instantiation for value: Type2
let foo: Foo = [label_2 = value];
let bar: Bar = [value];
// note that if types are redundant, the explicit form must be used!
// bar = [_2 = value];
```

### Type equivalencies and automatic type conversions

Generally, types with the same structure (same labels for the same types) are
equivalent, and implicitly labeled types may be treated as explicitly labeled.
However, the one exception is via the special `Unique` generic type.
Specifically, a type `Unique{T}` will have the same composition of types as the
original type `T`, but will be treated as a distinct type. Furthermore, unlike
normal generics, each type `Unique{T}` is also distinct from any other type
`Unique{T}` for the same base type `T`. (Unique{T} can be considered as creating
a singleton struct from `T` having a program-unique label.)

#### Enums and keyword equivalencies

All keyword types are simply unique enums with additional functionality provided
by interfaces (TODO page on this). For instance, `N2`, defined as `[(), ()]` is
equivalent to `Bool`, and `N256` is equivalent to `U8` and `S8`.

#### Singleton canonicalization

Any implicitly labeled singleton type is equivalent to its constituent type. In
other words, the types `A`, `(A)`, `(_1: A)`, `[A]`, and `[_1: A]` are all
equivalent, and may be treated as the type `A`. An interesting implication of
canonicalization is that every function is an arity-1 function with its sole
parameter being a tuple. Furthermore, parentheses and square brackets can be
naturally used for both grouping expressions and for constructing products and
sums, as the expression `[10 + (5 + 5)]` is equivalent to `[10 + 10]`, which is
equivalent to `20`.

#### Automatic conversion of sums

Canonicalization may seem to contradict the instatiation of implicitly labelled
sums. In the example with sums, `[value]` must have type `[Type2]`, which is
equivalent to `Type2`, but it is assigned to a variable of type
`[Type1, Type2, ...]`. In Halt, a choice may be automatically converted to a
choice with additional summands as long as the source labels and types match a
subset of the destination labels and types. Implicitly labeled sum values
without redundant types may be converted to an implicitly labeled sum of the
same types but with a different implicit label order.

