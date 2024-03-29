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

// Bar is equivalent to (_0: Type1, _1: Type2, _2: Type3)

// instantiation
let foo: Foo = (label_1 = value_1, label_2 = value_2, label_3 = value_3);
let bar: Bar = (value_1, value_2, value_3);
```
TODO: move to special types section (also include coproduct, accessor)
An array is expressed as follows:

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

// Bar is equivalent to [_0: Type1, _1: Type2, _2: Type3]

// instantiation for value: Type2
let foo: Foo = [label_2 = value];
let bar: Bar = [value];
// note that if types are redundant, the explicit form must be used!
// bar = [_1 = value];
```

### Type equivalencies and automatic type coercions

Generally, types with the same structure (same labels for the same types) are
equivalent, and implicitly labeled types may be treated as explicitly labeled.

#### Enums and keyword equivalencies

Most keyword types are simply enums with additional functionality provided by
interfaces (TODO page on this). For instance, `N2`, defined as `[(), ()]` is
equivalent to `Bool`, and `N256` is equivalent to `U8` and `S8`.

#### Singleton canonicalization

Any implicitly labeled singleton type can be coerced to and from its constituent
type. In other words, the types `A`, `(A)`, `(_1: A)`, `[A]`, and `[_1: A]` may
all be coerced to each other. In particular, field access uses the "canonical"
version of the type with all singleton compositions stripped away. Another
interesting implication of canonicalization is that every function may be
treated as an arity-1 function with its sole parameter being a tuple.
Furthermore, parentheses and square brackets can both be naturally used for both
grouping expressions and for constructing product and sum values, as the
singleton tuple `(5 + 5)` has type `(U32)`, but may be coerced to `U32`.

#### Sum up-coercions

Canonicalization may seem to contradict the instatiation of implicitly labelled
sums. In the example with sums, `[value]` must have type `[Type2]`, which is
equivalent to `Type2`, but it is assigned to a variable of type
`[Type1, Type2, ...]`. In Halt, a choice may be automatically coerced to a
choice with more summands as long as the source tags and types are a subset of
the destination tags and types.

#### Product down-coercions

While sums may coerce to larger sums, the reverse is true for products. A struct
may automatically be coerced to a struct with fewer factors as long as the
source fields and types are a superset of the destination fields and types.

