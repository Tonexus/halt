## Algeraic Data types

Halt's data types are all structural compositions via products and sums. A product of types
`A` and `B` may be expressed as `(a: A; b: B)`, and values of type `(a: A; b: B)` are
constructed from a value of type `A` *and* a value of type `B`.  Similarly, a
sum of types `A` and `B` may be expressed as `[a: A; b: B]`, and values of type
`[a: A, b: B]` are constructed from a value of type `A` *or* a value of type `B`.

### Primitive and keyword data types

- `Bool` - taking on either the value "true" or the value "false"

- `N1`, `N2`, ... - representing enums of n choices

- `U8`, `U16`, `U32`, `U64` - representing unsigned integers of their respective size

- `S8`, `S16`, `S32`, `S64` - representing signed integers of their respective size

- `F32` - representing 32-bit IEEE floating point numbers

### Product types

In Halt, general product types have the following forms:

```
// unit
Unit := ();

// explicitly labeled (struct/record)
Foo := (label_1: Type1; label_2: Type2; label_3: Type3);

// implicitly labeled (tuple)
Bar := (Type1; Type2; Type3);

// Bar is equivalent to (_0: Type1; _1: Type2; _2: Type3)

...

	// instantiation in imperative code
	let foo: Foo = (label_1 = value_1, label_2 = value_2, label_3 = value_3);
	let bar: Bar = (value_1, value_2, value_3);
```
TODO: move to special types section (also include coproduct, accessor)
An array is expressed as follows:

### Sum types

In Halt, general sum types have the following forms:

```
// bottom type
Bot := [];

// enum/choice
Foo := [label_1: Type1; label_2: Type2; label_3: Type3]

...

	// instantiation in imperative code for value: Type2
	let foo: Foo = [label_2 = value];
```

### Type equivalencies and automatic type coercions

Generally, types with the same structure (same labels for the same types) are
equivalent, and implicitly labeled types may be treated as explicitly labeled.

#### Enums and keyword equivalencies

Most keyword types are simply enums with additional functionality provided by
interfaces (TODO page on this). For instance, `N2`, defined as `[(), ()]` is
equivalent to `Bool`, and `N256` is equivalent to `U8` and `S8`.

#### Singleton canonicalization

Any singleton type is equivalent to the unwrapped type. In particular, `T` is the
same as `(T)`, which is the same as `((T))`, and so on. An
interesting implication of canonicalization is that every function may be
treated as an arity-1 function with its sole parameter being a tuple.
Furthermore, parentheses can both be naturally used for both
grouping expressions and for constructing products, as the
singleton tuple `(5 + 5)` has type `(U32)`, which is the same as `U32`.

#### Width subtyping

There is a natural subtyping relationship where a product with more fields may be used
in any place that expects fewer fields. Similarly, a sum with fewer variants may be
used in any place that expects more variants.

Thus, we have the width subtype relationship based on structure given by `(A; B; C) <: (A; B)`,
`(a: A; b: B; c: C) <: (a: A; b: B)`, and `[a: A; b: B] <: [a: A; b: B, c: C]`.

More on subtyping in `polymorphism.md`.