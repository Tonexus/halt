## Data types

Data types fall into three categories: structural types, behavioral types, and
concrete types.

### Structural types

Structural types reflect the composition of smaller data structures into larger,
more complex structures. Structural types are generally algebraic data types.

#### Primitive data types

The basic building blocks of structural types are the true primtive data types
and the pseudoprimitive data types. The true primitive data types consist of

* `()` - the unit type

* Reference types - see other page for more details (to be added)

The pseudoprimitive data types include

* `Bool` - taking on either the value "true" or the value "false"

* `U8`, `U16`, `U32`, `U64` - representing unsigned integers of their respective size

* `S8`, `S16`, `S32`, `S64` - representing signed integers of their respective size

* `Float` - representing IEEE floating point numbers

#### Compositional types

Primitive data types can be combined via two compositional data types, the
product type and the sum type.

##### Product types

Product types have the following form:

```
// type itself
Foo := (
    label_1: Type1,
    label_2: Type2,
    ...
)

...

    // initialization, inferring types of bar and baz
    let foo = (label_1 = bar, label_2 = baz, ...)
```

A product type represents a type that includes all of the constituent types.
Halt has syntactic sugar for two important varieties of product types, the tuple
and the array. A tuple is expressed as follows:

```
// type itself
Foo := (Type1, Type2, ...)

...

    // initialization, inferring types of bar and baz
    let foo = (bar, baz, ...)
```

The above tuple type is equivalent to the following product type:

```
(
    _1: Type1,
    _2: Type2,
    ...
)
```

Product types are ordered by their fields, and can be implicitly cast to a tuple
and from a tuple with the same order (though their types are not the same).

```
let foo: (a: U32, b: U32, c: U32) = (a = 1, b = 2, c = 3);
let bar: (U32, U32, U32) = foo; // bar = (1, 2, 3)
let baz: (x: U32, y: U32, z: U32) = bar; // baz = (x = 1, y = 2, z = 3)
// cannot implicitly cast from foo to bar directly
```

An array is expressed as follows:

```
Arr[Type, length]
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

Furthermore, the array can be initialized as a tuple of uniform type or using
the `map` higher order function (see generics, typing for more) as follows:

```
    // if val1, val2, val3 share the same type, foo_tuple is an array
    let arr_tuple = (val_1, val_2, val_3);

    // closure closes over state variables in initializing array
    let n = &mut 0;
    let arr_fn: Arr[U32, len] = ().map \|{
        *n = *n + 1;
        return *b;
    };
```

##### Sum types

Sum types have the following form:

```
// type itself
Foo := \+{
    label_1: Type1,
    label_2: Type2,
    ...
}

...

    // initialization (to variant label_2)
    let foo = \+{label_1: Type1, label_2: Type2 = bar, label_3: Type3, ...};
```


A sum type represents a type that is one of the constituent types. For example,
the Bool primitive type can be treated as the following sum type:

```
\+{
    true:  (),
    false: (),
}
```

Similarly, each of the pseudoprimitive data types can be represented as a sum
type.

##### Compositional type equivalencies

Any singleton compositional type is equivalent to the constituent type. For
instance, all of the following types are equivalent, regardless of labels:

```
(
    foo: U32,
)
```

```
\+{
    foo: U32,
}
```

```
\+{
    bar: (
        baz: U32,
    ),
}
```

```
((((U32))))
```

```
Arr[U32; 1]
```

Furthermore, a product type of any type and the unit type is equivalent to the
same product type without the unit type. Thus, the following types are
equivalent to the unit type:

```
(((()), ()), ())
```

```
(
    foo: (),
    bar: (),
    baz: (),
)
```

```
Arr[Arr[Arr[(), 100], 100], 100]
```

Similarly, empty sum type is a special type that cannot take on any values.

