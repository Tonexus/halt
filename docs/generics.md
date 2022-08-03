## Generics

There are two forms of generic types, universally quantified types and
existentially quantified types.

### Universal types

Universal types allow parameterized algebraic data types, generic functions, and
interfaces.

#### Parameterized algebraic data types

Arrays are the most basic parameterized algebraic data type, whose parameters
are the data type of the members and the number of members. Parameterized types
may also take constant values as parameters.

#### Generic functions

Universal quantification allows for functions to act on parameterized types.

```
reduce[A, B, USize c]: (
    (arr: Arr[A, c], init: B, fn: (A, B) => B) -> B
)[A, B, const USize c] := \|{
    let acc = init;
    loop x from arr {
        acc = fn(x, acc);
    }
    return acc;
}
```

#### Interfaces

Interfaces reflect how a type interacts with other types, and can be defined
regardless of the underlying data. Such interactions are defined by methods
that act on the underlying data type.

```
Interface[T] := (
    foo: T -> U32,
    bar: T -> Arr[U8, 10],
)[T];

InterfaceTwo[T] := (
    baz: T -> Bool,
)[T];

// interfaces can be combined
InterfaceBoth[T] := (
   one: Interface[T],
   two: InterfaceTwo[T],
);
```

An interface can be implemented for any type parameter as follows:

```
// Interface as defined above

// _default refers to the interface a type is implict interface implementation
// used, but an interface for a type may have multiple implementations
_default: Interface[()] := (
    foo = \|{return 42;},
    bar = \|{return "void\n\n\n\n\n\n"},
);

other: Interface[()] := (
    foo = \|{return 0;},
    bar = \|{return "0000000000";},
);

_default: Interface[U32] := (
    foo = \|{return self;},
    bar = \|{return "number\n\n\n\n"},
);
```

Interfaces can be referred to abstractly using existential types.

### Existential types

Existential types allow the hiding of type information in algebraic data types.
Generally, existential types include a value of an unknown type and functions
from that unknown type to known types. When supplied a value of an existential
type, the consumer does not need to know and cannot know the specific type that
is being existentially quantified.

#### Abstract types

Abstract types are how interfaces are referred to without their type parameter
nor their implementation.

```
// Interface as defined above

AbstractInterface := (?T, Interface[?T])[?T];

// This is equivalent to AbstractInterface := Abstr[Interface]
// as the type Abstr is defined as follows:

// Abstr[I[_]] := (?T, I[?T])[I[_], ?T];

// There is non-hiding version defined as

// Concr[I[_], T] := (T, I[T])[I[_], T];
```

Abstract types have syntactic sugar for calling their methods as follows:

```
// Interface, _defaults, and other as defined above

func: (a: Abstr[Interface]) -> U32 := \{
    // a@b(c) is equiv to a._1.b(a._0, c)
    return a@foo();
};

    ...

    let x: U32 = func(()); // x holds 42
    let y: U32 = func(71); // y holds 71
    let z: U32 = func(((), other)) // z holds 0
```

