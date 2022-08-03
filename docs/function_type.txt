## Function-like types

There are two primary function-like types, the basic function type and the
closure type. However, generally speaking, a function-like type is any
structural type with a function as a constituent type. Function-like types are
not first class, as their values cannot be dynamically reassigned, ensuring that
every function call is compile-time constant.

(Maybe more to come? Conflicts with UFCS.)

A basic function `f` has type `f: A -> B` if it takes an argument of type `A`
and returns a value of type `B`.

```
// function that takes a product type of two unsigned 32 bit integers and
// returns an unsigned 32 bit integer
foo: (a: U32, b: U32) -> U32 := \|{
    ...
}

    // foo can be called in multiple ways:
    // call on product type directly
    let in = (a = 1, b = 2);
    let out = foo in;

    // construct product type at call site
    let in_1 = 1;
    let in_2 = 2;
    let out = foo(a = in_1, b = in_2)

    // use implicit tuple to product type casting
    let out = foo(in_1, in_2);

    // uniform function call syntax is supported
    let out = in_1.foo(in_2);

    // alternate ufcs
    let out = in_1.foo in_2;
```

A closure `c` has type `c: A => B` if it takes an argument of type `A` and
returns a value of type `B` while closing over some other unknown values of
unknown types. `A => B` is shorthand for the existentially quantified generic
type `(?, (A, ?) -> B)[?]` that closes over a variable with type parameter `?`.
(See generics for more.)

```
foo: (a: U32) -> U32 := \|{
    // closes over a
    bar: (b: bool) => U32 := \|{
        if b {
	    return a;
	} else {
	    return 0;
	}
    }
}
```

Note that the type of a closure can be inferred. Any variable in a closure that
is not bound by a let statement is "free", and must be an argument or be closed
over. Any free variable that is bound in a parent scope is implicitly closed
over, unless the closure's type signature explicitly denotes the variable as an
argument. Arguments are implictly ordered by usage. A basic function is simply a
closure that does not close over an empty product type, as `((), (A, ()) -> B)`
is simply `A -> B`. As such, higher order functions are usually defined as
taking closures.

```
foo: ((a: U32, b: Arr[U32, ?]) -> Arr[U32, ?])[const ?] := \|{
    // map: ((Arr[?A, C], ?A => ?B) -> Arr[?B, C])[?A, ?B, const C]
    return b.map \|{  // from type of map, compiler infers type of closure
        return x + a; // closure infers that c is an argument
    };
}
```



