# Halt

**Halt** (**H**alt is an **A**bstract **L**anguage that **T**erminates) is a
statically typed imperative language that is heavily inspired by Rust. (Intended)
Features include:

- Strong static typing
    - Algebraic data types
    - Parametric and ad hoc polymorphism
    - A monadic effect system (as I understand them, a better type theorist please correct me)
        - Termination by default
        - Continuations
- Immutability by default
- Compile-time constant evaluation
- Borrow checking
- Multiple allocator strategies (including ones that allow garbage collection)
