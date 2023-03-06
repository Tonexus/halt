# Halt (**H**alt is an **A**bstract **L**anguage that **T**erminates)

Halt is a statically typed imperative language that is heavily inspired by Rust.
(Intended) features include:

- Strong static typing
    - [Algebraic data types](docs/algebraic_data_types.md)
    - Parametric and ad hoc polymorphism
    - A monadic effect system (as I understand them, a better type theorist please correct me)
        - Termination by default
        - Continuations
- Aliasable XOR mutable
    - Immutability by default
    - Aliasability and mutability defined per struct/tuple field
- Compile-time constant evaluation
- Borrow checking
- RAII
- Multiple allocator strategies (possibly allowing garbage collected shared pointers)
