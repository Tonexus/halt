# HALT (HALT is an Abstract Language that Terminates)

Halt is a statically typed imperative language that is heavily inspired by Rust.
(Intended) features include:

- Strong static typing
    - Algebraic data types
    	- Structural rather than nominal types
    - Parametric and subtype polymorphism
    	- Higher-kinded types
    - A [coeffect](https://tomasp.net/coeffects/) system, including
        - IO (display, file system, networking, etc.)
        - Memory allocation
        - Termination by default (enabling [simple static resource analysis](https://people.csail.mit.edu/meyer/meyer-ritchie.pdf))
        - Continuations
- Aliasable XOR mutable using [substructural types](https://en.wikipedia.org/wiki/Substructural_type_system) and [uniqueness types](https://en.wikipedia.org/wiki/Uniqueness_type)
    - Immutability by default
    - Aliasability and mutability defined per struct/tuple field
- Array index types to reduce bounds checking (inspired by functional lenses)
- Compile-time constant evaluation
- "Borrow checking" using above substructural, uniqueness, and [fractional uniqueness](https://arxiv.org/abs/2310.18166) types
- RAII
- Multiple allocator strategies (possibly allowing garbage collected shared pointers)
