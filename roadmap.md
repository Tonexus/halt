# Roadmap

### Simple working interpreter

1. Revamp parser. Include definitions, imperative statements, expressions, and
type expressions as normal expressions, including algebraic data types, function
types, parametric polymorphism via type functions, and subtype polymorphism via
type let expressions.

2. Revamp type checker for above algebraic data types and higher kinded types.

3. Implement tree-walking interpreter.

### Special types

1. Add array type (`Arr`), can be instantiated from homogeneous tuple or
function across all index types.

2. Add uniqueness type (`Unq`), can be for uncopiable capabilities.

3. Add mutability as interface (`Mut` gives access to assignment operator `=`)

4. Add reference as interface (`Ref` gives access to dereference operator `$`)

5. Add copy as interface (`Copy`), but cannot be present with uniqueness type.

6. Add linear type that must be destructured.

7. Add drop as interface that automatically destructures linear types.

### Borrow checker

1. Track lifetimes/reference dependencies in functions.

### x86 code generation

1. Incrementally implement code generation for some subfunctions.

2. Add compile-time constant evaluation using interpreter.

### Metadata analyzer

1. Construct call graph.

2. From leaves, upper bound time complexity (bounded loops) of each function.

3. From leaves, upper bound stack usage of each function.

4. From leaves, upper bound total memory usage of each function.
