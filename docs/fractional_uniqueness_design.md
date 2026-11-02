# Fractional Uniqueness: Design Summary (So Far)

Items are tagged:

- **[Decided]**
- **[Proposed]**
- **[Open]**

---

## 1. Design background

The idea of this iteration of fractional uniqueness stems directly from Granule, though there is a longer intellectual history, going back to Girard's linear logic and uniqueness types in Clean.

Fractional uniqueness is key to Halt's design for references, particularly mimicking Rust's borrow checker and mutable XOR aliasable. Particularly relevant here is the interaction between uniqueness and existential polymorphism, specifically containers.

### 1.1 Affine core [Decided]

- Passing a value to a function consumes it unless the function returns it.
- There is one duplication special function `Copy`, which takes one value and returns two.
- Reference *usage* returns the reference, so it does not consume it.

### 1.2 Qualified substructural and uniqueness typing [Decided, being superseded in part]

A plain data type has various forms, determining whether it is unique, it may be copied, and it must be used. These are three independent axes that each contain a subtype relationship based on whether a usage constraint can be weakened.

| Axis | Default | Opt-out | Subtyping |
|---|---|---|---|
| Aliasing | `unique` | `shared` | `unique <: shared` |
| Duplication | `copyable` | `affine` | `copyable <: affine` |
| Obligation | `irrelevant` | `relevant` | `irrelevant <: relevant` |

- A plain `T` starts as unique, copyable, irrelevant. Each axis may be set independently.
- `relevant` means the value must eventually be destructured, while `irrelevant` allows for a silent drop.
- `copyable` is required for a value to be passed to `Copy`.
- `unique` is required for mutation of a reference.
- **The unique/shared axis is replaced by fractions** (section 3). Copyable/affine and relevant/irrelevant remain.
- Syntax for writing qualifiers and fractions on types is still a TODO.

---

## 2. Brands and Fractions

Here we discuss representing `unique` and `shared` using brands and fractions.

### 2.1 Reference type [Decided]

```
Ref(T, rho, f)
```

- `rho` is a **brand**: a generated, type-level identity for the underlying storage. It also denotes the "whole" permission.
- `f` is a **fraction** of the brand, written as a formal linear combination of brands (e.g. `0.5 * rho`, `rho_1 + sigma_1`).
- Brands and fractions are **erased at runtime**.
- Creating a reference from an owned value generates a fresh brand, roughly `rho |> Ref(T, rho, rho)` **[Proposed reading]**.
- **[Proposed]** We may use fractions to model partial borrows (borrowing specific fields or array elements) as well.

### 2.2 Uniqueness rule [Decided]

- `f == rho`: the reference is `unique`, and mutation is allowed.
- `f < rho`: the reference is `shared`, and only read access is allowed.
- This **fully replaces** the unique/shared qualifier.
- For data where coalescing never matters, use something like `T(1, 0)` (a plain-data int).

### 2.3 Operations [Decided]

- **Split / Copy**: `Copy : T(rho, sigma) => (T(rho, sigma/2); T(rho, sigma/2))`. The exact syntax is not final. For values it is still a bitwise copy, and since brands and fractions are type-level there is no conflict.
- **Coalesce**: `Ref(T, rho, a)` and `Ref(T, rho, b)` combine into `Ref(T, rho, a + b)`. Uniqueness can be recovered from shared references.
- **Weakening**: `T(rho, s1)` can be weakened to `T(rho, s2)` where `0 <= s2 <= s1`.
- **Fraction polymorphism**: read-only functions quantify over fractions, e.g. `forall f. Ref(T, rho, f) => (Ref(T, rho, f); R)`.

### 2.4 Soundness invariants [Decided]

- Any live fraction falls within `0 < f <= rho`.
- At any point in a program, fractions of a brand always sum to at most `rho`, and only split and coalesce change them.
- Freeing a reference (retoring the owned value) requires the full `T(rho, rho)`, so a leftover fraction makes use-after-free impossible.

### 2.5 Miscellaneous points

- **Reads at fraction 0 [Open]**: `T(1, 0)` must be readable (pure data), but a ref at fraction 0 should not be readable (see 3.4). Proposed resolution: separate pure values (a private bitwise snapshot, safe to read) from `Ref`s (a zero-fraction ref is a witness only).
- **Weakening a relevant `Ref` [Proposed]**: this would be a leak. Prefer a split that returns the remainder, and reserve plain weakening for irrelevant values like `T(1, 0)`.
- **Coalescing at the value level [Proposed]**: coalescing two copied values keeps one set of bytes and discards the other. This is sound only when neither half could have been mutated, which holds since each is shared. For aggregates it recurses structurally.

---

## 3. Relevance, `Dead`, and Witnesses

### 3.1 References are relevant [Decided]

- A `Ref` must eventually be coalesced, and the owned value is then restored from the unique reference.
- This makes "the borrow ends and the owner regains access" a type-checked obligation (the analogue of Rust's borrow ending).
- Relevance propagates structurally: a struct or closure holding a `Ref` is relevant. So a function that returns something holding a borrow passes the obligation to the caller.
- Early exits (`return`, `break`) must have discharged all relevant values.

### 3.2 `Dead` [Decided]

- `kill : Ref(T, rho, f) => Dead(T, rho, f)`.
- `Dead` is relevant and unusable (by itself): a promise never to read or write through the reference. It exists for uniqueness bookkeeping.
- Killing **immediately discards the underlying pointer data**. `Dead` is zero-sized at runtime **[Proposed reading]**.
- `Dead` coalesces with live refs. Live wins: `Ref + Dead -> Ref`, `Dead + Dead -> Dead` **[Proposed]**. At the value level the live side supplies the pointer and bytes.

### 3.3 `Dead(T, rho, rho)` is a compile error [Decided]

- It is relevant and has no way to become irrelevant, so it is uninhabitable in any program that compiles.
- This arises only if *all* live references are killed.
- **[Proposed]** Report the error at the `kill` call that removes the last live fraction, not at scope end.
- Invariant **[Proposed]**: while any borrow exists, at least one fraction is live.

### 3.4 Zero-fraction witness `Ref(T, rho, 0)` [Open, leaning toward needed]

- Stores a pointer value, is unusable for reads, and is **irrelevant and copyable** **[Proposed]**.
- It is sound because brands are generative: after the full `Ref(T, rho, rho)` is freed, no fraction of `rho` can exist, so a stale witness cannot be revived.
- It is needed for the "all dead" case: `Dead(T, rho, rho) + Ref(T, rho, 0) -> Ref(T, rho, rho)`.
- It must be created **before** the pointer goes into a container, by splitting off a zero part: `Ref(T, rho, f) => (Ref(T, rho, f); Ref(T, rho, 0))`.
- Without a witness, `Dead(T, rho, rho)` remains an error, meaning "you forgot to keep a pointer".
- The cost is bookkeeping: someone must hold one witness per brand.

---

## 4. Containers with Aggregate Accounting

Here we extend the Granule-based framework for containers.

### 4.1 Problem [Decided]

There is no good way to have a `Vec` of references that is heterogeneous over brand. Existentials hide the brands, so the fractions cannot be coalesced. Instead, **the container carries the accounting**.

### 4.2 Typing [Decided]

`Vec(Ref(T, A, B))`, where `A` is a formal sum of brands and `B` a formal sum of fractions.

- Empty vec: `Vec(Ref(T, 0, 0))`. Let `rho = rho_1 + rho_2` and `sigma = sigma_1 + sigma_2`.
- **Insertion** is set-union on `A` and true addition on `B`.
  - Insert `r1: Ref(T, rho, rho_1)`: `Vec(Ref(T, rho, rho_1))`.
  - Insert `r3: Ref(T, sigma, sigma_1)`: `Vec(Ref(T, rho + sigma, rho_1 + sigma_1))`.
  - Insert `r2: Ref(T, rho, rho_2)`: `B` becomes `rho + sigma_1` (the `rho` coefficient reaches `rho`, not `2 rho`).
  - After inserting `r4: Ref(T, sigma, sigma_2)`: `Vec(Ref(T, rho + sigma, rho + sigma))`.
- **Access mode**: `B < A` means read-only, and `B = A` means mutable. Equality must be checked componentwise per brand **[Proposed]**.

### 4.3 Two pathways [Decided]

1. **Kill the vec** while `B < A`. The result is `Dead(T, rho + sigma, rho_1 + sigma_1)`, which splits into `Dead(T, rho, rho_1)` and `Dead(T, sigma, sigma_1)`. These coalesce with the outside refs `r2` and `r4`, which restores uniqueness even after refs were "lost" into the container.
2. **Insert everything** until `B = A`. Elements now alias but can be mutated **sequentially**, which is sound. Killing the vec then yields `Dead(T, rho + sigma, rho + sigma)`, which is why witnesses (3.4) are needed.

### 4.4 Consequences of pathway 2 [Proposed]

- Access must be sequential. There is no API that holds two element refs at once (no `swap`, no `sort` with a mutating comparator).
- Elements are pointers sharing storage. They are not value copies.

### 4.5 Operations [Decided]

- **`get`**: returns the reference and makes the vec **dormant**. The ref must be put back to restore the vec.
- **`pop` / `remove`**: just delete. The vec's type is unchanged, and you still wait until the whole vec is killed to reassemble unique refs.
- **`iter` / `iter_mut`** are the same operation, since mutability depends entirely on the fractions. The iterator becomes dormant until the current element is put back.
- **`collect`** needs extra machinery (section 5).

### 4.6 Possible refinements [Proposed]

- **Loans**: `get : Vec(A, B) => tau |> (Ref(T, tau, f); Dormant(tau, Vec(A, B)))`, with `f = tau` if `B = A` and `f < tau` otherwise. A fresh loan brand `tau` isolates the element ref from the real brands, and shared loans can be split freely.
- **`Dormant` as a distinct relevant type**, so `insert` cannot be called on a vec with an element out on loan.
- **Type as upper bound**: after `pop` and `remove` the type is an upper bound on what is owed, not an exact inventory. An emptied vec still has to be killed.
- **`pop` / `remove` return unit** (or `Dead`), since a returned live ref would have an unknown brand and could never be discharged.
- **`kill` on a container** frees the container's own buffer and returns a bare `Dead`, with no vec structure left.

---

## 5. Iteration

### 5.1 Terminology

- **Iterable**: can produce iterators, possibly many times.
- **Iterator**: a stateful cursor advanced with `next`.
- Orthogonal choice, made when creating the iterator: **lending** (source survives, elements are loaned) vs. **draining/splitting** (elements escape).

### 5.2 Two iterator patterns [Decided]

| | Lending iterator | Draining ("splitting") iterator |
|---|---|---|
| Fraction taken | all of the collection's fractions | half of the collection's fraction |
| Access during iteration | mutable if `B = A` | read-only |
| Elements | each borrows and returns all fractions | each borrows half of the iterator's remaining fraction |
| Purpose | mutation during iteration | `collect` into a new (possibly mutable) collection |

### 5.3 Mutable collect example [Decided]

```
let arr:  Vec(Ref(Int, rho_1 + ..., rho_1 + ...)) = ...;        // fully unique
let iter: Iter(Ref(Int, rho_1 + ..., (rho_1 + ...) / 2)) = arr.draining_iter();
let tree: Tree(Ref(Int, rho_1 + ..., (rho_1 + ...) / 2)) = iter.collect();   // half fraction
tree = coalesce(tree, kill(arr));    // tree is now at full fraction, mutable
```

- `draining_iter` acts as a vec-level `Copy`: the source and the iterator each get half.
- `kill(arr)` yields a zero-sized receipt that coalesces back with the live half.
- **[Proposed]** "splitting" or "snapshot" may be a better name than "draining", since the source is not emptied at the value level.

### 5.4 Geometric halving [Decided]

Each `next` gives the element half of the iterator's remaining fraction, and the iterator is retyped:

```
let foo: Iter(Ref(..., A)) = ...;
let bar: Ref(..., A/2) = foo.next();   // foo retyped to A/2
let baz: Ref(..., A/4) = foo.next();   // foo retyped to A/4
```

- After `n` steps the elements hold `f/2 + ... + f/2^n` and the iterator holds `f/2^n`. The total is exactly `f` at every step.
- `collect` is implemented as a loop inserting elements into an output collection, which gains `B/2 + B/4 + ...`.

### 5.5 Open typing issues

- **Loop invariant [Proposed]**: the iterator's remaining fraction changes each step, so the loop needs an existentially hidden fraction. Roughly the iterator is `g |> Iter(A, g)` and the accumulator is `h |> Collection(A, h)`, with invariant `h + g = B`. Each step sets `g' = g/2` and `h' = h + g/2`. This requires linear arithmetic with division by 2.
- **Residual cleanup [Decided that cleanup is needed; Open how]**: a finite loop always leaves `B/2^n` in the iterator. On exhaustion or early exit (`break`, `take(k)`), something must dispose of it. `for` loops over such iterators probably need an automatic cleanup step.
- **Where the residual goes [Open]**: does it coalesce into the output, giving a collection at the iterator's full fraction as in the example, or stay in the receipt, giving a simpler but weaker result? Absorbing the residual is what makes the example's `B/2` come out exactly.
- **Explicit vs. combined primitive [Open]**: the user could write the `kill`/`coalesce` step explicitly as above, or `collect` could handle it via something like `collect_unique`. Forgetting the step is already caught by relevance, since the receipt is relevant.
- **Collecting from a lending iterator [Proposed]**: a naive `collect` would count the same fractions in two containers. Either treat it as vec-level `Copy` (both results shared) or require `into_iter` (source consumed).
- **`for` desugaring [Proposed]**: must insert the put-back on every exit path (`continue`, `break`, `return`), and elements cannot escape the loop body in the lending case.

---

## 6. Open Questions Index

1. Syntax for qualifiers and fractions on types (`Copy` signature, `Ref` forms).
2. Reads at fraction 0 versus the pure-data `T(1, 0)` case (2.5).
3. Whether `Ref(T, rho, 0)` witnesses are needed, and how they are managed (3.4).
4. Whether `Dormant` is a distinct type (4.6).
5. How to type the loop invariant for the geometric series (5.5).
6. Where the residual goes in `collect` and whether there is a combined primitive (5.5).
7. How dynamic numbers of refs are handled in general (deferred).
8. Whether copyability is inferred structurally or declared, and how `Copy` behaves on types that must not be split, such as file handles (flagged early, deferred).
9. Closure qualifiers: whether a closure's copyability and relevance follow from its captures.
10. Whether types may depend on values, and which phase evaluates parametric types.
