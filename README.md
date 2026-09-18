# choice

Pattern unification extended with a "choice" operator.

## Motivation

The goal is to solve higher-order unification problems that have a unique solution even though some of their subproblems have multiple solutions.

Consider the following unification problem:

```
metactx:
  ?0 : ℕ → ℕ → ℕ → ℕ → Type

m : ℕ, n : ℕ ⊢ (?0 m m n n ≟ (m ≤ n)) ∧ (?0 m n n m ≟ (m ≤ n))
```

This has the unique solution `?0 ≔ λ m _ n _ → m ≤ n`, but each of its subproblems has four distinct solutions.

The problem is clearly outside the pattern fragment. Pruning can't handle this situation either, because it can only prune vars that don't occur on the RHS. We also can't postpone these, since there are no other subproblems. We could of course use backtracking, but that may be expensive.

The idea is that, when solving the first subproblem, we represent the alternatives using the choice operators:

```
metactx:
  ?0 : ℕ → ℕ → ℕ → ℕ → Type = λ a b c d → (a |₀ b) ≤ (c |₁ d)

m : ℕ, n : ℕ ⊢ ?0 m n n m ≟ (m ≤ n)
```

We then continue working on the remaining problem:

```
metactx:
  ?0 : ℕ → ℕ → ℕ → ℕ → Type = λ a b c d → (a |₀ b) ≤ (c |₁ d)

m : ℕ, n : ℕ ⊢ ?0 m n n m ≟ (m ≤ n)
    ⇓ reduce ?0 m n n m
m : ℕ, n : ℕ ⊢ ((m |₀ n) ≤ (n |₁ m)) ≟ (m ≤ n)
    ⇓ decompose
m : ℕ, n : ℕ ⊢ ((m |₀ n) ≟ m) ∧ ((n |₁ m) ≟ n)
```

At this point, we can determine that the right branches can't satisfy the equations, and refine the choices so that they select the left branches. Now, `?0` computes to `λ m _ n _ → m ≤ n`, which is the solution we want.

So, roughly speaking, the idea is to keep a finite set of possible unifiers, and let other constraints rule out alternatives.

## Acknowledgment

This implementation extends András Kovács's [elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo).
It is also his idea to represent alternatives using the choice operator.
My original idea was to use a meta-level enum type and case expressions over enum values to keep track of the alternatives.
