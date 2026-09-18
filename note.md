# Implementation note

## Interaction between choice and large elimination

```agda
f : (b : Bool) → if b then (ℕ → ℕ) else ℕ

-- whoa!
f (true |₀ false) : (ℕ → ℕ) |₀ ℕ
```
