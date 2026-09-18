# Implementation note

## Interaction between choice and large elimination

```agda
f : (b : Bool) → if b then (ℕ → ℕ) else ℕ

-- whoa!
f (true |₀ false) : (ℕ → ℕ) |₀ ℕ
```

## Problem cases

Standard unification

- Rigid / Rigid → decompose
- Flex / Rigid → solve
- Flex / Flex → intersection? postpone? whatever

New cases!

- Choice / Rigid → refine
- Choice / Flex → solve + refine?
- Choice / Choice → intersection?
