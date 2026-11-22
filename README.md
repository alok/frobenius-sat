# frobenius-sat

Use any external SAT solver from Lean 4, but trust only what Lean can check.

`solve solver cnf : IO (Verdict cnf)` runs the solver and returns one of three verdicts:

- `.sat a h` where `h : cnf.eval a = true`. Lean evaluated every clause under the model.
- `.unsat h` where `h : cnf.Unsat`. Lean's verified LRAT checker (`Std.Tactic.BVDecide.LRAT.check_sound`, the same checker `bv_decide` uses) accepted the solver's certificate.
- `.rejected reason` for anything else. That includes a correct answer that comes with no certificate.

## Solvers

- `Solver.cadical`: the CaDiCaL that ships in every Lean toolchain (`$(lean --print-prefix)/bin/cadical`).
- `Solver.frobenius`: reads `FROBENIUS_SAT_BIN`. Frobenius (announced 2026-11-21) **has no public release**. This preset assumes it will speak CaDiCaL's interface: DIMACS in, `s`/`v` lines out, and a textual LRAT proof written to the second path. If it does not, pass your own `Solver` with custom `args`.

## CLI

```bash
lake exe frobenius-sat problem.cnf                       # cadical
FROBENIUS_SAT_BIN=/path/to/frobenius lake exe frobenius-sat --solver frobenius problem.cnf
```

Exit codes: 10 means SAT (checked), 20 means UNSAT (checked), 1 means rejected.

## Tests

```bash
lake exe tests
```

The suite runs honest CaDiCaL on a small SAT instance and on pigeonhole PHP(4,3), PHP(6,5) and PHP(8,7). It also runs six lying solvers, and all six must be rejected:

- UNSAT with no proof
- UNSAT with a forged proof
- the right UNSAT answer with no certificate
- SAT with a bad model
- SAT on an unsatisfiable formula
- no verdict at all

Toolchain: Lean `v4.36.0`, no Mathlib.

License: Apache-2.0 (same as Lean core).
