import FrobeniusSat

open FrobeniusSat

/-- `frobenius-sat [--solver cadical|frobenius] FILE.cnf`
Exit 10 = SAT checked, 20 = UNSAT checked, 1 = rejected. -/
def main (args : List String) : IO UInt32 := do
  let (solver, file) ← match args with
    | ["--solver", "frobenius", f] => pure (← Solver.frobenius, f)
    | ["--solver", "cadical", f] | [f] => pure (Solver.cadical, f)
    | _ => throw <| .userError "usage: frobenius-sat [--solver cadical|frobenius] FILE.cnf"
  let cnf ← IO.ofExcept (parseDimacs (← IO.FS.readFile file))
  let t0 ← IO.monoMsNow
  let v ← solve solver cnf
  IO.println s!"{solver.name}: {v.describe} in {(← IO.monoMsNow) - t0} ms"
  return match v with
    | .sat .. => 10
    | .unsat _ => 20
    | .rejected _ => 1
