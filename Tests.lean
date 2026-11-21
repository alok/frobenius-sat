import FrobeniusSat

open FrobeniusSat Std.Sat

/-- Pigeonhole: `n+1` pigeons, `n` holes. Unsatisfiable. Var `p*n + h + 1` = pigeon p in hole h. -/
def php (n : Nat) : CNF Nat :=
  let v (p h : Nat) : Int := p * n + h + 1
  let atLeast := (List.range (n+1)).map fun p => (List.range n).map (v p)
  let excl := (List.range n).flatMap fun h =>
    (List.range (n+1)).flatMap fun p => (List.range (n+1)).filterMap fun q =>
      if p < q then some [-(v p h), -(v q h)] else none
  cnfOfInts (atLeast ++ excl)

def satFormula : CNF Nat := cnfOfInts [[1, 2], [-1, 3], [-2, -3], [2, 3]]

/-- A dishonest solver: a shell script that prints `out` and writes `proof`. -/
def liar (dir : System.FilePath) (name out proof : String) : IO Solver := do
  let p := dir / name
  IO.FS.writeFile p s!"#!/bin/sh\nprintf '{proof}' > \"$2\"\nprintf '{out}'\n"
  discard <| IO.Process.output { cmd := "chmod", args := #["+x", p.toString] }
  return { name, bin := p, args := fun c pr => #[c.toString, pr.toString] }

def expect (fails : IO.Ref Nat) (label : String) (ok : Bool) : IO Unit := do
  IO.println s!"{if ok then "PASS" else "FAIL"}  {label}"
  unless ok do fails.modify (· + 1)

def isSat {f} : Verdict f → Bool | .sat .. => true | _ => false
def isUnsat {f} : Verdict f → Bool | .unsat _ => true | _ => false
def isRejected {f} : Verdict f → Bool | .rejected _ => true | _ => false

def main : IO UInt32 := do
  let pre ← IO.Process.output { cmd := "lean", args := #["--print-prefix"] }
  let cad := Solver.cadical (System.FilePath.mk pre.stdout.trimAscii.toString / "bin" / "cadical")
  let fails ← IO.mkRef 0
  let expect := expect fails
  do
    -- honest solver
    expect "cadical: small SAT formula is SAT with checked model" (isSat (← solve cad satFormula))
    for n in [3, 5, 7] do
      expect s!"cadical: PHP({n+1},{n}) is UNSAT with checked LRAT" (isUnsat (← solve cad (php n)))
    -- negative controls: each must be rejected
    IO.FS.withTempDir fun dir => do
      let s ← liar dir "liar-unsat-noproof" "s UNSATISFIABLE\\n" ""
      expect "liar: UNSAT on a SAT formula, empty proof -> rejected" (isRejected (← solve s satFormula))
      let s ← liar dir "liar-unsat-fakeproof" "s UNSATISFIABLE\\n" "1 0 0\\n"
      expect "liar: UNSAT on a SAT formula, forged empty-clause step -> rejected"
        (isRejected (← solve s satFormula))
      let s ← liar dir "liar-unsat-php-noproof" "s UNSATISFIABLE\\n" ""
      expect "liar: right answer (PHP UNSAT) but no certificate -> rejected"
        (isRejected (← solve s (php 3)))
      let s ← liar dir "liar-sat" "s SATISFIABLE\\nv 1 2 3 0\\n" ""
      expect "liar: SAT with a model that falsifies a clause -> rejected" (isRejected (← solve s satFormula))
      let s ← liar dir "liar-sat-php" "s SATISFIABLE\\nv 1 -2 -3 4 0\\n" ""
      expect "liar: SAT on PHP (unsat) -> rejected" (isRejected (← solve s (php 3)))
      let s ← liar dir "liar-silent" "" ""
      expect "liar: no verdict -> rejected" (isRejected (← solve s satFormula))
    -- DIMACS round trip
    match parseDimacs (php 3).dimacs with
    | .ok f => expect "DIMACS round trip preserves UNSAT" (isUnsat (← solve cad f))
    | .error e => expect s!"DIMACS round trip parses ({e})" false
  let fails ← fails.get
  IO.println s!"{fails} failure(s)"
  return if fails = 0 then 0 else 1
