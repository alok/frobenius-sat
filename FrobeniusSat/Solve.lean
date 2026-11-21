import FrobeniusSat.Dimacs

/-!
# Bring your own solver, Lean checks the receipt

`solve` writes a CNF to DIMACS, runs an external solver, and returns a `Verdict cnf`.
A `Verdict` cannot be built from the solver's word alone:

* `.sat a h` carries `h : cnf.eval a = true`, which Lean decided by evaluating every clause.
* `.unsat h` carries `h : cnf.Unsat`, obtained from Lean's verified LRAT checker
  (`Std.Tactic.BVDecide.LRAT.check_sound`) on the solver's certificate.

Anything else, including a fast answer with no certificate, is `.rejected`.
-/

namespace FrobeniusSat

open Std.Sat
open Std.Tactic.BVDecide

/-- How to invoke an external solver. It must accept a DIMACS file, print `s SATISFIABLE`
with `v` lines or `s UNSATISFIABLE`, and on UNSAT write a textual LRAT proof to `proof`. -/
structure Solver where
  name : String
  bin : System.FilePath
  args : (cnf proof : System.FilePath) → Array String

/-- CaDiCaL, as shipped in every Lean toolchain for `bv_decide`. -/
def Solver.cadical (bin : System.FilePath := "cadical") : Solver where
  name := "cadical"
  bin := bin
  args cnf proof := #["--lrat", "--binary=false", "-q", cnf.toString, proof.toString]

/-- Frobenius. It is not public yet; set `FROBENIUS_SAT_BIN` once it is. The argument shape
assumes it speaks the same LRAT interface as CaDiCaL; adjust `args` if it does not. -/
def Solver.frobenius : IO Solver := do
  let some bin ← IO.getEnv "FROBENIUS_SAT_BIN"
    | throw <| .userError "FROBENIUS_SAT_BIN is not set: Frobenius has no public release yet"
  return { name := "frobenius", bin, args := (Solver.cadical bin).args }

/-- A solver answer that Lean has independently checked. -/
inductive Verdict (cnf : CNF Nat) where
  | sat (assignment : Nat → Bool) (h : cnf.eval assignment = true)
  | unsat (h : cnf.Unsat)
  | rejected (reason : String)

def Verdict.describe {cnf : CNF Nat} : Verdict cnf → String
  | .sat .. => "SAT (model checked by Lean)"
  | .unsat _ => "UNSAT (LRAT certificate checked by Lean)"
  | .rejected r => s!"REJECTED: {r}"

/-- Check a claimed model. -/
def checkModel (cnf : CNF Nat) (a : Nat → Bool) : Verdict cnf :=
  if h : cnf.eval a = true then .sat a h
  else .rejected "solver claimed SAT but its model falsifies a clause"

/-- Check a claimed LRAT refutation. -/
def checkRefutation (cnf : CNF Nat) (proof : ByteArray) : Verdict cnf :=
  match LRAT.parseLRATProof proof with
  | .error e => .rejected s!"solver claimed UNSAT but its LRAT proof does not parse: {e}"
  | .ok actions =>
    if h : LRAT.check actions cnf then .unsat (LRAT.check_sound actions cnf h)
    else .rejected "solver claimed UNSAT but Lean's LRAT checker rejected the certificate"

/-- Run `solver` on `cnf` and check whatever it says. -/
def solve (solver : Solver) (cnf : CNF Nat) : IO (Verdict cnf) := do
  IO.FS.withTempDir fun dir => do
    let cnfPath := dir / "problem.cnf"
    let proofPath := dir / "proof.lrat"
    IO.FS.writeFile cnfPath cnf.dimacs
    IO.FS.writeFile proofPath ""
    let out ← IO.Process.output { cmd := solver.bin.toString, args := solver.args cnfPath proofPath }
    let says (s : String) := (out.stdout.splitOn "\n").any (·.trimAscii.toString == s)
    if says "s SATISFIABLE" then
      let m := parseModel out.stdout
      return checkModel cnf fun v => m.getD v false
    else if says "s UNSATISFIABLE" then
      return checkRefutation cnf (← IO.FS.readBinFile proofPath)
    else
      return .rejected s!"{solver.name} gave no verdict (exit {out.exitCode}): {out.stderr.take 200}"

end FrobeniusSat
