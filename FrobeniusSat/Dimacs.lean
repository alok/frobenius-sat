import Std.Sat.CNF
import Std.Tactic.BVDecide.LRAT

/-!
# DIMACS in and out

Lean's `Std.Sat.CNF Nat` numbers variables from `0`; DIMACS numbers them from `1`.
Everywhere in this library, Lean variable `i` is DIMACS variable `i + 1`, matching
`Std.Sat.CNF.dimacs` and the LRAT checker that `bv_decide` uses.
-/

namespace FrobeniusSat

open Std.Sat

/-- Build a clause from DIMACS-style nonzero integers (`3` = var 2 true, `-3` = var 2 false). -/
def clauseOfInts (lits : List Int) : CNF.Clause Nat :=
  lits.foldl (init := .empty) fun c l => c.add (l.natAbs - 1) (l > 0)

/-- Build a CNF from DIMACS-style clauses. -/
def cnfOfInts (clauses : List (List Int)) : CNF Nat :=
  clauses.foldl (init := .empty) fun f c => f.add (clauseOfInts c)

/-- Parse a DIMACS CNF file body. Comments (`c`) and the header (`p`) are skipped;
clauses may span lines and are terminated by `0`. -/
def parseDimacs (s : String) : Except String (CNF Nat) := do
  let mut f : CNF Nat := .empty
  let mut cur : List Int := []
  for line in s.splitOn "\n" do
    let line := line.trimAscii.toString
    if line.isEmpty || line.startsWith "c" || line.startsWith "p" || line.startsWith "%" then
      continue
    for tok in line.splitOn " " do
      if tok.isEmpty then continue
      match tok.toInt? with
      | none => throw s!"bad DIMACS token: {tok}"
      | some 0 => f := f.add (clauseOfInts cur.reverse); cur := []
      | some l => cur := l :: cur
  unless cur.isEmpty do f := f.add (clauseOfInts cur.reverse)
  return f

/-- Number of variables (DIMACS sense) mentioned by the CNF. -/
def numVars (f : CNF Nat) : Nat :=
  f.clauses.foldl (init := 0) fun m c => c.atoms.foldl (init := m) fun m v => max m (v + 1)

/-- Parse the `v ...` lines of a SAT-competition style solver output into an assignment.
Unmentioned variables default to `false`; the model is checked afterwards anyway. -/
def parseModel (out : String) : Std.HashMap Nat Bool := Id.run do
  let mut m : Std.HashMap Nat Bool := {}
  for line in out.splitOn "\n" do
    let line := line.trimAscii.toString
    unless line.startsWith "v" do continue
    for tok in (line.drop 1).toString.splitOn " " do
      match tok.toInt? with
      | some l => if l ≠ 0 then m := m.insert (l.natAbs - 1) (l > 0)
      | none => pure ()
  return m

end FrobeniusSat
