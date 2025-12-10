import AoC.Util.IO
import AoC.Util.Parse
open AoC.Util


namespace AoC.Day09

def diff (this other: Nat) :=
  if this > other then this - other else other - this

def connectingPoints (A B: Nat × Nat): List (Nat × Nat) :=
  if A.fst == B.fst then
    if A.snd < B.snd then
      List.range (B.snd - A.snd) |>.map ((A.fst, · + A.snd))
    else
      List.range (A.snd - B.snd) |>.map ((A.fst, · + B.snd))
  else
    if A.fst < B.fst then
      List.range (B.fst - A.fst) |>.map ((· + A.fst, A.snd))
    else
      List.range (A.fst - B.fst) |>.map ((· + B.fst, A.snd))

def part1 (input: String) :=
  let points := lines input
    |>.map (fun line =>
      let point := line.splitToList (· = ',') |>.map (·.toNat!)
      (point[0]!,point[1]!)
    )
  points.mapIdx (fun idx point => (idx,point))
  |>.flatMap (fun (idx,point) => points.drop (idx + 1) |>.map ((point,·)))
  |>.map (fun (P,Q) => (1 + diff P.fst Q.fst) * (1 + diff P.snd Q.snd))
  |>.max?.get!

def part2 (input: String) :=
  let points := lines input
    |>.map (fun line =>
      let point := line.splitToList (· = ',') |>.map (·.toNat!)
      (point[0]!,point[1]!)
    )
  let border := points.mapIdx (fun idx point => (point,points[idx+1]?))
    |>.foldl (fun acc (A,B) =>
      let B := match B with
      | none => points[0]!
      | some B => B
      acc ++ connectingPoints A B
    ) []
  points.mapIdx (fun idx point => (idx,point))
  |>.flatMap (fun (idx,point) => points.drop (idx + 1) |>.map ((point,·)))
  |>.mergeSort (fun (P₁,Q₁) (P₂,Q₂) => (1 + diff P₁.fst Q₁.fst) * (1 + diff P₁.snd Q₁.snd) ≥ (1 + diff P₂.fst Q₂.fst) * (1 + diff P₂.snd Q₂.snd))
  -- find first rectangle, that does not contain any border point
  |>.find? (fun (P,Q) =>
    ¬border.any (fun (x,y) =>
      if P.fst < Q.fst then
        if P.snd < Q.snd then
          P.fst < x ∧ x < Q.fst ∧ P.snd < y ∧ y < Q.snd
        else
          P.fst < x ∧ x < Q.fst ∧ Q.snd < y ∧ y < P.snd
      else
        if P.snd < Q.snd then
          Q.fst < x ∧ x < P.fst ∧ P.snd < y ∧ y < Q.snd
        else
          Q.fst < x ∧ x < P.fst ∧ Q.snd < y ∧ y < P.snd
    )
  )
  |>.map (fun (P,Q) => (1 + diff P.fst Q.fst) * (1 + diff P.snd Q.snd))
  |>.get!


def run : IO Unit := do
  let input ← readInput 9
  IO.println s!"Part 1: {part1 input}"
  IO.println s!"Part 2: {part2 input}"

end AoC.Day09
