import AoC.Util.IO
import AoC.Util.Parse
open AoC.Util


namespace AoC.Day12

/-- Remove all occurrences of columns in `cols` from a row -/
def removeColumns (cols: List (Nat × Nat)) (row: List (Nat × Nat)): List (Nat × Nat) :=
  row.filter (fun col => col ∉ cols)

/-- Remove all rows that intersect with a given row -/
def removeConflictingRows (row: List (Nat × Nat)) (matrix: List (List (Nat × Nat))): List (List (Nat × Nat)) :=
  matrix.filter (fun row' => ∀ col ∈ row, col ∉ row')

/-- Remove a row's columns from the entire matrix -/
def reduceMatrix (row: List (Nat × Nat)) (matrix: List (List (Nat × Nat))): List (List (Nat × Nat)) :=
  removeConflictingRows row matrix
  |>.map (removeColumns row)

/-- Collect all columns appearing in the matrix -/
def columns (matrix: List (List (Nat × Nat))): List (Nat × Nat) :=
  matrix.flatten.eraseDups

/-- Choose a column (simple heuristic: first column) -/
def chooseColumn (matrix: List (List (Nat × Nat))) : Option (Nat × Nat) :=
  columns matrix |>.head?

/-- Select all rows that cover a given column -/
def rowsCovering (col: Nat × Nat) (matrix: List (List (Nat × Nat))): List (List (Nat × Nat)) :=
  matrix.filter (fun row => col ∈ row)

/--
Algorithm X: returns all exact covers.
Each solution is a list of rows.
-/
partial def algorithmX (matrix: List (List (Nat × Nat))) (nPieces: Nat): Option (List (List (Nat × Nat))) :=
  match chooseColumn matrix with
  | none =>
    -- No columns left: valid exact cover
    some []
  | some col =>
    let candidates := rowsCovering col matrix
    candidates.filterMap (fun row =>
      match algorithmX (reduceMatrix row matrix) (nPieces - 1) |>.map (fun sol => row :: sol) with
      | none => none
      | some sol => if sol.length = nPieces then some sol else none
    )
    |>.find? (fun _ => true)  -- find first correct result



def part1 (input: String) :=
  let shapes := lines input
    |>.reverse.dropWhile (fun line => ¬(line.contains '#' ∨ line.contains '.'))
    |>.reverse
    |>.foldl (fun (curShapeId,shapes) line =>
      if line.endsWith ":" then
        let shapeId := line.takeWhile (· ≠ ':') |>.toNat!
        (shapeId, (shapeId,[])::shapes)
      else
        (curShapeId, shapes.map (fun (shapeId,shape) =>
          if shapeId ≠ curShapeId then
            (shapeId,shape)
          else
            (shapeId, shape ++ [line])
        ))
    ) (0,([]: List (Nat × List String)))
    |>.snd
    |>.map (fun (shapeId,shape) => (shapeId, shape.mapIdx (fun y (line: String) => line.toList.mapIdx (fun x sym => if sym = '#' then some (y,x) else none)) |>.flatten |>.filterMap id))
    |>.map (fun (shapeId,shape) =>
      let sort := fun (shape: List (Nat × Nat)) => shape.mergeSort (fun a b => if a.fst = b.fst then a.snd ≤ b.snd else a.fst ≤ b.fst)
      let rotate90 := fun (shape: List (Nat × Nat)) => sort (shape.map Prod.swap)
      let rotate180 := fun (shape: List (Nat × Nat)) => sort (shape.map (fun ((y: Nat),(x: Nat)) => (2-y,x)))
      (shapeId, [shape, rotate90 shape, rotate180 shape, rotate180 (rotate90 shape)].eraseDups))

  let puzzles := lines input
    |>.reverse.takeWhile (fun line => ¬(line.contains '#' ∨ line.contains '.'))
    |>.map (fun line => (
        (line.takeWhile (· ≠ 'x') |>.trim.toNat!, line.dropWhile (· ≠ 'x') |>.drop 1 |>.takeWhile (· ≠ ':') |>.trim.toNat!),
        line.dropWhile (· ≠ ':') |>.drop 1 |>.trim.splitToList (· = ' ') |>.mapIdx (fun idx count => (idx,count.trim.toNat!))
      )
    )
    |>.map (fun (size,pieces) => (size, pieces, List.range size.snd |>.flatMap (fun y => List.range size.fst |>.map (fun x => (y,x)))))

  let nPieces := puzzles.map (·.snd.fst.map Prod.snd |>.sum)
  let rows := puzzles.map (fun puzzle =>
      let pieces := puzzle.snd.fst.flatMap (fun (id,count) =>
          match shapes.find? (·.fst = id) with
          | none => []
          | some (_,rotations) => List.range count |>.flatMap (fun i => rotations.map (fun shape => ((id+1)*100 + i,(id+1)*100 + i)::shape))
        )
      let offsets := List.range (puzzle.fst.snd - 2) |>.flatMap (fun y => List.range (puzzle.fst.fst - 2) |>.map (fun x => (y,x)))
      offsets.flatMap (fun (dy,dx) => pieces.map (fun l => l.take 1 ++ (l.drop 1).map (fun (y,x) => (y+dy,x+dx))))
    )

  rows.zip nPieces
  |>.filterMap (fun (rows,nPieces) => algorithmX rows nPieces)
  |>.length

def part1_trivial (input: String) :=
  lines input
  |>.reverse.takeWhile (fun line => ¬(line.contains '#' ∨ line.contains '.'))
  |>.filter (fun line => (
      ((line.takeWhile (· ≠ 'x') |>.trim.toNat!) * (line.dropWhile (· ≠ 'x') |>.drop 1 |>.takeWhile (· ≠ ':') |>.trim.toNat!)) ≥
      (line.dropWhile (· ≠ ':') |>.drop 1 |>.trim.splitToList (· = ' ') |>.map (·.trim.toNat!) |>.sum) * 9
    )
  )
  |>.length

def part2 :=
  "Merry Christmas"

def run : IO Unit := do
  let input ← readInput 12
  --IO.println s!"Part 1: {part1 input}"
  IO.println s!"Part 1: {part1_trivial input}"
  IO.println s!"Part 1: {part2}"

end AoC.Day12
