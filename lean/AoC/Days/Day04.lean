import AoC.Util.IO
import AoC.Util.Parse
open AoC.Util


namespace AoC.Day04

def countRolls (input: String): Nat :=
  lines input
  |>.map (fun line => line.toList.count '@')
  |>.sum

def removeRolls (input: String): String :=
  let lines := lines input;
  String.intercalate "\n" (lines.foldl (fun (processed,prev,lines) cur =>
    let cur := cur.toList
    let next := lines[0]?.getD "" |>.toList
    let diff := cur.mapIdx (fun i c => (i,c))
      |>.map (fun (i,c) => if c = '@' then
          let rolls := List.range 3
            |>.filter (fun n => 0 < n + i ∧ n + i < cur.length + 1)
            |>.map (· + i - 1)
            |>.map (fun n => [prev[n]?,cur[n]?,next[n]?].filterMap id |>.count '@')
            |>.sum
          if rolls ≤ 4 then '.' else c
        else
          c
      )
    (processed ++ [diff.asString], cur, lines.drop 1)
  ) ([],"".toList,lines.drop 1)).fst

partial def removeRollsRec (input: String): String :=
  let processed := removeRolls input
  if countRolls input = countRolls processed then processed else removeRollsRec processed

def part1 (input: String) :=
  countRolls input - countRolls (removeRolls input)

def part2 (input: String) :=
  countRolls input - countRolls (removeRollsRec input)

def run : IO Unit := do
  let input ← readInput 4
  IO.println s!"Part 1: {part1 input}"
  IO.println s!"Part 2: {part2 input}"

end AoC.Day04
