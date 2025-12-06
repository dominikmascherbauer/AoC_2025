import AoC.Util.IO
import AoC.Util.Parse
open AoC.Util


namespace AoC.Day06

def part1 (input: String) :=
  let sublists := lines input
    |>.reverse
    |>.map (fun line => line.splitToList (· = ' ') |>.filter (¬·.isEmpty) |>.map (·.trim))
  let ops := sublists[0]!
  let values := sublists.drop 1 |>.map (·.map (·.toNat!))
  values.drop 1
  |>.foldl (fun acc sublist =>
    acc.zip sublist
    |>.mapIdx (fun i (a,b) =>
      if ops[i]! = "*" then a * b else a + b
    )
  ) values[0]!
  |>.sum

def part2 (input: String) :=
  let lengths := linesNoTrim input
    |>.getLast!
    |>.splitToList (fun c => c = '*' ∨ c = '+')
    |>.map (·.length)
    |>.filter (· ≠ 0)
  let lengths := lengths.take (lengths.length - 1) ++ [lengths.getLast! + 1]
  let ops := lines input
    |>.getLast!
    |>.toList
    |>.filter (fun c => c ≠ ' ')
  let values := linesNoTrim input
    |>.reverse.drop 1
    |>.map (fun line =>
      lengths.foldl (fun (rest,acc) l =>
        (rest.drop (l + 1), acc ++ [rest.take l |>.toList.map (fun c => if c = ' ' then 0 else c.toString.toNat!)])
      ) (line,[])
      |>.snd
    )
    |>.reverse
  values.drop 1
  |>.foldl (fun acc sublist =>
    acc.mapIdx (fun idx line => line ++ [sublist[idx]!])
  ) (values[0]!.map ([·]))
  |>.mapIdx (fun idx col =>
    let op := ops[idx]!
    let vals := col.drop 1
    |>.foldl (fun acc num =>
      acc.zip num |>.map (fun (a,b) => if b = 0 then a else a*10 + b)
    ) col[0]!
    vals.drop 1
    |>.foldl (fun acc num =>
      if op = '*' then acc * num else acc + num
    ) vals[0]!
  )
  |>.sum

def run : IO Unit := do
  let input ← readInput 6
  IO.println s!"Part 1: {part1 input}"
  IO.println s!"Part 2: {part2 input}"

end AoC.Day06
