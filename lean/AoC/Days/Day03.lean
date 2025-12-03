import AoC.Util.IO
import AoC.Util.Parse
open AoC.Util


namespace AoC.Day03

def part1 (input: String): Nat :=
  lines input
  |>.map (fun line => line.toList.map (fun c => c.toNat - '0'.toNat))
  |>.map (fun l =>
    let x := match l.dropLast.max? with
    | none => 0
    | some x => x

    x * 10 + match l.dropWhile (. < x) |>.drop 1 |>.max? with
    | none => 0
    | some x => x
  )
  |>.sum

def part2 (input: String): Nat :=
  lines input
  |>.map (fun line => line.toList.map (fun c => c.toNat - '0'.toNat))
  |>.map (fun l =>
    List.range 12
    |>.map (fun n => l.take (l.length - n))
    |>.foldr (fun sl (i,r) =>
      let sl := sl.drop i
      let x := match sl.max? with
      | none => 0
      | some x => x
      (i + (sl.idxOf x) + 1, r ++ [x])
    ) (0,[0])
    |>.snd
    |>.map (·.digitChar)
    |>.asString
    |>.toNat!
  )
  |>.sum

def run : IO Unit := do
  let input ← readInput 3
  IO.println s!"Part 1: {part1 input}"
  IO.println s!"Part 2: {part2 input}"

end AoC.Day03
