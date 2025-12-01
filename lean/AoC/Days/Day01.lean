import AoC.Util.IO
import AoC.Util.Parse
open AoC.Util


namespace AoC.Day01

def part1 (input: String): Nat :=
  lines input
  |>.map (fun line => (line.drop 1 |>.toInt!) * (if line.take 1 == "L" then -1 else 1))
  |>.foldl (fun l dial => l ++ [(l.getLast! + dial) % 100]) [50]
  |>.filter (· = 0)
  |>.length

def part2 (input: String): Nat :=
  lines input
  |>.map (fun line => (line.drop 1 |>.toInt!) * (if line.take 1 == "L" then -1 else 1))
  |>.foldl (
      fun ⟨acc,zeros⟩ dial =>
        let afterDial := acc + dial
        (afterDial % 100, zeros + (if afterDial <= 0 ∧ acc != 0 then 1 else 0) + Int.natAbs afterDial/100)
    ) (50,0)
  |>.snd


def run : IO Unit := do
  let input ← readInput 1
  IO.println s!"Part 1: {part1 input}"
  IO.println s!"Part 2: {part2 input}"

end AoC.Day01
