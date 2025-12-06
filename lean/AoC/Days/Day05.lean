import AoC.Util.IO
import AoC.Util.Parse
open AoC.Util


namespace AoC.Day05

def part1 (input: String) :=
  let lists := lines input
  |>.splitBy (·.contains '-' ↔ ·.contains '-')
  let ranges := lists[0]!.map (fun range => let nats := range.splitToList (· = '-') |>.map (·.toNat!); (nats[0]!,nats[1]!))
  let ingredients := lists[1]!.map (·.toNat!)
  ingredients.countP (fun id => ranges.any (fun (l,u) => l ≤ id ∧ id ≤ u))

def part2 (input: String) :=
  lines input
  |>.splitBy (·.contains '-' ↔ ·.contains '-')
  |>.get!Internal 0
  |>.map (fun range => let nats := range.splitToList (· = '-') |>.map (·.toNat!); (nats[0]!,nats[1]!))
  |>.foldl (fun acc range =>
    let lr := acc.find? (fun (l,u) => l ≤ range.fst ∧ range.fst ≤ u);
    let ur := acc.find? (fun (l,u) => l ≤ range.snd ∧ range.snd ≤ u);
    let acc := acc.filter (fun (l,u) => l < range.fst ∨ range.snd < u);
    match lr,ur with
    | none, none => acc ++ [range]
    | some lr, none => acc.filter (· ≠ lr) |>.append [(lr.fst,range.snd)]
    | none, some ur => acc.filter (· ≠ ur) |>.append [(range.fst,ur.snd)]
    | some lr, some ur => acc.filter (fun r => r ≠ ur ∧ r ≠ lr) |>.append [(lr.fst,ur.snd)]
  ) []
  |>.map (fun (l,u) => u - l + 1)  -- +1 because both limits are inclusive
  |>.sum

def run : IO Unit := do
  let input ← readInput 5
  IO.println s!"Part 1: {part1 input}"
  IO.println s!"Part 2: {part2 input}"

end AoC.Day05
