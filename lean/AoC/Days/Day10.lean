import AoC.Util.IO
import AoC.Util.Parse
open AoC.Util


namespace AoC.Day10

partial def bfs (goal: Nat) (btns: List Nat) (visited: List Nat) (queue: Std.Queue (Nat × Nat)) :=
  match queue.dequeue? with
  | none => 0
  | some ((depth,lights),queue) =>
    if visited.contains lights then
      bfs goal btns visited queue
    else
      if lights = goal then
        depth
      else
        bfs goal btns (lights::visited) (queue.enqueueAll (btns.map (fun btn => (depth+1, Nat.xor lights btn))))


def part1 (input: String) :=
  let lights := lines input
    |>.map (fun line =>
      line.drop 1
      |>.takeWhile (· ≠ ']')
      |>.toList.mapIdx (fun idx c => if c = '#' then Nat.shiftLeft 1 idx else 0)
      |>.sum
    )
  let buttons := lines input
    |>.map (fun line => line.dropWhile (· ≠ ' ') |>.takeWhile (· ≠ '{') |>.trim)
    |>.map (fun buttons =>
      buttons.splitToList (· = ' ')
      |>.map (fun button =>
        button.drop 1
        |>.dropRight 1
        |>.splitToList (· = ',')
        |>.map (·.toNat!)
        |>.foldl (fun acc n => acc + (Nat.shiftLeft 1 n)) 0
      )
    )
  lights.mapIdx (fun idx light => bfs light buttons[idx]! [] (Std.Queue.mk [] [(0,0)]))
  |>.sum


def patterns (btns: List (List Nat)): List (Nat × List Nat) :=
  let combs := aux btns[0]!.length [] btns
  -- filter duplicate combinations
  combs.filter (fun (n,comb) => ¬combs.any (fun (n2,comb2) => comb = comb2 ∧ n2 < n))
where
  aux (btnSize: Nat) (pressedBtns: List (List Nat)) (btns: List (List Nat)) :=
    match btns with
    | [] =>
      let init := List.range btnSize |>.map (fun _ => 0)
      [(pressedBtns.length, pressedBtns.foldl (fun (acc: List Nat) btn => acc.zipWith (·+·) btn) init)]
    | btn::btns =>
      (aux btnSize (btn::pressedBtns) btns) ++
      (aux btnSize pressedBtns btns)

partial def getMinPresses (patterns: List (Nat × (List Nat))) (joltages: List Nat) (cache: List ((List Nat) × Nat) := []): Nat × List ((List Nat) × Nat) :=
  match cache.find? (·.fst = joltages) with
  | some v => (v.snd, cache)
  | none =>
    if joltages.all (· = 0) then
      (0, (joltages,0)::cache)
    else
      match patterns.filter (fun (_,pattern) => joltages.zip pattern |>.all (fun (j,pj) => j ≥ pj ∧ j % 2 = pj % 2))
        |>.map (fun (presses,pattern) => (presses,joltages.zipWith (fun j jp => (j-jp)/2) pattern))
        |>.foldl (fun (minPresses,cache) (cost,joltages) =>
          let (recMinPresses,cache) := getMinPresses patterns joltages cache
          (min minPresses (cost + 2*recMinPresses), cache)
        ) (1_000_000,cache) with -- start with a very high default value for min number of presses
      | (minPresses,cache) => (minPresses, (joltages,minPresses)::cache)


def part2 (input: String) :=
  let joltagesList := lines input
    |>.map (fun line =>
      line.dropWhile (· ≠ '{')
      |>.drop 1
      |>.takeWhile (· ≠ '}')
      |>.splitToList (· = ',')
      |>.map (·.toNat!)
    )
  let buttonsList := lines input
    |>.map (fun line => line.dropWhile (· ≠ ' ') |>.takeWhile (· ≠ '{') |>.trim)
    |>.map (fun buttons =>
      buttons.splitToList (· = ' ')
      |>.map (fun button =>
        button.drop 1
        |>.dropRight 1
        |>.splitToList (· = ',')
        |>.map (·.toNat!)
      )
    )

  joltagesList.zip buttonsList
  |>.map (fun (joltages,buttons) => (joltages,buttons.map (fun btn => (List.range joltages.length |>.map (fun idx => if idx ∈ btn then 1 else 0)))))
  |>.map (fun (joltages,buttons) => getMinPresses (patterns buttons) joltages)
  |>.map (·.fst)
  |>.sum

def run : IO Unit := do
  let input ← readInput 10
  IO.println s!"Part 1: {part1 input}"
  IO.println s!"Part 2: {part2 input}"

end AoC.Day10
