import AoC.Util.IO
import AoC.Util.Parse
open AoC.Util


namespace AoC.Day07

def part1 (input: String) :=
  lines input
  |>.foldl (fun acc line =>
    match acc with
    | (n,[]) => (n, [line.map (fun c => if c = 'S' then '|' else c)])
    | (n,x::xs) =>
      let idxs := x.toList.mapIdx (fun idx c => (idx,c))
        |>.filter (fun (_,c) => c = '|')
        |>.map (·.fst)
      let nLine := idxs.foldl (fun (n,line) idx =>
          match line.toList[idx]? with
          -- no splitter at the edge -> so dont worry about idx - 1 beeing negative
          | some '^' => (n + 1, line.take (idx - 1) |>.append "|^|" |>.append (line.drop (idx + 2)))
          | _ => (n,line.take idx |>.append "|" |>.append (line.drop (idx + 1)))
        ) (0,line)
      (n + nLine.fst, nLine.snd::x::xs)
  ) (0,[])
  |>.fst

def part2 (input: String) :=
  (lines input
  |>.foldl (fun acc line =>
    match acc with
    | [] => [line.toList.map (fun c => if c = 'S' then (1,'|') else (0,c))]
    | x::xs =>
      let idxs := x.mapIdx (fun idx c => (idx,c))
        |>.filter (fun (_,(_,c)) => c = '|')
        |>.map (·.fst)
      let nLine := idxs.foldl (fun line idx =>
          let nTimelines := x[idx]!.fst
          match line[idx]? with
          -- no splitter at the edge -> so dont worry about idx - 1 beeing negative
          | some (_,'^') => (line.take (idx - 1)) ++ [(line[idx-1]!.fst + nTimelines, '|')] ++ [(nTimelines,'^')] ++ [(line[idx+1]!.fst + nTimelines, '|')] ++ (line.drop (idx + 2))
          | _ => (line.take idx) ++ [(line[idx]!.fst + nTimelines, '|')] ++ (line.drop (idx + 1))
        ) (line.toList.map ((0,·)))
      nLine::x::xs
  ) [])[0]!.map (·.fst)
  |>.sum


def run : IO Unit := do
  let input ← readInput 7
  IO.println s!"Part 1: {part1 input}"
  IO.println s!"Part 2: {part2 input}"

end AoC.Day07
