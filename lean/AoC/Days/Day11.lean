import AoC.Util.IO
import AoC.Util.Parse
open AoC.Util


namespace AoC.Day11


partial def searchPaths (device: String) (goal: String) (conns: List (String × List String)) (visited: List String := []) (cache: List (String × List (List String)) := []): (List (List String)) × List (String × List (List String)) :=
  if device = goal then
    ([[]],cache)
  else if device ∈ visited then
    ([],cache)
  else
    let visited := device::visited
    match cache.find? (·.fst = device) with
    | some (_,cachedPaths) => (cachedPaths, cache)
    | none =>
      let (paths,cache) := conns.filter (fun conn => conn.fst = device)
        |>.flatMap (fun conn => conn.snd)
        |>.foldl (fun (storedPaths,cache) device =>
          let (paths,cache) := searchPaths device goal conns visited cache
          (storedPaths ++ (paths.map (device::·)),cache)
        ) ([],cache)
      (paths,(device,paths)::cache)


def part1 (input: String) :=
  let connections := lines input
    |>.map (fun line => (line.takeWhile (· ≠ ':') |>.trim, line.dropWhile (· ≠ ':') |>.drop 1 |>.trim.splitToList (· = ' ') |>.map (·.trim)))

  searchPaths "you" "out" connections
  |>.fst.length

def part2 (input: String) :=
  let connections := lines input
    |>.map (fun line => (line.takeWhile (· ≠ ':') |>.trim, line.dropWhile (· ≠ ':') |>.drop 1 |>.trim.splitToList (· = ' ') |>.map (·.trim)))

  let srv_fft := searchPaths "svr" "fft" connections
  let fft_dac := searchPaths "fft" "dac" connections
  let dac_out := searchPaths "dac" "out" connections
  srv_fft.fst.length * fft_dac.fst.length * dac_out.fst.length


def run : IO Unit := do
  let input ← readInput 11
  IO.println s!"Part 1: {part1 input}"
  IO.println s!"Part 2: {part2 input}"

end AoC.Day11
