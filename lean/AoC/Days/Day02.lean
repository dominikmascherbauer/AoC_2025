import AoC.Util.IO
import AoC.Util.Parse
open AoC.Util


namespace AoC.Day02

def part1 (input: String) :=
  input.splitToList (· = ',')
  |>.map (fun range => (range.takeWhile (· ≠ '-') |>.trim, range.dropWhile (· ≠ '-') |>.drop 1 |>.trim))
  |>.map (fun (l,u) => ((l.toNat!, u.toNat!), (l.take (l.length/2) |>.toNat? |>.or (some 0) |>.get!, u.dropRight (u.length/2) |>.toNat!)))
  |>.flatMap (
    fun ((l,u),(ls,us)) =>
      List.range (us - ls + 1)
      |>.map (ls + ·)
      |>.map (fun n => s!"{n}{n}".toNat!)
      |>.filter (fun n => l ≤ n ∧ n ≤ u)
    )
  |>.sum

/- bruteforce

  input.splitToList (· = ',')
  |>.map (fun range => (range.takeWhile (· ≠ '-') |>.trim.toNat!, range.dropWhile (· ≠ '-') |>.drop 1 |>.trim.toNat!))
  |>.flatMap (
    fun ((l,u)) =>
      List.range (u - l + 1)
      |>.map (fun n => s!"{l+n}")
      |>.filter (fun n => (n.take (n.length/2)) = (n.drop (n.length/2)))
      |>.map (·.toNat!)
    )
  |>.sum

-/

def part2 (input: String) :=
  input.splitToList (· = ',')
  |>.map (fun range => (range.takeWhile (· ≠ '-') |>.trim, range.dropWhile (· ≠ '-') |>.drop 1 |>.trim))
  |>.map (fun (l,u) => (
      (l,u),
      List.range (u.length/2)
      |>.map (· + 1)
      |>.map (fun n => (n, (l.take n |>.toNat!, u.dropRight (l.length - n) |>.toNat!)))
    ))
  |>.map (fun ((l,u),ranges) =>
    ((l.toNat!, u.toNat!),
    ranges.flatMap (fun (n,(rl,ru)) =>
      List.range (ru - rl + 1)
      |>.map (s!"{· + rl}".take n |>.toNat!)
      |>.eraseDups
      |>.flatMap (fun k =>
        List.range (u.length - l.length + 1)
        |>.map (· + l.length)
        |>.filter (·%n = 0)
        |>.filter (·/n > 1)
        |>.map (fun len => String.join (List.replicate (len/n) s!"{k}") |>.toNat!)
      )
    )))
  |>.flatMap (fun ((l,u),candidates) => candidates.filter (fun k => l ≤ k ∧ k ≤ u) |>.eraseDups)
  |>.sum

def run : IO Unit := do
  let input ← readInput 2
  IO.println s!"Part 1: {part1 input}"
  IO.println s!"Part 2: {part2 input}"

end AoC.Day02
