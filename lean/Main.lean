import AoC.Days.Day01
import AoC.Days.Day02
import AoC.Days.Day03
import AoC.Days.Day04
import AoC.Days.Day05
import AoC.Days.Day06
import AoC.Days.Day07
import AoC.Days.Day08
import AoC.Days.Day09
import AoC.Days.Day10
import AoC.Days.Day11
import AoC.Days.Day12


def main (args : List String) : IO Unit := do
  match args with
  | ["1"] => AoC.Day01.run
  | ["2"] => AoC.Day02.run
  | ["3"] => AoC.Day03.run
  | ["4"] => AoC.Day04.run
  | ["5"] => AoC.Day05.run
  | ["6"] => AoC.Day06.run
  | ["7"] => AoC.Day07.run
  | ["8"] => AoC.Day08.run
  | ["9"] => AoC.Day09.run
  | ["10"] => AoC.Day10.run
  | ["11"] => AoC.Day11.run
  | ["12"] => AoC.Day12.run
  | _ => IO.println "Usage: lake exe aoc <day>"
