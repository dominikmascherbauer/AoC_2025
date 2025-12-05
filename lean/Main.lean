import AoC.Days.Day01
import AoC.Days.Day02
import AoC.Days.Day03
import AoC.Days.Day04


def main (args : List String) : IO Unit := do
  match args with
  | ["1"] => AoC.Day01.run
  | ["2"] => AoC.Day02.run
  | ["3"] => AoC.Day03.run
  | ["4"] => AoC.Day04.run
  | _ => IO.println "Usage: lake exe aoc <day>"
