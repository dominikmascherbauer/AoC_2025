import AoC.Days.Day01
import AoC.Days.Day02


def main (args : List String) : IO Unit := do
  match args with
  | ["1"] => AoC.Day01.run
  | ["2"] => AoC.Day02.run
  | _ => IO.println "Usage: lake exe aoc <day>"
