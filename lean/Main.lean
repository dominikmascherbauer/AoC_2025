import AoC.Days.Day01


def main (args : List String) : IO Unit := do
  match args with
  | ["1"] => AoC.Day01.run
  | _ => IO.println "Usage: lake exe aoc <day>"
