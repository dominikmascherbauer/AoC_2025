namespace AoC.Util

def pad2 (n : Nat) : String :=
  if n < 10 then s!"0{n}" else s!"{n}"


def readInput (day: Nat): IO String := do
  let fname := s!"resources/Day{pad2 day}"
  IO.FS.readFile fname

end AoC.Util
