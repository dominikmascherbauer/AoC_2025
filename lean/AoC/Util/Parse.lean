namespace AoC.Util

def lines (s : String) : List String :=
  s.splitToList (· = '\n')
  |>.map String.trim
  |>.filter (· ≠ "")

def linesNoTrim (s : String) : List String :=
  s.splitToList (· = '\n')
  |>.filter (· ≠ "")

def ints (s : String) : List Int :=
  s.splitToList (· = ' ')
  |>.filterMap String.toInt?

end AoC.Util
