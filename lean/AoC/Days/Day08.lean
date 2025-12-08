import AoC.Util.IO
import AoC.Util.Parse
open AoC.Util


namespace AoC.Day08

structure JunctionBox where
  x: Nat
  y: Nat
  z: Nat
deriving BEq, Inhabited

instance : ToString JunctionBox where
  toString box := s!"({box.x},{box.y},{box.z})"

def diff (this other: Nat) :=
  if this > other then this - other else other - this

def JunctionBox.distance (this other: JunctionBox): Nat :=
  (((diff this.x other.x).pow 2) + ((diff this.y other.y).pow 2) + ((diff this.z other.z).pow 2)).toFloat.sqrt.toUInt64.toNat

def part1 (input: String) :=
  let boxes := lines input
    |>.map (fun line =>
      let parsed := line.splitToList (· = ',') |>.map (·.toNat!);
      JunctionBox.mk parsed[0]! parsed[1]! parsed[2]!
    )
  boxes.mapIdx (fun idx box => (idx,box))
  |>.flatMap (fun (idx,box1) => boxes.drop (idx + 1) |>.map (fun box2 => (box1.distance box2, (box1,box2))))
  |>.mergeSort (fun (dist1,_) (dist2,_) => dist1 ≤ dist2)
  |>.foldl (fun (nConns,acc) conn =>
    if nConns ≥ 1000 then
      (nConns,acc)
    else
      let box1 := conn.snd.fst;
      let box2 := conn.snd.snd;
      match acc with
      | [] => (nConns+1,[[box1,box2]])
      | circuits =>
        let circ1 := circuits.find? (·.contains box1)
        let circ2 := circuits.find? (·.contains box2)
        match circ1, circ2 with
        | none, none => (nConns+1,[box1,box2]::acc)
        | some circ1, none => (nConns+1,(box2::circ1)::(acc.filter (· != circ1)))
        | none, some circ2 => (nConns+1,(box1::circ2)::(acc.filter (· != circ2)))
        | some circ1, some circ2 => (nConns+1,((circ1 ++ circ2).eraseDups)::(acc.filter (fun circ => circ != circ1 ∧ circ != circ2)))
  ) (0,[])
  |>.snd
  |>.map (·.length)
  |>.mergeSort (fun a b => b ≤ a)
  |>.take 3
  |>.foldl (fun a b => a*b) 1

def part2 (input: String) :=
  let boxes := lines input
    |>.map (fun line =>
      let parsed := line.splitToList (· = ',') |>.map (·.toNat!);
      JunctionBox.mk parsed[0]! parsed[1]! parsed[2]!
    )
  let connectingBoxes := boxes.mapIdx (fun idx box => (idx,box))
    |>.flatMap (fun (idx,box1) => boxes.drop (idx + 1) |>.map (fun box2 => (box1.distance box2, (box1,box2))))
    |>.mergeSort (fun (dist1,_) (dist2,_) => dist1 ≤ dist2)
    |>.foldl (fun (lastConn,acc) conn =>
      if acc.length = 1 ∧ acc[0]!.length = boxes.length then
        (lastConn, acc)
      else
        let box1 := conn.snd.fst;
        let box2 := conn.snd.snd;
        match acc with
        | [] => (conn,[[box1,box2]])
        | circuits =>
          let circ1 := circuits.find? (·.contains box1)
          let circ2 := circuits.find? (·.contains box2)
          match circ1, circ2 with
          | none, none => (conn,[box1,box2]::acc)
          | some circ1, none => (conn,(box2::circ1)::(acc.filter (· != circ1)))
          | none, some circ2 => (conn,(box1::circ2)::(acc.filter (· != circ2)))
          | some circ1, some circ2 => (conn,((circ1 ++ circ2).eraseDups)::(acc.filter (fun circ => circ != circ1 ∧ circ != circ2)))
    ) ((0,boxes[0]!,boxes[0]!),[])
    |>.fst.snd
  connectingBoxes.fst.x * connectingBoxes.snd.x

def run : IO Unit := do
  let input ← readInput 8
  IO.println s!"Part 1: {part1 input}"
  IO.println s!"Part 2: {part2 input}"

end AoC.Day08
