type evalstrat =
  | Strict
  | Byname
  | Byneed

val emit : evalstrat:evalstrat -> Sll.pure Sll.program -> string
