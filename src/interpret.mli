open Sll

exception Interpret_error of string

val run : program -> expr
