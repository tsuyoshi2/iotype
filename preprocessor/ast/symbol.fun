(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Symbol (S: SYMBOL_STRUCTS): SYMBOL = 
struct

open S

type t = string

fun name s = s

fun fromString s = s

fun toString s = s

val layout = Layout.str o toString

val op <= = String.<=
val compare = String.compare

val asterisk = fromString "*"
val bogus = fromString "<bogus>"
val equal = fromString "="
val itt = fromString "it"
val unit = fromString "unit"

fun equals (a, b) = a = b

end
