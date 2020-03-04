(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Tyvar (S: TYVAR_STRUCTS): TYVAR = 
struct

open S

structure Wrap = Region.Wrap
open Wrap
type node' = {name: string,
              equality: bool}
type t = node' Wrap.t
type obj = t

fun toString (tyvar: t) =
   let val {name, equality, ...} = node tyvar
   in (if equality then "''" else "'") ^ name
   end

val layout = Layout.str o toString

local
   fun make sel (tyvar:t) = sel (node tyvar)
in
   val name = make #name
   val isEquality = make #equality
end

fun sameName (a, a') = name a = name a'

fun newRegion ({name, equality}, region) =
   makeRegion ({name = name,
                equality = equality},
               region)

fun new f = newRegion (f, Region.bogus)

fun newLike a = newRegion ({equality = isEquality a,
                            name = name a},
                           region a)

fun newString (s, {left, right}) =
   newRegion (if String.size s > 1
                 andalso #"'" = String.sub (s, 1)
                 then {name = String.extract (s, 2, NONE),
                       equality = true}
              else {name = String.extract (s, 1, NONE),
                    equality = false},
              Region.make {left = left, right = right})

local open Layout
in
   fun layouts ts =
      case Vector.length ts of
         0 => empty
       | 1 => layout (Vector.sub (ts, 0))
       | _ => Layout.tuple (Vector.foldr (fn (x, y) => layout x :: y) nil ts)
end

end
