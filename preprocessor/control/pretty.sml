(* Copyright (C) 2003-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Pretty: PRETTY =
struct

open Layout

fun casee {default, rules, test} =
   let
      fun vectorToListMap (a, f) = Vector.foldr (fn (a, ac) => f a :: ac) [] a
      val rules =
         case default of
            NONE => rules
          | SOME l => Vector.concat [rules, Vector.fromList [(str "_", l)]]
   in
      align [seq [str "case ", test, str " of"],
             indent (alignPrefix (vectorToListMap
                                  (rules, fn (lhs, rhs) =>
                                   mayAlign [seq [lhs, str " =>"], rhs]),
                                  "| "),
                     2)]
   end

fun conApp {arg, con, targs} =
   seq [con,
        empty,
        case arg of
           NONE => empty
         | SOME x => seq [str " ", x]]

fun handlee {catch, handler, try} =
   align [try,
          seq [str "handle ", catch, str " => ", handler]]

fun longid (ls, l) = seq (separate (ls @ [l], "."))

fun nest (prefix, x, y) =
   align [seq [str prefix, x],
          str "in",
          indent (y, 3),
          str "end"]

fun lett (d, e) = nest ("let ", d, e)

fun locall (d, d') = nest ("local ", d, d')

fun primApp {args, prim, targs} =
   seq [prim,
        empty,
        str " ",
        tuple (Vector.foldr op :: nil args)]

fun raisee exn = seq [str "raise ", exn]

fun seq es = mayAlign (separateLeft (Vector.foldr op :: nil es, ";"))

end
