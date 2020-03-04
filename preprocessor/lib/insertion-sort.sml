(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure InsertionSort: INSERTION_SORT =
struct

open Array

(* Based on page 108 of Programming Pearls, by Bentley. *)
fun sort (a: 'a array, op <= : 'a * 'a -> bool): unit =
   let
      fun x i = sub (a, i)
      val _ =
         Array.appi
         (fn (i, _) =>
          let
             val t = x i
             fun sift (j: int) =
                (if j > 0
                      then
                         let
                            val j' = j - 1
                            val z = x j'
                         in
                            if z <= t
                               then j
                            else (update (a, j, z)
                                  ; sift j')
                         end
                   else j)
             val _ = update (a, sift i, t)
          in ()
          end)
   in
      ()
   end

end
