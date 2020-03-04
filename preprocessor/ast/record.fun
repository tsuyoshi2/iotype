(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* empty tuple is also a record *)

functor Record (S: RECORD_STRUCTS): RECORD = 
struct

open S

datatype 'a t =
   Tuple of 'a vector
 | Record of (Field.t * 'a) vector

val tuple = Tuple

fun toVector r =
   case r of
      Tuple v => Vector.mapi (fn (i, x) => (Field.Int i, x)) v
    | Record r => r

fun detupleOpt (r: 'a t): 'a vector option =
   case r of
      Tuple t => SOME t
    | Record _ => NONE

fun sort v =
   QuickSort.sortVector (v, fn ((s, _), (s', _)) => Field.<= (s, s'))

fun fromVector v =
   let
      fun isTuple v : bool =
         let
            val n = Vector.length v
            fun loop i =
               if i >= n then true
               else case Vector.sub (v, i) of
                  (Field.Int i', _) => if i = i' then loop (i + 1) else false
                  | _ => false
         in
            loop 0
         end
      val v = if isSorted then sort v else v
   in
      if isTuple v
         then Tuple (Vector.map #2 v)
      else Record v
   end

val peek: 'a t * Field.t -> 'a option =
   fn (r, f) =>
   case r of
      Record r => (
         case Vector.find (fn (f', _) => Field.equals (f, f')) r of
            NONE => NONE
            | SOME (_, x) => SOME x)
    | Tuple t =>
         if Vector.length t = 0
            then NONE
         else (case f of
                  Field.Int i =>
                     if 0 <= i andalso i < Vector.length t
                        then SOME (Vector.sub (t, i))
                     else NONE
                | Field.Symbol _ => NONE)

fun range r =
   case r of
      Tuple t => t
    | Record r => Vector.map #2 r

fun exists (r, p) =
   case r of
      Tuple xs => Vector.exists p xs
    | Record r => Vector.exists (fn (_, x) => p x) r

fun forall (r, p) = not (exists (r, not o p))

fun map (r: 'a t, f: 'a -> 'b): 'b t =
   case r of
      Tuple xs => Tuple (Vector.map f xs)
    | Record r => Record (Vector.map (fn (field, a) => (field, f a)) r)

fun foreach (r: 'a t, f: 'a -> unit): unit =
   case r of
      Tuple xs => Vector.app f xs
    | Record r => Vector.app (f o #2) r

fun change (r: 'a t, f: 'a vector -> 'b vector * 'c): 'b t * 'c =
   case r of
      Tuple xs => let val (ys, c) = f xs
                  in (Tuple ys, c)
                  end
    | Record r => let val (fs, xs) = (Vector.map #1 r, Vector.map #2 r)
                      val (ys, c) = f xs
                  in (Record (Vector.tabulate (Vector.length r, fn i => (Vector.sub (fs, i), Vector.sub (ys, i)))), c)
                  end

fun zip (y, z) = fromVector (Vector.tabulate (Vector.length y, fn i => (Vector.sub (y, i), Vector.sub (z, i))))

fun layout {record, layoutTuple, separator, extra, layoutElt} =
   case (record, extra) of
      (Tuple xs, "") => layoutTuple xs
    | _ =>
         let
            val r = toVector record
            open Layout
         in seq [str "{",
                mayAlign (separateRight (
			Vector.foldr (fn ((f, x), l) =>
				(seq [Field.layout f, str separator, layoutElt x]) :: l
			) nil r
		        , ",")),
                str extra,
                str "}"]
         end

end
