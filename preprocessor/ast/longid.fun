(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Longid (S: LONGID_STRUCTS): LONGID =
struct

open S

datatype node = T of {strids: Strid.t list,
                      id: Id.t}

type node' = node
structure Wrap = Region.Wrap
open Wrap
type t = node Wrap.t
type obj = t

fun split id =
   let
      val T {strids, id, ...} = node id
   in
      (strids, id)
   end

val equals =
   fn (id, id') =>
   let
      val T {strids=ss, id=i} = node id
      val T {strids=ss', id=i'} = node id'
   in
      ListPair.allEq Strid.equals (ss, ss') andalso Id.equals (i, i')
   end

fun long (strids, id) =
   makeRegion (T {strids = strids, id = id},
               case strids of
                  [] => Id.region id
                | s :: _ => Region.append (Strid.region s, Id.region id))

fun short id = long ([], id)

fun layout id =
   let
      val T {strids, id} = node id
      open Layout
   in
      seq [case strids of
              [] => empty
            | _ => seq [seq (separate (List.map Strid.layout strids, ".")),
                        str "."],
           Id.layout id]
   end

val toString = Layout.toString o layout

fun fromSymbols (ss: Symbol.t list, region: Region.t): t =
   let
      val srs =
         case Region.left region of
             NONE => List.map (fn s => (s, region)) ss
           | SOME p =>
             let
                val file = SourcePos.file p
                val line = SourcePos.line p
		fun unfoldr (a, f) =
		   let
		      fun loop (a, bs) =
			 case f a of
			    NONE => bs
			  | SOME (b, a') => loop (a', b :: bs)
		   in
		      loop (a, [])
		   end
		fun unfold (a, f) = rev (unfoldr (a, f))
             in
                unfold
                ((ss, SourcePos.column p),
                 fn (s::ss, cl) =>
                    let
                       val cr = cl + String.size (Symbol.toString s)
                    in
                       SOME
                       ((s, Region.make
                            {left = SourcePos.make {column = cl,
                                                    file = file,
                                                    line = line},
                             right = SourcePos.make {column = cr,
                                                     file = file,
                                                     line = line}}),
                        (ss, cr + 1))
                    end
                  | ([], _) => NONE)
             end
      val (strids, id) =
		let
			fun loop (r, l) = case l of
				[] => raise List.Empty
				| a :: [] => (rev r, a)
				| a :: b => loop (a :: r, b)
		in
			loop ([], srs)
		end
   in
      makeRegion (T {strids = List.map Strid.fromSymbol strids,
                     id = Id.fromSymbol id},
                  region)
   end

end
