(* Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature TYVAR_STRUCTS = 
   sig
   end

signature TYVAR = 
   sig
      include TYVAR_STRUCTS
      type t
      val layout: t -> Layout.t
      include WRAPPED sharing type obj = t

      val isEquality: t -> bool
      val layouts: t vector -> Layout.t
      val name: t -> string
      val newLike: t -> t
      (* newString "'a" creates a type variable named a
       * newString "''a" creates an equality type variable named a
       *)
      val newString: string * {left: SourcePos.t,
                               right: SourcePos.t} -> t
      val sameName: t * t -> bool
      val toString: t -> string
   end
