(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SYMBOL_STRUCTS = 
   sig
   end

signature SYMBOL = 
   sig
      include SYMBOL_STRUCTS

      type t

      (* <= is alphabetical order *)
      val <= : t * t -> bool
      val asterisk: t
      val bogus: t
      val compare: t * t -> order
      val equal: t
      val equals: t * t -> bool
      val fromString: string -> t
      val itt: t
      val layout: t -> Layout.t
      val toString: t -> string
      val unit: t
   end
