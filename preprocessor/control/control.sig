(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CONTROL =
   sig
      (* Tracing and other informative messages.
       * Some take a verbosity argument that specifies the verbosity level at
       * which messages should be printed. 
       *)
      val message: (unit -> Layout.t) -> unit
      val messageStr: string -> unit
      val sizeMessage: string * 'a -> Layout.t
      val indent: unit -> unit
      val unindent: unit -> unit
      val getDepth: unit -> int

      (*------------------------------------*)
      (*          Error Reporting           *)
      (*------------------------------------*)
      val checkFile: File.t * {fail: string -> 'a,
                               name: string,
                               ok: unit -> 'a} -> 'a
      val checkForErrors: string -> unit
      val error: Region.t * Layout.t * Layout.t -> unit
      val errorStr: Region.t * string -> unit
      (* abort compilation once this many errors reached *)
      val errorThreshhold: int ref
      val numErrors: int ref
      val warning: Region.t * Layout.t * Layout.t -> unit

   end
