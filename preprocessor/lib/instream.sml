(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Instream: INSTREAM =
struct

open Instream0

fun outputAll (ins: t, out: Out.t): unit =
   let
      fun loop () =
         case input ins of
            "" => ()
          | s => (Out.output (out, s); loop ())
   in
      loop ()
   end

fun 'a withClose (ins: t, f: t -> 'a): 'a =
   Exn.finally (fn () => f ins, fn () => close ins)

fun 'a withIn (f: string, g: t -> 'a): 'a =
   withClose (openIn f handle IO.Io _ =>
              Error.bug (concat ["Instream.withIn: cannot open ", f]), g)

fun withNull f = withIn ("/dev/zero", f)

fun lines ins = rev (foldLines (ins, [], op ::))

end

structure In = Instream
