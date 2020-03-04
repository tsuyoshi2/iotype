(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Control: CONTROL =
struct

val depth: int ref = ref 0
fun getDepth () = !depth
fun indent () = depth := !depth + 3
fun unindent () = depth := !depth - 3

fun message (th: unit -> Layout.t): unit =
      let
              val out = Out.error
              val lay = th ()
           in
              if Layout.isEmpty lay
                 then ()
              else (Layout.output (Layout.indent (lay, !depth), out)
                    ; Out.newline out)
           end

fun messageStr (s: string): unit =
   message (fn () => Layout.str s)

fun time () =
   let
      open Time
      val {children, self, gc, ...} = times ()
      fun add {utime, stime} = utime + stime
   in
      (add self + add children, add gc)
   end

fun timeToString {total, gc} =
   let
      fun fmt (x, n) = Real.fmt (StringCvt.FIX (SOME n)) x
      val toReal = Real.fromLargeInt o Time.toMilliseconds
      val per =
         if Time.equals (total, Time.zero)
            then "0"
         else fmt (100.0 * (toReal gc / toReal total), 0)
      fun t2s t =
         fmt (Real./ (toReal t, 1000.0), 2)
   in concat [t2s (Time.- (total, gc)), " + ", t2s gc, " (", per, "% GC)"]
   end

(*------------------------------------*)
(*               Errors               *)
(*------------------------------------*)

val numErrors: int ref = ref 0

val errorThreshhold: int ref = ref 20

fun die x = raise Fail x

local
   fun msg (kind: string, r: Region.t, msg: Layout.t, extra: Layout.t): unit =
      let
         open Layout
         val p =
            case Region.left r of
               NONE => "<bogus>"
             | SOME p => SourcePos.toString p
         val msg = Layout.toString msg
         val msg =
            Layout.str
            (concat [String.str (Char.toUpper (String.sub (msg, 0))),
                     String.extract (msg, 1, NONE),
                     "."])
         in
            outputl (align [seq [str (concat [kind, ": "]), str p, str "."],
                            indent (align [msg,
                                           indent (extra, 2)],
                                    2)],
                     Out.error)
      end
in
   fun warning (r, m, e) = msg ("Warning", r, m, e)
   fun error (r, m, e) =
      let
         val () = numErrors := !numErrors + 1
         val _ = msg ("Error", r, m, e)
      in
         if !numErrors = !errorThreshhold
            then die "compilation aborted: too many errors"
         else ()
      end
end

fun errorStr (r, msg) = error (r, Layout.str msg, Layout.empty)

fun checkForErrors (name: string) =
   if !numErrors > 0
      then die (concat ["compilation aborted: ", name, " reported errors"])
   else ()

fun checkFile (f: File.t, {fail: string -> 'a, name, ok: unit -> 'a}): 'a = let
   fun check (test, msg, k) =
      if test f then
         k ()
      else
         fail (concat ["File ", name, " ", msg])
   in
      check (File.doesExist, "does not exist", fn () =>
             check (File.canRead, "cannot be read", ok))
   end

(*---------------------------------------------------*)
(*                  Compiler Passes                  *)
(*---------------------------------------------------*)

datatype 'a display =
   NoDisplay
  | Layout of 'a -> Layout.t
  | Layouts of 'a * (Layout.t -> unit) -> unit

fun 'a sizeMessage (name: string, a: 'a): Layout.t =
   let
	open Layout
	fun toCommaString n =
	   let
	      fun loop (chars, accum) =
		 let
		    fun done () = implode (rev chars @ accum)
		 in
		    case chars of
		       x1 :: x2 :: x3 :: chars =>
			  (case chars of
			      [] => done ()
			    | [#"~"] => done ()
			    | _ => loop (chars, #"," :: x3 :: x2 :: x1 :: accum))
		     | _ => done ()
		 end
	   in loop (rev (explode (Int.toString n)), [])
	   end
   in str (concat [name, " size = ",
                   toCommaString (MLton.size a), " bytes"])
   end

val diagnosticWriter: (Layout.t -> unit) option ref = ref NONE

fun diagnostics f =
   case !diagnosticWriter of
      NONE => ()
    | SOME w => f w

fun diagnostic f = diagnostics (fn disp => disp (f ()))

end
