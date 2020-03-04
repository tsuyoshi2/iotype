(*
	IOType
	Copyright 2011 Christopher Cramer

	Permission to use, copy, modify, and distribute this software and its
	documentation for any purpose and without fee is hereby granted,
	provided that the above copyright notice appear in all copies and that
	both the copyright notice and this permission notice and warranty
	disclaimer appear in supporting documentation, and that the name of
	the above copyright holders, or their entities, not be used in
	advertising or publicity pertaining to distribution of the software
	without specific, written prior permission.

	The above copyright holders disclaim all warranties with regard to
	this software, including all implied warranties of merchantability and
	fitness. In no event shall the above copyright holders be liable for
	any special, indirect or consequential damages or any damages
	whatsoever resulting from loss of use, data or profits, whether in an
	action of contract, negligence or other tortious action, arising out
	of or in connection with the use or performance of this software.
*)
structure Bool = struct
	open Bool
	fun writeBool (p, x) = BinIO.output1 (p, if x then 0w1 else 0w0)
	fun readBool p = case BinIO.input1 p of
		SOME 0w0 => SOME false
		| SOME 0w1 => SOME true
		| _ => NONE
end

val writeBool = Bool.writeBool
val readBool = Bool.readBool