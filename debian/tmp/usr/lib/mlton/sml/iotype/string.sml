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
structure String = struct
	open String
	fun writeString (p, x) = (
		writeInt (p, size x)
		; BinIO.output (p, Byte.stringToBytes x)
	)
	fun readString p = case readInt p of
		NONE => NONE
		| SOME n =>
			let
				val v = BinIO.inputN (p, n)
			in
				if Int.< (Word8Vector.length v, n) then NONE
				else SOME (Byte.bytesToString v)
			end
end
val writeString = String.writeString
val readString = String.readString
