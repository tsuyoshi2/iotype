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
functor IOMonoArraySlice (
	structure Array: sig
		type array
		type elem
		val writeElem: BinIO.outstream * elem -> unit
		val readElem: BinIO.instream -> elem option
		val writeArray: BinIO.outstream * array -> unit
		val readArray: BinIO.instream -> array option
	end
	structure ArraySlice: MONO_ARRAY_SLICE
		where type elem = Array.elem
		where type array = Array.array
) = struct
	open ArraySlice
	val writeElem = Array.writeElem
	val readElem = Array.readElem
	val writeArray = Array.writeArray
	val readArray = Array.readArray
	fun writeSlice (p, s) =
		let
			val (array, offset, length) = base s
		in
			Array.writeArray (p, array)
			; writeInt (p, offset)
			; writeInt (p, length)
		end
	fun readSlice p = case Array.readArray p of
		NONE => NONE
		| SOME array => (case readInt p of
			NONE => NONE
			| SOME offset => (case readInt p of
				NONE => NONE
				| length => SOME (slice (array, offset, length))
			)
		)
end
