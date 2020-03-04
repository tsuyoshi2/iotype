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
functor IOMonoArray (
	structure Element: ELEMENT
	structure Array: MONO_ARRAY where type elem = Element.elem
	structure Vector: sig
		include MONO_VECTOR
		val writeVector: BinIO.outstream * vector -> unit
		val readVector: BinIO.instream -> vector option
	end where type vector = Array.vector
) = struct
	open Array
	val writeElem = Element.writeElem
	val readElem = Element.readElem
	val writeVector = Vector.writeVector
	val readVector = Vector.readVector
	fun writeArray (p, a) = (
		writeInt (p, length a)
		; app (fn x => Element.writeElem (p, x)) a
	)
	fun readArray p = case readInt p of
		NONE => NONE
		| SOME 0 => SOME (Array.fromList nil)
		| SOME n => (case Element.readElem p of
			NONE => NONE
			| SOME x =>
				let
					val a = array (n, x)
					fun loop i =
						if i = n then SOME a
						else case Element.readElem p of
							NONE => NONE
							| SOME y => (
								Array.update (a, i, y)
								; loop (i + 1)
							)
				in
					loop 1
				end
		)
end
