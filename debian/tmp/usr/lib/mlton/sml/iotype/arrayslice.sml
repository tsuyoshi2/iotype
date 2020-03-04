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
structure ArraySlice = struct
	open ArraySlice
	fun writeSlice w (p, s) =
		let
			val (array, offset, length) = base s
		in
			writeArray w (p, array)
			; writeInt (p, offset)
			; writeInt (p, length)
		end
	fun readSlice r p = case readArray r p of
		NONE => NONE
		| SOME array => (case readInt p of
			NONE => NONE
			| SOME offset => (case readInt p of
				NONE => NONE
				| length => SOME (slice (array, offset, length))
			)
		)
end
