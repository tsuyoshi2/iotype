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
structure IntInf = struct
	open IntInf
	fun writeInt (p, x) =
		let
			fun loop (l, y) = 
				let
					val m = Word8.fromLargeInt (andb (y, 0xff)) :: l
				in
					if y = 0 orelse y = ~1 then m
					else loop (m, ~>> (y, 0w8))
				end
			val v = Word8Vector.fromList (rev (loop (nil, x)))
		in
			Word.writeWord (p, Word.fromInt (Word8Vector.length v))
			; BinIO.output (p, v)
		end
	fun readInt p = case Word.readWord p of
		NONE => NONE
		| SOME m =>
			let
				val n = Word.toInt m
				val v = BinIO.inputN (p, n)
				val op * = Int.*
				val op - = Int.-
				val op + = Int.+
				val op < = Int.<
				fun align (x, i) = << (x, Word.fromInt ((n - i - 1) * 8))
				fun loop (i, x) =
					if i = n - 1 then x
					else loop (
						i + 1
						, orb (
							x
							, align (
								Word8.toLargeInt (
									Word8Vector.sub (v, i)
								), i
							)
						)
					)
			in
				if Word8Vector.length v < n then NONE
				else SOME (loop (0, Word8.toLargeIntX (Word8Vector.sub (v, n - 1))))
			end
end
