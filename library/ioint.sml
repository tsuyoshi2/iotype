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
functor IOInt (Integer: INTEGER) = struct
	open Integer
	local
		fun fixed n =
			let
				val (bytesPerElem, subVecX, update) =
					if Int.<= (n, 8) then (
						1
						, Word8.toLargeX o Word8Vector.sub
						, fn (a, i, w) =>
							Word8Array.update (
								a
								, i
								, Word8.fromLarge w
							)
					) else if Int.<= (n, 16) then (
						2
						, PackWord16Little.subVecX
						, PackWord16Little.update
					) else if Int.<= (n, 32) then (
						4
						, PackWord32Little.subVecX
						, PackWord32Little.update
					) else if Int.<= (n, 64) then (
						8
						, PackWord64Little.subVecX
						, PackWord64Little.update
					) else raise Fail "unsupported int size"
			in {
				readInt = fn p =>
					let
						val v = BinIO.inputN (p, bytesPerElem)
					in
						if Int.< (Word8Vector.length v, bytesPerElem) then
							NONE
						else SOME (
							fromLarge (
								LargeWord.toLargeIntX (
									subVecX (v, 0)
								)
							)
						)
					end
				, writeInt = fn (p, x) =>
					let
						val a = Word8Array.array (bytesPerElem, 0w0)
					in
						update (a, 0, LargeWord.fromLargeInt (toLarge x))
						; BinIO.output (p, Word8Array.vector a)
					end
			} end
		val infinite = {
			readInt = (Option.map fromLarge) o IntInf.readInt
			, writeInt = fn (p, x) => IntInf.writeInt (p, toLarge x)
		}
	in
		val {readInt, writeInt} = case precision of
			NONE => infinite
			| SOME n => fixed n
	end
end
