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
structure Array2 = struct
	open Array2
	fun writeArray w (p, a) = (
		writeInt (p, nCols a)
		; writeInt (p, nRows a)
		; app RowMajor (fn x => w (p, x)) a
	)
	fun readArray r p =
		case readInt p of
			NONE => NONE
			| SOME columns => (case readInt p of
				NONE => NONE
				| SOME rows => (
					let
						exception Eof
					in
						SOME (tabulate RowMajor (
							rows
							, columns
							, fn _ => (case r p of
								NONE => raise Eof
								| SOME x => x
							)
						))
					end
					handle Eof => NONE
				)
			)
	fun writeRegion w (p, {base, row, col, nrows, ncols}) = (
		writeArray w (p, base)
		; writeInt (p, row)
		; writeInt (p, col)
		; writeOption writeInt (p, nrows)
		; writeOption writeInt (p, ncols)
	)
	fun readRegion r p = case readArray r p of
		NONE => NONE
		| SOME base => (case readInt p of
			NONE => NONE
			| SOME row => (case readInt p of
				NONE => NONE
				| SOME col => (case readOption readInt p of
					NONE => NONE
					| SOME nrows => (case readOption readInt p of
						NONE => NONE
						| SOME ncols => SOME {
							base = base
							, row = row
							, col = col
							, nrows = nrows
							, ncols = ncols
						}
					)
				)
			)
		)
	fun writeTraversal (p, t) = Word8.writeWord (
		p
		, case t of
			RowMajor => 0w0
			| ColMajor => 0w1
	)
	fun readTraversal p = case Word8.readWord p of
		NONE => NONE
		| SOME 0w0 => SOME RowMajor
		| SOME _ => SOME ColMajor
end
