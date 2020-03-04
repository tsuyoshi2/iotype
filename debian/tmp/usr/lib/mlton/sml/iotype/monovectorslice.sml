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
structure CharVectorSlice = struct
	open CharVectorSlice
	val readElem = Char.readChar
	val writeElem = Char.writeChar
	val readVector = String.readString
	val writeVector = String.writeString
	val readSlice = Substring.readSubstring
	val writeSlice = Substring.writeSubstring
end
structure BoolVectorSlice = IOMonoVectorSlice (
	structure Vector = BoolVector
	structure VectorSlice = BoolVectorSlice
)
structure WordVectorSlice = IOMonoVectorSlice (
	structure Vector = WordVector
	structure VectorSlice = WordVectorSlice
)
structure Word8VectorSlice = IOMonoVectorSlice (
	structure Vector = Word8Vector
	structure VectorSlice = Word8VectorSlice
)
structure Word16VectorSlice = IOMonoVectorSlice (
	structure Vector = Word16Vector
	structure VectorSlice = Word16VectorSlice
)
structure Word32VectorSlice = IOMonoVectorSlice (
	structure Vector = Word32Vector
	structure VectorSlice = Word32VectorSlice
)
structure Word64VectorSlice = IOMonoVectorSlice (
	structure Vector = Word64Vector
	structure VectorSlice = Word64VectorSlice
)
structure IntVectorSlice = IOMonoVectorSlice (
	structure Vector = IntVector
	structure VectorSlice = IntVectorSlice
)
structure Int8VectorSlice = IOMonoVectorSlice (
	structure Vector = Int8Vector
	structure VectorSlice = Int8VectorSlice
)
structure Int16VectorSlice = IOMonoVectorSlice (
	structure Vector = Int16Vector
	structure VectorSlice = Int16VectorSlice
)
structure Int32VectorSlice = IOMonoVectorSlice (
	structure Vector = Int32Vector
	structure VectorSlice = Int32VectorSlice
)
structure Int64VectorSlice = IOMonoVectorSlice (
	structure Vector = Int64Vector
	structure VectorSlice = Int64VectorSlice
)
structure RealVectorSlice = IOMonoVectorSlice (
	structure Vector = RealVector
	structure VectorSlice = RealVectorSlice
)
structure Real32VectorSlice = IOMonoVectorSlice (
	structure Vector = Real32Vector
	structure VectorSlice = Real32VectorSlice
)
structure Real64VectorSlice = IOMonoVectorSlice (
	structure Vector = Real64Vector
	structure VectorSlice = Real64VectorSlice
)
