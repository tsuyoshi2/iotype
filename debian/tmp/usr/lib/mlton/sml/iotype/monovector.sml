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
structure CharVector = struct
	open CharVector
	val readVector = String.readString
	val writeVector = String.writeString
end
structure BoolVector = IOMonoVector (
	structure Element = BoolElement
	structure Vector = BoolVector
)
structure IntVector = IOMonoVector (
	structure Element = IntElement
	structure Vector = IntVector
)
structure Int8Vector = IOMonoVector (
	structure Element = Int8Element
	structure Vector = Int8Vector
)
structure Int16Vector = IOMonoVector (
	structure Element = Int16Element
	structure Vector = Int16Vector
)
structure Int32Vector = IOMonoVector (
	structure Element = Int32Element
	structure Vector = Int32Vector
)
structure Int64Vector = IOMonoVector (
	structure Element = Int64Element
	structure Vector = Int64Vector
)
structure WordVector = IOMonoVector (
	structure Element = WordElement
	structure Vector = WordVector
)
structure Word8Vector = IOMonoVector (
	structure Element = Word8Element
	structure Vector = Word8Vector
)
structure Word16Vector = IOMonoVector (
	structure Element = Word16Element
	structure Vector = Word16Vector
)
structure Word32Vector = IOMonoVector (
	structure Element = Word32Element
	structure Vector = Word32Vector
)
structure Word64Vector = IOMonoVector (
	structure Element = Word64Element
	structure Vector = Word64Vector
)
structure Real32Vector = IOMonoVector (
	structure Element = Real32Element
	structure Vector = Real32Vector
)
structure Real64Vector = IOMonoVector (
	structure Element = Real64Element
	structure Vector = Real64Vector
)
structure RealVector = IOMonoVector (
	structure Element = RealElement
	structure Vector = RealVector
)
