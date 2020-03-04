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
structure BoolArray = IOMonoArray (
	structure Element = BoolElement
	structure Array = BoolArray
	structure Vector = BoolVector
)
structure CharArray = IOMonoArray (
	structure Element = CharElement
	structure Array = CharArray
	structure Vector = CharVector
)
structure IntArray = IOMonoArray (
	structure Element = IntElement
	structure Array = IntArray
	structure Vector = IntVector
)
structure Int8Array = IOMonoArray (
	structure Element = Int8Element
	structure Array = Int8Array
	structure Vector = Int8Vector
)
structure Int16Array = IOMonoArray (
	structure Element = Int16Element
	structure Array = Int16Array
	structure Vector = Int16Vector
)
structure Int32Array = IOMonoArray (
	structure Element = Int32Element
	structure Array = Int32Array
	structure Vector = Int32Vector
)
structure Int64Array = IOMonoArray (
	structure Element = Int64Element
	structure Array = Int64Array
	structure Vector = Int64Vector
)
structure WordArray = IOMonoArray (
	structure Element = WordElement
	structure Array = WordArray
	structure Vector = WordVector
)
structure Word8Array = IOMonoArray (
	structure Element = Word8Element
	structure Array = Word8Array
	structure Vector = Word8Vector
)
structure Word16Array = IOMonoArray (
	structure Element = Word16Element
	structure Array = Word16Array
	structure Vector = Word16Vector
)
structure Word32Array = IOMonoArray (
	structure Element = Word32Element
	structure Array = Word32Array
	structure Vector = Word32Vector
)
structure Word64Array = IOMonoArray (
	structure Element = Word64Element
	structure Array = Word64Array
	structure Vector = Word64Vector
)
structure RealArray = IOMonoArray (
	structure Element = RealElement
	structure Array = RealArray
	structure Vector = RealVector
)
structure Real32Array = IOMonoArray (
	structure Element = Real32Element
	structure Array = Real32Array
	structure Vector = Real32Vector
)
structure Real64Array = IOMonoArray (
	structure Element = Real64Element
	structure Array = Real64Array
	structure Vector = Real64Vector
)
