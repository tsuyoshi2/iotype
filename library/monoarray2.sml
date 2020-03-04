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
structure BoolArray2 = IOMonoArray2 (
	structure Element = BoolElement
	structure MonoArray2 = BoolArray2
	structure Vector = BoolVector
)
structure CharArray2 = IOMonoArray2 (
	structure Element = CharElement
	structure MonoArray2 = CharArray2
	structure Vector = CharVector
)
structure IntArray2 = IOMonoArray2 (
	structure Element = IntElement
	structure MonoArray2 = IntArray2
	structure Vector = IntVector
)
structure Int8Array2 = IOMonoArray2 (
	structure Element = Int8Element
	structure MonoArray2 = Int8Array2
	structure Vector = Int8Vector
)
structure Int16Array2 = IOMonoArray2 (
	structure Element = Int16Element
	structure MonoArray2 = Int16Array2
	structure Vector = Int16Vector
)
structure Int32Array2 = IOMonoArray2 (
	structure Element = Int32Element
	structure MonoArray2 = Int32Array2
	structure Vector = Int32Vector
)
structure Int64Array2 = IOMonoArray2 (
	structure Element = Int64Element
	structure MonoArray2 = Int64Array2
	structure Vector = Int64Vector
)
structure WordArray2 = IOMonoArray2 (
	structure Element = WordElement
	structure MonoArray2 = WordArray2
	structure Vector = WordVector
)
structure Word8Array2 = IOMonoArray2 (
	structure Element = Word8Element
	structure MonoArray2 = Word8Array2
	structure Vector = Word8Vector
)
structure Word16Array2 = IOMonoArray2 (
	structure Element = Word16Element
	structure MonoArray2 = Word16Array2
	structure Vector = Word16Vector
)
structure Word32Array2 = IOMonoArray2 (
	structure Element = Word32Element
	structure MonoArray2 = Word32Array2
	structure Vector = Word32Vector
)
structure Word64Array2 = IOMonoArray2 (
	structure Element = Word64Element
	structure MonoArray2 = Word64Array2
	structure Vector = Word64Vector
)
structure RealArray2 = IOMonoArray2 (
	structure Element = RealElement
	structure MonoArray2 = RealArray2
	structure Vector = RealVector
)
structure Real32Array2 = IOMonoArray2 (
	structure Element = Real32Element
	structure MonoArray2 = Real32Array2
	structure Vector = Real32Vector
)
structure Real64Array2 = IOMonoArray2 (
	structure Element = Real64Element
	structure MonoArray2 = Real64Array2
	structure Vector = Real64Vector
)
