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
structure CharArraySlice = IOMonoArraySlice (
	structure Array = CharArray
	structure ArraySlice = CharArraySlice
)
structure BoolArraySlice = IOMonoArraySlice (
	structure Array = BoolArray
	structure ArraySlice = BoolArraySlice
)
structure WordArraySlice = IOMonoArraySlice (
	structure Array = WordArray
	structure ArraySlice = WordArraySlice
)
structure Word8ArraySlice = IOMonoArraySlice (
	structure Array = Word8Array
	structure ArraySlice = Word8ArraySlice
)
structure Word16ArraySlice = IOMonoArraySlice (
	structure Array = Word16Array
	structure ArraySlice = Word16ArraySlice
)
structure Word32ArraySlice = IOMonoArraySlice (
	structure Array = Word32Array
	structure ArraySlice = Word32ArraySlice
)
structure Word64ArraySlice = IOMonoArraySlice (
	structure Array = Word64Array
	structure ArraySlice = Word64ArraySlice
)
structure IntArraySlice = IOMonoArraySlice (
	structure Array = IntArray
	structure ArraySlice = IntArraySlice
)
structure Int8ArraySlice = IOMonoArraySlice (
	structure Array = Int8Array
	structure ArraySlice = Int8ArraySlice
)
structure Int16ArraySlice = IOMonoArraySlice (
	structure Array = Int16Array
	structure ArraySlice = Int16ArraySlice
)
structure Int32ArraySlice = IOMonoArraySlice (
	structure Array = Int32Array
	structure ArraySlice = Int32ArraySlice
)
structure Int64ArraySlice = IOMonoArraySlice (
	structure Array = Int64Array
	structure ArraySlice = Int64ArraySlice
)
structure RealArraySlice = IOMonoArraySlice (
	structure Array = RealArray
	structure ArraySlice = RealArraySlice
)
structure Real32ArraySlice = IOMonoArraySlice (
	structure Array = Real32Array
	structure ArraySlice = Real32ArraySlice
)
structure Real64ArraySlice = IOMonoArraySlice (
	structure Array = Real64Array
	structure ArraySlice = Real64ArraySlice
)
