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
structure CharElement = struct
	type elem = char
	val writeElem = writeChar
	val readElem = readChar
end
structure BoolElement = struct
	type elem = bool
	val writeElem = writeBool
	val readElem = readBool
end
structure IntElement = IntElement (Int)
structure Int8Element = IntElement (Int8)
structure Int16Element = IntElement (Int16)
structure Int32Element = IntElement (Int32)
structure Int64Element = IntElement (Int64)
structure WordElement = WordElement (Word)
structure Word8Element = WordElement (Word8)
structure Word16Element = WordElement (Word16)
structure Word32Element = WordElement (Word32)
structure Word64Element = WordElement (Word64)
structure RealElement = RealElement (Real)
structure Real32Element = RealElement (Real32)
structure Real64Element = RealElement (Real64)
