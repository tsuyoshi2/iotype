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
signature ELEMENT = sig
	type elem
	val writeElem: BinIO.outstream * elem -> unit
	val readElem: BinIO.instream -> elem option
end
functor IntElement (Int: sig
	include INTEGER
	val writeInt: BinIO.outstream * int -> unit
	val readInt: BinIO.instream -> int option
end) :> ELEMENT where type elem = Int.int
= struct
	type elem = Int.int
	val writeElem = Int.writeInt
	val readElem = Int.readInt
end
functor WordElement (Word: sig
	include WORD
	val writeWord: BinIO.outstream * word -> unit
	val readWord: BinIO.instream -> word option
end) :> ELEMENT where type elem = Word.word
= struct
	type elem = Word.word
	val writeElem = Word.writeWord
	val readElem = Word.readWord
end
functor RealElement (Real: sig
	include REAL
	val writeReal: BinIO.outstream * real -> unit
	val readReal: BinIO.instream -> real option
end) :> ELEMENT where type elem = Real.real
= struct
	type elem = Real.real
	val writeElem = Real.writeReal
	val readElem = Real.readReal
end
