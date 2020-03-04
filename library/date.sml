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
structure Date = struct
	open Date
	fun writeWeekday (p, w) = BinIO.output1 (
		p
		, case w of
			Mon => 0w0
			| Tue => 0w1
			| Wed => 0w2
			| Thu => 0w3
			| Fri => 0w4
			| Sat => 0w5
			| Sun => 0w6
	)
	fun readWeekday p = case BinIO.input1 p of
		SOME 0w0 => SOME Mon
		| SOME 0w1 => SOME Tue
		| SOME 0w2 => SOME Wed
		| SOME 0w3 => SOME Thu
		| SOME 0w4 => SOME Fri
		| SOME 0w5 => SOME Sat
		| SOME 0w6 => SOME Sun
		| _ => NONE
	fun writeMonth (p, m) = BinIO.output1 (
		p
		, case m of
			Jan => 0w0
			| Feb => 0w1
			| Mar => 0w2
			| Apr => 0w3
			| May => 0w4
			| Jun => 0w5
			| Jul => 0w6
			| Aug => 0w7
			| Sep => 0w8
			| Oct => 0w9
			| Nov => 0w10
			| Dec => 0w11
	)
	fun readMonth p = case BinIO.input1 p of
		SOME 0w0 => SOME Jan
		| SOME 0w1 => SOME Feb
		| SOME 0w2 => SOME Mar
		| SOME 0w3 => SOME Apr
		| SOME 0w4 => SOME May
		| SOME 0w5 => SOME Jun
		| SOME 0w6 => SOME Jul
		| SOME 0w7 => SOME Aug
		| SOME 0w8 => SOME Sep
		| SOME 0w9 => SOME Oct
		| SOME 0w10 => SOME Nov
		| SOME 0w11 => SOME Dec
		| _ => NONE
	fun writeDate (p, d) = (
		writeInt (p, year d)
		; writeMonth (p, month d)
		; BinIO.output1 (p, Word8.fromInt (day d))
		; BinIO.output1 (p, Word8.fromInt (hour d))
		; BinIO.output1 (p, Word8.fromInt (minute d))
		; BinIO.output1 (p, Word8.fromInt (second d))
		; Option.writeOption Time.writeTime (p, offset d)
	)
	fun readDate p =
		let
			infix |>
			fun x |> f = case x of
				NONE => NONE
				| SOME y => f y
		in
			readInt p |> (fn year =>
			readMonth p |> (fn month =>
			BinIO.input1 p |> (fn day =>
			BinIO.input1 p |> (fn hour =>
			BinIO.input1 p |> (fn minute =>
			BinIO.input1 p |> (fn second =>
			Option.readOption Time.readTime p |> (fn offset => SOME (date {
				year = year
				, month = month
				, day = Word8.toInt day
				, hour = Word8.toInt hour
				, minute = Word8.toInt minute
				, second = Word8.toInt second
				, offset = offset
			})
			)))))))
		end
end
