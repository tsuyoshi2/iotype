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
structure Real64 = IOReal (
	structure PackReal = PackReal64Little
	structure Real = Real64
)
structure Real32 = IOReal (
	structure PackReal = PackReal32Little
	structure Real = Real32
)
structure Real = struct
	open Real
	val (readReal, writeReal) =
		if radix = Real64.radix andalso precision = Real64.precision then
			(
				(Option.map (fromLarge IEEEReal.TO_NEAREST o Real64.toLarge))
					o Real64.readReal
				, fn (p, x) => Real64.writeReal (p
					, Real64.fromLarge IEEEReal.TO_NEAREST (toLarge x))
			)
		else if radix = Real32.radix andalso precision = Real32.precision then
			(
				(Option.map (fromLarge IEEEReal.TO_NEAREST o Real32.toLarge))
					o Real32.readReal
				, fn (p, x) => Real32.writeReal (p
					, Real32.fromLarge IEEEReal.TO_NEAREST (toLarge x))
			)
		else raise Fail "unsupported real size"
end
val writeReal = Real.writeReal
val readReal = Real.readReal
