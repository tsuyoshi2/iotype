val () = OS.Process.exit (
	let
		val name = OS.FileSys.tmpName ()
	in
		let
			val outstream = BinIO.openOut name
		in
			Int32.writeInt (outstream, 123456789)
			; BinIO.closeOut outstream
		end
		; let
			val instream = BinIO.openIn name
		in
			(case Int32.readInt instream of
				SOME 123456789 => OS.Process.success
				| _ => OS.Process.failure
			) before BinIO.closeIn instream
		end
		before OS.FileSys.remove name
	end
)
