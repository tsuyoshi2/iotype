val () = OS.Process.exit (
	let
		val name = OS.FileSys.tmpName ()
	in
		let
			val outstream = BinIO.openOut name
		in
			writeInt (outstream, 1234)
			; BinIO.closeOut outstream
		end
		; let
			val instream = BinIO.openIn name
		in
			(case readInt instream of
				SOME 1234 => OS.Process.success
				| _ => OS.Process.failure
			) before BinIO.closeIn instream
		end
		before OS.FileSys.remove name
	end
)
