val () = OS.Process.exit (
	let
		val name = OS.FileSys.tmpName ()
	in
		let
			val outstream = BinIO.openOut name
		in
			writeChar (outstream, #"X")
			; BinIO.closeOut outstream
		end
		; let
			val instream = BinIO.openIn name
		in
			(case readChar instream of
				SOME #"X" => OS.Process.success
				| _ => OS.Process.failure
			) before BinIO.closeIn instream
		end
		before OS.FileSys.remove name
	end
)
