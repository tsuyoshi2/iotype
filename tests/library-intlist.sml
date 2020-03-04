val () = OS.Process.exit (
	let
		val name = OS.FileSys.tmpName ()
	in
		let
			val outstream = BinIO.openOut name
		in
			writeList writeInt (outstream, [1234, 5678, 9012])
			; BinIO.closeOut outstream
		end
		; let
			val instream = BinIO.openIn name
		in
			(case readList readInt instream of
				SOME [1234, 5678, 9012] => OS.Process.success
				| _ => OS.Process.failure
			) before BinIO.closeIn instream
		end
		before OS.FileSys.remove name
	end
)
