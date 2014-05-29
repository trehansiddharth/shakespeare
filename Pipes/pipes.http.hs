{-# LANGUAGE DatatypeContexts, OverloadedStrings #-}

module Pipes.Http where
	import Network
	import Network.HTTP hiding (GET, POST, Request)
	import Network.Socket.Internal (PortNumber (PortNum))
	import Network.HTTP.Server
	import Network.HTTP.Server.Logger
	import Network.HTTP.Base
	import System.IO.Strict (hGetContents)
	import System.IO hiding (hGetContents)
	import Control.Monad
	import Pipes
	import qualified Data.ByteString as BS
	import qualified Data.ByteString.Internal as BS (c2w, w2c)

	--bind :: PortNumber -> IO (Pipe (Pipe (Request BS.ByteString) BS.ByteString) a)
	bind site = spawn (bindSocket (PortNum site)) >>= return . invert

	bindSocket site pipe = withSocketsDo . serverWith (Config stdLogger "localhost" site) $ \sockAddr url req -> do
		p <- construct
		push p req
		push pipe (invert p)
		msg <- pull p
		return . insertHeader HdrContentLength (show . BS.length $ msg) . Response (2, 0, 0) "" [] $ msg
	
	{--portHandler h pipe = do
		top <- hGetLine h >>= return . words
		let protocol = head top
		let url = (!! 1) $ top
		load <- getData 0 h
		let d = if protocol == "POST" then POST url load else GET url
		print d
		push pipe d
		resp <- pull pipe
		(hPutStr h . write_msg . b2s) resp

	getData i h = do
		d <- hGetLine h
		d2 <- if d == ""
			then replicateM i (hGetChar h) >>= return . s2b
			else do
				let params = words d
				if params == []
					then replicateM i (hGetChar h) >>= return . s2b
					else if head params == "Content-Length:"
						then getData (read (params !! 1)) h
						else getData i h
		return d2--}
	
	write_msg msg = concat [bare_headers, "\r\nContent-Length: ", (show . length) msg, "\r\n\r\n", msg, "\r\n"]
	
	keep_alive_headers = "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nConnection: Keep-Alive"
	bare_headers = "HTTP/1.1 200 OK\r\nContent-Type: text/html"
	medium_headers = "HTTP/1.1 200 OK\r\nServer: spin.hs/0.0.1 (Ubuntu)\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\nContent-Encoding: gzip"
	full_headers = "HTTP/1.1 200 OK\r\nServer: spin.hs/0.0.1 (Ubuntu)\r\nDate: Sun, 02 Feb 2014 15:52:00 GMT\r\nContent-Type: text/html\r\nLast-Modified: Mon, 06 May 2013 10:26:49 GMT\r\nTransfer-Encoding: chunked\r\nContent-Encoding: gzip"

	b2s = map BS.w2c . BS.unpack
	s2b = BS.pack . map BS.c2w
