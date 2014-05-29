module Server where
	import Network.HTTP.Server --(rqMethod, rqBody, RequestMethod (GET, POST))
	import Network.URI
	import System.IO
	import Control.Monad
	import Shakespeare
	import Phonetics
	import Pipes
	import Pipes.Http
	import qualified Data.ByteString as BS
	import qualified Data.ByteString.Internal as BS (c2w, w2c)
	import Control.Concurrent

	runServer :: IO ()
	runServer = do
		pClients <- bind 8000
		bst <- getEntries "Phonetics/cmudict.0.7a.txt"
		forever $ handleRequests bst pClients

	handleRequests bst pClients = do
		pipe <- pull pClients
		request <- pull pipe
		forkIO $ case rqMethod request of
			GET -> do
				let url = (uriPath . rqURI) request
				content <- BS.readFile $ case url of
					"/" -> "build/Elm/shakespeare.html"
					u -> "static/" ++ (getFilename u)
				push pipe content
			POST -> do
				let corpus = rqBody request
				poem <- runShakespeare (Right bst) corpus 2 8
				push pipe poem
		return ()
			where
				getFilename = reverse . takeWhile (/= '/') . reverse