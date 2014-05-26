module Server where
	import Network
	import Network.HTTP hiding (GET, POST)
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
		forkIO $ case request of
			GET url -> do
				content <- BS.readFile $ case url of
					"/" -> "build/Elm/shakespeare.html"
					u -> "static/" ++ (getFilename u)
				push pipe content
			POST url corpus -> do
				poem <- runShakespeare (Right bst) corpus 2 8
				push pipe poem
		return ()
			where
				getFilename = reverse . takeWhile (/= '/') . reverse