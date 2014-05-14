module Server where
	import Network
	import Network.HTTP
	import System.IO
	import Control.Monad
	import Shakespeare
	import Pipes
	import Pipes.Http
	import qualified Data.ByteString as BS
	import qualified Data.ByteString.Internal as BS (c2w, w2c)

	runServer :: IO ()
	runServer = do
		pClients <- bind 8000
		forever $ do
			pClient <- pull pClients
			msg <- pull pClient
			BS.putStrLn msg
			push pClient "<h1>Hello!</h1>"