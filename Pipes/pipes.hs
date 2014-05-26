module Pipes where
	import Control.Concurrent
	import Control.Concurrent.MVar

	data Pipe a b = Pipe (MVar a, MVar b)

	pull :: Pipe a b -> IO a
	pull (Pipe (pin, pout)) = takeMVar pin

	push :: Pipe a b -> b -> IO ()
	push (Pipe (pin, pout)) x = putMVar pout x

	construct :: IO (Pipe a b)
	construct = do
		m1 <- newEmptyMVar
		m2 <- newEmptyMVar
		return (Pipe (m1, m2))

	spawn :: (Pipe a b -> IO ()) -> IO (Pipe a b)
	spawn thread = do
		pipe <- construct
		forkIO $ thread pipe
		return pipe

	invert :: Pipe a b -> Pipe b a
	invert (Pipe (pin, pout)) = Pipe (pout, pin)