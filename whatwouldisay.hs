module WhatWouldISay where
	import Prelude hiding (id, (.))
	import Probabilities
	import Probabilities.Markov
	import Probabilities.DistBuilder
	import Probabilities.Lens
	import System.Random
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy
	import Control.Monad.Identity
	import Control.Category

	firstTwo :: [a] -> Maybe (a, a)
	firstTwo [] = Nothing
	firstTwo [x] = Nothing
	firstTwo (x:y:xs) = Just (x, y)

	runWhatWouldISay :: String -> IO ()
	runWhatWouldISay corpus = do
		g <- newStdGen
		let text = concat . map words . lines $ corpus
		runDistT (runStateT whatWouldISay text) g
		return ()

	whatWouldISay :: RandomGen r => StateT [String] (DistributionT r IO) ()
	whatWouldISay = do
		dBigrams <- with (newSnd newDistBuilder) buildBigrams
		let transitionModel = \s -> dBigrams `givenA` (== s)
		(startState, _) <- lift dBigrams
		let markovModel = Markov (Certain startState) transitionModel
		with (newState markovModel) say

	buildBigrams :: RandomGen r => StateT ([String], DistBuilder (String, String)) (DistributionT r IO) (DistributionT r IO (String, String))
	buildBigrams = do
		maybePair <- with onlyFst $ gets firstTwo
		case maybePair of
			Just pair -> do
				with onlyFst $ modify (drop 1)
				with onlySnd $ addValueState pair
				buildBigrams
			Nothing -> do
				with onlySnd $ getDistState

	say :: RandomGen r => StateT (MarkovT r IO String) (DistributionT r IO) ()
	say = do
		word <- collapseState
		lift . lift . putStr $ word ++ " "
		transitionState
		if hasPunctuation word then lift . lift . putStrLn $ "" else say
			where
				hasPunctuation word = elem '.' word || elem '!' word || elem '?' word