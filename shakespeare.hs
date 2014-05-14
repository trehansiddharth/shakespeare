{-# LANGUAGE OverloadedStrings #-}

module Shakespeare where
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
	import qualified Data.ByteString as BS
	import qualified Data.ByteString.Internal as BS (c2w, w2c)
	import Phonetics
	import Data.List (sortBy)
	import Data.Ord
	import Data.Tuple (swap)
	import Data.List (intersperse)

	firstTwo :: [a] -> Maybe (a, a)
	firstTwo [] = Nothing
	firstTwo [x] = Nothing
	firstTwo (x:y:xs) = Just (x, y)

	fromJust :: Maybe a -> a
	fromJust Nothing = error "fromJust can only be applied to Just x patterns"
	fromJust (Just x) = x

	reverseSnd :: Ord b => (a, b) -> (a, b) -> Ordering
	reverseSnd x y = case compare (snd x) (snd y) of
		EQ -> EQ
		LT -> GT
		GT -> LT

	bsconcat :: BS.ByteString -> BS.ByteString -> BS.ByteString
	bsconcat x y = BS.concat [x, y]

	runShakespeare :: BS.ByteString -> IO ()
	runShakespeare corpus = do
		g <- newStdGen
		let text = concat . map (BS.split (BS.c2w ' ')) . concat . map (BS.split (BS.c2w '\r')) . BS.split (BS.c2w '\n') $ corpus
		runDistT (runStateT shakespeare text) g
		return ()

	shakespeare :: RandomGen r => StateT [BS.ByteString] (DistributionT r IO) ()
	shakespeare = do
		rhymes <- selectBestRhymes 2
		firstword <- gets head
		modify (++ [firstword])
		dBigrams <- with (newSnd newDistBuilder) buildBigrams
		let transitionModel = buildTransitionModel dBigrams
		let startState = concat . map (\(x, y) -> [x, y]) $ rhymes
		let markovModel = Markov (Certain startState) transitionModel
		let startPoem = replicate (length startState) ""
		poem <- with (newState (markovModel, startPoem)) $ say 10
		lift . lift . BS.putStrLn $ poem

	getRhymes :: RandomGen r => StateT [BS.ByteString] (DistributionT r IO) [((BS.ByteString, BS.ByteString), Float)]
	getRhymes = do
		vocabulary <- get
		bst <- lift . lift $ getEntries
		return $ do
			w1 <- vocabulary
			w2 <- vocabulary
			guard (w1 /= w2)
			guard (w1 > w2)
			let r = rhymeCoefficient bst w1 w2
			guard (r > Nothing)
			return ((w1, w2), fromJust r)

	selectBestRhymes :: RandomGen r => Int -> StateT [BS.ByteString] (DistributionT r IO) [(BS.ByteString, BS.ByteString)]
	selectBestRhymes n = getRhymes >>= return . reverse . selectBest [] . map fst . sortBy reverseSnd
		where
			selectBest bests [] = bests
			selectBest bests ((x, y):zs) = if length bests == n
				then bests
				else if notSelected bests (x, y)
					then selectBest ((x, y) : bests) zs
					else selectBest bests zs
			notSelected bests (x, y) = lookup x bests == Nothing
				&& lookup x (map swap bests) == Nothing
				&& lookup y bests == Nothing
				&& lookup y (map swap bests) == Nothing

	buildBigrams :: RandomGen r => StateT ([BS.ByteString], DistBuilder (BS.ByteString, BS.ByteString)) (DistributionT r IO) (DistributionT r IO (BS.ByteString, BS.ByteString))
	buildBigrams = do
		maybePair <- with onlyFst $ gets firstTwo
		case maybePair of
			Just pair -> do
				with onlyFst $ modify (drop 1)
				with onlySnd $ addValueState pair
				buildBigrams
			Nothing -> do
				with onlySnd $ getDistState

	buildTransitionModel :: RandomGen r => DistributionT r IO (BS.ByteString, BS.ByteString) -> ([BS.ByteString] -> DistributionT r IO [BS.ByteString])
	buildTransitionModel d [] = return []
	buildTransitionModel d (x:xs) = do
		x' <- d `givenB` (== x)
		xs' <- buildTransitionModel d xs
		return (x':xs')

	say :: RandomGen r => Int -> StateT (MarkovT r IO [BS.ByteString], [BS.ByteString]) (DistributionT r IO) BS.ByteString
	say 0 = do
		poem <- gets snd
		return . BS.concat . intersperse "\n" $ poem
	say lineLength = do
		words <- with onlyFst collapseState
		poem <- gets snd
		with onlySnd $ put $ zipWith bsconcat (map (bsconcat " ") words) poem
		with onlyFst transitionState
		say (lineLength - 1)