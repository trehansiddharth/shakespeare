{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}

module Phonetics where
	import Data.Char
	import qualified Data.ByteString as BS
	import System.Random
	import Control.Monad
	import Data.Array.IO
	import qualified Data.ByteString.Internal as BS (c2w, w2c)

	data Phoneme = Phoneme BS.ByteString (Maybe Int)
		deriving (Eq, Show, Read)

	data Entry = Entry { word :: BS.ByteString, phonetic :: [Phoneme] }
		deriving (Eq, Show, Read)

	data BST a = Tree { left :: (BST a), value :: a, right :: (BST a) } | Empty
		deriving (Eq, Show, Read, Functor)

	size Empty = 0
	size (Tree l x r) = 1 + max (size l) (size r)

	insert :: Ord a => BST a -> a -> BST a
	insert Empty x = Tree Empty x Empty
	insert (Tree l y r) x	| x < y		= Tree (insert l x) y r
							| otherwise	= Tree l y (insert r x)

	findEntry :: BST Entry -> BS.ByteString -> Maybe Entry
	findEntry Empty x = Nothing
	findEntry (Tree l y r) x = guard (x /= "") >> guard (word y /= "") >> tryFind
			where
				tryFind	| x < word y	= findEntry l x
						| x > word y	= findEntry r x
						| x == word y	= Just y

	rhymeCoefficient :: BST Entry -> BS.ByteString -> BS.ByteString -> Maybe Float
	rhymeCoefficient bst w1 w2 = do
		e1 <- findEntry bst $ uppercase w1
		e2 <- findEntry bst $ uppercase w2
		return $ endings (phonetic e1) (phonetic e2)

	uppercase = BS.map (BS.c2w . toUpper . BS.w2c)
	lowercase = BS.map (BS.c2w . toLower . BS.w2c)
	
	endings ps qs = sum $ takeWhile (> 0.0) $ zipWith match (reverse ps) (reverse qs)
	match (Phoneme x m) (Phoneme y n) = if x == y
		then case (m, n) of
			(Just u, Just v) -> if u == v then 1.5 else 1.0
			_ -> 1.0
		else 0.0

	getEntries dictFile = do
		dictionary <- BS.readFile dictFile
		entries <- shuffle $ BS.split (BS.c2w '\n') dictionary
		let bst = fmap parseword . foldl insert Empty . filter (/= "") $ entries
		return bst

	parseword entry = Entry w p
		where
			fields = filter (/= "") . BS.split (BS.c2w ' ') $ entry
			w = head fields
			p = map parseP . tail $ fields
			numbers = map (BS.c2w . head . show) [0 .. 9]
			parseP ph = if elem (BS.last ph) numbers
				then Phoneme (BS.init ph) (Just . read . return . BS.w2c . BS.last $ ph)
				else Phoneme ph Nothing

	shuffle :: [a] -> IO [a]
	shuffle xs = do
		ar <- newArray n xs
		forM [1..n] $ \i -> do
			j <- randomRIO (i,n)
			vi <- readArray ar i
			vj <- readArray ar j
			writeArray ar j vi
			return vj
			where
				n = length xs
				newArray :: Int -> [a] -> IO (IOArray Int a)
				newArray n xs =  newListArray (1,n) xs