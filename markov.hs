import qualified Data.Map as Map
import qualified Data.List as List
import Random (randomRIO)
import Control.Monad (liftM)

---- utility functions

-- add a carriage return to each string
--
-- ["a", "b"] => ["a\n", "b\n"]
addCarriageReturns :: [String] -> [String]
addCarriageReturns = map (++ "\n")

-- groupInto 2 "hello" => ["he","el","ll","lo"]
groupInto :: Int -> String -> [String]
groupInto len = takeWhile isLen . groups
  where
    isLen = (len ==) . length
    groups word = take len word : groups (drop 1 word)

-- takeLast 2 "test" => "st"
takeLast :: Int -> [a] -> [a]
takeLast n ls = drop (length ls - n) ls

-- butLast "test" => "tes"
butLast :: [a] -> [a]
butLast word = take (length word - 1) word

-- return a random element from the list
--
-- "abcde" => "d"
rand :: [a] -> IO a
rand xs = liftM (xs !!) (randomRIO (0, length xs - 1))

---- app functions

type Table = Map.Map String String  -- the markov table
type Order = Int

-- pairize 2 "hello" => [("he","l"), ("el","l"), ("ll","o")]
pairize :: Order -> String -> [(String, String)]
pairize order = map (splitAt order) . groupInto (order + 1)

-- train table with the given left/right pair
train :: (String, String) -> Table -> Table
train (left, right) = Map.insertWith (++) left right

-- train the table with all pairs in the word
trainWord :: Order -> String -> Table -> Table
trainWord order word table = foldr train table (pairize order word)

-- train table with all words
trainWords :: Order -> [String] -> Table
trainWords order = foldr (trainWord order) Map.empty

-- return keys from table that are starting rows
starters :: Table -> [String]
starters = filter startsWithCapital . Map.keys
  where startsWithCapital = flip elem ['A'..'Z'] . head

-- return true if the word has ended
hasEnded :: String -> Bool
hasEnded = (== '\n') . last

-- get order of table
getOrder :: Table -> Int
getOrder = length . head . Map.keys

-- construct a new word, based on the table
buildWord :: Table -> IO String
buildWord table = rand (starters table) >>= buildWordFoo
  where
    order = getOrder table
    buildWordFoo word
      | hasEnded word = return word
      | otherwise = rand options >>= addLetter
      where
        options = table Map.! takeLast order word
        addLetter = buildWordFoo . (word ++) . (: [])

-- construct a list of words
buildWords :: Order -> [String] -> Int -> [IO String]
buildWords order words n = take n buildWords'
  where
    table = trainWords order (addCarriageReturns words)
    buildWords' = buildWord table : buildWords'

-- print words
printAll :: [IO String] -> IO ()
printAll [] = putStrLn "\ndone"
printAll words = do
                  head words >>= putStrLn . butLast
                  printAll (tail words)

main = do
        file <- readFile "male_names.txt"
        let names = addCarriageReturns (words file)
        printAll (buildWords 3 names 25)
