import qualified Data.Map as Map
import qualified Data.List as List
import Random (randomRIO)
import Control.Monad (liftM)

------------------------------------------------------------
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

------------------------------------------------------------
---- app functions

sampleNames :: [String]
sampleNames = ["Aeneas", "Amadeus", "Andreas", "Antonius", "Apollos", "Atticus", "Augustus", "Aurelius", "Caesar", "Caius", "Cassius", "Cato", "Cicero", "Claudius", "Cornelius", "Cosmo", "Cyrus", "Decimus", "Demetrius", "Felix", "Flavius", "Gaius", "Horatio", "Justus", "Lazarus", "Lucius", "Magnus", "Marcellus", "Marcus", "Marius", "Maximus", "Nero", "Octavius", "Philo", "Primus", "Quintus", "Remus", "Romanus", "Romulus", "Rufus", "Seneca", "Septimus", "Severus", "Stephanus", "Tarquin", "Theon", "Tiberius", "Titus", "Urban"]

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
buildWord table = rand (starters table) >>= buildWord'
  where
    order = getOrder table
    buildWord' word
      | hasEnded word = return word
      | otherwise = rand options >>= addLetter
      where
        options = table Map.! takeLast order word
        addLetter = buildWord' . (word ++) . (: [])

-- construct a list of words from a list of words
buildWords :: Order -> [String] -> Int -> IO [String]
buildWords order words n = sequence (take n buildWords')
  where
    table = trainWords order (addCarriageReturns words)
    buildWords' = buildWord table : buildWords'

-- remove names that were in the original list
filterWords :: [String] -> IO [String] -> IO [String]
filterWords originals news = liftM filtering news
  where filtering = filter (not . flip elem originals)

main = do
  putStrLn "\n"
  file <- readFile "male_names.txt"
  let oldNames = addCarriageReturns (words file)
  newNames <- filterWords oldNames (buildWords 2 oldNames 40)
  mapM_ (putStrLn . butLast) newNames
  putStrLn "\n"
