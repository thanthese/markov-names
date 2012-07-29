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
groupInto :: Int -> [a] -> [[a]]
groupInto len = takeWhile isLen . groups
  where
    isLen = (len ==) . length
    groups ls = take len ls : groups (tail ls)

-- takeLast 2 "test" => "st"
takeLast :: Int -> [a] -> [a]
takeLast n ls = drop (length ls - n) ls

-- butLast "test" => "tes"
butLast :: [a] -> [a]
butLast ls = take (length ls - 1) ls

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

-- get markov order of table
getOrder :: Table -> Int
getOrder = length . head . Map.keys

-- parse raw data from file into list of name: ["Tom", "Harry"]
parseFile :: String -> [String]
parseFile = addCarriageReturns . words

-- clean and print a completed word
printWord :: String -> IO ()
printWord = putStrLn . butLast

-- construct a new word, based on the table
buildWord :: Table -> IO String
buildWord table = rand (starters table) >>= buildWord'
  where
    order = getOrder table
    ending = takeLast order
    buildWord' word
      | hasEnded word = return word
      | otherwise = rand options >>= buildWord' . addLetter
      where
        options = table Map.! ending word
        addLetter = (word ++) . (: [])

-- construct an infinite list of words of original words from a table.  The
-- input list of words should well-formatted: they should start with capitals
-- and end with carriage returns.  All words returned will pass the filter
-- function.
buildWords :: (String -> Bool) -> Table -> [IO String]
buildWords filterFn table =
  (buildWord table >>= check) : buildWords filterFn table
    where check word
            | filterFn word = return word
            | otherwise = head (buildWords filterFn table)

-- create a function that determines whether a given word is old
isOld :: [String] -> (String -> Bool)
isOld oldNames = not . flip elem oldNames

main = do
  let order = 2
  let len = 20
  let filename = "male_names.txt"
  rawFileData <- readFile filename
  let oldNames = parseFile rawFileData
  let table = trainWords order oldNames
  let filterFn = isOld oldNames
  newNames <- (sequence . take len . buildWords filterFn) table
  mapM_ printWord newNames
