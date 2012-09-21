import Data.List (tails, nub, partition)
import Data.Map (insertWith, empty, fromList, Map, keys, findWithDefault, (!))

data Unit a = Start | Unit a | End
  deriving (Eq, Ord)

instance Show a => Show (Unit a) where
  show Start = "<"
  show End = ">"
  show (Unit a) = show a

sampleNames = ["Aeneas", "Amadeus", "Andreas", "Antonius", "Apollos", "Atticus", "Augustus", "Aurelius", "Caesar", "Caius", "Cassius", "Cato", "Cicero", "Claudius", "Cornelius", "Cosmo", "Cyrus", "Decimus", "Demetrius", "Felix", "Flavius", "Gaius", "Horatio", "Justus", "Lazarus", "Lucius", "Magnus", "Marcellus", "Marcus", "Marius", "Maximus", "Nero", "Octavius", "Philo", "Primus", "Quintus", "Remus", "Romanus", "Romulus", "Rufus", "Seneca", "Septimus", "Severus", "Stephanus", "Tarquin", "Theon", "Tiberius", "Titus", "Urban"]
sampleUnits = map toUnits sampleNames
  where toUnits n = [Start] ++ map Unit n ++ [End]
sampleUnit = head sampleUnits

groupInto size = filter ((size ==) . length) . map (take size) . tails

takeLast n ls = drop (length ls - n) ls

train table link = insertWith (++) prefix suffix table
  where (prefix, suffix) = splitAt (length link - 1) link

trainAll :: (Ord a) => [[Unit a]] -> Int -> Map [Unit a] [Unit a]
trainAll units order = foldl train empty allLinks
  where allLinks = concatMap (groupInto order) units

buildAll :: (Ord a) => Map [Unit a] [Unit a] -> [[Unit a]]
buildAll table = buildAll' [] (startingPrefixes table)
  where
    order = (length . head . keys) table
    uniqs = fmap nub table
    buildAll' built [] = built
    buildAll' built building = buildAll' (built ++ a) b
      where
        new = concatMap add building
        add n = ((uniqs !) . takeLast order n)
        (a, b) = partition ((== End) . last) new


startingPrefixes :: Eq a => Map [Unit a] b -> [[Unit a]]
startingPrefixes = filter ((== Start) . head) . keys

{-

buildAll (trainAll sampleUnits 3)

let uniqs = fmap nub (trainAll sampleUnits 3)

findWithDefault [] (map Unit "hello") uniqs

partition (==1) [1,1,2,3,1,4,2,1]

-}

testGroupInto = groupInto 2 "hello" == ["he","el","ll","lo"]
testTrain = train empty "hello" == fromList [("hell", "o")]
