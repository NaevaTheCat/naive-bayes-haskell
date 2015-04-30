module MyBayes 
    ( Classifier
    , PTotal
    , newClassifier
    , newTotal
    , classify
    , train
    ) where


import qualified Data.List as L
import qualified Data.Char as C

type Word = String
type Prob = Double
type FeatureStats = (Word,Double)

data Classifier = Classifier
    { catagoryName :: Word
    , statList :: [FeatureStats]
    , nTrained :: Integer
    } deriving(Show)
data PTotal = PTotal
    { statsTotal :: [FeatureStats]
    , allTrained :: Integer
    } deriving(Show)
--- Write show instances for training to disk

newClassifier :: Word -> Classifier
newClassifier name = Classifier name [] 0

newTotal :: PTotal
newTotal = PTotal [] 0

classify :: [Classifier] -> PTotal -> String -> Word
classify cs pt string = 
    fst . head . L.sortBy (\a b -> compare (snd b) (snd a)) . 
        map (\x -> classProb (x,pt) string) $ cs

classProb :: (Classifier, PTotal) -> String -> (Word,Prob)
classProb (c,pt) s = 
    let vocab = fromIntegral $ length (statsTotal pt)
        s' = filter (not . flip L.isInfixOf stopWords) . words . map C.toLower . filter (not.C.isPunctuation) . subNewLines $ s
        freqsInC = L.foldl' (\acc x -> (+acc).snd $ x) 0 (statList c)
        sCounts = map maybeToFreq $ map (flip lookup (statList c)) s'
        prior   = log $ (fromIntegral $ nTrained c) / (fromIntegral $ allTrained pt)
        pCat = L.foldl' (\acc x -> acc+(log((x+1)/(freqsInC + vocab)))) prior sCounts
    in (catagoryName c, pCat)

maybeToFreq :: Maybe Double -> Double
maybeToFreq s = case s of 
                     Just s' -> s' 
                     Nothing -> 0

train :: (Classifier, PTotal) -> String -> (Classifier, PTotal)
train (classifier, totals) string = 
    let s0 = subNewLines string
        s1 = filter (not . C.isPunctuation) s0
        s2 = words . map C.toLower $ s1
        s3 = filter (not . L.any C.isSymbol) s2 --removes latex maths
        s4 = L.group . L.sort $ s3
        s5 = filter (not . flip L.isInfixOf stopWords) s4
        stats = map wordToStats s5
        nd = L.foldl' (\acc x -> (+ acc).snd $ x) 0 stats
        newCList = genericApp (statList classifier) stats
        newTList = genericApp (statsTotal totals) stats
    in (Classifier (catagoryName classifier) newCList ((nTrained classifier) + 1), PTotal newTList ((allTrained totals)+1))

genericApp clas stats = 
    let (isIn,out) = L.partition (flip elem (map fst clas) . fst) stats
        (clasIn,clasOut) = L.partition (flip elem (map fst stats) . fst) clas
        inSorted = L.sort isIn
        cInSorted = L.sort clasIn
        modIns = zipWith (\a b -> (fst a,(snd a)+(snd b))) inSorted cInSorted
        modOuts = clasOut ++ out
    in modIns ++ modOuts

wordToStats :: [Word] -> FeatureStats
wordToStats xs = (head xs, fromIntegral.length $ xs)

normStat :: FeatureStats -> Double -> FeatureStats
normStat (w,n) nd = (w,n / nd)

stopWords = []

subNewLines :: String -> String
subNewLines []     = []
subNewLines (x:xs) =
    case x of
         '\n' -> ' ':subNewLines xs
         '\t' -> ' ':subNewLines xs
         '\r' -> ' ':subNewLines xs
         _    -> x  :subNewLines xs
