module Core.Interval where

-- base
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Tuple (swap)
import Data.Monoid

type Interval a = ((Double, Double), a)

-- sample
data MyInt = My Int deriving (Show, Eq)
instance Monoid MyInt where
 My i `mappend` My j = My (min i j)
 mempty = My (maxBound :: Int)

-- algorithm
joinIntervals :: (Eq a, Monoid a) => [Interval a] -> [Interval a]
joinIntervals = sanitize . foldr joinInterval [] . sortBy (comparing fst)

joinInterval :: Monoid a => (Interval a) -> [Interval a] -> [Interval a]
joinInterval x [] = [x]
joinInterval x (y:ys) = init intvls ++ joinInterval (last intvls) ys
 where intvls = joinTwoIntervals x y

joinTwoIntervals :: Monoid a => Interval a -> Interval a -> [Interval a]
joinTwoIntervals intvl@((s,e), lim) intvl'@((s',e'), lim')
    | e < s'    = [intvl, intvl']
    | e' < e    = filter (uncurry (<) . fst) [((s,s'), lim), ((s',e'), lim2), ((e',e), lim)]
    | otherwise = filter (uncurry (<) . fst) [((s,s'), lim), ((s',e), lim2), ((e,e'), lim')]
 where lim2 = lim <> lim'

sanitize :: (Eq a) => [Interval a] -> [Interval a]
sanitize [] = []
sanitize [x] = [x]
sanitize (intvl@((s,e), lim) : intvl'@((s',e'), lim') : rest)
    | lim == lim' = sanitize (((s, e'), lim) : rest)
    | otherwise   = intvl : sanitize (intvl' : rest)

reverseIntervals :: [Interval a] -> [Interval a]
reverseIntervals = reverse . map (\ (x,y) -> (swap x, y))

fillInterval :: (Eq a, Monoid a) => Double -> Double -> [Interval a] -> [Interval a]
fillInterval s e intvls = joinIntervals $ ((s,e), mempty):intvls


