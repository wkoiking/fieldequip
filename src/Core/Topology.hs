module Core.Topology where

-- base
import Data.List (nub, delete, minimumBy, maximumBy)
import Data.Ord (comparing)
import Control.Monad

-- fgl
import Data.Graph.Inductive.Internal.Queue

-- Classes
class Reversible a where
  rev :: a -> a
  rev = id

undir :: Reversible a => [a] -> [a]
undir as = as ++ map rev as

spanQualified :: String -> (String, String)
spanQualified txt = case span (/= '.') txt of
  (ls, rs) | not $ null rs -> (ls, tail rs)
           | otherwise -> ([], ls)

dequalify :: String -> String
dequalify = snd . spanQualified

qualifier :: String -> String
qualifier = fst . spanQualified

-- Data
type Graph lab = [Edge lab]
type Path lab  = [Edge lab]
type PointName = String
type EdgeName = String
data Fletching = Reverse | Normal | Common deriving (Eq, Ord, Show)

data Edge lab = Edge {
 origPoint      :: PointName,
 destPoint'     :: PointName,
 origFletching  :: Fletching,
 destFletching' :: Fletching,
 edgeLabel  :: lab
} deriving (Eq, Ord, Show)

points :: Graph lab -> [PointName]
points = nub . concatMap (\ (Edge p p' _ _ _) -> [p, p'])

edgeToPoints :: Edge lab -> [(PointName, Fletching)]
edgeToPoints (Edge p p' k k' _) = [(p, k), (p', k')]

pathToPoints :: Path a -> [(PointName, Fletching)]
pathToPoints = filter ((/= Common) . snd) . tail . init . concatMap edgeToPoints

instance Reversible lab => Reversible (Edge lab) where
  rev (Edge p p' k k' lab) = Edge p' p k' k (rev lab)

-- Algorithms
canonicalizeHome :: [Path lab] -> Path lab
canonicalizeHome = canonicalize' flip

canonicalizeStart :: [Path lab] -> Path lab
canonicalizeStart = canonicalize' id

canonicalize' f pss = minimumBy (f $ comparing (map snd . pathToPoints)) $ filter ((== min) . numOfReverse) pss
 where numOfReverse = length . filter (\ x -> Reverse == snd x) . pathToPoints
       min = minimum (map numOfReverse pss)

findPaths :: Eq lab => (Edge lab -> Bool) -> Graph lab -> Edge lab -> Edge lab-> [Path lab]
findPaths isStop g e s
  | s == e   = return [s]
  | otherwise = do
      n <- suc g s
      guard (not (isStop n) || n == e)
      liftM  (s:) $ findPaths isStop (delete s g) e n

reachableFromDir :: (Eq lab) => Graph lab -> Edge lab -> [Edge lab]
reachableFromDir g e 
 | elem e g  = bfenInternal (queuePut e mkQueue) g
 | otherwise = []

reachable :: Eq lab => (Graph lab -> b -> [Edge lab]) -> Graph lab -> b -> [Edge lab]
reachable fromSomething g s = nub $ concatMap (reachableFromDir g) (fromSomething g s)

bfenInternal :: (Eq lab) => Queue (Edge lab) -> Graph lab -> [Edge lab]
bfenInternal q g | queueEmpty q || null g = []
                 | otherwise              = 
      case match e g of
        (Just e, g')  -> e:bfenInternal (queuePutList (suc g e) q') g'
        (Nothing, g') -> bfenInternal q' g'
        where (e,q') = queueGet q

match :: Eq lab => Edge lab -> Graph lab -> (Maybe (Edge lab), Graph lab)
match e g
 | elem e g = (Just e, delete e g)
 | otherwise = (Nothing, g)

suc :: Graph lab -> Edge lab -> [Edge lab]
suc g (Edge _ p _ Common _) = filter f g
 where f (Edge q q' k k' lab) = p == q && k /= Common
suc g (Edge _ p _ _ _)  = filter f g
 where f (Edge q q' k k' lab) = p == q && k == Common

-- Checker
fromEdges :: (Reversible lab) => [Edge lab] -> Either String (Graph lab) -- won't consume any input
fromEdges = isGraph . undir

isGraph :: Graph lab -> Either String (Graph lab) -- won't consume any input
isGraph g = mapM_ checkPoint (points g) >> return g
 where checkPoint :: PointName -> Either String ()
       checkPoint p = do
         checkNumOf Normal
         checkNumOf Reverse
         checkNumOf Common
        where ks = map origFletching $ filter ((p ==) . origPoint) g
              checkNumOf k
                | n > 1     = fail $ show p ++ " have " ++ show n ++ " number of " ++ show k
                | otherwise = return ()
               where n = length $ filter (k ==) ks
