{-# LANGUAGE TemplateHaskell #-}
module Frontend where

-- ToDo

import Core.Topology
import Core.CommonData

-- my-parser
import MyParser

-- base
import Data.Maybe (catMaybes, isJust, fromJust, mapMaybe, maybeToList)
import Data.Either (lefts, rights)
import Data.List
import Data.Maybe
import Data.Monoid hiding (Dual, (<>))
import Control.Monad
import Control.Applicative
import Control.Arrow hiding ((|||))

-- transformers
import Control.Monad.Trans.State as S

-- lens
import Control.Lens hiding ((#), (&), beside)

-- parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (State)

-- Data Structure
data Specification = Specification
    { _specGraph    :: SpecGraph
    , _specPoints   :: [PointData]
    , _specStations :: [StationData]
    , _specRadis    :: [[RadiData]]
    , _specGrads    :: [[GradData]]
    , _specTunnel   :: [[TunnelData]]
    } deriving (Show)

data InputData = InputData
    { _inputLines    :: [Line]
    , _inputPoints   :: [PointData]
    , _inputStations :: [StationData]
    , _inputRadis    :: [[RadiData]]
    , _inputGrads    :: [[GradData]]
    , _inputTunnel   :: [[TunnelData]]
    } deriving (Show)

type SpecPath  = [SpecEdge]
type SpecEdge  = Edge SpecLabel
type SpecGraph = Graph SpecLabel
data SpecLabel = Spec [Stop] deriving (Show, Eq, Ord)
data Stop      = Stop String deriving (Show, Eq, Ord)

instance Reversible SpecLabel where
    rev (Spec ss) = Spec (reverse ss)

qualify :: String -> SpecEdge -> SpecEdge
qualify str (Edge p p' k k' (Spec ss)) = Edge p p' k k' (Spec $ map qualify' ss)
 where qualify' (Stop stName) = Stop $ str ++ "." ++ stName

type Line        = (Maybe EdgeName, Maybe Double, EndType, EndType, [[InputChunk]])
data EndType     = EndAt (Double, Double) | ConnectPoint | ConnectTo (Double, Double) deriving (Show, Eq, Ord)
type InputChunk  = Either InputStop InputPoint
type InputStop   = (Maybe Bool, StationName, IsILStation)
type InputPoint  = (Bool, Bool, PointName)
type IsILStation = Bool

type PreGraph = [PreEdge]
type PrePath  = (Maybe EdgeName, EndType, EndType, [PreEdge])
type PreEdge  = (PointName, PointName, [InputStop])

makeLenses ''Specification
makeLenses ''InputData

type Error a = Either String a
type Count a = S.State Int a

nextNum :: S.State Int String
nextNum = do
    n <- S.get
    S.put (n+1)
    return (show n)

trainLength :: Double
trainLength = 130

fcc x'
    | 2500 <= x = 134
    | 2000 <= x = 120
    | 1500 <= x = 101
    | 1200 <= x = 97
    | 1000 <= x = 103
    | 700 <= x = 89
    | 600 <= x = 88
    | 550 <= x = 80
    | 400 <= x = 76
    | 363 <= x = 73
    | 350 <= x = 73
    | 300 <= x = 68
    | 200 <= x = 55
    | otherwise = 55
 where x = abs x'

fgg x'
    | 35 <= x = 65
    | 30 <= x = 70
    | 25 <= x = 75
    | 20 <= x = 80
    | 15 <= x = 85
    | 10 <= x = 90
    | 5 <= x = 95
    | 0 <= x = 100
 where x = abs x'

readSpecification :: String -> Error Specification
readSpecification str = case parse inputDataP "" str of
    Left err -> fail $ show err
    Right inData -> let ls = _inputLines inData
                        pds = _inputPoints inData
                        rad = _inputRadis inData
                        gra = _inputGrads inData
                        tunnel = _inputTunnel inData
                        st = _inputStations inData
                    in do
                        (g, pds') <- linesToSpecGraph ls pds
                        return $ Specification g pds' st rad gra tunnel 

linesToSpecGraph :: [Line] -> [PointData] -> Error (SpecGraph, [PointData])
linesToSpecGraph ls pds = liftM ((`tup` pds') . concat) $ mapM (prePathToSpecPath g pds') ps
 where (ps, edss) = unzip $ S.evalState (mapM lineToPrePath ls) 0
       pds' = pds ++ concat edss
       g = concatMap (view _4) ps
       tup = (,)

prePathToSpecPath :: PreGraph -> [PointData] -> PrePath -> Error [SpecEdge]
prePathToSpecPath g pds (mEdgeName, st, et, [e]) = return $ map (qualify str) $ toSpecEdge Normal Normal e
 where str = case mEdgeName of
           Nothing -> ""
           Just str -> str
prePathToSpecPath g pds (mEdgeName, st, et, es) = do
    let str = case mEdgeName of
            Nothing -> ""
            Just str -> str
    h' <- preEdgeToSpecEdge1 h
    mid' <- liftM concat $ mapM preEdgeToSpecEdge mid
    l' <- preEdgeToSpecEdge2 l
    return $ map (qualify str) $ h' ++ mid' ++ l'
 where g' = undirectify $ simplify g \\ simplify es
       h = head es
       l = last es
       mid = tail $ init es
       preEdgeToSpecEdge1 :: (PointName, PointName, [InputStop]) -> Error [SpecEdge]
       preEdgeToSpecEdge1 e@(pn1, pn2, ss) = do
           f2 <- mf2 pn2
           return $ toSpecEdge Reverse f2 e
       preEdgeToSpecEdge2 :: (PointName, PointName, [InputStop]) -> Error [SpecEdge]
       preEdgeToSpecEdge2 e@(pn1, pn2, ss) = do
           f1  <- mf1 pn1
           return $ toSpecEdge f1 Reverse e
       preEdgeToSpecEdge :: (PointName, PointName, [InputStop]) -> Error [SpecEdge]
       preEdgeToSpecEdge e@(pn1, pn2, ss) = do
           f1  <- mf1 pn1
           f2  <- mf2 pn2
           return $ toSpecEdge f1 f2 e
       mf2 pn2 = do
           (d2,_)  <- lookupPointPos pn2 pds
           d2' <- lookupOtherSideKm pn2 pds g'
           return $ if d2' - d2 > 0 then Common else Normal
       mf1 pn1 = do
           (d1,_) <- lookupPointPos pn1 pds
           d1' <- lookupOtherSideKm pn1 pds g'
           return $ if d1' - d1 > 0 then Normal else Common

simplify = map (\ (p,p',_) -> (p, p'))
undirectify = concatMap (\ (p, p') -> [(p,p'),(p',p)])

lookupPointPos :: PointName -> [PointData] -> Error (Double, Double)
lookupPointPos pn pds = case lookup pn pds' of
    Nothing -> fail $ "The position is not defined for " ++ pn
    Just x -> return x
 where pds' = map (\ (PointData pn pos _) -> (pn, pos)) pds
 
lookupOtherSideKm :: PointName -> [PointData] -> [(PointName, PointName)] -> Error Double
lookupOtherSideKm pn pds g = case lookup pn g of
    Nothing  ->  fail $ "There is no crossover edge for " ++ pn
    Just pn' -> case lookup pn' pds' of
        Nothing -> fail $ "The position is not defined for other side of cross over that is: " ++ pn ++ " -> " ++ pn'
        Just x -> return x
 where pds' = map (\ (PointData pn (x,y) _) -> (pn, x)) pds

toSpecEdge :: Fletching -> Fletching -> PreEdge -> [SpecEdge]
toSpecEdge f1 f2 e@(pn, pn', ss) = [Edge pn pn' f1 f2 (Spec ss1)]
                                ++ [Edge pn' pn f2 f1 (Spec (reverse ss2))]
 where (ss1, ss2) = mapMaybe filterRightDir &&& mapMaybe filterWrongDir $ ss

filterRightDir, filterWrongDir :: InputStop -> Maybe Stop
filterRightDir (Just False, sn, _)  = Nothing
filterRightDir (_, sn, _)          = Just (Stop sn)
filterWrongDir  (Just True, sn, _) = Nothing
filterWrongDir (_, sn, _)        = Just (Stop sn)

lineToPrePath :: Line -> Count (PrePath, [PointData])
lineToPrePath (mEdgeName, rayer, st, et, css) = do
    n <- nextNum
    n' <- nextNum
    let cs = concat css
        (ls, rs@(Right (_, isRightStop, pn):_)) = span isLeft cs
        (ls', Right (isLeftStop, _, pn')) = (reverse *** head) $ span isLeft $ reverse cs
        rightStop
            | isRightStop = [(Just True, pn, False)]
            | otherwise   = []
        leftStop
            | isLeftStop = [(Just False, pn', False)]
            | otherwise  = []
        (start, pdsStart) = case st of
            ConnectPoint -> ([], [])
            EndAt pos -> let en = "End" ++ n in ([(en, pn, lefts ls ++ rightStop)], [PointData en pos 0])
            ConnectTo pos -> let on = "OtherLine" ++ n in ([(on, pn, lefts ls ++ rightStop)], [PointData on pos 0])
        (end, pdsEnd) = case et of
            ConnectPoint -> ([], [])
            EndAt pos -> let en' = "End" ++ n' in ([(pn', "End" ++ n', lefts ls' ++ leftStop)], [PointData en' pos 0])
            ConnectTo pos -> let on' = "OtherLine" ++ n' in ([(pn', on', lefts ls' ++ leftStop)], [PointData on' pos 0])
    return ( (mEdgeName, st, et, start ++ takePairs rs ++ end), pdsStart ++ pdsEnd )

takePairs :: [InputChunk] -> [PreEdge]
takePairs [] = []
takePairs (Right (_, isRightStop, pn):xs) = case span isLeft xs of
    (_, []) -> []
    (ls, rs@(Right (isLeftStop, _, pn'):_)) -> let leftStop
                                                       | isLeftStop = [(Just False, pn', False)]
                                                       | otherwise  = []
                                               in (pn, pn', leftStop ++ lefts ls ++ rightStop) : takePairs rs
 where rightStop
           | isRightStop = [(Just True, pn, False)]
           | otherwise   = []
       
isLeft (Left _) = True
isLeft _        = False

type LineNum = Double
-- Parser
inputDataP :: Parser InputData
inputDataP = fileP $ do
    headderP "Line"
    es <- lexeme $ lineP `endBy` eol
    let pointLineNumAsoc :: [(PointName, LineNum)]
        pointLineNumAsoc = concatMap inputLineToAsoc es
        inputLineToAsoc :: Line -> [(PointName, LineNum)]
        inputLineToAsoc (_, Nothing, _, _, _) = []
        inputLineToAsoc (_, Just n, _, _, css) = map (\ (_,_,z) -> (z,n) ) ps
         where ps = rights $ concat css
    headderP "Point"
    defPointNum <- headderOptionP "DefNum" numberP
    ps <- lexeme $ pointP pointLineNumAsoc defPointNum `endBy` eol
    headderP "Station"
    defPlatformLength <- headderOptionP "DefPlatfomrLength" (liftM fromIntegral int)
    defPlatformType <- headderOptionP "DefPlatformType" plTypeP
    defDwellTime <- headderOptionP "DefDwellTime" (liftM fromIntegral int)
    let asocIsIL = toAsocIsIL es
    ss <- lexeme $ stationP defPlatformLength defPlatformType defDwellTime asocIsIL `endBy` eol
    headderP "Curve"
    cs <- lexeme $ many1 alignmentsP
    headderP "Grad"
    gs <- lexeme $ many1 alignmentsP
    headderP "Tunnel"
    ts <- lexeme $ many1 tunnelsP
    return $ InputData es ps ss (map (map toRadi) cs) (map (map toGrad) gs) ts

toAsocIsIL :: [Line] -> [(String, IsILStation)]
toAsocIsIL ls = nub $ concatMap toAsocIsIL' ls
toAsocIsIL' :: Line -> [(String, IsILStation)]
toAsocIsIL' (_,_,_,_,chunks) = map (\ (_,n,isIL) -> (n, isIL)) $ lefts $ concat chunks

headderP :: String -> Parser Char
headderP str = do
    lexemeIL $ char '#'
    lexemeIL $ string str
    eol

headderOptionP :: String -> Parser a -> Parser a
headderOptionP str optionP = do
    lexemeIL $ string "##"
    lexemeIL $ string str
    lexemeIL $ char ':'
    optionP <* eol

alignmentsP :: Parser [(String, Double, String, (Double,Double))]
alignmentsP = try $ do
    lexemeIL $ string "##"
    lineName <- lexemeIL nameP
    lexeme $ char ':'
    lexeme $ (alignmentP lineName) `endBy` eol

alignmentP :: String -> Parser (String, Double, String, (Double,Double))
alignmentP lineName = do
    name <- nameP
    sec <- intervalP
    radi <- float
    return (name, radi, lineName, sec)

tunnelsP :: Parser [TunnelData]
tunnelsP = try $ do
    lexemeIL $ string "##"
    lineName <- lexemeIL nameP
    lexeme $ char ':'
    lexeme $ (tunnelP lineName) `endBy` eol

tunnelP :: String -> Parser TunnelData
tunnelP lineName = do
    name <- nameP
    sec <- intervalP
    tunnel <- tunnelTypeP
    return (sec, Tunnel name tunnel lineName)

tunnelTypeP :: Parser TunnelType
tunnelTypeP = lexemeIL $ option Ground $ choice [string "single" >> return SingleShield
                                                ,string "dual" >> return DualShield
                                                ,string "viaduct" >> return Viaduct]

toRadi :: (String, Double, String, (Double,Double)) -> RadiData
toRadi (n, d, str, sec) = (sec, (Radi n d str))
toGrad :: (String, Double, String, (Double,Double)) -> GradData
toGrad (n, d, str, sec) = (sec, (Grad n d str))


lineP :: Parser Line
lineP = do
    (mName, mNum) <- option (Nothing, Nothing) lineHeadderP
    startType <- option ConnectPoint (try $ lineStartP mNum)
    css <- lexemeIL $ inputChunksP `sepBy` (try $ lexemeIL $ string "-|-")
    endType <- option ConnectPoint (try $ lineEndP mNum)
    when (null css) $ fail "Null line"
    unless (length css == 1 || all justOneIL css) $ fail "There need to be exactly one IL Statiino in each Zone"
    return $ (mName, mNum, startType, endType, css)
 where lineHeadderP = do
           mlineName <- optionMaybe nameP
           mlineNum <- optionMaybe $ lexemeIL $ char '@' >> liftM fromIntegral int
           lexemeIL (char ':')
           return (mlineName, mlineNum)
       justOneIL cs = length (filter isILStop cs) == 1
       isILStop (Left (_,_,True)) = True
       isILStop _                 = False

lineStartP :: Maybe Double -> Parser EndType
lineStartP mNum = choice [try endStartP, try connectStartP]
 where endStartP = do
           pos <- lineEndPosP mNum
           lexemeIL $ string "]-"
           return (EndAt pos)
       connectStartP = do
           pos <- lineEndPosP mNum
           lexemeIL $ string "<=="
           return (ConnectTo pos)

lineEndP :: Maybe Double -> Parser EndType
lineEndP mNum = choice [try endEndP, try connectEndP]
 where endEndP = do
           lexemeIL $ string "-["
           pos <- lineEndPosP mNum
           return (EndAt pos)
       connectEndP = do
           lexemeIL $ string "==>"
           pos <- lineEndPosP mNum
           return (ConnectTo pos)

lineEndPosP :: Maybe Double -> Parser (Double, Double)
lineEndPosP mNum = choice [try tupleP, single]
 where single = case mNum of
           Just defLineNum -> liftM2 (,) float (return defLineNum)
           Nothing -> fail "The posiiton is not defined for Some Line End"


inputChunksP :: Parser [InputChunk]
inputChunksP = liftM catMaybes (many1 inputChunkP')

inputChunkP' :: Parser (Maybe InputChunk)
inputChunkP'
 = choice [try $ liftM Just $ eitherP inputStopP inputPointP,  try $ lexemeIL $ string "--" >> return Nothing]

inputStopP :: Parser InputStop
inputStopP = lexemeIL $ do
    isLeft <- option False $ char '<' >> notFollowedBy (string "==") >> return True
    (name,isIL) <- choice [try parseILSt, liftM2 (,) nameP (return False)]
    isRight <- option False $ char '>' >> return True
    case (isRight, isLeft) of
        (True, False) -> return (Just True, name, isIL)
        (False, True) -> return (Just False, name, isIL)
        otherwise -> return (Nothing, name, isIL)
 where parseILSt = between (string "*") (string "*") $ do
           name <- nameP
           return (name, True)

inputPointP :: Parser InputPoint
inputPointP = lexemeIL $ do
    isRight <- option False $ char '>' >> return True
    name <- liftM2 (++) (many1 digit) (ifany "AB")
    isLeft <- option False $ char '<' >> notFollowedBy (string "==") >> return True
    return (isRight, isLeft, name)

pointP :: [(PointName, LineNum)] -> PointNum -> Parser PointData
pointP asocList defNum = do
    name <- lexemeIL $ liftM2 (++) (many1 digit) (ifany "AB")
    let posP = choice [try tupleP, single]
         where single = case lookup name asocList of
                   Just defLineNum -> liftM2 (,) float (return defLineNum)
                   Nothing -> fail $ "The posiiton is not defined for " ++ name
    pos <- posP
    num <- option defNum numberP
    return (PointData name pos num)


tupleP :: Parser (Double, Double)
tupleP = lexemeIL $ between (string "(" ) (string ")") $ do
    f <- float
    lexemeIL $ char ','
    i <- liftM fromIntegral int
    return (f,i)

type PlatformLength = Double

stationP :: PlatformLength -> PlatformType -> DwellTime
            -> [(String, IsILStation)] -> Parser StationData
stationP defPlatformLength defPlatformType defDwellTime asoc = do
  name <- nameP
  isIL <- case lookup name asoc of
      Nothing -> fail $ name ++ " does not exists in asoc"
      Just x -> return x
  place <- choice [fromTo, center]
  (dwell, plType) <- option (defDwellTime, defPlatformType) $ try $ liftM2 (,) float plTypeP
  return (StationData name place dwell plType isIL)
 where center = do
         c <- float
         return (c - d, c + d)
          where d = defPlatformLength / 2
       fromTo = between (string "(" ) (string ")") $ do
           f <- float
           char ','
           t <- float
           return (f,t)

-- maybeToParser :: Maybe a -> Parser a
-- maybeToParser x = case x of
--     Just x -> return x
--     Nothing -> fail "evaluated Nothing in parser"

plTypeP :: Parser PlatformType
plTypeP = lexemeIL $ choice [try $ string "island" >> return Island
                            ,try $ string "side" >> return Side
                            ,try $ string "none" >> return None]

intervalP :: Parser (Double, Double)
intervalP = do
    d1 <- float
    d2 <- float
    return (d1,d2)

nameP :: Parser String
nameP = lexemeIL $ liftM2 (:) letter (many1 $ choice [try alphaNum, char '_'])

numberP :: Parser Int
numberP = do
    lexemeIL $ char '#' >> liftM fromIntegral int

edgeToStops :: SpecEdge -> [Stop]
edgeToStops (Edge _ _ _ _ (Spec ss)) = ss

findPath :: Specification -> Stop -> Stop -> [(SpecPath, Stop, Stop)]
findPath spec orig dest = do
    e <- origEdges
    e' <- destEdges
    let paths = findPaths anyStop g e' e
    guard $ not $ null paths
    let path = canonicalizeStart paths
        stops = concatMap edgeToStops path
    guard $ [orig,dest] `isInfixOf` stops
    return (path, orig, dest)
 where origEdges = fromStop spec orig
       destEdges = fromStop spec dest
       g = _specGraph spec

anyStop :: SpecEdge -> Bool
anyStop e = let Spec ss = edgeLabel e in not $ null ss

fromStop :: Specification -> Stop -> [SpecEdge]
fromStop spec stop = filter (has stop) g
 where g = _specGraph spec
       has stop e = let Spec ss = edgeLabel e  in stop `elem` ss

fileP :: Parser a -> Parser a
fileP p = do
 spaces
 contents <- p
 spaces
 eof
 return contents

eitherP :: Parser a -> Parser b -> Parser (Either a b)
eitherP leftP rightP = choice [try $ liftM Left leftP, liftM Right rightP]
