{-# LANGUAGE TemplateHaskell #-}
module Backend where

import Core.Interval
import Core.Topology
import Core.CommonData
import Core.DrawInterval
import Core.Replace

-- diagrams-svg
import Diagrams.Prelude hiding (option, Dual, Line, view, Qualifiable, First, getFirst, over)
import Diagrams.Core.Points
import Diagrams.Coordinates
import Diagrams.Backend.SVG

-- SVGFonts
import Graphics.SVGFonts

-- lens
import Control.Lens hiding (beside, (&), (#))

-- base
import Control.Monad
import Data.Monoid hiding ((<>))
import Data.List
import Data.Maybe
import Data.Ord (comparing)

-- transformers
import Control.Monad.Trans.State

data Implementation = Implementation
    { _implGraph    :: ObjGraph
    , _implPoints   :: [PointData]
    , _implStations :: [StationData]
    , _implRadis    :: [[RadiData]]
    , _implGrads    :: [[GradData]]
    , _implTunnel   :: [[TunnelData]]
    } deriving (Show)

type ObjPath  = [ObjEdge]
type ObjGraph = Graph ObjLabel
type ObjEdge  = Edge ObjLabel
data ObjLabel = ObjLabel [ObjectData] deriving (Show, Eq, Ord)
instance Reversible ObjLabel where
  rev (ObjLabel ss) = ObjLabel (reverse ss)

data ObjectData = ObjectData
  { _objName        :: String
  , _objType        :: ObjectType
  , _objStationName :: String
  , _objLineName    :: String
  , _objPos         :: (Double, Double)
  } deriving (Show, Eq, Ord)

nubObj :: [ObjectData] -> [ObjectData]
nubObj = nubBy cmp
 where cmp obj1 obj2 = let pos1 = _objPos obj1
                           pos2 = _objPos obj2
                           type1 = _objType obj1
                           type2 = _objType obj2
                       in case (type1, type2) of
                           (Balise bt1 _, Balise bt2 _) -> pos1 == pos2 && bt1 == bt2
                           _ -> pos1 == pos2 && type1 == type2
                        -- ここはUp Downで違うと判定されてしまっている！

data ObjectType = PointMachine PointType Direction Chirality
                | Balise BaliseType Direction
                | Signal SignalType Direction
                | Bond BondType
                | AxleCounter
                | Antenna AntennaType
                | StationStop deriving (Show, Eq, Ord)

data BaliseType  = P0 | P1 | P2 | P3 | CP | L | R | ORP             deriving (Show, Eq, Ord)
data AntennaType = WRS | WRSSingleShield | SRS | DoubleSRS | SRSBranch | DoubleSRSBranch  deriving (Show, Eq, Ord)
type PointType   = Int
data BondType    = Set                                              deriving (Show, Eq, Ord)
data SignalType  = MainSig | ShuntSig | MiddleSig                   deriving (Show, Eq, Ord)
data Direction   = Up | Down                                        deriving (Show, Eq, Ord)
data Chirality   = LeftHand | RightHand                             deriving (Show, Eq, Ord)

makeLenses ''ObjectData

type Error a = Either String a

(startKm, endKm) = (-537.292, 15147.952) -- 後で書き換える必要あり

(upLineYPos, downLineYPos) = (1, 3) -- 後で書き換える必要あり

platformYCenterPos = 0 -- 後で書き換える必要あり

-- カーブやトンネルに対するアンテナの特性
unitLength = 250

afunc :: Prod -> Double
afunc (Prod (Raz r) (Tun t))
    | t == DualShield || t ==SingleShield = 1 -- トンネルでは直線とみなす
    | r == (1/0)                          = 1
    | otherwise                           = 1 + 140 / r

makeCP :: Direction -> Double -> [ObjectData] -> [ObjectData]
makeCP dir d bs = concatMap (makeCP' dir) bs''
  where bs'' = zip bs' (tail bs')
        bs' = filter isBalise bs
        isBalise (ObjectData n1 (Balise _ _) _ _ pos@(x,y)) | y == d = True
        isBalise _ = False

makeCP' :: Direction -> (ObjectData, ObjectData) -> [ObjectData]
makeCP' dir (ObjectData n (Balise _ _) _ _ (x,y), ObjectData n' (Balise _ _) _ _ (x',y'))
  = [ObjectData ("between" ++ n ++ " and " ++ n') (Balise CP dir) "" "" (min x x'+ interval * fromIntegral i, y) | i <- [1 .. numOfcp]]
 where interval = abs (x - x') / (fromIntegral numOfcp + 1)
       numOfcp = floor (abs (x - x') / 500)

-- Draw
drawImplementation :: Implementation -> Error (Diagram SVG R2)
drawImplementation (Implementation g pds ss (rs1:rs2:_) (gs1:gs2:_) (ts:_)) = do
    dg <- drawObjGraph pds g
    let dss    = translateY (100 * platformYCenterPos) $ mconcat $ map drawStationData ss
        pdds   = mconcat $ map drawPointData pds
        fill   = fillInterval startKm endKm
        fill2  = fillInterval startKm endKm
        drs1   = drawRadis $ fill rs1
        drs2   = drawRadis $ fill rs2
        dgs1   = drawGrads $ fill2 gs1
        dgs2   = drawGrads $ fill2 gs2
        dts    = mconcat $ map drawTunnelData ts
        pBalise = sortBy (comparing (fst . _objPos)) $ concatMap ((\ (ObjLabel xs) -> xs) . edgeLabel) g
        upCPs = makeCP Up upLineYPos pBalise
        downCPs = makeCP Down downLineYPos pBalise
        objs = nubObj $ pBalise ++ upCPs ++ downCPs
        dobjs   = mconcat $ map drawObject $ objs
        dobjKm = mconcat $ map drawObjectKm $ objs
        hLine = p2 (startKm, 0) ~~ p2 (endKm, 0)
    return $ vcat' (with & sep .~ 10) [dgs1, drs1, hLine, dobjKm, hLine, centerY (dg <> pdds <> dobjs) <> dss, drs2, dgs2, dts] # lw medium

countObject :: Implementation -> [(String, String, Int)]
countObject (Implementation g pds ss (rs1:rs2:_) (gs1:gs2:_) (ts:_)) = sumObj $ concatMap toCountFormat objs
 where pBalise = sortBy (comparing (fst . _objPos)) $ concatMap ((\ (ObjLabel xs) -> xs) . edgeLabel) g
       upCPs = makeCP Up 1 pBalise
       downCPs = makeCP Down 3 pBalise
       objs = nubObj $ pBalise ++ upCPs ++ downCPs

toCountFormat :: ObjectData -> [(String, String, Int)]
toCountFormat (ObjectData _ (Balise P0 _) stName _ _) = [("P0", stName, 1)]
toCountFormat (ObjectData _ (Balise P1 _) stName _ _) = [("P1", stName, 1)]
toCountFormat (ObjectData _ (Balise P2 _) stName _ _) = [("P2", stName, 1)]
toCountFormat (ObjectData _ (Balise CP _) stName _ _) = [("CP", stName, 1)]
toCountFormat (ObjectData _ (Antenna WRS) stName _ _) = [("WRS", stName, 1)]
toCountFormat (ObjectData _ (Antenna WRSSingleShield) stName _ _) = [("WRS", stName, 2)]
toCountFormat (ObjectData _ (Antenna SRS) stName _ _) = [("SRS", stName, 1)]
toCountFormat (ObjectData _ (Antenna DoubleSRS) stName _ _) = [("SRS", stName, 2)]
toCountFormat (ObjectData _ (Antenna SRSBranch) stName _ _) = [("SRS", stName, 1), ("Branching Filter", stName, 1)]
toCountFormat (ObjectData _ (Antenna DoubleSRSBranch) stName _ _) = [("SRS", stName, 1), ("Branching Filter", stName, 2)]
toCountFormat (ObjectData _ _ _ _ _) = []

sumObj :: [(String, String, Int)] -> [(String, String, Int)]
sumObj = map f . groupBy (\ (a,b,c) (d,e,f) -> a == d && b == e) . sortBy (comparing (\ (a,b,c) -> (a,b)))
 where f xs@((a,b,c):_) = (a,b, sum $ map (\ (_,_,c) -> c) xs)

-- data AntennaData = AntennaData String Double AntennaType
countAntenna :: [AntennaData] -> [(String, String, Int)]
countAntenna as = sumObj $ concatMap toCountFormatAntenna as

toCountFormatAntenna (AntennaData _ _ WRS) = [("WRS", "WholeLine", 1)]
toCountFormatAntenna (AntennaData _ _ WRSSingleShield) = [("WRS", "WholeLine", 2)]
toCountFormatAntenna (AntennaData _ _ SRS) = [("SRS", "WholeLine", 1)]
toCountFormatAntenna (AntennaData _ _ DoubleSRS) = [("SRS", "WholeLine", 2)]
toCountFormatAntenna (AntennaData _ _ SRSBranch) = [("SRS", "WholeLine", 1), ("Branching Filter", "WholeLine", 1)]
toCountFormatAntenna (AntennaData _ _ DoubleSRSBranch) = [("SRS", "WholeLine", 1), ("Branching Filter", "WholeLine", 2)]
-- toCountFormatAntenna (AntennaData _ _ _) = []

-- data ObjectData = ObjectData
--   { _objName        :: String
--   , _objType        :: ObjectType
--   , _objStationName :: String
--   , _objLineName    :: String
--   , _objPos         :: (Double, Double)
--   } deriving (Show, Eq, Ord)
--
-- data ObjectType = PointMachine PointType Direction Chirality
--                 | Balise BaliseType Direction
--                 | Signal SignalType Direction
--                 | Bond BondType
--                 | AxleCounter
--                 | Antenna AntennaType
--                 | StationStop deriving (Show, Eq, Ord)
--
-- data BaliseType  = P0 | P1 | P2 | P3 | CP | L | R | ORP             deriving (Show, Eq, Ord)
-- data AntennaType = WRS | WRSSingleShield | SRS | DoubleSRS | SRSBranch | DoubleSRSBranch  deriving (Show, Eq, Ord)
-- type PointType   = Int
-- data BondType    = Set                                              deriving (Show, Eq, Ord)
-- data SignalType  = MainSig | ShuntSig | MiddleSig                   deriving (Show, Eq, Ord)
-- data Direction   = Up | Down                                        deriving (Show, Eq, Ord)
-- data Chirality   = LeftHand | RightHand                             deriving (Show, Eq, Ord)

drawObjectKm :: ObjectData -> (Diagram SVG R2)
drawObjectKm (ObjectData n1 (Balise bt dir) startSt startLine pos@(x,y)) = translate (r2 (x, 0)) (km === txt)
 where txt = textDef (show bt)
       km :: Diagram SVG R2
       km = rotate (1/4 @@ turn) (textDef (showKm x))

drawObjGraph :: [PointData] -> ObjGraph -> Error (Diagram SVG R2)
drawObjGraph pds = liftM mconcat . mapM (drawEdge pds)

drawEdge :: [PointData] -> ObjEdge -> Error (Diagram SVG R2)
drawEdge pds (Edge pn pn' _ _ _) = do
    (x,yy) <- lookupPointPos pn pds
    (x',yy') <- lookupPointPos pn' pds
    let y = 100 * yy
        y' = 100 * yy'
    return $ p2 (x,y) ~~ p2 (x',y')

lookupPointPos :: PointName -> [PointData] -> Error (Double, Double)
lookupPointPos pn pds = case lookup pn pds' of
    Nothing -> fail $ "The position is not defined for " ++ pn
    Just x -> return x
 where pds' = map (\ (PointData pn pos _) -> (pn, pos)) pds

drawStationData :: StationData -> Diagram SVG R2
drawStationData (StationData sn (f, t) _ Island _) = translateX centerOffset platformWithTxt
 where platformWithTxt = beside unit_X platform lkm ||| rkm
       platform = rect (t - f) 50 <> textDef sn 
       centerOffset = (t + f) / 2
       lkm = rotate (1/4 @@ turn) $ textDef (showKm f)
       rkm = rotate (1/4 @@ turn) $ textDef (showKm t)

drawStationData (StationData sn (f, t) _ Side _) = translateX centerOffset platformWithTxt
 where platformWithTxt = beside unit_X platform lkm ||| rkm
       platform = centerY (vcat' (with & sep .~ 280) $ [aPlatform, aPlatform]) <> textDef sn
       aPlatform = rect (t - f) 50
       centerOffset = (t + f) / 2
       lkm = rotate (1/4 @@ turn) $ textDef (showKm f)
       rkm = rotate (1/4 @@ turn) $ textDef (showKm t)

drawStationData (StationData sn (f, t) _ None _) = translateX centerOffset $ textDef sn
 where centerOffset = (t + f) / 2

type Position = (Double, Double)

drawPointData :: PointData -> Diagram SVG R2
drawPointData (PointData pn (x, y) _) = translate (x ^& (100 * y)) $ textDef pn

drawTunnelData :: TunnelData -> Diagram SVG R2
drawTunnelData tnl = mempty

-- putAntenna
data AntennaData = AntennaData String Double AntennaType

putAntennas :: Double -> Double -> [RadiData] -> [TunnelData] -> [(String, (Double, Double))] -> [AntennaData]
putAntennas s e rs ts srsSt = srs ++ wrs
 where srs   = putSRSAntennas ts srsSt
       wrs   = putWRSAntennas prods srs
       prods = makeSections s e rs ts

-- Antenna配置 ToDo
-- トンネルの変化点にWRSを置く件
-- アンテナの分配数
-- 分配するアンテナとそのとなりまでFixed Antennaとして配置する

putSRSAntennas :: [TunnelData] -> [(String, (Double, Double))] -> [AntennaData]
putSRSAntennas tunnel = concatMap (putSRSAntenna tunnel)

putSRSAntenna tunnel (n, (f, t)) = [srsFrom, srsTo]
 where srsPosFrom = f - 10
       srsPosTo = t + 10
       srsFrom = AntennaData (n ++ "_SRS") srsPosFrom (antennaType srsPosFrom)
       srsTo   = AntennaData (n ++ "_SRS") srsPosTo   (antennaType srsPosTo)
       antennaType pos
           | findTunnelType tunnel pos == DualShield   = DoubleSRS
           | findTunnelType tunnel pos == SingleShield = DoubleSRS
           | otherwise                             = SRS

data Prod = Prod Raz Tun deriving (Eq, Ord, Show)

instance Monoid Prod where
 mempty = Prod mempty mempty
 Prod r t `mappend` Prod r' t'  = Prod (r `mappend` r') (t `mappend` t')

data Raz = Raz Double deriving (Eq, Ord, Show)
data Tun = Tun TunnelType deriving (Eq, Ord, Show)

instance Monoid Raz where
 mempty = Raz (1/0)
 (Raz i) `mappend` (Raz j) = Raz $ min i j

instance Monoid Tun where
 mempty = Tun Ground
 (Tun i ) `mappend` (Tun j) = Tun $ max i j

instance Monoid Radi where
 mempty = Radi "" (1/0) ""
 (Radi n i ln) `mappend` (Radi n' j ln') = Radi (n ++ n') (sign * min (abs i) (abs j)) (ln ++ ln')
  where sign
            | abs i < abs j = signum i
            | otherwise     = signum j
instance Monoid Grad where
 mempty = Grad "" 0 ""
 (Grad n i ln) `mappend` (Grad n' j ln') = Grad (n ++ n') (sign * max (abs i) (abs j)) (ln ++ ln')
  where sign
            | abs i > abs j = signum i
            | otherwise     = signum j

makeSections :: Double -> Double -> [RadiData] -> [TunnelData] -> [Interval Prod]
makeSections s e rs ts = joinIntervals $ rs' ++ ts'
 where rs' = fillInterval s e $ map (over _2 $ \ (Radi _ r _)   -> Prod (Raz (abs r)) mempty) rs
       ts' = fillInterval s e $ map (over _2 $ \ (Tunnel _ t _) -> Prod mempty (Tun t)) ts

putWRSAntennas :: [Interval Prod] -> [AntennaData] -> [AntennaData]
putWRSAntennas ts as = concatMap (putWRSSection ts) $ zip ps (tail ps)
 where ps = map (\ (AntennaData _ pos _) -> pos) as

-- x軸が距離，y軸がf(Radius, TunnelType)のグラフを考える
-- let u = アンテナ間の単位面積基準
--     a = 隣接するFixed Antenna間の総面積
--     n = ceil (a / u)
--     u' = a / n
-- u'ごとにアンテナを（n-1)コ 配置していく
putWRSSection :: [Interval Prod] -> (Double, Double) -> [AntennaData]
putWRSSection ts (s,e) = putWRSSection' s u' u' (n - 1) ts'
 where ts'  = filter (\ ((f,t),_) -> s < t && f < e) ts
       n = ceiling (a / unitLength)
       u' = a / fromIntegral n
       a = case ts' of
           []       -> 0
           [(_, x)] -> afunc x * (e - s )
           xs        -> let ((f,t), x)    = head xs
                            middle        = sum [(e - s) * afunc x | ((s,e), x) <- tail $ init xs]
                            ((f',t'), x') = last xs
                        in (t - s) * afunc x + middle + (e - f') * afunc x'

putWRSSection' :: Double -> Double -> Double -> Int -> [Interval Prod] -> [AntennaData]
putWRSSection' _ _ _ 0 _  = []
putWRSSection' _ _ _ _ [] = []
putWRSSection' pos power u n ts@(((_,e), x@(Prod (Raz r) (Tun t))):rest)
    | power <= square = AntennaData "WRS" pos' wrsType : putWRSSection' pos' u u (n - 1) ts
    | power >  square = putWRSSection' e (power - square) u n rest
 where pos' = pos + power / afunc x
       square = afunc x * (e - pos)
       wrsType
           | t == DualShield   = WRS
           | t == SingleShield = WRSSingleShield
           | otherwise         = WRS

findTunnelType :: [TunnelData] -> Double -> TunnelType
findTunnelType tunnel pos = case getFirst $ mconcat $ map (First . tunnelType' pos) tunnel of
    Nothing -> Ground
    Just t -> t
 where tunnelType' :: Double -> TunnelData -> Maybe TunnelType
       tunnelType' pos ((s,e), Tunnel _ tType _)
           | s <= pos && pos <= e = Just tType
           | otherwise            = Nothing

--Decide SRS StationDatas
decideSRSStation :: [StationData] -> [(String, (Double, Double))]
decideSRSStation sds
 | all (\ x -> 3 <= length x) $ groupStations sds = filterSRS sds
 | otherwise                                      = filterSRS $ addSRSStations sds

filterSRS :: [StationData] -> [(String, (Double, Double))]
filterSRS = mapMaybe f
 where f (StationData n ft _ _ isIL)
           | isIL      = Just (n, ft)
           | otherwise = Nothing

groupStations :: [StationData] -> [[StationData]]
groupStations = filter (not . _stationIsILStation . head) . groupBy f
 where f x y = _stationIsILStation x == _stationIsILStation y

addSRSStations :: [StationData] -> [StationData]
addSRSStations ss
    | findGroupGreedy ss = ss
    | otherwise          = addSRSStations (addSRSStation ss)
 where findGroupGreedy :: [StationData] -> Bool
       findGroupGreedy ss
           | length ss <= 3 = any isIL ss
           | otherwise      = case splitAt 3 ss of
                  (fst3, rest)
                      | n == 0    -> False
                      | n == 1    -> findGroupGreedy rest
                      | otherwise -> findGroupGreedy $ dropBeforeSndIL ss
                   where n = length $ filter isIL fst3

dropBeforeSndIL ss = dropWhile isNotIL xs
 where (r:xs) = dropWhile isNotIL ss

addSRSStation :: [StationData] -> [StationData]
addSRSStation ss = updateToIL st ss
 where st = chooseAStation $ groupNonSRSStations ss
       updateToIL st = map f
        where -- f stData@(n, ft, isIL) = if n == st then (n, ft, True) else stData
              f = execState $ do
                  n <- use stationName
                  when (n == st) $ stationIsILStation .= True

chooseAStation :: [(Int, Double, StationName)] -> StationName
chooseAStation = (\(i,d,n) -> n) . maximum

groupNonSRSStations :: [StationData] -> [(Int, Double, StationName)]
groupNonSRSStations ss = case span isNotIL ss of
    ([], []) -> []
    ([], (r:ss)) -> groupNonSRSStations' (r:ss)
    (bbs@(b:bs), (r:ss)) -> (length bbs, pos r - pos b, nearestFromCenter ((pos r + pos b) / 2) bbs) : groupNonSRSStations' (r:ss)
    (bs, []) -> [(length bs, pos (last bs) - pos (head bs), nearestFromCenter ((pos (last bs) + pos (head bs)) / 2) bs)]

groupNonSRSStations' :: [StationData] -> [(Int, Double, StationName)]
groupNonSRSStations' [] = []
groupNonSRSStations' (r:ss) = case span isNotIL ss of
    ([], [] )    -> []
    (bs, [])     -> [(length bs, pos (last bs) - pos r, nearestFromCenter ((pos (last bs) + pos r) / 2) bs)]
    (bs, r':ss') -> (length bs, pos r' - pos r, nearestFromCenter ((pos r + pos r') / 2) bs) : groupNonSRSStations' (r':ss')

pos :: StationData -> Double
pos st = let (f,t) = _stationPlatformPos st in (f + t) / 2

isNotIL, isIL :: StationData -> Bool
isNotIL = not . _stationIsILStation
isIL = _stationIsILStation

nearestFromCenter :: Double -> [StationData] -> StationName
nearestFromCenter mid ss = _stationName $ minimumBy f $ oneOrTwoCentralStation ss
 where f = comparing (\ x -> abs (pos x - mid))

oneOrTwoCentralStation ::[StationData] -> [StationData]
oneOrTwoCentralStation [] = []
oneOrTwoCentralStation ss = nub [ss !! (floor num), ss !! (ceiling num)]
 where num = fromIntegral (length ss - 1) / 2

-- Draw Antennas
drawAntennas :: [AntennaData] -> Diagram SVG R2
drawAntennas = mconcat . map drawAntenna

drawAntenna :: AntennaData -> Diagram SVG R2
drawAntenna (AntennaData n pos WRS) = translateX pos antenna
 where antenna = img === km
       km = rotate (1/4 @@ turn) (textDef (showKm pos))
       img = antennaImg red "WRS"
drawAntenna (AntennaData n pos WRSSingleShield) = translateX pos antenna
 where antenna = centerY (img === strutY 5 === img) === km
       km = rotate (1/4 @@ turn) (textDef (showKm pos))
       img = antennaImg red "WRS"
drawAntenna (AntennaData n pos SRS) = translateX pos antenna
 where antenna = img === km
       km = rotate (1/4 @@ turn) (textDef (showKm pos))
       img = antennaImg blue "SRS"
drawAntenna (AntennaData n pos DoubleSRS) = translateX pos antenna
 where antenna = centerX (img ||| img) === km
       km = rotate (1/4 @@ turn) (textDef (showKm pos))
       img = antennaImg blue "SRS"
drawAntenna (AntennaData n pos SRSBranch) = translateX pos antenna
 where antenna = img === km
       km = rotate (1/4 @@ turn) (textDef (showKm pos))
       img = antennaImg blue "SRS"
drawAntenna (AntennaData n pos DoubleSRSBranch) = translateX pos antenna
 where antenna = centerX (img ||| img) === km
       km = rotate (1/4 @@ turn) (textDef (showKm pos))
       img = antennaImg blue "SRS"

antennaImg col str
 = scale 20 antenna === fc col (textDef str) === strutY 5 
 where antenna = beside unitY (style (square 1)) (style $ tr === vrule 0.5)
       tr = scaleY (- 1.1) $ eqTriangle 0.8
       style = lc col . lw medium

-- balise
drawObject :: ObjectData -> Diagram SVG R2
drawObject (ObjectData n1 (Balise bt dir) startSt startLine pos@(x,y)) = translate (r2 (x, 100 * y)) img
 where txt = textDef (show bt)
       km :: Diagram SVG R2
       km = rotate (1/4 @@ turn) (textDef (showKm x))
       under a b = beside unitY a b
       img | bt  == P0    = (scale (-1) $ baliseImg bt) `under` txt -- `under` km
           | dir == Up   = baliseImg bt === txt -- === km
           | dir == Down = (scale (-1) $ baliseImg bt) `under` txt -- `under` km

baliseImg P0  = scale 20 $ baliseSquare # fc blue # lw medium
baliseImg P1  = scale 20 $ baliseTriangle # lc blue # lw medium
baliseImg P2  = scale 20 $ baliseTriangle # lc blue # lw medium
baliseImg CP  = scale 20 $ baliseTriangle # lc purple # lw medium
baliseImg L   = scale 20 $ baliseTriangle # lc green # lw medium
baliseImg R   = scale 20 $ baliseTriangle # lc red # lw medium
baliseImg ORP = scale 20 $ baliseTriangle # lc aqua # lw medium

baliseSquare = centerXY $ scaleY 0.6 $ square 1
-- baliseSquare = align unitY $ scaleY 0.6 $ square 1
baliseTriangle = align unitY $ scaleY 0.7 $ eqTriangle 1

-- drawObject (ObjectData n1 (Balise P0 Down) startSt startLine pos@(x,y))
-- drawObject (ObjectData n1 (Balise P1 Up) startSt startLine pos@(x,y))
-- drawObject (ObjectData n1 (Balise P1 Down) startSt startLine pos@(x,y))
-- drawObject (ObjectData n1 (Balise P2 Up) startSt startLine pos@(x,y))
-- drawObject (ObjectData n1 (Balise P2 Down) startSt startLine pos@(x,y))

-- drawObj (Obj name (Balise bt dir) st li v@(x,y)) = translate (r2 v) img
--  where txt = textDef (show bt)
--        km = rotate (1/4 @@ turn) (textDef (show x))
--        under a b = beside unitY a b
--        img | dir == Up   = baliseImg bt === txt === km
--            | dir == Down =  (scale (-1) $ baliseImg bt) `under` txt `under` km

-- data ObjectData = ObjectData
--   { _objName        :: String
--   , _objType        :: ObjectType
--   , _objStationName :: String
--   , _objLineName    :: String
--   , _objPos         :: (Double, Double)
--   } deriving (Show, Eq, Ord)
