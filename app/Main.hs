module Main where

-- ToDo
-- SRSのアンテナのブランチの処理
-- SRS Stationを決定するアルゴリズムを調査
-- 駅ごと、連動区ごとの員数の集計

-- base
import Data.List
import Data.Function
import Data.Maybe
import Control.Monad

-- fieldequip
import Core.Topology
import Core.CommonData
import Core.Interval
import Core.DrawInterval
import Frontend hiding (Error)
import Backend

-- diagrams-core
import Diagrams.Core.Points

-- diagrams-lib
import Diagrams.Prelude hiding (option, Dual, Line, view, Qualifiable, First, getFirst, over, e)
import Diagrams.Coordinates

-- diagrams-svg
import Diagrams.Backend.SVG

-- SVGFont
import Graphics.SVGFonts

-- transformers
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Maybe hiding (maybeToExceptT)
import Control.Monad.Trans.Class (lift)

-- transferers-compat
import Control.Monad.Trans.Except

(s, e) = (-537.292, 15147.952)

main = do
    str <- readFile "./test/input/SAO6_Data.md"
    case readSpecification str of
        Left err1 -> putStrLn ("Parse Error: " ++ err1)
        Right spec -> case specToImpl spec of
            Left err2 -> putStrLn ("Auto alignment Error: " ++ err2)
            Right impl -> do
                mapM_ (putStrLn . show) $ countObject impl
                case drawImplementation impl of
                    Left err3     -> putStrLn ("Drawing Error: " ++ err3)
                    Right diagram -> do
                        let ss     = _implStations impl
                            (rs:_) = _implRadis impl
                            (ts:_) = _implTunnel impl
                            srsSt  = decideSRSStation ss
                            as     = putAntennas s e rs ts srsSt
                        mapM_ (putStrLn . show) $ countAntenna as
                        renderSVG "./test/output/sample.svg" (Width 5000) $ diagram === drawAntennas as

specToImpl :: Specification -> Error Implementation
specToImpl spec@(Specification g p s r gr t) = do
    case S.runState (runExceptT $ putBalises spec) g' of
        (Left e, _) -> fail e
        (Right e, g'') -> return $ Implementation g'' p s r gr t
 where g' = specGraphToObjGraph g

specGraphToObjGraph = map specEdgeToObjEdge
 where specEdgeToObjEdge (Edge pn pn' k k' _) = Edge pn pn' k k' (ObjLabel [])

-- putBalise
putBalises :: Specification -> ExceptT String (S.State ObjGraph) ()
putBalises spec = do
    let sds   = _specStations spec
        pds   = _specPoints spec
        g     = _specGraph spec
        paths = concat [findPath spec s s' | s <- stops, s' <- stops]
         where stops = concatMap edgeToStops g
    mapM_ (putBalise sds pds) paths

-- StopはLineNameがqualifyされているが，
-- StationDataはされていない
-- MaybeTからExceptTに変更する
-- はみ出たら置かなければ良い
putBalise :: [StationData] -> [PointData] -> (SpecPath, Stop, Stop) -> ExceptT String (S.State ObjGraph) ()
putBalise sds pds (path, orig@(Stop startStName), dest@(Stop endStName)) = do
    objGraph <- lift S.get
    origChainage <- maybeToExceptT "failed in finding orig" $ lookupStationChainage orig sds
    destChainage <- maybeToExceptT "failed in finding dest" $ lookupStationChainage dest sds
    let (sign, dir)
            | (destChainage - origChainage) > 0 = (id, Up)
            | otherwise                         = (negate, Down)
        xStartP0 = origChainage + sign (trainLength / 2)
        xStopP0' = xStopP0 - sign trainLength
        xStopP1  = xStopP0 - sign 300
        xStopP2  = xStopP0 - sign 20
        xStopP2' = xStopP2 - sign trainLength
        xStopP0  = destChainage + sign (trainLength / 2)
        xs = [xStartP0, xStopP1, xStopP2, xStopP2', xStopP0, xStopP0']
        mxs = map Just xs
        upDateEdge :: (SpecEdge, ObjectData) -> (S.State ObjGraph) ()
        upDateEdge (e1@(Edge pn1 pn1' _ _ _), objData) = do
            objGraph <- S.get
            let objGraph' = map (\ e2@(Edge pn2 pn2' k k' (ObjLabel objs)) -> if pn1 == pn2 && pn1' == pn2'
                    then Edge pn1 pn1' k k' (ObjLabel $ insertBy (f dir) objData objs)
                    else e2) objGraph
                 where f Up = compare `on` _objPos
                       f Down = flip (compare `on` _objPos)
            S.put objGraph'
    let mes@[e1,e2,e3,e4,e5,e6] = map (lookupCrossEdge pds path) xs
        mys   = map (liftM $ lookupY pds) $ zipWith (liftM2 (,)) mes mxs
        mps    = zipWith (liftM2 (,)) mxs mys
        mobjs' = map Just [ObjectData n1 (Balise P0 dir) startSt startLine
                          ,ObjectData n1 (Balise P1 dir) endSt endLine
                          ,ObjectData n1 (Balise P2 dir) endSt endLine
                          ,ObjectData n1 (Balise P2 dir) endSt endLine
                          ,ObjectData n1 (Balise P0 dir) endSt endLine
                          ,ObjectData n1 (Balise P0 dir) endSt endLine
                          ]
        mobjs     = zipWith (liftM2 ($)) mobjs' mps
        updates   = catMaybes $ zipWith (liftM2 (,)) mes mobjs
        objGraph' = S.execState (mapM_ upDateEdge updates) objGraph
    lift $ S.put objGraph'
 where edgeToLineSeg :: [PointData] -> SpecEdge -> Maybe (Double, Double, Double, Double)
       edgeToLineSeg pds (Edge pn pn' _ _ _) = do
           (x1, y1) <- lookup pn pds'
           (x2, y2) <- lookup pn' pds'
           return (x1, y1, x2, y2)
        where pds' = map (\ (PointData pn pos _) -> (pn, pos)) pds
       lookupY :: [PointData] -> (SpecEdge, Double) -> Double
       lookupY pds (e, x) = fromJust $ do
           (x1, y1, x2, y2) <- edgeToLineSeg pds e
           return $ (y2 - y1) * (x - x1) / (x2 - x1) + y1
       n1 = "" -- Baliseの名前はどうする？
       lookupStationChainage :: Stop -> [StationData] -> Maybe Double
       lookupStationChainage (Stop n) = lookup n' . stationDatasToAsoc
           where stationDatasToAsoc = map (\ (StationData n (f,t) _ _ _) -> (n, (f + t) / 2))
                 n' = dequalify n
       lookupCrossEdge :: [PointData] -> SpecPath -> Double -> Maybe SpecEdge
       lookupCrossEdge pds path x = find (\e -> do
           case edgeToLineSeg pds e of
               Nothing -> False
               Just (x1, y1, x2, y2) -> (x1 <= x && x <= x2 || x2 <= x && x <= x1) && (x1 /= x2)) path
       (startSt, startLine) = spanQualified startStName
       (endSt, endLine) = spanQualified endStName

-- maybeToMaybeT :: Maybe a -> MaybeT (S.State s) a
    -- maybeToMaybeT = MaybeT . return

maybeToExceptT :: String -> Maybe a -> ExceptT String (S.State s) a
maybeToExceptT e m = ExceptT $ return $ case m of
    Just m -> Right m
    Nothing -> Left e

-- errorToExceptT :: Either e a -> ExceptT e (S.State s) a
-- errorToExceptT = ExceptT . return

-- maybeToExcept :: String -> Maybe a -> Either String a
-- maybeToExcept e m = case m of
--     Just a -> Right a
--     Nothing -> Left e

-- throwError
-- ErrorT :: m (Either e a) -> ErrorT e m a
-- MaybeT :: m (Maybe a) -> MaybeT m a
