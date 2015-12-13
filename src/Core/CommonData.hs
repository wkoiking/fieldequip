{-# LANGUAGE TemplateHaskell #-}
module Core.CommonData where

import Core.Interval

-- lens
import Control.Lens

type StationName = String
type PointNum    = Int
type DwellTime   = Double

data StationData = StationData
    { _stationName         :: String
    , _stationPlatformPos  :: (Double, Double) -- From, To
    , _stationDwellTime    :: Double
    , _stationPlatformType :: PlatformType
    , _stationIsILStation  :: Bool
    } deriving (Show, Eq, Ord)

data PlatformType = Island | Side | None deriving (Show, Eq, Ord)

data PointData = PointData
    { _pointName :: String
    , _pointPos  :: (Double, Double)
    , _pointNum  :: Int
    } deriving (Show, Eq, Ord)

type RadiData = Interval Radi
type GradData = Interval Grad
type TunnelData = Interval Tunnel

data Radi = Radi
    { _radiName :: String
    , _radius :: Double
    , _radiLineName :: String
    } deriving (Show, Eq, Ord)

data Grad = Grad
    { _gradName :: String
    , _gradient :: Double
    , _gradLineName :: String
    } deriving (Show, Eq, Ord)

data Tunnel = Tunnel
    { _tunnelName :: String
    , _tunnelType :: TunnelType
    , _tunnelLineName :: String
    } deriving (Show, Eq, Ord)

data TunnelType = Ground | DualShield | SingleShield | Viaduct deriving (Eq, Ord, Show)

makeLenses ''StationData
makeLenses ''PointData
makeLenses ''Radi
makeLenses ''Grad
makeLenses ''Tunnel
