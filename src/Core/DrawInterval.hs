module Core.DrawInterval where

import Core.Interval
import Core.CommonData
import Core.Replace

-- base
import Numeric (showFFloat)

-- SVGFonts
import Graphics.SVGFonts

-- diagrams-svg
import Diagrams.Prelude hiding (Duration, interval)
import Diagrams.Backend.SVG
import Diagrams.Core.Points


show2Digit d = showFFloat (Just 2) d ""

-- Draw
textDef t = scale textSize $ fc black $ lc black $ stroke $ textSVG' (TextOpts t lin INSIDE_H KERN False 1 1)
textSize = 25

drawGrads :: [GradData] -> Diagram SVG R2
drawGrads itvls = mconcat (map drawGrad itvls) <> lastKm
 where lastKm = translateX e $ rotate (1/4 @@ turn) $ textDef (showKm e)
       ((_,e), Grad _ _ _) = last itvls

drawGrad :: GradData -> Diagram SVG R2
drawGrad ((s,e), Grad n d _) = pathImg <> km
 where title = translateX mid $ textDef n === textDef (show2Digit d)
       mid = (s + e) / 2
       km = translateX s $ rotate (1/4 @@ turn) $ textDef (showKm s)
       pathImg | 0 /= d = (scaleY (signum d) $ p2 (s,25) ~~ p2 (e,-25)) <> title
               | d == 0 = p2 (s, 0) ~~ p2 (e, 0)

drawRadis :: [RadiData] -> Diagram SVG R2
drawRadis itvls = mconcat (map drawRadi itvls) <> lastKm
 where lastKm = translateX e $ rotate (1/4 @@ turn) $ textDef (showKm e)
       ((_, e), Radi _ _ _) = last itvls

drawRadi :: RadiData -> Diagram SVG R2
drawRadi ((s,e), Radi n d  _) = translateX s $ pathImg <> km
 where unitArc = scaleY 50 $ centerY $ scaleX (1/2) $ align unit_X $ arc (0 @@ turn) (1/2 @@ turn)
       dist = e - s
       title = translateX (dist / 2) $ textDef n === textDef (show2Digit d)
       km = rotate (1/4 @@ turn) $ (textDef (showKm s))
       pathImg | (1/0) /= d = (scaleY (signum d) $ scaleX dist unitArc) <> title
               | d == (1/0) = fromOffsets [dist *^ unitX]
