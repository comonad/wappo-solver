{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE RankNTypes                #-}



import qualified Wappo

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine as Backend
--import Diagrams.Backend.Postscript.CmdLine as Backend
import Diagrams.TwoD.Offset
--import qualified Graphics.SVGFonts as SF
--import qualified Graphics.SVGFonts.ReadFont
import System.IO.Unsafe
import Diagrams.TwoD.Image

import Diagrams.Trail  -- for trailPoints

visPoints :: [P2 Double] -> Diagram B
visPoints pts = atPoints pts (repeat (circle 0.03 # lw none # fc blue))

visCorners = visPoints . trailPoints


-- Backend.pagesMain :: [QDiagram Postscript V2 Double Any] -> IO ()
-- type Diagram b = QDiagram b (V b) (N b) Any
type Dia = Diagram B

--lin2 :: Graphics.SVGFonts.ReadFont.PreparedFont Double
--lin2 = unsafePerformIO SF.bit

--text' :: Double -> String -> Dia
--text' d s = (strokeP $ SF.textSVG' (SF.TextOpts lin2 SF.INSIDE_H SF.KERN False d d) s) # lw none


player = unsafePerformIO $ do
  (Right i)<-loadImageEmb "icons/icons8-man-walking-100.png"
  return $ image i # sized (dims2D 1 1)
--text' 1 "L⊙⚝⚐⛝🙋🙌🛉R"# fc black # lw none  # showOrigin
test :: [Dia]
test = [ (
           visCorners (square 1)
       <> (vrule 1 # dashingL [0.2,0.05,0.5,0.05,0.2] 0 # lineCap LineCapRound # lc red # translate (r2 (-0.5, 0)))
       <> (hrule 1 # lineCap LineCapRound # lw 10 # lc blue # translate (r2 (0,-0.5)))
       <> (player )
       <> (square 1 # fc aqua # lw none  # showOrigin)
         ) `atop` circle 2
       ]


main :: IO ()
main = do
  Backend.defaultMain $ (test::[Dia]) !! 0
  --Backend.pagesMain (test::[Dia])
  --Wappo.main



