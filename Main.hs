{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import qualified Wappo

import Diagrams.Prelude
--import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Postscript.CmdLine as Backend




-- Backend.pagesMain :: [QDiagram Postscript V2 Double Any] -> IO ()
-- type Diagram b = QDiagram b (V b) (N b) Any


test :: [Diagram B]
test = [square 1 # fc aqua `atop` circle 1]

main :: IO ()
main = do
  Backend.pagesMain (test::[Diagram B])
  --Wappo.main



