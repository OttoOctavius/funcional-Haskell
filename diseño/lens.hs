{-# LANGUAGE TemplateHaskell #-}
--https://hackage.haskell.org/package/lens-tutorial-1.0.2/docs/Control-Lens-Tutorial.html
import Control.Lens hiding (element)

data Atom = Atom { _element :: String, _point :: Point } deriving (Show)

data Point = Point { _x :: Double, _y :: Double } deriving (Show)

makeLenses ''Atom
makeLenses ''Point

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

main = let atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
       in print $ shiftAtomX atom