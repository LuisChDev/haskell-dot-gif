{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports #-}
module Main (main) where

-- import HaskellDotGif (animate)
import "haskell-dot-gif" Control.Parallel (qsort, qsort'')
import System.Random

main :: IO ()
main = do
  let lista = take 5000000 $ randoms @Int @StdGen $ mkStdGen 1000
  putTextLn $ show $ fromMaybe 0 $ viaNonEmpty last $ qsort lista
-- main = animate
