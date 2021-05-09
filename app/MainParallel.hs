{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports #-}
module Main where

import "haskell-dot-gif" Control.Parallel
import System.Random

main :: IO ()
main = simulation 5

-- main :: IO ()
-- main = do
--   let lista = take 5000000 $ randoms @Int @StdGen $ mkStdGen 1000
--   putTextLn $ show $ fromMaybe 0 $ viaNonEmpty last $ qsort'' 0 lista
