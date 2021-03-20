{-# Language QuasiQuotes #-}
{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# Language LambdaCase #-}
module Language.ISPC 
  ( compile
  , call
  ) where

import Control.Monad.IO.Class
import Data.Foldable
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import System.Process
import System.Exit

compile :: [String] -> String -> Q [Dec]
compile args source = do
  sourceFile <- addTempFile "ispc"
  liftIO $ writeFile sourceFile source 
  objectFile <- addTempFile "o"
  depFile <- addTempFile "dep"
  handle <- liftIO $ spawnProcess "ispc" $ [sourceFile,"-o",objectFile,"-MMM",depFile] ++ args
  addModFinalizer $ 
    liftIO (waitForProcess handle) >>= \case
      ExitFailure code -> fail "ispc: compilation failed"
      ExitSuccess -> do
        deps <- liftIO $ readFile depFile
        for_ (lines deps) addDependentFile
  addForeignFilePath RawObject objectFile
  pure []

uniqueName :: String -> Q Name
uniqueName method = newName . show =<< newName method

call :: String -> TypeQ -> ExpQ
call method ty = do
  name <- uniqueName method
  stmt <- forImpD CCall Unsafe method name ty
  addTopDecls [stmt]
  varE name
