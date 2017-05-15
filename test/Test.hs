#!/usr/bin/env stack
{- stack
     --resolver lts-6.6
     --install-ghc
     runghc
     --package turtle-1.3.0
     --package text
     --package aeson
     --package bytestring
     --package directory
     --package filepath
     --package optparse-applicative-0.13.2.0
-}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle.Shell
import Turtle.Line
import Turtle.Prelude hiding (die)

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getAppUserDataDirectory, getDirectoryContents, getCurrentDirectory)
import System.FilePath ((</>), dropExtension)
import qualified Data.ByteString.Lazy as BS

import Data.Monoid ((<>))
import Data.List
import Control.Applicative
import Control.Monad
import Data.String
import GHC.IO.Exception (ExitCode(..))
import System.Exit (die)

data Packages = Packages {
      patched :: [Text],
      vanilla :: [Text]
    } deriving (Show, Eq, Ord)

instance FromJSON Packages where
    parseJSON (Object v) = Packages <$> v.: "patched" <*> v.: "vanilla"
    parseJSON _ = empty

parsePackagesFile :: FilePath -> IO (Maybe Packages)
parsePackagesFile fname = do
  contents <- BS.readFile fname
  let packages = decode contents
  patched' <- patchedLibraries
  return $ fmap (\p -> p { patched = patched' }) packages

packagesFilePath :: IO FilePath
packagesFilePath = return "packages.json"

patchedLibraries :: IO [Text]
patchedLibraries = do
  packageListing <- getDirectoryContents "patches"
  let packages = map T.pack
               . nub
               . map dropExtension
               . filter (\p -> p `notElem` ["",".",".."])
               $ packageListing
      distinctPackages = delete "directory" -- Temporary until exceptions are implemented
                       . nub
                       . map (T.dropEnd 1 . T.dropWhileEnd (/= '-'))
                       $ packages

  return $ packages ++ distinctPackages

buildPackage :: Text -> IO ()
buildPackage pkg = do
  let outString = "Installing package " ++ T.unpack pkg ++ "..."
      lenOutString = length outString
      dashes = replicate lenOutString '-'
  print "Current Directory:"
  getCurrentDirectory >>= putStrLn
  putStrLn dashes
  putStrLn outString
  putStrLn dashes
  sh $ procExitOnError "etlas" ["--patches-dir", ".", "install", pkg] empty

procExitOnError :: Text -> [Text] -> Shell Line -> Shell ()
procExitOnError prog args shellm = do
  exitCode <- proc prog args shellm
  case exitCode of
    ExitFailure code -> liftIO $ die ("ExitCode " ++ show code)
    ExitSuccess -> return ()

main :: IO ()
main = do
  let vmUpdateCmd = "etlas update"
  _ <- shell vmUpdateCmd ""
  etlasPkgs <- packagesFilePath
  pkg <- parsePackagesFile etlasPkgs
  case pkg of
    Nothing -> die "Problem parsing your packages.json file"
    Just pkg' -> do
      let packages = (patched pkg') <> (vanilla pkg')
      mapM_ buildPackage packages
