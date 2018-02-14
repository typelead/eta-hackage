{-# LANGUAGE OverloadedStrings #-}
module Main where

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
import System.Process

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
               . sort
               . nub
               . map dropExtension
               . filter (\p -> p `notElem` ["",".",".."])
               $ packageListing
  return $ filterLibraries packages

-- These will not be built for various reasons.
ignoredPackages :: [Text]
ignoredPackages = ["singletons" ,"directory", "servant-docs"]

ignoredPackageVersions :: [Text]
ignoredPackageVersions = []

filterLibraries :: [Text] -> [Text]
filterLibraries set0 = recentVersions ++ remoteVersions ++ concat restVersions
  where (recentVersions, restVersions) = unzip $ map findAndExtractMaximum
                                               $ groupBy grouping set1
        remoteVersions = map actualName recentVersions
        set1 = filter (\s -> not ((any (== (actualName s)) ignoredPackages) ||
                                  (any (== s) ignoredPackageVersions))) set0
        grouping p1 p2 = actualName p1 == actualName p2

actualName :: Text -> Text
actualName = T.dropEnd 1 . T.dropWhileEnd (/= '-')

actualVersion :: Text -> [Int]
actualVersion = map (read . T.unpack) . T.split (== '.') .  T.takeWhileEnd (/= '-')

cmpVersion :: [Int] -> [Int] -> Ordering
cmpVersion xs ys
  | (x:_) <- dropWhile (== 0) $ map (uncurry (-)) $ zip xs ys
  = compare x 0
  | otherwise = compare (length xs) (length ys)

findAndExtractMaximum :: [Text] -> (Text, [Text])
findAndExtractMaximum g = (last pkgVersions, init pkgVersions)
  where pkgVersions = sortBy (\a b -> cmpVersion (actualVersion a) (actualVersion b)) g

buildPackage :: Text -> IO ()
buildPackage pkg = do
  let outString = "Installing package " ++ T.unpack pkg ++ "..."
      lenOutString = length outString
      dashes = replicate lenOutString '-'
  putStrLn dashes
  putStrLn outString
  putStrLn dashes
  procExitOnError "etlas" ["--patches-dir", ".", "install", T.unpack pkg]

procExitOnError :: String -> [String] -> IO ()
procExitOnError prog args = do
  exitCode <- rawSystem prog args
  case exitCode of
    ExitFailure code -> die ("ExitCode " ++ show code)
    ExitSuccess -> return ()

main :: IO ()
main = do
  _ <- system "etlas update"
  etlasPkgs <- packagesFilePath
  pkg <- parsePackagesFile etlasPkgs
  case pkg of
    Nothing -> die "Problem parsing your packages.json file"
    Just pkg' -> do
      let packages = (patched pkg') <> (vanilla pkg')
      mapM_ buildPackage packages
