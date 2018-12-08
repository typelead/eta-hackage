{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>), dropExtension)
import qualified Data.ByteString.Lazy as BS

import Data.Monoid ((<>))
import Data.List
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.String
import GHC.IO.Exception (ExitCode(..))
import System.Exit (die)
import System.Process

data Packages = Packages {
      patched :: [Text],
      vanilla :: [Text],
      ignored :: [Text],
      ignoredVersions :: [Text]
    } deriving (Show, Eq, Ord)

instance FromJSON Packages where
    parseJSON (Object v) =
      Packages <$> v.: "patched"
               <*> v.: "vanilla"
               <*> v.: "ignored"
               <*> v.: "ignored-versions"
    parseJSON _ = empty

parsePackagesFile :: FilePath -> IO (Maybe Packages)
parsePackagesFile fname = do
  contents <- BS.readFile fname
  traverse modifyPatchedLibraries $ decode contents

packagesFilePath :: IO FilePath
packagesFilePath = return "packages.json"

modifyPatchedLibraries :: Packages -> IO Packages
modifyPatchedLibraries pkgs = do
  packageListing <- getDirectoryContents "patches"
  let packages = map T.pack
               . sort
               . nub
               . map dropExtension
               . filter (\p -> p `notElem` ["",".",".."])
               $ packageListing
  return $ pkgs { patched = filterLibraries pkgs packages }

filterLibraries :: Packages -> [Text] -> [Text]
filterLibraries Packages { ignored         = ignoredPackages
                         , ignoredVersions = ignoredPackageVersions } set0 =
  recentVersions
  -- TODO: Find a way to validate older versions too that isn't crazy slow.
  -- ++ remoteVersions ++ concat restVersions
  where (recentVersions, restVersions) = unzip $ map findAndExtractMaximum
                                               $ groupBy grouping set1
        remoteVersions = map actualName recentVersions
        set1 = filter (\s -> not ((any (== (actualName s)) ignoredPackages) ||
                                  (any (== s) ignoredPackageVersions))) set0
        grouping p1 p2 = actualName p1 == actualName p2

actualName :: Text -> Text
actualName = T.dropEnd 1 . T.dropWhileEnd (/= '-')

actualVersion :: Text -> [Int]
actualVersion = map (read . T.unpack) . T.split (== '.') . actualVersion'

actualVersion' :: Text -> Text
actualVersion' = T.takeWhileEnd (/= '-')

cmpVersion :: [Int] -> [Int] -> Ordering
cmpVersion xs ys
  | (x:_) <- dropWhile (== 0) $ map (uncurry (-)) $ zip xs ys
  = compare x 0
  | otherwise = compare (length xs) (length ys)

findAndExtractMaximum :: [Text] -> (Text, [Text])
findAndExtractMaximum g = (last pkgVersions, init pkgVersions)
  where pkgVersions = sortBy (\a b -> cmpVersion (actualVersion a) (actualVersion b)) g

procExitOnError :: String -> String -> [String] -> IO ()
procExitOnError dir prog args = do
  (_, _, _, ph) <- createProcess (proc prog args) { cwd = Just dir }
  exitCode <- waitForProcess ph
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
          constraints = map (\p -> T.unpack $ actualName p <> "==" <> actualVersion' p) packages
          packageNames = map (T.unpack . actualName) packages
          tmpDir  = "testing"
          tmpFile = "testing/cabal.project"
      etaHackageRoot <- makeAbsolute "."
      bracket (createDirectoryIfMissing True tmpDir)
              (const (removeFile tmpFile)) $ \_ -> do
        forM (zip packageNames constraints) $ \(pkg, constr) -> do
          let projectFile = (unlines ["independent-goals: True",
                                      "extra-packages: " <> constr,
                                      "tests: False",
                                      "benchmarks: False"])
          writeFile tmpFile projectFile
          putStrLn $ "[BUILDING] " <> constr
          procExitOnError tmpDir "etlas" ["--patches-dir", etaHackageRoot, "build", pkg]
        return ()
