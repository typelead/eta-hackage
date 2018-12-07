import System.Directory
import System.FilePath ((</>), dropExtension)
import Data.List

main = do
  pkgVers1 <- fromPatchesFolder
  pkgVers2 <- fromWorkingFile
  let preferences = map toPreference
                  . groupBy (\(a1,_) (a2,_) -> a1 == a2)
                  . map splitPkgVersion
                  . sort
                  . nub
                  $ pkgVers1 ++ pkgVers2
      splitPkgVersion pkgVer = (reverse (drop 1 rest), reverse reverseVer)
        where (reverseVer, rest) = break (== '-') $ reverse pkgVer
      toPreference pairs@((pkg,_):_) =
        "pref-ver: " ++ pkg ++ " " ++ intercalate " || " versions
        where versions = map ((\v -> "== " ++ v) . snd) pairs
  writeFile "PREFERENCES" (unlines preferences)


fromPatchesFolder = do
  patchFiles <- getDirectoryContents "patches"
  return . map dropExtension
         . filter (\p -> p `notElem` ["",".",".."])
         $ patchFiles

fromWorkingFile = do
  contents <- readFile "WORKING"
  let makePkgVers l = map ((pkg <> "-") <>) vers
        where (pkg:vers) = words l
  return $ concatMap makePkgVers $ lines $ contents
