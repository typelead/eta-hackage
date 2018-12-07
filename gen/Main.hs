import System.Directory
import System.FilePath ((</>), dropExtension)
import Data.List

main = do
  patchFiles <- getDirectoryContents "patches"
  let preferences = map toPreference
                  . groupBy (\(a1,_) (a2,_) -> a1 == a2)
                  . map splitPkgVersion
                  . sort
                  . nub
                  . map dropExtension
                  . filter (\p -> p `notElem` ["",".",".."])
                  $ patchFiles
      splitPkgVersion pkgVer = (reverse (drop 1 rest), reverse reverseVer)
        where (reverseVer, rest) = break (== '-') $ reverse pkgVer
      toPreference pairs@((pkg,_):_) =
        "pref-ver: " ++ pkg ++ " " ++ intercalate " || " versions
        where versions = map ((\v -> "== " ++ v) . snd) pairs
  writeFile "PREFERENCES" (unlines preferences)
