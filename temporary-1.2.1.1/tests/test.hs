{-# LANGUAGE CPP #-}
import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import System.Directory
import System.IO
import System.FilePath
import System.Environment.Compat
import Data.Bits
import Data.List
import GHC.IO.Handle
#ifndef mingw32_HOST_OS
import System.Posix.Files
#endif

import System.IO.Temp

main = do
  -- force single-thread execution, because changing TMPDIR in one of the
  -- tests may leak to the other tests
  setEnv "TASTY_NUM_THREADS" "1"
#ifndef mingw32_HOST_OS
  setFileCreationMask 0
#endif
  sys_tmp_dir <- getCanonicalTemporaryDirectory

  defaultMain $ testGroup "Tests"
    [ testCase "openNewBinaryFile" $ do
        (fp, fh) <- openNewBinaryFile sys_tmp_dir "test.txt"
        let fn = takeFileName fp
        assertBool ("Does not match template: " ++ fn) $
          ("test" `isPrefixOf` fn) && (".txt" `isSuffixOf` fn)
        assertBool (fp ++ " is not in the right directory " ++ sys_tmp_dir) $
          takeDirectory fp `equalFilePath` sys_tmp_dir
        hClose fh
        assertBool "File does not exist" =<< doesFileExist fp
#ifndef mingw32_HOST_OS
        status <- getFileStatus fp
        fileMode status .&. 0o777  @?= 0o666 
#endif
        removeFile fp
    , testCase "withSystemTempFile" $ do
        (fp, fh) <- withSystemTempFile "test.txt" $ \fp fh -> do
          let fn = takeFileName fp
          assertBool ("Does not match template: " ++ fn) $
            ("test" `isPrefixOf` fn) && (".txt" `isSuffixOf` fn)
          assertBool (fp ++ " is not in the right directory " ++ sys_tmp_dir) $
            takeDirectory fp `equalFilePath` sys_tmp_dir
          assertBool "File not open" =<< hIsOpen fh
          hPutStrLn  fh "hi"
          assertBool "File does not exist" =<< doesFileExist fp
#ifndef mingw32_HOST_OS
          status <- getFileStatus fp
          fileMode status .&. 0o777  @?= 0o600
#endif
          return (fp, fh)
        assertBool "File still exists" . not =<< doesFileExist fp
        assertBool "File not closed" =<< hIsClosed fh
    , testCase "withSystemTempDirectory" $ do
        fp <- withSystemTempDirectory "test.dir" $ \fp -> do
          let fn = takeFileName fp
          assertBool ("Does not match template: " ++ fn) $
            ("test.dir" `isPrefixOf` fn)
          assertBool (fp ++ " is not in the right directory " ++ sys_tmp_dir) $
            takeDirectory fp `equalFilePath` sys_tmp_dir
          assertBool "Directory does not exist" =<< doesDirectoryExist fp
#ifndef mingw32_HOST_OS
          status <- getFileStatus fp
          fileMode status .&. 0o777  @?= 0o700
#endif
          return fp
        assertBool "Directory still exists" . not =<< doesDirectoryExist fp
    , testCase "writeSystemTempFile" $ do
        fp <- writeSystemTempFile "blah.txt" "hello"
        str <- readFile fp
        "hello" @?= str
        removeFile fp
    , testCase "emptySystemTempFile" $ do
        fp <- emptySystemTempFile "empty.txt"
        assertBool "File doesn't exist" =<< doesFileExist fp
        removeFile fp
    , testCase "withSystemTempFile returns absolute path" $ do
        bracket_ (setEnv "TMPDIR" ".") (unsetEnv "TMPDIR") $ do
          withSystemTempFile "temp.txt" $ \fp _ ->
            assertBool "Not absolute" $ isAbsolute fp
    , testCase "withSystemTempDirectory is not interrupted" $ do
        -- this mvar is both a channel to pass the name of the directory
        -- and a signal that we finished creating files and are ready
        -- to be killed
        mvar1 <- newEmptyMVar
        -- this mvar signals that the withSystemTempDirectory function
        -- returned and we can check whether the directory has survived
        mvar2 <- newEmptyMVar
        threadId <- forkIO $
          (withSystemTempDirectory "temp.test." $ \dir -> do
            replicateM_ 100 $ emptyTempFile dir "file.xyz"
            putMVar mvar1 dir
            threadDelay $ 10^6
          ) `finally` (putMVar mvar2 ())
        dir <- readMVar mvar1
        -- start sending exceptions
        replicateM_ 10 $ forkIO $ killThread threadId
        -- wait for the thread to finish
        readMVar mvar2
        -- check whether the directory was successfully removed
        assertBool "Directory was not removed" . not =<< doesDirectoryExist dir
    ]
