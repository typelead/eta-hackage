{-# LANGUAGE CPP #-}
-- | Functions to create temporary files and directories.
--
-- Most functions come in two flavours: those that create files/directories
-- under the system standard temporary directory and those that use the
-- user-supplied directory.
--
-- The functions that create files/directories under the system standard
-- temporary directory will return canonical absolute paths (see
-- 'getCanonicalTemporaryDirectory'). The functions use the user-supplied
-- directory do not canonicalize the returned path.
--
-- The action inside 'withTempFile' or 'withTempDirectory' is allowed to
-- remove the temporary file/directory if it needs to.
--
-- == Templates and file names
--
-- The treatment of templates differs somewhat for files vs directories.
--
-- For files, the template has form @name.ext@, and a random number will be
-- placed between between the name and the extension to yield a unique file
-- name, e.g.  @name1804289383846930886.ext@.
--
-- For directories, no extension is recognized, so a number will be simply
-- appended to the end of the template. Moreover, the number will be
-- smaller, as it is derived from the current process's PID
-- (but the result is still a unique directory name). So, for instance,
-- the directory template @dir@ may result in a directory named @dir30112@.
module System.IO.Temp (
    withSystemTempFile, withSystemTempDirectory,
    withTempFile, withTempDirectory,
    openNewBinaryFile,
    createTempDirectory,
    writeTempFile, writeSystemTempFile,
    emptyTempFile, emptySystemTempFile,
    -- * Re-exports from System.IO
    openTempFile,
    openBinaryTempFile,
    -- * Auxiliary functions
    getCanonicalTemporaryDirectory
  ) where

import qualified Control.Monad.Catch as MC

import Control.Monad.IO.Class
import System.Directory
import System.IO (Handle, hClose, openTempFile, openBinaryTempFile,
       openBinaryTempFileWithDefaultPermissions, hPutStr)
import System.IO.Error        (isAlreadyExistsError)
import System.Posix.Internals (c_getpid)
import System.FilePath        ((</>))

-- | Create, open, and use a temporary file in the system standard temporary directory.
--
-- The temp file is deleted after use.
--
-- Behaves exactly the same as 'withTempFile', except that the parent temporary directory
-- will be that returned by 'getCanonicalTemporaryDirectory'.
withSystemTempFile :: (MonadIO m, MC.MonadMask m) =>
                      String   -- ^ File name template
                   -> (FilePath -> Handle -> m a) -- ^ Callback that can use the file
                   -> m a
withSystemTempFile template action = liftIO getCanonicalTemporaryDirectory >>= \tmpDir -> withTempFile tmpDir template action

-- | Create and use a temporary directory in the system standard temporary directory.
--
-- Behaves exactly the same as 'withTempDirectory', except that the parent temporary directory
-- will be that returned by 'getCanonicalTemporaryDirectory'.
withSystemTempDirectory :: (MonadIO m, MC.MonadMask m) =>
                           String   -- ^ Directory name template
                        -> (FilePath -> m a) -- ^ Callback that can use the directory
                        -> m a
withSystemTempDirectory template action = liftIO getCanonicalTemporaryDirectory >>= \tmpDir -> withTempDirectory tmpDir template action


-- | Create, open, and use a temporary file in the given directory.
--
-- The temp file is deleted after use.
withTempFile :: (MonadIO m, MC.MonadMask m) =>
                FilePath -- ^ Parent directory to create the file in
             -> String   -- ^ File name template
             -> (FilePath -> Handle -> m a) -- ^ Callback that can use the file
             -> m a
withTempFile tmpDir template action =
  MC.bracket
    (liftIO (openTempFile tmpDir template))
    (\(name, handle) -> liftIO (hClose handle >> ignoringIOErrors (removeFile name)))
    (uncurry action)

-- | Create and use a temporary directory inside the given directory.
--
-- The directory is deleted after use.
withTempDirectory :: (MC.MonadMask m, MonadIO m) =>
                     FilePath -- ^ Parent directory to create the directory in
                  -> String   -- ^ Directory name template
                  -> (FilePath -> m a) -- ^ Callback that can use the directory
                  -> m a
withTempDirectory targetDir template =
  MC.bracket
    (liftIO (createTempDirectory targetDir template))
    (liftIO . ignoringIOErrors . removeDirectoryRecursive)

-- | Create a unique new file, write (text mode) a given data string to it,
--   and close the handle again. The file will not be deleted automatically,
--   and only the current user will have permission to access the file.
--
-- @since 1.2.1
writeTempFile :: FilePath    -- ^ Parent directory to create the file in
              -> String      -- ^ File name template
              -> String      -- ^ Data to store in the file
              -> IO FilePath -- ^ Path to the (written and closed) file
writeTempFile targetDir template content = MC.bracket
    (openTempFile targetDir template)
    (\(_, handle) -> hClose handle)
    (\(filePath, handle) -> hPutStr handle content >> return filePath)

-- | Like 'writeTempFile', but use the system directory for temporary files.
--
-- @since 1.2.1
writeSystemTempFile :: String      -- ^ File name template
                    -> String      -- ^ Data to store in the file
                    -> IO FilePath -- ^ Path to the (written and closed) file
writeSystemTempFile template content
    = getCanonicalTemporaryDirectory >>= \tmpDir -> writeTempFile tmpDir template content

-- | Create a unique new empty file. (Equivalent to 'writeTempFile' with empty data string.)
--   This is useful if the actual content is provided by an external process.
--
-- @since 1.2.1
emptyTempFile :: FilePath    -- ^ Parent directory to create the file in
              -> String      -- ^ File name template
              -> IO FilePath -- ^ Path to the (written and closed) file
emptyTempFile targetDir template = MC.bracket
    (openTempFile targetDir template)
    (\(_, handle) -> hClose handle)
    (\(filePath, _) -> return filePath)

-- | Like 'emptyTempFile', but use the system directory for temporary files.
--
-- @since 1.2.1
emptySystemTempFile :: String      -- ^ File name template
                    -> IO FilePath -- ^ Path to the (written and closed) file
emptySystemTempFile template
    = getCanonicalTemporaryDirectory >>= \tmpDir -> emptyTempFile tmpDir template


ignoringIOErrors :: MC.MonadCatch m => m () -> m ()
ignoringIOErrors ioe = ioe `MC.catch` (\e -> const (return ()) (e :: IOError))

-- | Like 'openBinaryTempFile', but uses 666 rather than 600 for the
-- permissions.
--
-- Equivalent to 'openBinaryTempFileWithDefaultPermissions'.
openNewBinaryFile :: FilePath -> String -> IO (FilePath, Handle)
openNewBinaryFile = openBinaryTempFileWithDefaultPermissions

-- | Create a temporary directory.
createTempDirectory
  :: FilePath -- ^ Parent directory to create the directory in
  -> String -- ^ Directory name template
  -> IO FilePath
createTempDirectory dir template = do
  pid <- c_getpid
  findTempName pid
  where
    findTempName x = do
      let dirpath = dir </> template ++ show x
      r <- MC.try $ mkPrivateDir dirpath
      case r of
        Right _ -> return dirpath
        Left  e | isAlreadyExistsError e -> findTempName (x+1)
                | otherwise              -> ioError e

mkPrivateDir :: String -> IO ()
mkPrivateDir s = createDirectory s

-- | Return the absolute and canonical path to the system temporary
-- directory.
--
-- >>> setCurrentDirectory "/home/feuerbach/"
-- >>> setEnv "TMPDIR" "."
-- >>> getTemporaryDirectory
-- "."
-- >>> getCanonicalTemporaryDirectory
-- "/home/feuerbach"
getCanonicalTemporaryDirectory :: IO FilePath
getCanonicalTemporaryDirectory = getTemporaryDirectory >>= canonicalizePath
