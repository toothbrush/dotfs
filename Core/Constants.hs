{-
 - Everything here is evil by definition.
 -}
{-# LANGUAGE Haskell98 #-}
module Core.Constants where

import Core.Datatypes

import System.Fuse
import System.Posix.Files

-- there is a system call for this.
-- but never mind, since we have a read-only file
-- system, we don't need that.
dotfsGetFileSystemStats :: Conf -> String -> IO (Either Errno FileSystemStats)
dotfsGetFileSystemStats dp str = -- use stats from home dp
  return $ Right FileSystemStats
    { fsStatBlockSize  = 512
    , fsStatBlockCount = 1000
    , fsStatBlocksFree = 0
    , fsStatBlocksAvailable = 0
    , fsStatFileCount  = 5      -- IS THIS CORRECT?
    , fsStatFilesFree  = 10     -- WHAT IS THIS?
    , fsStatMaxNameLength = 255 -- SEEMS SMALL?
    }

-- this is dummy data, and turns out to never be
-- shown, even when doing `ls -la`
dirStat = FileStat {
    statEntryType = Directory
  , statFileMode  = ownerReadMode
  , statLinkCount = 5
  , statFileOwner = 666
  , statFileGroup = 666
  , statSpecialDeviceID = 0
  , statFileSize  = 4096
  , statBlocks    = 1
  , statAccessTime= 0
  , statModificationTime = 0
  , statStatusChangeTime = 0
  }


