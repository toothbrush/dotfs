{-
 - Everything here is evil by definition.
 -}
module Core.Constants where

import Core.Datatypes

import System.Fuse
import System.Posix.Files

-- TODO: there must be a system call for this.
funionGetFileSystemStats :: DirPair -> String -> IO (Either Errno FileSystemStats)
funionGetFileSystemStats dp str = -- use stats from home dp
  return $ Right FileSystemStats
    { fsStatBlockSize  = 512
    , fsStatBlockCount = 1000
    , fsStatBlocksFree = 1000
    , fsStatBlocksAvailable = 1000
    , fsStatFileCount  = 5      -- IS THIS CORRECT?
    , fsStatFilesFree  = 10     -- WHAT IS THIS?
    , fsStatMaxNameLength = 255 -- SEEMS SMALL?
    }

-- TODO: real directory stat, not this fake info.
-- why was this even necessary?
dirStat = FileStat {
    statEntryType = Directory
  , statFileMode = foldr1 unionFileModes
                     [ ownerReadMode
                 --    , ownerWriteMode
                     , ownerExecuteMode
                 --    , groupReadMode
                 --    , groupExecuteMode
                 --    , otherReadMode
                 --    , otherExecuteMode
                 --    , groupWriteMode
                 --    , otherWriteMode
                     ]
  , statLinkCount = 5
  , statFileOwner = 1000
  , statFileGroup = 1000
  , statSpecialDeviceID = 0
  , statFileSize  = 4096
  , statBlocks    = 1
  , statAccessTime= 0
  , statModificationTime = 0
  , statStatusChangeTime = 0
  }


