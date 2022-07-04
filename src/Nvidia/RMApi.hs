{-|
Module      : Nvidia.RMApi
Description : Base implementation for nvidia interaction for mdev-gpu.
Copyright   : (c) 2022 2666680 Ontario Inc. O\A Arc Compute
License     : GNU GPL v.2
Maintainer  : michael@arccompute.io
Stability   : experimental
Portability : POSIX

Implementation of the RMApi as required by mdev-gpu.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Nvidia.RMApi
  ( module Nvidia.RMApi.Types
  , module Nvidia.RMApi
  ) where

import Data.Bits
import Foreign (Storable(..))
import Foreign.C.Types
import Foreign.Ptr
import System.Posix.Types
import System.Posix.IOCtl
import Text.Printf

-- Internal imports.
import Nvidia.Errors
import Nvidia.RMApi.IoctlCodes
import Nvidia.RMApi.Types

{-| Composes a normal object id.
-}
composeNormalDevId :: CUInt -- ^ Root id to use.
  -> CUInt                  -- ^ GPU id to use.
  -> CUInt                  -- ^ Handle id to use.
  -> CUInt                  -- ^ Resulting object id to connect to the system.
composeNormalDevId root gpu handle =
  ((root + gpu) `shift` 8) .|. (handle .&. ((1 `shift` 8) - 1))

{-| Version check to ensure we are capable of running our code.
-}
versionCheck :: Fd      -- ^ File descriptor to check.
  -> Bool               -- ^ If we skip the version check or not.
  -> IO ()              -- ^ Runs the version check command.
versionCheck fd skip =
  ioctl_ fd NvIOCVersionCheck (defaultNvVersionCheck skip)

{-| Allocates a resource in NVIDIA Kernel module.
-}
allocRes :: Fd          -- ^ File descriptor to allocate the resource on.
  -> RmAllocRes         -- ^ Resource allocation type.
  -> IO (RmAllocRes)    -- ^ Allocated resource.
allocRes fd alloc = do
  ret <- ioctl fd NvIOCAllocRes alloc
  let s = status (ret :: RmAllocRes)
  if s /= NvOk
    then do
        printf
          "Failed to create resource:\n\tclient: 0x%X\n\tparent: 0x%.8X\n\tobject: 0x%.8X\n\tclass: 0x%.8X\n\tstatus: %s\n"
          (fromIntegral (hRoot (ret :: RmAllocRes)) :: Integer)
          (fromIntegral (hObjectParent (ret :: RmAllocRes)) :: Integer)
          (fromIntegral (hObjectNew (ret :: RmAllocRes)) :: Integer)
          (fromIntegral (hClass (ret :: RmAllocRes)) :: Integer)
          (show (status (ret :: RmAllocRes)))
        error "Status failed"
    else
        printf "Created resource with id: 0x%.8X\n" (fromIntegral (hObjectNew (ret :: RmAllocRes)) :: Integer) >>
        return ret

{-| Frees a resource allocated in the NVIDIA Kernel module.
-}
freeRes :: Fd           -- ^ File descriptor to allocate the resource on.
  -> RmFreeRes          -- ^ Resource deallocation type.
  -> IO (RmFreeRes)     -- ^ Deallocated resource.
freeRes fd free = do
  ret <- ioctl fd NvIOCFreeRes free
  let s = status (ret :: RmFreeRes)
  if s /= NvOk
    then do
        putStrLn $ "Failed to free: " ++ (show ret)
        error "Status failed"
    else
        printf "Freed resource with id: 0x%.8X\n" (fromIntegral (hObjectOld (ret :: RmFreeRes)) :: Integer) >>
        return ret

{-| This command sends a command to the NVIDIA Kernel module.
-}
ctrlRes :: (Storable d)
  => Fd                 -- ^ File descriptor to run the control event on.
  -> CUInt              -- ^ Client Id.
  -> CUInt              -- ^ Object Id.
  -> CUInt              -- ^ Command to run.
  -> Ptr d              -- ^ Pointer to control parameter.
  -> IO (Ptr d)         -- ^ Modified pointer paramter
ctrlRes fd clientId objectId command params = do
  paramDeref <- peek params
  let paramSize = fromIntegral $ sizeOf paramDeref
      newPtr = castPtr params
      cmdStruct = defaultRmControlRes { client = clientId
                                      , object = objectId
                                      , cmd = command
                                      , params = newPtr
                                      , paramsSize = paramSize
                                      } :: RmControlRes
  ret <- ioctl fd NvIOCControlRes cmdStruct
  let s = status (ret :: RmControlRes)
  if s /= NvOk
    then do
        putStrLn $ "Failed to control: " ++ (show ret)
        error "Status failed"
    else
        return params

{-| This command sends a command to the NVIDIA Kernel module, but does not
respond with a return value.
-}
ctrlRes_ :: (Storable d) =>
  Fd                    -- ^ File descriptor to run the control event on.
  -> CUInt              -- ^ Client Id.
  -> CUInt              -- ^ Object Id.
  -> CUInt              -- ^ Command to run.
  -> Ptr d              -- ^ Pointer to control parameter.
  -> IO (Ptr d)         -- ^ Modified pointer paramter
ctrlRes_ fd clientId objectId command params = do
  paramDeref <- peek params
  let paramSize = fromIntegral $ sizeOf paramDeref
      newPtr = castPtr params
      cmdStruct = defaultRmControlRes { client = clientId
                                      , object = objectId
                                      , cmd = command
                                      , params = newPtr
                                      , paramsSize = paramSize
                                      } :: RmControlRes
  ret <- ioctl fd NvIOCControlRes cmdStruct
  let s = status (ret :: RmControlRes)
  if s /= NvOk
    then do
        putStrLn $ "Failed to control: " ++ (show ret)
        error "Status failed"
    else
        return params

{-| This command sends a command with no parameters to the NVIDIA Kernel module.
-}
ctrlRes' :: Fd                 -- ^ File descriptor to run the control event on.
  -> CUInt                     -- ^ Client Id.
  -> CUInt                     -- ^ Object Id.
  -> CUInt                     -- ^ Command to run.
  -> IO ()                     -- ^ Command only executed for side effects
ctrlRes' fd clientId objectId command = do
  let cmdStruct = defaultRmControlRes { client = clientId
                                      , object = objectId
                                      , cmd = command
                                      } :: RmControlRes
  ret <- ioctl fd NvIOCControlRes cmdStruct
  let s = status (ret :: RmControlRes)
  if s /= NvOk
    then do
        putStrLn $ "Failed to control: " ++ (show ret)
        error "Status failed"
    else
        return ()
