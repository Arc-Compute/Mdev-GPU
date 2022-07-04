{-|
Module      : Common.Device
Description : Common functions to interact with \/dev files.
Copyright   : (c) 2022 2666680 Ontario Inc. O\A Arc Compute
License     : GNU GPL v.2
Maintainer  : michael@arccompute.io
Stability   : experimental
Portability : POSIX

This file contains a set of common functions which can be
used to ensure that devices are correctly created when necessary.
-}
module Common.Device
  ( createCDevId
  , getCDevMajor
  , getParam
  ) where

import Data.Bits
import Foreign.C.Types
import System.Posix.Types

{-| Internal function for getting the device id from the \/proc\/devices
file.
-}
getMajor :: String -- ^ Device to search for
  -> [String]      -- ^ All lines to parse.
  -> Maybe Integer -- ^ If the device is in the list get the value.
getMajor _ [] = Nothing
getMajor chartype (curr:remaining)
  | chartype == (last $ words curr) = Just (read (head $ words curr) :: Integer)
  | otherwise = getMajor chartype remaining

{-| Gets a param from a driver param file.
-}
getParam :: String -- ^ Param to get.
  -> [String]      -- ^ All lines to parse.
  -> Maybe Integer -- ^ If the param is in the list, get the value.
getParam _ [] = Nothing
getParam param (curr:remaining)
  | param == (init $ head $ words curr) = Just (read (last $ words curr) :: Integer)
  | otherwise = getParam param remaining

{-| Creates a device id for if it is necessary to
mknod and create a device that does not exist.
-}
createCDevId :: CULong -- ^ Major of the chardev to create.
  -> CULong            -- ^ Minor of the chardev to create.
  -> CDev              -- ^ Chardev's device id for creation.
createCDevId major minor =
  fromIntegral $
        ((major .&. 0xFFFFF000) `shift` 32) .|.
        ((major .&. 0x00000FFF) `shift` 8) .|.
        ((minor .&. 0xFFFFFF00) `shift` 12) .|.
        (minor .&. 0x000000FF)

{-| Gets the Major from the \/proc\/devices file.

=== __Examples__

This is an example to get the NVIDIA Chardev from the
system:

>>> getCDevMajor "nvidia-frontend"
195
-}
getCDevMajor :: String   -- ^ Type of chardev to create.
  -> IO (Maybe Integer)  -- ^ If the chardev is creatable, than returns
                         --   the major of the chardev.
getCDevMajor chartype =
  readFile "/proc/devices" >>= return . (getMajor chartype) . lines
