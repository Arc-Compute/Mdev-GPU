{-|
Module      : Nvidia.Device
Description : Code to interact with a low level implementation of the nvidia device.
Copyright   : (c) 2022 2666680 Ontario Inc. O\A Arc Compute
License     : GNU GPL v.2
Maintainer  : michael@arccompute.io
Stability   : experimental
Portability : POSIX

Here we hold all the internal code for low level nvidia device control. Such as opening and closing nvidia device
files.
-}
{-# LANGUAGE OverloadedStrings #-}
module Nvidia.Device
  ( openNvDev
  ) where

import Data.Bits
import Data.Maybe
import Foreign.C.Types
import System.Directory
import System.Posix.IO
import System.Posix.Files
import System.Posix.Types

-- Internal dependancies
import Common.Device

{-| Internal function to create the device name, and ensure the file exists.
-}
createNvDevName :: CUInt  -- ^ Minor number to open.
  -> IO (Maybe String)    -- ^ Nvidia device name.
createNvDevName minor =
  let suffix = if minor == 255 then "ctl" else show minor
      filename = "/dev/nvidia" ++ suffix
  in do
    exists <- fileExist filename
    driverLoaded <- doesFileExist "/proc/driver/nvidia/params"
    if exists
      then return (Just filename)
      else if driverLoaded
              then do
                putStrLn $ "Creating the filename: " ++ filename
                charDev <- getCDevMajor "nvidia-frontend"
                mode <- readFile "/proc/driver/nvidia/params" >>= return . (getParam "DeviceFileMode") . lines
                if isJust mode && isJust charDev
                  then createDevice
                        filename
                        (fromIntegral ((fromIntegral (fromJust mode) :: CUInt) .|. 0x00002000) :: CMode)
                        (createCDevId (fromIntegral (fromJust charDev)) (fromIntegral minor)) >>
                       return (Just filename)
                  else return Nothing
              else return Nothing

{-| Opens a nvidia device, if the device does not exist, this function creates it.
-}
openNvDev :: CUInt -- ^ Minor number for a \/dev\/nvidia%d number (255 == ctl)
  -> IO (Maybe Fd) -- ^ If we are able to load a nvidia device.
openNvDev minor =
  createNvDevName minor >>=
  \s ->
    (if isNothing s
        then return Nothing
        else openFd (fromJust s) ReadWrite Nothing defaultFileFlags >>= return . Just)
