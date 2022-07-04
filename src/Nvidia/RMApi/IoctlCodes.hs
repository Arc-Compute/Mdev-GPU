{-|
Module      : Nvidia.RMApi.IoctlCodes
Description : Different possible IOCtl commands that we need to allow.
Copyright   : (c) 2022 2666680 Ontario Inc. O\A Arc Compute
License     : GNU GPL v.2
Maintainer  : michael@arccompute.io
Stability   : experimental
Portability : POSIX

Different possible IOCtls that can be called at different times.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Nvidia.RMApi.IoctlCodes where

import Foreign (Storable(..))
import System.Posix.IOCtl

-- Internal imports.
import Nvidia.RMApi.Types

{-| This type is a holder to ensure we can perform a version check over
ioctl.
-}
data NvIOCVersionCheck = NvIOCVersionCheck

{-| This type is a holder to ensure we can allocate devices.
-}
data NvIOCAllocRes = NvIOCAllocRes

{-| This type is a holder to ensure we can free resources.
-}
data NvIOCFreeRes = NvIOCFreeRes

{-| Control command for a resource.
-}
data NvIOCControlRes = NvIOCControlRes

instance IOControl NvIOCVersionCheck NvVersionCheck where
  ioctlReq _ = 0xC04846D2

instance IOControl NvIOCAllocRes RmAllocRes where
  ioctlReq _ = 0xC020462B

instance IOControl NvIOCFreeRes RmFreeRes where
  ioctlReq _ = 0xC0104629

instance IOControl NvIOCControlRes RmControlRes where
  ioctlReq _ = 0xC020462A
