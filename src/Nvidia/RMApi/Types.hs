{-|
Module      : Nvidia.RMApi.Types
Description : Types the are available to the RM API.
Copyright   : (c) 2022 2666680 Ontario Inc. O\A Arc Compute
License     : GNU GPL v.2
Maintainer  : michael@arccompute.io
Stability   : experimental
Portability : POSIX

Here is where we store types that have to be exposed to the
RM Api.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Nvidia.RMApi.Types where

import Control.Applicative ((<$>), (<*>))
import Data.Vector.Fixed.Storable (Vec(..))
import GHC.Generics
import Foreign (Storable(..))
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import qualified Data.Vector.Fixed as V

-- Internal imports.
import Common.Types (VoidPtr)
import Nvidia.Errors

{-| NVIDIA requires a version check to ensure that the binary offsets are correct between the
userland program and the kernel module.
-}
data NvVersionCheck = NvVersionCheck
  { cmd :: CUInt                       -- ^ Command override (Allows you to hide the version)
  , reply :: CUInt                     -- ^ Reply if the driver is correct or not.
  , driverVersion :: Vec 64 CChar      -- ^ Where we store the driver string.
  } deriving (Show)

instance Storable NvVersionCheck where
  sizeOf _ = 72
  alignment _ = 8
  peek ptr = NvVersionCheck
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
  poke ptr (NvVersionCheck { .. }) = do
    pokeByteOff ptr 0 cmd
    pokeByteOff ptr 4 reply
    pokeByteOff ptr 8 driverVersion

{-| Provides a default NVIDIA version to be sent to the RM Core.

NOTE: This is currently hardcoded to the 510.47.03 driver version as it is our current testing version.
-}
defaultNvVersionCheck :: Bool          -- ^ If we skip the check or not.
  -> NvVersionCheck                    -- ^ Valid version for version check of the GPU.
defaultNvVersionCheck skip =
  let command = if skip then 0x32 else 0x00
  in NvVersionCheck command 0 $
        V.fromList ([0x35, 0x31, 0x30, 0x2E, 0x34, 0x37, 0x2E, 0x30, 0x33] ++ repeat 0)

{-| Type from the NVIDIA Kernel module for allocating a resource in the kernel module.
-}
data RmAllocRes = RmAllocRes
  { hRoot :: CUInt
  , hObjectParent :: CUInt
  , hObjectNew :: CUInt
  , hClass :: CUInt
  , pAllocParams :: Ptr VoidPtr
  , status :: NvErrorStatus
  } deriving (Show)

instance Storable RmAllocRes where
  sizeOf _ = 32
  alignment _ = 8
  peek ptr = RmAllocRes
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 12
    <*> peekByteOff ptr 16
    <*> peekByteOff ptr 24
  poke ptr (RmAllocRes { .. }) = do
    pokeByteOff ptr 0 hRoot
    pokeByteOff ptr 4 hObjectParent
    pokeByteOff ptr 8 hObjectNew
    pokeByteOff ptr 12 hClass
    pokeByteOff ptr 16 pAllocParams
    pokeByteOff ptr 24 status

{-| Provides a default RmAllocRes structure.
-}
defaultRmAllocRes :: RmAllocRes
defaultRmAllocRes =
  RmAllocRes 0 0 0 0 nullPtr NvOk

{-| Structure to free a device.
-}
data RmFreeRes = RmFreeRes
  { hRoot :: CUInt
  , hObjectParent :: CUInt
  , hObjectOld :: CUInt
  , status :: NvErrorStatus
  } deriving (Show)

instance Storable RmFreeRes where
  sizeOf _ = 16
  alignment _ = 4
  peek ptr = RmFreeRes
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 12
  poke ptr (RmFreeRes { .. }) = do
    pokeByteOff ptr 0 hRoot
    pokeByteOff ptr 4 hObjectParent
    pokeByteOff ptr 8 hObjectOld
    pokeByteOff ptr 12 status

{-| Provides a default RmFreeRes structure.
-}
defaultRmFreeRes :: RmFreeRes
defaultRmFreeRes = RmFreeRes 0 0 0 NvOk

{-| Object sent over an RMControl to attach a set of available GPU ids.
-}
data Nv0000CtrlGpuAttachIdsParams = Nv0000CtrlGpuAttachIdsParams
  { gpuIds :: Vec 32 CUInt
  , failedGpuId :: CUInt
  } deriving (Show)

instance Storable Nv0000CtrlGpuAttachIdsParams where
  sizeOf _ = 132
  alignment _ = 4
  peek ptr = Nv0000CtrlGpuAttachIdsParams
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 128
  poke ptr (Nv0000CtrlGpuAttachIdsParams { .. }) = do
    pokeByteOff ptr 0 gpuIds
    pokeByteOff ptr 128 failedGpuId

{-| Default object for attaching GPU Ids.
-}
defaultNv0000CtrlGpuAttachIdsParams :: Nv0000CtrlGpuAttachIdsParams
defaultNv0000CtrlGpuAttachIdsParams =
  Nv0000CtrlGpuAttachIdsParams (V.replicate 0) 0

{-| Object to get a list of attached GPU Ids.
-}
data Nv0000CtrlGpuGetAttachIdsParams = Nv0000CtrlGpuGetAttachIdsParams
  { gpuIds :: Vec 32 CUInt
  } deriving (Show)

instance Storable Nv0000CtrlGpuGetAttachIdsParams where
  sizeOf _ = 128
  alignment _ = 4
  peek ptr = Nv0000CtrlGpuGetAttachIdsParams
    <$> peekByteOff ptr 0
  poke ptr (Nv0000CtrlGpuGetAttachIdsParams { .. }) =
    pokeByteOff ptr 0 gpuIds

{-| Default object to get the GPU Ids.
-}
defaultNv0000CtrlGpuGetAttachIdsParams :: Nv0000CtrlGpuGetAttachIdsParams
defaultNv0000CtrlGpuGetAttachIdsParams =
  Nv0000CtrlGpuGetAttachIdsParams (V.replicate 0)

{-| Object to detach the GPU Ids.
-}
data Nv0000CtrlGpuDetachIdsParams = Nv0000CtrlGpuDetachIdsParams
  { gpuIds :: Vec 32 CUInt
  } deriving (Show)

instance Storable Nv0000CtrlGpuDetachIdsParams where
  sizeOf _ = 128
  alignment _ = 4
  peek ptr = Nv0000CtrlGpuDetachIdsParams
    <$> peekByteOff ptr 0
  poke ptr (Nv0000CtrlGpuDetachIdsParams { .. }) =
    pokeByteOff ptr 0 gpuIds

{-| Default object to detach all the GPU Ids.
-}
defaultNv0000CtrlGpuDetachIdsParams :: Nv0000CtrlGpuDetachIdsParams
defaultNv0000CtrlGpuDetachIdsParams =
  Nv0000CtrlGpuDetachIdsParams (V.replicate 0)

{-| Object to send a control message to the NVIDIA kernel module.
-}
data RmControlRes = RmControlRes
  { client :: CUInt
  , object :: CUInt
  , cmd :: CUInt
  , flags :: CUInt
  , params :: Ptr VoidPtr
  , paramsSize :: CUInt
  , status :: NvErrorStatus
  } deriving (Show)

instance Storable RmControlRes where
  sizeOf _ = 32
  alignment _ = 8
  peek ptr = RmControlRes
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 12
    <*> peekByteOff ptr 16
    <*> peekByteOff ptr 24
    <*> peekByteOff ptr 28
  poke ptr (RmControlRes { .. }) = do
    pokeByteOff ptr 0 client
    pokeByteOff ptr 4 object
    pokeByteOff ptr 8 cmd
    pokeByteOff ptr 12 flags
    pokeByteOff ptr 16 params
    pokeByteOff ptr 24 paramsSize
    pokeByteOff ptr 28 status

{-| Command to send an RM Control call.
-}
defaultRmControlRes :: RmControlRes
defaultRmControlRes =
  RmControlRes 0 0 0 0 nullPtr 0 NvOk

{-| Helper alias for handling get id info.
-}
type SzName = Vec 128 CChar

{-| Gets the id info from the given GPU.
-}
data Nv0000CtrlGpuGetIdInfoParams = Nv0000CtrlGpuGetIdInfoParams
  { gpuId :: CUInt
  , gpuFlags :: CUInt
  , deviceInstance :: CUInt
  , subDeviceInstance :: CUInt
  , szName :: Ptr SzName
  , sliStatus :: CUInt
  , boardId :: CUInt
  , gpuInstance :: CUInt
  , numaId :: CUInt
  } deriving (Show)

instance Storable Nv0000CtrlGpuGetIdInfoParams where
  sizeOf _ = 40
  alignment _ = 8
  peek ptr = Nv0000CtrlGpuGetIdInfoParams
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 12
    <*> peekByteOff ptr 16
    <*> peekByteOff ptr 24
    <*> peekByteOff ptr 28
    <*> peekByteOff ptr 32
    <*> peekByteOff ptr 36
  poke ptr (Nv0000CtrlGpuGetIdInfoParams { .. }) = do
    pokeByteOff ptr 0 gpuId
    pokeByteOff ptr 4 gpuFlags
    pokeByteOff ptr 8 deviceInstance
    pokeByteOff ptr 12 subDeviceInstance
    pokeByteOff ptr 16 szName
    pokeByteOff ptr 24 sliStatus
    pokeByteOff ptr 28 boardId
    pokeByteOff ptr 32 gpuInstance
    pokeByteOff ptr 36 numaId

{-| Default values for getting the id info from the system.
-}
defaultNv0000CtrlGpuGetIdInfoParams :: Nv0000CtrlGpuGetIdInfoParams
defaultNv0000CtrlGpuGetIdInfoParams =
  Nv0000CtrlGpuGetIdInfoParams 0 0 0 0 nullPtr 0 0 0 0

{-| Gets the list of probed gpu ids.
-}
data Nv0000CtrlGpuGetProbedIdsParams = Nv0000CtrlGpuGetProbedIdsParams
  { gpuIds :: Vec 32 CUInt
  , excludedGpuIds :: Vec 32 CUInt
  } deriving (Show)

instance Storable Nv0000CtrlGpuGetProbedIdsParams where
  sizeOf _ = 256
  alignment _ = 8
  peek ptr = Nv0000CtrlGpuGetProbedIdsParams
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 128
  poke ptr (Nv0000CtrlGpuGetProbedIdsParams { .. }) = do
    pokeByteOff ptr 0 gpuIds
    pokeByteOff ptr 128 excludedGpuIds

{-| Default object to get the probed ids.
-}
defaultNv0000CtrlGpuGetProbedIdsParams :: Nv0000CtrlGpuGetProbedIdsParams
defaultNv0000CtrlGpuGetProbedIdsParams =
  Nv0000CtrlGpuGetProbedIdsParams (V.replicate 0) (V.replicate 0)

{-| Object to get base PCI info.
-}
data Nv0000CtrlGpuGetPciInfoParams = Nv0000CtrlGpuGetPciInfoParams
  { gpuId :: CUInt
  , domain :: CUInt
  , bus :: CUShort
  , slot :: CUShort
  } deriving (Show)

instance Storable Nv0000CtrlGpuGetPciInfoParams where
  sizeOf _ = 12
  alignment _ = 4
  peek ptr = Nv0000CtrlGpuGetPciInfoParams
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 10
  poke ptr (Nv0000CtrlGpuGetPciInfoParams { .. }) = do
    pokeByteOff ptr 0 gpuId
    pokeByteOff ptr 4 domain
    pokeByteOff ptr 8 bus
    pokeByteOff ptr 10 slot

{-| Default object to get Base PCI info.
-}
defaultNv0000CtrlGpuGetPciInfoParams :: Nv0000CtrlGpuGetPciInfoParams
defaultNv0000CtrlGpuGetPciInfoParams =
  Nv0000CtrlGpuGetPciInfoParams 0 0 0 0

{-| Object to allocate a Nv0080 object.
-}
data Nv0080AllocParams = Nv0080AllocParams
  { deviceId :: CUInt
  , hClientShare :: CUInt
  , hTargetClient :: CUInt
  , hTargetDevice :: CUInt
  , flags :: CUInt
  , vaSpaceSize :: CULong
  , vaStartInternal :: CULong
  , vaLimitInternal :: CULong
  , vaMode :: CUInt
  } deriving (Show)

instance Storable Nv0080AllocParams where
  sizeOf _ = 64
  alignment _ = 8
  peek ptr = Nv0080AllocParams
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 12
    <*> peekByteOff ptr 16
    <*> peekByteOff ptr 24
    <*> peekByteOff ptr 32
    <*> peekByteOff ptr 40
    <*> peekByteOff ptr 48
  poke ptr (Nv0080AllocParams { .. }) = do
    pokeByteOff ptr 0 deviceId
    pokeByteOff ptr 4 hClientShare
    pokeByteOff ptr 8 hTargetClient
    pokeByteOff ptr 12 hTargetDevice
    pokeByteOff ptr 16 flags
    pokeByteOff ptr 24 vaSpaceSize
    pokeByteOff ptr 32 vaStartInternal
    pokeByteOff ptr 40 vaLimitInternal
    pokeByteOff ptr 48 vaMode

{-| Default object to allocate a Nv0080 object.
-}
defaultNv0080AllocParams :: Nv0080AllocParams
defaultNv0080AllocParams =
  Nv0080AllocParams 0 0 0 0 0 0 0 0 0

{-| Type alias to help create a sub device allocation.
-}
type Nv2080AllocParams = CUInt

{-| Data to get PCI information.
-}
data BusGetPciInfo = BusGetPciInfo
  { devId :: CUInt
  , subDevId :: CUInt
  , revisionId :: CUInt
  , extId :: CUInt
  } deriving (Show)

instance Storable BusGetPciInfo where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = BusGetPciInfo
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 12
  poke ptr (BusGetPciInfo { .. }) = do
    pokeByteOff ptr 0 devId
    pokeByteOff ptr 4 subDevId
    pokeByteOff ptr 8 revisionId
    pokeByteOff ptr 12 extId

{-| Gets PCI Bus information.
-}
defaultBusGetPciInfo :: BusGetPciInfo
defaultBusGetPciInfo = BusGetPciInfo 0 0 0 0

{-| vGPU configuration object.
-}
data RmVgpuConfig = RmVgpuConfig
  { discardPrevious :: CUInt
  , vgpuTypeNumber :: CUInt
  , vgpuName :: Vec 32 CChar
  , vgpuClass :: Vec 32 CChar
  , sign :: Vec 128 CChar
  , pact :: Vec 128 CChar
  , maxInstances :: CUInt
  , numHeads :: CUInt
  , maxResX :: CUInt
  , maxResY :: CUInt
  , maxPixel :: CUInt
  , frlConfig :: CUInt
  , cuda :: CUInt
  , eccSupported :: CUInt
  , gpuInstanceSize :: CUInt
  , multiMdev :: CUInt
  , encCap :: CULong
  , vdevId :: CULong
  , pdevId :: CULong
  , fbLen :: CULong
  , mappableVideoSize :: CULong
  , fbRes :: CULong
  , bar1Len :: CULong
  , frlEnable :: CUInt
  , vgpuExtraParams :: Vec 1027 CUInt
  }

instance Storable RmVgpuConfig where
  sizeOf _ = 0x11C0
  alignment _ = 8
  peek ptr = RmVgpuConfig
    <$> peekByteOff ptr 0   -- discardPrevious 4 + (4 padding)
    <*> peekByteOff ptr 8   -- vgpuTypeNumber 4
    <*> peekByteOff ptr 12  -- vgpuName 32
    <*> peekByteOff ptr 44  -- vgpuClass 32
    <*> peekByteOff ptr 76  -- sign 128
    <*> peekByteOff ptr 204 -- pact 128 + (4 padding)
    <*> peekByteOff ptr 336 -- maxInstances 4
    <*> peekByteOff ptr 340 -- numHeads 4
    <*> peekByteOff ptr 344 -- maxResX 4
    <*> peekByteOff ptr 348 -- maxResY 4
    <*> peekByteOff ptr 352 -- maxPixel 4
    <*> peekByteOff ptr 356 -- frlConfig 4
    <*> peekByteOff ptr 360 -- cuda 4
    <*> peekByteOff ptr 364 -- eccSupported 4
    <*> peekByteOff ptr 368 -- gpuInstanceSize 4
    <*> peekByteOff ptr 372 -- multiMdev 4
    <*> peekByteOff ptr 376 -- encCap 8
    <*> peekByteOff ptr 384 -- vdevId 8
    <*> peekByteOff ptr 392 -- pdevId 8
    <*> peekByteOff ptr 400 -- fbLen 8
    <*> peekByteOff ptr 408 -- mappableVideoSize 8
    <*> peekByteOff ptr 416 -- fbRes 8
    <*> peekByteOff ptr 424 -- bar1Len 8
    <*> peekByteOff ptr 432 -- frlEnable 4
    <*> peekByteOff ptr 436 -- vgpuExtraParams 4108
  poke ptr (RmVgpuConfig { .. }) = do
    pokeByteOff ptr 0 discardPrevious
    pokeByteOff ptr 8 vgpuTypeNumber
    pokeByteOff ptr 12 vgpuName
    pokeByteOff ptr 44 vgpuClass
    pokeByteOff ptr 76 sign
    pokeByteOff ptr 204 pact
    pokeByteOff ptr 336 maxInstances
    pokeByteOff ptr 340 numHeads
    pokeByteOff ptr 344 maxResX
    pokeByteOff ptr 348 maxResY
    pokeByteOff ptr 352 maxPixel
    pokeByteOff ptr 356 frlConfig
    pokeByteOff ptr 360 cuda
    pokeByteOff ptr 364 eccSupported
    pokeByteOff ptr 368 gpuInstanceSize
    pokeByteOff ptr 372 multiMdev
    pokeByteOff ptr 376 encCap
    pokeByteOff ptr 384 vdevId
    pokeByteOff ptr 392 pdevId
    pokeByteOff ptr 400 fbLen
    pokeByteOff ptr 408 mappableVideoSize
    pokeByteOff ptr 416 fbRes
    pokeByteOff ptr 424 bar1Len
    pokeByteOff ptr 432 frlEnable
    pokeByteOff ptr 436 vgpuExtraParams

{-| Default vgpu configuration object.
-}
defaultRmVgpuConfig :: RmVgpuConfig
defaultRmVgpuConfig =
  RmVgpuConfig
    0
    0
    (V.replicate 0)
    (V.fromList ((map castCharToCChar "Compute") ++ repeat 0))
    (V.fromList [ 0x47, 0xab, 0x8d, 0x39, 0xb3, 0xaf, 0xd0, 0x2c, 0x79, 0x6f, 0xd0, 0xc7, 0x7a, 0x78, 0x84, 0x68
                , 0xf0, 0x9b, 0x69, 0xe8, 0xb6, 0xc2, 0xc5, 0x05, 0x59, 0x97, 0xf2, 0x0f, 0x77, 0x3a, 0x94, 0x91
                , 0x92, 0x56, 0x2d, 0xf0, 0x4f, 0xae, 0xa6, 0x4d, 0xcc, 0x51, 0x32, 0x17, 0xe5, 0xda, 0xf0, 0x94
                , 0x42, 0x93, 0x51, 0x05, 0x49, 0xe9, 0x61, 0xfd, 0x22, 0x24, 0x6c, 0x8f, 0x88, 0xe3, 0x16, 0x63
                , 0x91, 0x04, 0x20, 0x6a, 0xea, 0x27, 0xc4, 0xe7, 0x11, 0xfc, 0x88, 0x24, 0xb9, 0xaa, 0x1b, 0x85
                , 0xc5, 0x1a, 0x48, 0x7f, 0x99, 0xf4, 0x8e, 0xda, 0x55, 0x2b, 0x4a, 0xfe, 0x48, 0x79, 0x75, 0x78
                , 0x48, 0x16, 0x02, 0x0f, 0x22, 0xc2, 0x9d, 0x20, 0xfa, 0xbb, 0x21, 0x39, 0x56, 0x78, 0xd8, 0x80
                , 0x96, 0x5c, 0x5c, 0xe4, 0x7c, 0xad, 0x87, 0x24, 0x20, 0x70, 0xad, 0x63, 0x84, 0x96, 0x92, 0x3f
                ])
    (V.fromList ((map castCharToCChar "NVIDIA-vComputeServer,9.0;Quadro-Virtual-DWS,5.0") ++ repeat 0))
    0
    0
    0
    0
    0
    0
    1
    0
    0
    0
    0
    0
    0
    0
    0
    0
    0
    0
    (V.replicate 0)
