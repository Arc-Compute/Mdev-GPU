{-|
Module      : Nvidia.MDev
Description : NVIDIA MDev interface implementation.
Copyright   : (c) 2022 2666680 Ontario Inc. O\A Arc Compute
License     : GNU GPL v.2
Maintainer  : michael@arccompute.io
Stability   : experimental
Portability : POSIX

This file creates mdev interface commands which can be used to provide a better communication with
specifically the NVIDIA Kernel module.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Nvidia.MDev
  ( openNvMDevInterface
  , closeNvMDevInterface
  , createNvMDevs
  ) where

import Data.Bits
import Data.Maybe
import Foreign (Storable(..), Word32)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Utils
import System.Posix.IO
import System.Posix.Types
import Text.Printf

import qualified Data.Vector.Fixed as V

-- Internal imports.
import Common.Types
import Nvidia.Device
import Nvidia.RMApi

{-| Function to create a list of MDevs on a requested GPU.
-}
createNvMDevs :: MediatedPhysicalGpu       -- ^ Device to create mdevs on.
  -> RequestedGpu                          -- ^ Requested GPU to create the mdevs.
  -> Bool                                  -- ^ Discard previous gpus.
  -> [MDevRequest]                         -- ^ List of mediated devices to create.
  -> IO ()                                 -- ^ Creates the mediated devices requested by the user.
createNvMDevs mpgpu rgpu discard requests = do
  let filteredResources =
        filter (\(_, gpu) -> validGpu gpu)
               (children (fst $ head (children (mpgpu :: MediatedPhysicalGpu)) :: NvidiaResource))
      vgpuConfigurators =
        map (\(nvg, Just gpu) ->
                (fst $ head (children ((fst $ head (children (nvg :: NvidiaResource))) :: NvidiaResource)), gpu)
            ) filteredResources
      ctl = (ctlFd (mpgpu :: MediatedPhysicalGpu))
  mapM_ (\(vgpuConfig, gpu) -> do
            let
              c = (client (vgpuConfig :: NvidiaResource))
              o = (object (vgpuConfig :: NvidiaResource))
              v = fromIntegral (vendorId (gpu :: Gpu)) :: Integer
              d = fromIntegral (deviceId (gpu :: Gpu)) :: Integer
              sv = fromIntegral (subvendorId (gpu :: Gpu)) :: Integer
              sd = fromIntegral (subdeviceId (gpu :: Gpu)) :: Integer
            printf "Attempting to create MDEVs on (0x%.4X, 0x%.4X, 0x%.4X, 0x%.4X)\n" v d sv sd
            mapM_ (\(idx, mdevReq) -> do
                      let discardPrev = discard && (idx == 0)
                          virtDisp = if isJust (virtDisplay (mdevReq :: MDevRequest))
                                        then fromJust (virtDisplay (mdevReq :: MDevRequest))
                                        else defaultVirtDisplay
                          mx = fromIntegral $ maxResX (virtDisp :: VirtDisplay)
                          my = fromIntegral $ maxResY (virtDisp :: VirtDisplay)
                          ec = if isJust (encCap (mdevReq :: MDevRequest))
                                  then fromJust (encCap (mdevReq :: MDevRequest))
                                  else 100
                          vdi = if isJust (vDevId (mdevReq :: MDevRequest))
                                   then fromJust (vDevId (mdevReq :: MDevRequest))
                                   else fromIntegral ((d `shift` 16) .|. sd)
                          pdi = if isJust (pDevId (mdevReq :: MDevRequest))
                                   then fromJust (pDevId (mdevReq :: MDevRequest))
                                   else fromIntegral d
                          b1Len = if isJust (bar1Len (mdevReq :: MDevRequest))
                                     then fromJust (bar1Len (mdevReq :: MDevRequest))
                                     else 0x100
                          cfg = defaultRmVgpuConfig { discardPrevious = if discardPrev then 1 else 0
                                                    , vgpuTypeNumber =
                                                        fromIntegral (num (mdevReq :: MDevRequest))
                                                    , vgpuName = castToVec (name (mdevReq :: MDevRequest))
                                                    , vgpuClass = castToVec (gpuClass (mdevReq :: MDevRequest))
                                                    , maxInstances =
                                                        fromIntegral (maxInstances (mdevReq :: MDevRequest))
                                                    , numHeads = fromIntegral (numHeads (virtDisp :: VirtDisplay))
                                                    , maxResX = mx
                                                    , maxResY = my
                                                    , maxPixel = mx * my
                                                    , frlConfig = fromIntegral (frlConfig (virtDisp :: VirtDisplay))
                                                    , cuda = 1
                                                    , eccSupported = if (ecc (mdevReq :: MDevRequest))
                                                                        then 1
                                                                        else 0
                                                    , multiMdev = if (multiMdev (mdevReq :: MDevRequest))
                                                                     then 1
                                                                     else 0
                                                    , encCap = fromIntegral ec
                                                    , vdevId = fromIntegral vdi
                                                    , pdevId = fromIntegral pdi
                                                    , fbLen = toBytes (fbLen (mdevReq :: MDevRequest))
                                                    , mappableVideoSize = toBytes (mapVideoSize (mdevReq :: MDevRequest))
                                                    , fbRes = toBytes (fbRes (mdevReq :: MDevRequest))
                                                    , bar1Len = fromIntegral b1Len
                                                    , frlEnable = fromIntegral (frlEnable (virtDisp :: VirtDisplay))
                                                    } :: RmVgpuConfig
                      _ <- with cfg (\par -> ctrlRes_ ctl c o 0xA0810101 par)
                      printf "Created mdev number %d: %s\n" idx (show mdevReq))
              (zip ([0..] :: [Integer]) requests)
            ctrlRes' ctl c o 0xA0810109
            printf "Created MDEVs on (0x%.4X, 0x%.4X, 0x%.4X, 0x%.4X)\n" v d sv sd
        )
    vgpuConfigurators
  where
    isEqual :: Maybe Word32 -> CUInt -> Bool
    isEqual (Just x) y = (fromIntegral x) == y
    isEqual Nothing _ = True

    validGpu :: Maybe Gpu -> Bool
    validGpu (Just gpu) =
      (isEqual (domain (rgpu :: RequestedGpu)) (domain (gpu :: Gpu))) &&
      (isEqual (bus (rgpu :: RequestedGpu)) (bus (gpu :: Gpu))) &&
      (isEqual (slot (rgpu :: RequestedGpu)) (slot (gpu :: Gpu))) &&
      (isEqual (function (rgpu :: RequestedGpu)) (function (gpu :: Gpu))) &&
      (isEqual (vendorId (rgpu :: RequestedGpu)) (vendorId (gpu :: Gpu))) &&
      (isEqual (deviceId (rgpu :: RequestedGpu)) (deviceId (gpu :: Gpu))) &&
      (isEqual (subvendorId (rgpu :: RequestedGpu)) (subvendorId (gpu :: Gpu))) &&
      (isEqual (subdeviceId (rgpu :: RequestedGpu)) (subdeviceId (gpu :: Gpu))) &&
      (isEqual (identifier (rgpu :: RequestedGpu)) (identifier (gpu :: Gpu)))
    validGpu _ = error "No GPU found."

    castToVec s = V.fromList ((map castCharToCChar s) ++ repeat 0)
    toBytes x = fromIntegral (x * 1024 * 1024) :: CULong

{-| Frees an internal resource tree from the GPU kernel module.
-}
freeNvResTree :: Fd                        -- ^ File descriptor to free the resource on.
  -> [(NvidiaResource, Maybe Gpu)]         -- ^ Resource tree.
  -> IO ()                                 -- ^ Cleans the resources from the nvidia resource tree.
freeNvResTree _ [] = return ()
freeNvResTree ctlFd resources =
  mapM_
    (\(NvidiaResource { .. }, a) ->
       let freeRm = defaultRmFreeRes { hRoot = client
                                     , hObjectParent = parent
                                     , hObjectOld = object
                                     } :: RmFreeRes
       in freeNvResTree ctlFd children >> freeRes ctlFd freeRm >>
          if isJust fd
                then if isJust a
                        then let gpuId = identifier ((fromJust a) :: Gpu)
                                 detach = defaultNv0000CtrlGpuDetachIdsParams { gpuIds = V.fromList ([ gpuId
                                                                                                     , 0xFFFFFFFF
                                                                                                     ] ++ repeat 0)
                                                                              } :: Nv0000CtrlGpuDetachIdsParams
                             in with detach (\par -> ctrlRes_ ctlFd client client 0x216 par) >>
                                closeFd (fromJust fd)
                        else closeFd (fromJust fd)
                else return ())
    resources

{-| Command to get the GPU list from the kernel.
-}
getGpus :: Fd                                   -- ^ File descriptor for the control file.
  -> CUInt                                      -- ^ Client ID to allocate commands on.
  -> IO ([(NvidiaResource, Maybe Gpu)])         -- ^ Returns a list of all known resources.
getGpus fd clientId = do
  probedIds <- with defaultNv0000CtrlGpuGetProbedIdsParams (\par -> ctrlRes fd clientId clientId 0x214 par >>= peek)
  let probedIds' = zip [0..] $ filter (/= 0xFFFFFFFF) $ V.toList (gpuIds (probedIds :: Nv0000CtrlGpuGetProbedIdsParams))
  devices <- mapM (\(idx, x) ->
                     let
                       idInfoParams = defaultNv0000CtrlGpuGetIdInfoParams { gpuId = x } :: Nv0000CtrlGpuGetIdInfoParams
                       pciInfo = defaultNv0000CtrlGpuGetPciInfoParams { gpuId = x } :: Nv0000CtrlGpuGetPciInfoParams
                       attachIds = defaultNv0000CtrlGpuAttachIdsParams { gpuIds = V.fromList ([x, 0xFFFFFFFF] ++ repeat 0) } :: Nv0000CtrlGpuAttachIdsParams
                       deviceId = composeNormalDevId clientId x 1
                       subDeviceId = composeNormalDevId clientId x 2
                       subDeviceAlloc = 0 :: Nv2080AllocParams
                       vgpuDeviceId = composeNormalDevId clientId x 3
                     in do
                       pci <- with pciInfo (\par -> ctrlRes fd clientId clientId 0x21B par >>= peek)
                       _ <- with attachIds (\par -> ctrlRes_ fd clientId clientId 0x215 par)
                       nvfd <- openNvDev idx
                       gpuInfo <- with idInfoParams (\par -> ctrlRes fd clientId clientId 0x202 par >>= peek)
                       let deviceAlloc = defaultNv0080AllocParams { deviceId = (deviceInstance gpuInfo) } :: Nv0080AllocParams
                       _ <- with deviceAlloc (\par -> allocRes
                                                        fd
                                                        (defaultRmAllocRes { hRoot = clientId
                                                                           , hObjectParent = clientId
                                                                           , hObjectNew = deviceId
                                                                           , hClass = 0x00000080
                                                                           , pAllocParams = (castPtr par)
                                                                           } :: RmAllocRes))
                       _ <- with subDeviceAlloc (\par -> allocRes
                                                           fd
                                                           (defaultRmAllocRes { hRoot = clientId
                                                                              , hObjectParent = deviceId
                                                                              , hObjectNew = subDeviceId
                                                                              , hClass = 0x00002080
                                                                              , pAllocParams = (castPtr par)
                                                                              } :: RmAllocRes))
                       busPciInfo <- with defaultBusGetPciInfo (\par -> ctrlRes fd clientId subDeviceId 0x20801801 par >>= peek)
                       _ <- allocRes
                                fd
                                (defaultRmAllocRes { hRoot = clientId
                                                   , hObjectParent = subDeviceId
                                                   , hObjectNew = vgpuDeviceId
                                                   , hClass = 0x0000A081
                                                   } :: RmAllocRes)
                       let devid = ((devId (busPciInfo :: BusGetPciInfo)) `shift` (-16))
                           subid = ((subDevId (busPciInfo :: BusGetPciInfo)) `shift` (-16))
                           gpu =
                             Gpu (domain (pci :: Nv0000CtrlGpuGetPciInfoParams))
                                 (fromIntegral $ bus (pci :: Nv0000CtrlGpuGetPciInfoParams))
                                 (fromIntegral $ slot (pci :: Nv0000CtrlGpuGetPciInfoParams))
                                 0
                                 0x10DE
                                 devid
                                 0x10DE
                                 subid
                                 x
                           vgpudev = (NvidiaResource Nothing clientId subDeviceId vgpuDeviceId 0x0000A081 [], Nothing)
                           subdev = (NvidiaResource Nothing clientId deviceId subDeviceId 0x00002080 [vgpudev], Nothing)
                           res = (NvidiaResource nvfd clientId clientId deviceId 0x00000080 [subdev], Just gpu)
                       printf
                         "Opened GPU 0x%.4X (vendor: 0x%.4X, device: 0x%.4X, subvendor: 0x%.4X, subdevice: 0x%4X)\n"
                         (fromIntegral x :: Integer)
                         (0x10DE :: Integer)
                         (fromIntegral devid :: Integer)
                         (0x10DE :: Integer)
                         (fromIntegral subid :: Integer)
                       return res) probedIds'
  return devices

{-| Opens an MDev Interface for the NVIDIA GPUs on the system.
-}
openNvMDevInterface :: Bool   -- ^ If we use the check for version mismatch or not.
  -> IO (MediatedPhysicalGpu) -- ^ Returns a fully functioning mediated physical GPU root,
                              -- that can be used to create MDevs on that particular GPU.
openNvMDevInterface mismatchCheck = do
  ctlFd <- openNvDev 255
  if isJust ctlFd
    then do
        let ctlFd' = fromJust ctlFd

        -- Version check to verify that we are capable to run the current
        -- version of the mdev gpu for nvidia.
        versionCheck ctlFd' mismatchCheck

        -- Allocates a root resource.
        rootRes <- allocRes ctlFd' defaultRmAllocRes
        let
          clientId = hObjectNew (rootRes :: RmAllocRes)

        -- Gets the GPUs.
        gpus <- getGpus ctlFd' clientId

        let
          rootResource =
            (NvidiaResource Nothing clientId clientId clientId 0x000000 gpus, Nothing)

        return $ NvidiaMdev ctlFd' clientId [rootResource]
    else return NotInitialized

{-| This closes the interface to the Nvidia MDev interface.
-}
closeNvMDevInterface :: MediatedPhysicalGpu -- ^ Mediated GPU.
  -> IO ()                                  -- ^ IO Side effects of closing
                                            -- the gpu.
closeNvMDevInterface (NvidiaMdev { .. }) =
  freeNvResTree ctlFd children >>
  closeFd ctlFd
closeNvMDevInterface _ =
  error "Invalid Mediated Physical GPU, function expects NvidiaMDev"
