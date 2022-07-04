{-|
Module      : Common.Types
Description : Common types between all GPUs.
Copyright   : (c) 2022 2666680 Ontario Inc. O\A Arc Compute
License     : GNU GPL v.2
Maintainer  : michael@arccompute.io
Stability   : experimental
Portability : POSIX

Here we develop an interface between several different GPU types for the purpose of creating a
common interaction layer with our codebase.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Types where

import Data.Aeson (FromJSON(..))
import Foreign (Storable(..), Word32, Word64)
import Foreign.C.Types
import System.Posix.Types
import YamlParse.Applicative

{-| This structure is a bare bones GPU that gives us some information over how
the physical connected GPU is.
-}
data Gpu = Gpu { domain :: CUInt                         -- ^ Domain for the PCI device.
               , bus :: CUInt                            -- ^ Bus for the PCI device.
               , slot :: CUInt                           -- ^ Slot for the PCI device.
               , function :: CUInt                       -- ^ Function for the PCI device.
               , vendorId :: CUInt                       -- ^ Vendor ID for the PCI device.
               , deviceId :: CUInt                       -- ^ Device ID for the PCI device.
               , subvendorId :: CUInt                    -- ^ Vendor id of the subdevice.
               , subdeviceId :: CUInt                    -- ^ Device id of the subdevice.
               , identifier :: CUInt                     -- ^ Identifier for the GPU.
               }
  deriving (Show)

{-| The NVIDIA kernel module implements it's own class based system to interact
with resources inside the kernel and speaking to the GPU. This is an incomplete
class which lacks a lot of the different classes that can be shown. We are
currently in the process of documenting and providing a tool for interacting
with the different allowed classes. This tool is available on the Arc-Compute
github and there is already extensive documentation efforts occuring on OpenMdev.
-}
data NvidiaResource = NvidiaResource                      -- ^ A resource for NVIDIA GPUs.
                        { fd :: Maybe Fd                  -- ^ Some physical resources require
                                                          -- open file descriptors for the
                                                          -- duration of the resource's lifecycle.
                        , client :: CUInt                 -- ^ All resources require a client/root.
                        , parent :: CUInt                 -- ^ All resources require a parent.
                        , object :: CUInt                 -- ^ All resources have objects.
                        , oClass :: CUInt                 -- ^ All resources have a class.
                        , children :: [(NvidiaResource, Maybe Gpu)]
                                                          -- ^ The possible children of the resource.
                        }
  deriving (Show)

{-| Mediated physical devices is a variant object for different forms of physical GPUs
that are mediated, if it is not possible to initialize GPUs, it returns a NotInitialized
object.
-}
data MediatedPhysicalGpu = NvidiaMdev                     -- ^ An initialized NVIDIA mediated physical device.
                           { ctlFd :: Fd                  -- ^ Control file descriptor.
                           , root :: CUInt                -- ^ Root id for the NVIDIA GPU.
                           , children :: [(NvidiaResource, Maybe Gpu)]
                                                          -- ^ Children which the mediated device can use.
                           }
                         | NotInitialized                 -- ^ A situation when the Mediated device cannot be initialized.
  deriving (Show)

{-| Valid enumerations of supported gpus, for our mediated device creation tool.
-}
data SupportedGpus = Nvidia                               -- ^ NVIDIA GPUs are available to create mdevs.
  deriving (Show)

{-| Structure to make it possible to fake void pointers in Haskell.
-}
data VoidPtr = VoidPtr

instance Storable VoidPtr where
  sizeOf _ = 0
  alignment _ = 0
  peek _ = return VoidPtr
  poke _ _ = return ()

{-| Structure to make it possible to make a mediated virtual display.
-}
data VirtDisplay = VirtDisplay
  { numHeads :: Word32                                     -- ^ Number of display heads to create.
  , maxResX :: Word32                                      -- ^ Maximum width of a display.
  , maxResY :: Word32                                      -- ^ Maximum height of a display.
  , frlConfig :: Word32                                    -- ^ The maximum FPS before we get limited
  , frlEnable :: Word32                                    -- ^ If we enable frame limiting or not.
  } deriving (Show, Eq)

instance YamlSchema VirtDisplay where
  yamlSchema =
    objectParser "Virtual Display" $
        VirtDisplay
          <$> optionalFieldWithDefault
                "numHeads"
                1
                "Number of heads on a mediated display."
          <*> optionalFieldWithDefault
                "maxResX"
                1024
                "Maximum width of the mediated display."
          <*> optionalFieldWithDefault
                "maxResY"
                1024
                "Maximum height of the mediated display."
          <*> optionalFieldWithDefault
                "frlConfig"
                120
                "Frame rate limiter for the mediated display."
          <*> optionalFieldWithDefault
                "frlEnable"
                0
                "Do we enable frame rate limiting or not?"

instance FromJSON VirtDisplay where
  parseJSON = viaYamlSchema

{-| Basic default configuration for a virtual display.

NOTE: This provides a single display of 1024 x 1024 frame limited at 120 fps.
-}
defaultVirtDisplay :: VirtDisplay
defaultVirtDisplay = VirtDisplay 1 1024 1024 120 0

{-| Object to request a specific type of GPU to ensure we only generate MDevs for the filtered GPUs.
-}
data RequestedGpu = RequestedGpu { domain :: Maybe Word32                         -- ^ Domain for the PCI device.
                                 , bus :: Maybe Word32                            -- ^ Bus for the PCI device.
                                 , slot :: Maybe Word32                           -- ^ Slot for the PCI device.
                                 , function :: Maybe Word32                       -- ^ Function for the PCI device.
                                 , vendorId :: Maybe Word32                       -- ^ Vendor ID for the PCI device.
                                 , deviceId :: Maybe Word32                       -- ^ Device ID for the PCI device.
                                 , subvendorId :: Maybe Word32                    -- ^ Vendor id of the subdevice.
                                 , subdeviceId :: Maybe Word32                    -- ^ Device id of the subdevice.
                                 , identifier :: Maybe Word32                     -- ^ Identifier for the GPU.
                                 }
  deriving (Show)

instance YamlSchema RequestedGpu where
  yamlSchema =
    objectParser "RequestedGpu" $
      RequestedGpu
        <$> optionalField "domain" "Domain for the PCI device."
        <*> optionalField "bus" "Bus for the PCI device."
        <*> optionalField "slot" "Slot for the PCI device."
        <*> optionalField "function" "Function for the PCI device."
        <*> optionalField "vendorId" "Vendor ID for the PCI device."
        <*> optionalField "deviceId" "Device ID for the PCI device."
        <*> optionalField "subvendorId" "Vendor ID of the subdevice."
        <*> optionalField "subdeviceId" "Device ID of the subdevice."
        <*> optionalField "identifier" "Internal GPU identification tag."

instance FromJSON RequestedGpu where
  parseJSON = viaYamlSchema

{-| Device to filter GPUs based on this structure.
-}
defaultRequestedGpu :: RequestedGpu
defaultRequestedGpu =
  RequestedGpu Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

{-| Request to provide an MDev device.
-}
data MDevRequest = MDevRequest
  { num :: Word32                                                          -- ^ Mdev number to use.
  , vDevId :: Maybe Word64                                                 -- ^ Virtual device id to use (guest).
  , pDevId :: Maybe Word64                                                 -- ^ Virtual device id to use (host).
  , name :: String                                                         -- ^ Name to use for the MDev.
  , gpuClass :: String                                                     -- ^ Class of the GPU to create.
  , maxInstances :: Word32                                                 -- ^ Maximum number of instances.
  , virtDisplay :: Maybe VirtDisplay                                       -- ^ If a virtual display is to be made.
  , ecc :: Bool                                                            -- ^ If we enable ECC.
  , multiMdev :: Bool                                                      -- ^ If we support multiple MDev mode.
  , fbLen :: Word32                                                        -- ^ Frame buffer length (IN MEGABYTES).
  , fbRes :: Word32                                                        -- ^ Frame buffer reserved (IN MEGABYTES).
  , mapVideoSize :: Word32                                                 -- ^ Mappable video size (IN MEGABYTES).
  , encCap :: Maybe Word32                                                 -- ^ What percentage of the encoder will
                                                                           -- be allocated to this GPU.
  , bar1Len :: Maybe Word32                                                -- ^ Bar 1 length.
  } deriving (Show)

instance YamlSchema MDevRequest where
  yamlSchema =
    objectParser "MDevRequest" $
      MDevRequest
        <$> requiredField "num" "MDev number to assign."
        <*> optionalField "vDevId" "Spoofing of the GPU to the guest."
        <*> optionalField "pDevId" "Spoofing of the GPU to the host."
        <*> optionalFieldWithDefault "name" "Arc GPU" "Name presented by the guest."
        <*> optionalFieldWithDefault "gpuClass" "Compute" "Class of GPU to use."
        <*> requiredField "maxInstances" "Number of instances to allocate."
        <*> optionalField "virtDisplay" "Virtual display to create for the MDev."
        <*> optionalFieldWithDefault "ecc" False "If ECC is enabled or not."
        <*> optionalFieldWithDefault "multiMdev" False "If we enable adding multiple mdevs into the same VM. (EXPERIMENTAL)"
        <*> requiredField "fbLen" "GPU vRAM size - reserved section in MBs."
        <*> requiredField "fbRes" "GPU vRAM reserved section in MBs."
        <*> optionalFieldWithDefault "mapVideoSize" 24 "Mappable video size in MBs, 24 by default."
        <*> optionalField "encCap" "Percentage we use the NvEnc hardware."
        <*> optionalField "bar1Len" "Bar 1 length, VERY EXPERIMENTAL."

instance FromJSON MDevRequest where
  parseJSON = viaYamlSchema

{-| Default mdev request.
-}
defaultMDevRequest :: MDevRequest
defaultMDevRequest =
  MDevRequest
    0
    Nothing
    Nothing
    "ARC GPU"
    "Compute"
    1
    Nothing
    False
    False
    976
    48
    24
    Nothing
    Nothing
