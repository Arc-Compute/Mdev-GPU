{-|
Module      : Nvidia.Errors
Description : Nvidia Error messages.
Copyright   : (c) 2022 2666680 Ontario Inc. O\A Arc Compute
License     : GNU GPL v.2
Maintainer  : michael@arccompute.io
Stability   : experimental
Portability : POSIX

Error messages for NVIDIA.
-}
module Nvidia.Errors
  ( NvErrorStatus(..)
  )
  where

import Data.Bimap
import Foreign (Storable(..))
import Foreign.C.Types

{-| All possible NVIDIA error statuses.
-}
data NvErrorStatus = NvOk -- ^ Ok message, the command succeeded correctly.
                   | NvBrokenFb -- ^ We have a broken framebuffer.
                   | NvSmallFb -- ^ The framebuffer is too small.
                   | NvBusyRetry -- ^ The command was busy, please retry after a timeout.
                   | NvCallbackNotScheduled -- ^ The callback was not scheduled.
                   | NvCardNotPresent -- ^ The card is not present on the system.
                   | NvCallCycle -- ^ Call cycle was detected.
                   | NvDMAInUse -- ^ The DMA is currently in use.
                   | NvDMANotLocked -- ^ Could not lock the requested DMA.
                   | NvDMANotUnlocked -- ^ Could not unlock the requested DMA.
                   | NvDualLink -- ^ Dual Link is currently in use.
                   | NvECC -- ^ Generic ECC Error.
                   | NvFIFOBadAccess -- ^ Invalid access of FIFO.
                   | NvFreqNotSupported -- ^ Requested frequency is not supported.
                   | NvDMANotInitialized -- ^ Requested DMA is not yet initialized.
                   | NvLostFromBus -- ^ GPU was lost from the bus.
                   | NvFullChipReset -- ^ GPU is currently in a full-chip reset.
                   | NvNotFullPower -- ^ GPU is not operating at full power.
                   | NvUuidNotFound -- ^ No UUID associated with GPU.
                   | NvHotSwitch -- ^ System is in a hot switch.
                   | NvI2C -- ^ Generic I2C error.
                   | NvI2CSpeedTooHigh -- ^ I2C speed is too high.
                   | NvIllegalAction -- ^ Illegal action attempted.
                   | NvInUse -- ^ Generic in use error.
                   | NvInflateCompressedDataFailed -- ^ Failed to inflate compressed data.
                   | NvInsertDuplicateName -- ^ Found a duplicate entry in the requested B-tree.
                   | NvInsufficientResources -- ^ Ran out of a critical resource, other than memory.
                   | NvInsufficientPermissions -- ^ The requestor does not have sufficient permissions.
                   | NvInsufficientPower -- ^ Low power.
                   | NvInvalidAccessType -- ^ This type of access is not allowed.
                   | NvInvalidAddress -- ^ Address is invalid.
                   | NvInvalidArgument -- ^ The given argument is invalid.
                   | NvInvalidBase -- ^ The base is invalid.
                   | NvInvalidChannel -- ^ Given channel-id is invalid.
                   | NvInvalidClass -- ^ Given class-id is invalid.
                   | NvInvalidClient -- ^ Client is invalid.
                   | NvInvalidCommand -- ^ Command passed is invalid.
                   | NvInvalidData -- ^ Data is invalid.
                   | NvInvalidDevice -- ^ Device passed is currently invalid.
                   | NvInvalidDMASpecifier -- ^ Requested DMA specifier is invalid.
                   | NvInvalidEvent -- ^ Invalid event occured.
                   | NvInvalidFlags -- ^ Invalid flags passed.
                   | NvInvalidFunction -- ^ The called function is invalid.
                   | NvInvalidHeap -- ^ Heap is corrupted.
                   | NvInvalidIndex -- ^ The index is invalid.
                   | NvInvalidIRQLevel -- ^ Requested IRQ level is invalid.
                   | NvInvalidLimit -- ^ Invalid limit.
                   | NvInvalidLockState -- ^ Requested lock state is invalid.
                   | NvInvalidMethod -- ^ Requested method is invalid.
                   | NvInvalidObject -- ^ Requested object is invalid.
                   | NvInvalidObjectBuffer -- ^ Object buffer is invalid.
                   | NvInvalidObjectHandle -- ^ Object handle is invalid.
                   | NvInvalidObjectNew -- ^ New object is invalid.
                   | NvInvalidObjectOld -- ^ Old object is invalid.
                   | NvInvalidObjectParent -- ^ Object's parent is invalid.
                   | NvInvalidOffset -- ^ The offset is invalid.
                   | NvInvalidOperation -- ^ Requested operation is invalid.
                   | NvInvalidOwner -- ^ Invalid owner.
                   | NvInvalidParamStruct -- ^ Invalid structure parameters.
                   | NvInvalidParameter -- ^ Invalid parameters.
                   | NvInvalidPath -- ^ Requested path is invalid.
                   | NvInvalidPointer -- ^ Invalid pointer.
                   | NvInvalidRegistryKey -- ^ Invalid registry key.
                   | NvInvalidRequest -- ^ Invalid request.
                   | NvInvalidState -- ^ Invalid state.
                   | NvInvalidStringLength -- ^ String length invalid.
                   | NvInvalidRead -- ^ Read operation is invalid.
                   | NvInvalidWrite -- ^ Write operation is invalid.
                   | NvInvalidXlate -- ^ Translation operation is invalid.
                   | NvIRQNotFiring -- ^ Requested IRQ is not firing.
                   | NvIRQEdgeTriggered -- ^ IRQ is edge triggered.
                   | NvMemoryTrainingFailed -- ^ Failed the memory training sequence.
                   | NvMismatchedSlave -- ^ Slave mismatch.
                   | NvMismatchedTarget -- ^ Target mismatch.
                   | NvMissingTableEntry -- ^ Requested entry missing from table.
                   | NvModuleLoadFailed -- ^ Failed to load requested module.
                   | NvMoreDataAvailable -- ^ There is more data available.
                   | NvMoreProcessingRequired -- ^ More processing is required for the given call.
                   | NvMultipleMemoryTypes -- ^ Multiple memory types found.
                   | NvNoFreeFifos -- ^ No more free FIFOs.
                   | NvNoIntrPending -- ^ No interrupt is pending.
                   | NvNoMemory -- ^ Out of memory.
                   | NvNoSuchDomain -- ^ Requested domain doesn't exist.
                   | NvNoValidPath -- ^ Caller did not specify a valid path.
                   | NvNotCompatible -- ^ Incompatible types.
                   | NvNotReady -- ^ Not ready.
                   | NvNotSupported -- ^ Call is not supported.
                   | NvObjectNotFound -- ^ Requested object is not found.
                   | NvObjectTypeMismatch -- ^ Specified objects mismatch.
                   | NvOperatingSystem -- ^ Operating system error.
                   | NvFoundOtherDevice -- ^ Found other device requested one.
                   | NvOutOfRange -- ^ Value is out of bounds.
                   | NvOverlappingUVMCommit -- ^ Overlapping unified virtual memory commit.
                   | NvPageTableNotAvailable -- ^ Requested page table unavailable.
                   | NvPIDNotFound -- ^ PID is not found.
                   | NvProtectionFault -- ^ Protection fault.
                   | NvRCError -- ^ Generic RC error.
                   | NvRejectedVBios -- ^ Rejected VBios.
                   | NvResetRequired -- ^ Reset required.
                   | NvStateInUse -- ^ State is currently in use.
                   | NvSignalPending -- ^ Signal pending.
                   | NvTimeout -- ^ Call timed out.
                   | NvTimeoutRetry -- ^ Call timed out, retry later.
                   | NvTooManyPrimaries -- ^ Too many primaries.
                   | NvUVMAddressInUse -- ^ Unified virtual address is currently in use.
                   | NvMaxSessionLimitReached -- ^ Maximum number of sessions reached.
                   | NvRMLibVersionMismatch -- ^ RM library version mismatch.
                   | NvPrivSecurity -- ^ Priv security violation.
                   | NvGPUInDebug -- ^ GPU is in debug mode.
                   | NvFeatureNotEnabled -- ^ Feature is not currently enabled.
                   | NvResourceLost -- ^ Resource was lost.
                   | NvPMUNotReady -- ^ PMU is not ready or initialized yet.
                   | NvFLCNAssert -- ^ Falcon assertion/error.
                   | NvFatalError -- ^ Fatal/unrecoverable error.
                   | NvMemoryError -- ^ Memory error.
                   | NvInvalidLicense -- ^ Invalid license.
                   | NvNVLinkInit -- ^ NVLink init error.
                   | NvNVLinkMinion -- ^ NVLink minion error.
                   | NvNVLinkClock -- ^ NVLink clock error.
                   | NvNVLinkTraining -- ^ NVLink training error.
                   | NvNVLinkConfiguration -- ^ NVLink configuration error.
                   | NvRISCVError -- ^ Generic RISC-V error.
                   | NvHotSwitchWarn -- ^ WARNING Hot switch.
                   | NvIncorrectPerfMonDataWarn -- ^ WARNING Incorrect performance monitoring data.
                   | NvSlaveMismatchWarn -- ^ WARNING Slave mismatch.
                   | NvTargetMismatchWarn -- ^ WARNING Target mismatch.
                   | NvMoreProcessingWarn -- ^ WARNING More processing required for this call.
                   | NvNothingWarn -- ^ WARNING Nothing to do.
                   | NvNullWarn -- ^ WARNING Null object found.
                   | NvOutOfRangeWarn -- ^ WARNING Value out of range.
                   | NvGeneric -- ^ Generic RM error.
  deriving (Ord, Eq, Show)

{-| Internal error number.
-}
errorEnum :: Bimap NvErrorStatus CUInt
errorEnum = fromList [ (NvOk, 0x00000000)
                     , (NvBrokenFb, 0x00000001)
                     , (NvSmallFb, 0x0000002)
                     , (NvBusyRetry, 0x00000003)
                     , (NvCallbackNotScheduled, 0x00000004)
                     , (NvCardNotPresent, 0x00000005)
                     , (NvCallCycle, 0x00000006)
                     , (NvDMAInUse, 0x00000007)
                     , (NvDMANotLocked, 0x00000008)
                     , (NvDMANotUnlocked, 0x00000009)
                     , (NvDualLink, 0x0000000A)
                     , (NvECC, 0x0000000B)
                     , (NvFIFOBadAccess, 0x0000000C)
                     , (NvFreqNotSupported, 0x0000000D)
                     , (NvDMANotInitialized, 0x0000000E)
                     , (NvLostFromBus, 0x0000000F)
                     , (NvFullChipReset, 0x00000010)
                     , (NvNotFullPower, 0x00000011)
                     , (NvUuidNotFound, 0x00000012)
                     , (NvHotSwitch, 0x00000013)
                     , (NvI2C, 0x00000014)
                     , (NvI2CSpeedTooHigh, 0x00000015)
                     , (NvIllegalAction, 0x00000016)
                     , (NvInUse, 0x00000017)
                     , (NvInflateCompressedDataFailed, 0x00000018)
                     , (NvInsertDuplicateName, 0x00000019)
                     , (NvInsufficientResources, 0x0000001A)
                     , (NvInsufficientPermissions, 0x0000001B)
                     , (NvInsufficientPower, 0x0000001C)
                     , (NvInvalidAccessType, 0x0000001D)
                     , (NvInvalidAddress, 0x0000001E)
                     , (NvInvalidArgument, 0x0000001F)
                     , (NvInvalidBase, 0x00000020)
                     , (NvInvalidChannel, 0x00000021)
                     , (NvInvalidClass, 0x00000022)
                     , (NvInvalidClient, 0x00000023)
                     , (NvInvalidCommand, 0x00000024)
                     , (NvInvalidData, 0x00000025)
                     , (NvInvalidDevice, 0x00000026)
                     , (NvInvalidDMASpecifier, 0x00000027)
                     , (NvInvalidEvent, 0x00000028)
                     , (NvInvalidFlags, 0x00000029)
                     , (NvInvalidFunction, 0x0000002A)
                     , (NvInvalidHeap, 0x0000002B)
                     , (NvInvalidIndex, 0x0000002C)
                     , (NvInvalidIRQLevel, 0x0000002D)
                     , (NvInvalidLimit, 0x0000002E)
                     , (NvInvalidLockState, 0x0000002F)
                     , (NvInvalidMethod, 0x00000030)
                     , (NvInvalidObject, 0x00000031)
                     , (NvInvalidObjectBuffer, 0x00000032)
                     , (NvInvalidObjectHandle, 0x00000033)
                     , (NvInvalidObjectNew, 0x00000034)
                     , (NvInvalidObjectOld, 0x00000035)
                     , (NvInvalidObjectParent, 0x00000036)
                     , (NvInvalidOffset, 0x00000037)
                     , (NvInvalidOperation, 0x00000038)
                     , (NvInvalidOwner, 0x00000039)
                     , (NvInvalidParamStruct, 0x0000003A)
                     , (NvInvalidParameter, 0x0000003B)
                     , (NvInvalidPath, 0x0000003C)
                     , (NvInvalidPointer, 0x0000003D)
                     , (NvInvalidRegistryKey, 0x0000003E)
                     , (NvInvalidRequest, 0x0000003F)
                     , (NvInvalidState, 0x00000040)
                     , (NvInvalidStringLength, 0x00000041)
                     , (NvInvalidRead, 0x00000042)
                     , (NvInvalidWrite, 0x00000043)
                     , (NvInvalidXlate, 0x00000044)
                     , (NvIRQNotFiring, 0x00000045)
                     , (NvIRQEdgeTriggered, 0x00000046)
                     , (NvMemoryTrainingFailed, 0x00000047)
                     , (NvMismatchedSlave, 0x00000048)
                     , (NvMismatchedTarget, 0x00000049)
                     , (NvMissingTableEntry, 0x0000004A)
                     , (NvModuleLoadFailed, 0x0000004B)
                     , (NvMoreDataAvailable, 0x0000004C)
                     , (NvMoreProcessingRequired, 0x0000004D)
                     , (NvMultipleMemoryTypes, 0x0000004E)
                     , (NvNoFreeFifos, 0x0000004F)
                     , (NvNoIntrPending, 0x00000050)
                     , (NvNoMemory, 0x00000051)
                     , (NvNoSuchDomain, 0x00000052)
                     , (NvNoValidPath, 0x00000053)
                     , (NvNotCompatible, 0x00000054)
                     , (NvNotReady, 0x00000055)
                     , (NvNotSupported, 0x00000056)
                     , (NvObjectNotFound, 0x00000057)
                     , (NvObjectTypeMismatch, 0x00000058)
                     , (NvOperatingSystem, 0x00000059)
                     , (NvFoundOtherDevice, 0x0000005A)
                     , (NvOutOfRange, 0x0000005B)
                     , (NvOverlappingUVMCommit, 0x0000005C)
                     , (NvPageTableNotAvailable, 0x0000005D)
                     , (NvPIDNotFound, 0x0000005E)
                     , (NvProtectionFault, 0x0000005F)
                     , (NvRCError, 0x00000060)
                     , (NvRejectedVBios, 0x00000061)
                     , (NvResetRequired, 0x00000062)
                     , (NvStateInUse, 0x00000063)
                     , (NvSignalPending, 0x00000064)
                     , (NvTimeout, 0x00000065)
                     , (NvTimeoutRetry, 0x00000066)
                     , (NvTooManyPrimaries, 0x00000067)
                     , (NvUVMAddressInUse, 0x00000068)
                     , (NvMaxSessionLimitReached, 0x00000069)
                     , (NvRMLibVersionMismatch, 0x0000006A)
                     , (NvPrivSecurity, 0x0000006B)
                     , (NvGPUInDebug, 0x0000006C)
                     , (NvFeatureNotEnabled, 0x0000006D)
                     , (NvResourceLost, 0x0000006E)
                     , (NvPMUNotReady, 0x0000006F)
                     , (NvFLCNAssert, 0x00000070)
                     , (NvFatalError, 0x00000071)
                     , (NvMemoryError, 0x00000072)
                     , (NvInvalidLicense, 0x00000073)
                     , (NvNVLinkInit, 0x00000074)
                     , (NvNVLinkMinion, 0x00000075)
                     , (NvNVLinkClock, 0x00000076)
                     , (NvNVLinkTraining, 0x00000077)
                     , (NvNVLinkConfiguration, 0x00000078)
                     , (NvRISCVError, 0x00000079)
                     , (NvHotSwitchWarn, 0x00010001)
                     , (NvIncorrectPerfMonDataWarn, 0x00010002)
                     , (NvSlaveMismatchWarn, 0x00010003)
                     , (NvTargetMismatchWarn, 0x00010004)
                     , (NvMoreProcessingWarn, 0x00010005)
                     , (NvNothingWarn, 0x00010006)
                     , (NvNullWarn, 0x00010007)
                     , (NvOutOfRangeWarn, 0x00010008)
                     , (NvGeneric, 0x0000FFFF)
                     ]

{-| Internal from error.
-}
fromError :: NvErrorStatus -> CUInt
fromError x = errorEnum ! x

{-| Internal to error.
-}
toError :: CUInt -> NvErrorStatus
toError x = errorEnum !> x

instance Storable NvErrorStatus where
  sizeOf _ = 4
  alignment _ =  4
  peek ptr = peekByteOff ptr 0 >>= return . toError
  poke ptr d = pokeByteOff ptr 0 (fromError d)
