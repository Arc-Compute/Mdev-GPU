{-|
Module      : Main
Description : CLI Client to create MDevs.
Copyright   : (c) 2022 2666680 Ontario Inc. O\A Arc Compute
License     : GNU GPL v.2
Maintainer  : michael@arccompute.io
Stability   : experimental
Portability : POSIX

This is where the CLI client is stored in order to provide us access to creating arbitrary MDevs.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Concurrent
import Data.Semigroup ((<>))
import Data.Text (Text)
import Options.Applicative
import YamlParse.Applicative (confDesc)

import Common.Config
import Common.Types
import Nvidia.MDev

data MdevCliControl = MdevCliControl
  { config :: Text
  , keep :: Bool
  }

control :: Parser MdevCliControl
control = MdevCliControl
  <$> strOption
      ( long "config"
      <> short 'c'
      <> metavar "CONFIG_FILE"
      <> help "Configuration file to use to generate mdevs (.yaml, .json)"
      )
  <*> switch
      ( long "keep"
      <> short 'k'
      <> help "Keeps previous MDevs before preceding."
      )

main :: IO ()
main = mdev =<< execParser opts
  where
    opts = info (control <**> helper)
      ( fullDesc
      <> progDesc "Creates arbitrary mediated devices for GPUs."
      <> header "Copyright (C) 2022  2666680 Ontario Inc. O\\A Arc Compute\n\nThis program is free software; you can redistribute it and/or\nmodify it under the terms of the GNU General Public License\nas published by the Free Software Foundation; either version 2\nof the License, or (at your option) any later version.\n\nThis program is distributed in the hope that it will be useful,\nbut WITHOUT ANY WARRANTY; without even the implied warranty of\nMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\nGNU General Public License for more details.\n\nYou should have received a copy of the GNU General Public License\nalong with this program; if not, write to the Free Software\nFoundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA."
      <> confDesc @MdevCliConfig
      )

mkMdev :: [GpuConfig] -> MediatedPhysicalGpu -> Bool -> IO ()
mkMdev [] _ _ = return ()
mkMdev (curr:rem) mdev disc = do
  if length (filteredGpus curr) == 0
    then createNvMDevs mdev defaultRequestedGpu disc (mdevConfigs curr) >>
         mkMdev rem mdev disc
    else mapM_ (\x -> createNvMDevs mdev x disc (mdevConfigs curr)) (filteredGpus curr) >>
         mkMdev rem mdev disc

mdev :: MdevCliControl -> IO ()
mdev (MdevCliControl { .. }) = do
  configs <- getConfig config
  gpu <- openNvMDevInterface True
  mkMdev configs gpu (not keep)
  closeNvMDevInterface gpu
