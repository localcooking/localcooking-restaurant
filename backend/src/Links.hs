{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module Links where

import Data.Monoid ((<>))
import Path.Extended (ToPath (..), ToLocation (..), Abs, File, fromPath, setFileExt, parseAbsFile)


data LogoLinks
  = LogoPng
  | LogoWhitePng
  | IconPng
  | IconSvg


instance ToPath LogoLinks Abs File where
  toPath x = case x of
    LogoPng      -> parseAbsFile (parent <> "logo")
    LogoWhitePng -> parseAbsFile (parent <> "logo-white")
    IconPng      -> parseAbsFile (parent <> "icon")
    IconSvg      -> parseAbsFile (parent <> "icon")
    where
      parent = "/static/images/"


instance ToLocation LogoLinks Abs File where
  toLocation x = case x of
    LogoPng      -> (setFileExt (Just "png") . fromPath) <$> toPath x
    LogoWhitePng -> (setFileExt (Just "png") . fromPath) <$> toPath x
    IconPng      -> (setFileExt (Just "png") . fromPath) <$> toPath x
    IconSvg      -> (setFileExt (Just "svg") . fromPath) <$> toPath x
