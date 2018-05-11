{-# LANGUAGE CPP #-}
module Pure.Data.Time (module Export) where

#ifdef __GHCJS__
import Pure.Data.Time.GHCJS as Export
#else
import Pure.Data.Time.GHC as Export
#endif
