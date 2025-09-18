-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
module Supabase.Miso.Core
  ( -- * Functions
    runSupabase
  , runSupabaseFrom
  , runSupabaseQuery
  , emptyArgs
  , successCallback
  , successCallbackFile
  , errorCallback
  ) where
-----------------------------------------------------------------------------
import Data.Aeson as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (isJust)
import Miso.String
import Miso.FFI (syncCallback1, File)
-----------------------------------------------------------------------------
import Control.Monad
import Language.Javascript.JSaddle hiding (Success)
import Control.Monad.Writer.Lazy
-----------------------------------------------------------------------------
-- | runSupabase('auth','signUp', args, successCallback, errorCallback);
runSupabase
  :: ToJSVal args
  => MisoString
  -- ^ Namespace
  -> MisoString
  -- ^ Method
  -> [args]
  -- ^ args
  -> Function
  -- ^ successful callback
  -> Function
  -- ^ errorful callback
  -> JSM ()
runSupabase namespace fnName args successful errorful = do
  args_ <- makeArgs args
  void $ jsg "globalThis" # "runSupabase" $
    (namespace, fnName, args_, successful, errorful)
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | runSupabase('auth','signUp', args, successCallback, errorCallback);
runSupabaseQuery
  :: ToJSVal args
  => MisoString
  -- ^ From
  -> MisoString
  -- ^ Method
  -> [args]
  -- ^ args
  -> Function
  -- ^ successful callback
  -> Function
  -- ^ errorful callback
  -> JSM ()
runSupabaseQuery from fnName args successful errorful = do
  args_ <- makeArgs args
  void $ jsg "globalThis" # "runSupabaseQuery" $
    (from, fnName, args_, successful, errorful)
-----------------------------------------------------------------------------
-- | runSupabase('auth','signUp', args, successCallback, errorCallback);
runSupabaseFrom
  :: ToJSVal args
  => MisoString
  -- ^ Namespace
  -> MisoString
  -- ^ From
  -> MisoString
  -- ^ Method
  -> [args]
  -- ^ args
  -> Function
  -- ^ successful callback
  -> Function
  -- ^ errorful callback
  -> JSM ()
runSupabaseFrom namespace from fnName args successful errorful = do
  args_ <- makeArgs args
  void $ jsg "globalThis" # "runSupabaseFrom" $
    (namespace, from, fnName, args_, successful, errorful)
-----------------------------------------------------------------------------
emptyArgs :: [JSVal]
emptyArgs = []
-----------------------------------------------------------------------------
successCallback
  :: FromJSON t
  => (action -> JSM ())
  -> (MisoString -> action)
  -> (t -> action)
  -> JSM Function
successCallback sink errorful successful = do
  syncCallback1 $ \result -> do
    fromJSON <$> fromJSValUnchecked result >>= \case
      Error msg -> do
        sink $ errorful (ms msg)
      Success result ->
        sink (successful result)
-----------------------------------------------------------------------------
successCallbackFile
  :: (action -> JSM ())
  -> (MisoString -> action)
  -> (File -> action)
  -> JSM Function
successCallbackFile sink errorful successful = do
  syncCallback1 $ \result -> do
    fromJSValUnchecked result >>= sink . successful
-----------------------------------------------------------------------------
errorCallback
  :: (action -> JSM ())
  -> (MisoString -> action)
  -> JSM Function
errorCallback sink errorful =
  syncCallback1 $ \result -> do
    fromJSON <$> fromJSValUnchecked result >>= \case
      Error msg -> do
        sink $ errorful (ms msg)
      Success result ->
        sink (errorful result)
-----------------------------------------------------------------------------

