-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE FlexibleInstances          #-}
-----------------------------------------------------------------------------
module Supabase.Miso.Core
  ( -- * Functions
    runSupabase
  , runSupabaseFrom
  , emptyArgs
  , successCallback
  , successCallbackFile
  , errorCallback
  , emptyOptions
  , (.+)
  , Opts
  , toOpts
  ) where
-----------------------------------------------------------------------------
import Data.Aeson as Aeson
import Data.Aeson.KeyMap qualified as KM
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

emptyOptions :: Value
emptyOptions = Aeson.Object KM.empty

(.+) :: ToJSON a => Value -> (KM.Key, a) -> Value
(.+) opts (k, v) =
  let v' = toJSON v
  in case opts of
    Aeson.Object o  -> Aeson.Object (KM.insert k v' o)
    _               -> Aeson.Object (KM.singleton k v')

-------------------------------------------------------------------------------

newtype Opts = Opts { unOpts :: Value }
  deriving (Eq, ToJSVal)

instance Semigroup Opts where
  Opts (Aeson.Object km1) <> Opts (Aeson.Object km2) = Opts $ Aeson.Object (KM.union km1 km2)
  Opts (Aeson.Object km1) <> _ = Opts $ Aeson.Object km1
  _ <> Opts (Aeson.Object km2) = Opts $ Aeson.Object km2
  _ <> _ = mempty

instance Monoid Opts where
  mempty = Opts (Aeson.Object KM.empty)

toOpts :: ToJSON a => Aeson.Key -> a -> Opts
toOpts k v = Opts $ Aeson.Object $ KM.singleton k (toJSON v)

-- opts1 :: Opts
-- opts1 = toOpts "limit" 1  <> toOpts "search" "windsurf"

-------------------------------------------------------------------------------

type Opts2 = Writer Opts ()

toOpts2 :: ToJSON a => Key -> a -> Opts2
toOpts2 k v = tell $ toOpts k v

opts2 :: Opts2
opts2 = do
  toOpts2 "limit" 10
  toOpts2 "search" "windsurf"

runOpts2 :: Opts2 -> Aeson.Value
runOpts2 = unOpts . execWriter

