-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
module Supabase.Miso.Storage
  ( -- * Functions
    createBucket
  , getBucket
  , listBuckets
  , updateBucket
  , deleteBucket
  , emptyBucket
  , uploadFile
  , downloadFile
  , listAllFiles
  , replaceFile
  , moveFile
  , copyFile
  , deleteFiles
  , createSignedUrl
  , createSignedUrls
  , createSignedUploadUrl
  , uploadToSignedUrl
  , getPublicUrl
    -- * Types
  , SortBy
  , column
  , order
  , SearchOptions
  , limit
  , offset
  , search 
  , sortBy
  , FileObject
  ) where
-----------------------------------------------------------------------------
import           Control.Applicative
import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           Data.HashMap.Strict (HashMap)
import           Data.Time
import           Data.Aeson
import           Control.Monad
import           Language.Javascript.JSaddle hiding (Success)
import           Miso hiding ((<#), offset)
import           Miso.FFI
-----------------------------------------------------------------------------
import           Supabase.Miso.Core
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-createbucket
createBucket
  :: MisoString
  -- ^ Bucket identifier
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
createBucket args opts successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  args_ <- toJSVal args
  opts_ <- toJSVal opts
  runSupabase "storage" "createBucket" [args_, opts_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-getbucket
getBucket
  :: MisoString
  -- ^ Bucket identifier
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
getBucket args successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  args_ <- toJSVal args
  runSupabase "storage" "getBucket" [args_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-listbuckets
listBuckets
  :: ([Value] -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
listBuckets successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  runSupabase "storage" "listBuckets" emptyArgs successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-updatebucket
updateBucket
  :: MisoString
  -- ^ Bucket identifier
  -> Value
  -- ^ options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
updateBucket args opts successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  args_ <- toJSVal args
  opts_ <- toJSVal opts
  runSupabase "storage" "updateBucket" [args_, opts_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-deletebucket
deleteBucket
  :: MisoString
  -- ^ Bucket identifier
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
deleteBucket args successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  args_ <- toJSVal args
  runSupabase "storage" "deleteBucket" [args_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-emptybucket
emptyBucket
  :: MisoString
  -- ^ Bucket identifier
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
emptyBucket args successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  args_ <- toJSVal args
  runSupabase "storage" "emptyBucket" [args_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-uploadfile
uploadFile
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ The file name
  -> File
  -- ^ The file to upload
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
uploadFile bucket fileName file options successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  options_ <- toJSVal options
  file_ <- toJSVal file
  fileName_ <- toJSVal fileName
  runSupabaseFrom "storage" bucket "upload" [fileName_, file_, options_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-downloadfile
downloadFile
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ The file name
  -> Value
  -- ^ Options
  -> (File -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Errorful
  -> Effect parent model action
downloadFile bucket fileName opts successful errorful = withSink $ \sink -> do
  successful_ <- successCallbackFile sink errorful successful
  errorful_ <- errorCallback sink errorful
  fileName_ <- toJSVal fileName
  opts_ <- toJSVal opts
  runSupabaseFrom "storage" bucket "download" [fileName_, opts_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-list
data SortBy = SortBy
  { _column :: Maybe MisoString
  , _order :: Maybe MisoString
  } 
-----------------------------------------------------------------------------
instance ToJSVal SortBy where
  toJSVal SortBy{..} = do
    o <- create
    forM_ _column $ \x -> set "column" x o
    forM_ _order $ \x -> set "order" x o
    toJSVal o
-----------------------------------------------------------------------------
instance Semigroup SortBy where
  SortBy c1 o1 <> SortBy c2 o2 = SortBy (c2 <|> c1) (o2 <|> o1)
-----------------------------------------------------------------------------
instance Monoid SortBy where
  mempty = SortBy Nothing Nothing
-----------------------------------------------------------------------------
column, order :: MisoString -> SortBy
column x = mempty { _column = Just x }
order x = mempty { _order = Just x }
-----------------------------------------------------------------------------
data SearchOptions = SearchOptions
  { _limit :: Maybe Int
  , _offset :: Maybe Int
  , _search :: Maybe MisoString
  , _sortBy :: Maybe SortBy
  }
-----------------------------------------------------------------------------
instance ToJSVal SearchOptions where
  toJSVal SearchOptions{..} = do
    o <- create
    forM_ _limit $ \x -> set "limit" x o
    forM_ _offset $ \x -> set "offset" x o
    forM_ _search $ \x -> set "search" x o
    forM_ _sortBy $ \x -> set "sortBy" x o
    toJSVal o
-----------------------------------------------------------------------------
instance Semigroup SearchOptions where
  SearchOptions l1 o1 s1 sb1 <> SearchOptions l2 o2 s2 sb2 =
    SearchOptions (l2 <|> l1) (o2 <|> o1) (s2 <|> s1) (sb2 <|> sb1)
-----------------------------------------------------------------------------
instance Monoid SearchOptions where
  mempty = SearchOptions Nothing Nothing Nothing Nothing
-----------------------------------------------------------------------------
limit, offset :: Int -> SearchOptions
limit x = mempty { _limit = Just x }
offset x = mempty { _offset = Just x }
-----------------------------------------------------------------------------
search :: MisoString -> SearchOptions
search x = mempty { _search = Just x }
-----------------------------------------------------------------------------
sortBy :: SortBy -> SearchOptions
sortBy x = mempty { _sortBy = Just x }
-----------------------------------------------------------------------------

{-
data Bucket = Bucket
  { created_at :: MisoString
  , id :: MisoString
  , name :: MisoString
  , owner :: MisoString
  , public :: Bool
  , updated_at :: MisoString
  , allowed_mime_types :: [MisoString]
  , file_size_limit :: Maybe Int
  , type_ :: Maybe MisoString   -- TODO BucketType?
  } deriving (Show)

instance FromJSON Bucket where
  parseJSON = withObject "Bucket" $ \v ->
    Bucket
      <$> v .: "created_at"
      <*> v .: "id"
      <*> v .: "name"
      <*> v .: "owner"
      <*> v .: "public"
      <*> v .: "updated_at"
      <*> v .: "allowed_mime_types"
      <*> v .: "file_size_limit"
      <*> v .: "type"
-}

-----------------------------------------------------------------------------

data FileObject = FileObject
  { -- bucket_id :: MisoString
  -- , buckets :: [Bucket]
  created_at :: Maybe MisoString
  , id :: Maybe MisoString
  , last_accessed_at :: Maybe MisoString
  -- TODO , metadata :: Map MisoString Value
  , name :: MisoString
  , owner :: Maybe MisoString
  -- , updated_at :: MisoString
  } deriving (Show)

instance FromJSON FileObject where
  parseJSON = withObject "FileObject" $ \v ->
    FileObject
      -- <$> v .: "bucket_id"
      <$> v .: "created_at"
      <*> v .: "id"
      <*> v .: "last_accessed_at"
      <*> v .: "name"
      <*> (v .: "owner" <|> pure Nothing)


listAllFiles
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ The file name
  -> SearchOptions
  -- ^ Options
  -> ([FileObject] -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Errorful
  -> Effect parent model action
listAllFiles bucket fileName options successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  options_ <- toJSVal options
  fileName_ <- toJSVal fileName
  runSupabaseFrom "storage" bucket "list" [fileName_, options_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-list
replaceFile
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ The file name
  -> File
  -- ^ The file to upload
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
replaceFile bucket fileName file options successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  options_ <- toJSVal options
  file_ <- toJSVal file
  fileName_ <- toJSVal fileName
  runSupabaseFrom "storage" bucket "update" [fileName_, file_, options_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-move
moveFile
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ Target file
  -> MisoString
  -- ^ New file
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
moveFile bucket target destination opts successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  target_ <- toJSVal target
  destination_ <- toJSVal destination
  opts_ <- toJSVal opts
  runSupabaseFrom "storage" bucket "move" [target_, destination_, opts_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-copy
copyFile
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ Target file
  -> MisoString
  -- ^ New file
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
copyFile bucket target destination opts successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  target_ <- toJSVal target
  destination_ <- toJSVal destination
  opts_ <- toJSVal opts
  runSupabaseFrom "storage" bucket "copy" [target_, destination_, opts_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-remove
deleteFiles
  :: MisoString
  -- ^ Bucket identifier
  -> [MisoString]
  -- ^ Files to remove
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
deleteFiles bucket files successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  files_ <- toJSVal files
  runSupabaseFrom "storage" bucket "remove" [files_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-createsignedurl
createSignedUrl
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ File to sign
  -> Int
  -- ^ Expiration time
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
createSignedUrl bucket file expiry successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  file_ <- toJSVal file
  expiry_ <- toJSVal expiry
  runSupabaseFrom "storage" bucket "createSignedUrl" [file_, expiry_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-createsignedurls
createSignedUrls
  :: MisoString
  -- ^ Bucket identifier
  -> [MisoString]
  -- ^ Files to sign
  -> Int
  -- ^ Expiration time
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
createSignedUrls bucket files expiry successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  files_ <- toJSVal files
  expiry_ <- toJSVal expiry
  runSupabaseFrom "storage" bucket "createSignedUrls" [files_, expiry_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-createsigneduploadurl
createSignedUploadUrl
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ File to upload
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
createSignedUploadUrl bucket file opts successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  file_ <- toJSVal file
  opts_ <- toJSVal opts
  runSupabaseFrom "storage" bucket "createSignedUploadUrl" [file_, opts_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-uploadtosignedurl
uploadToSignedUrl
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ File name
  -> MisoString
  -- ^ Token
  -> File
  -- ^ File to upload
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
uploadToSignedUrl bucket fileName token file opts successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  fileName_ <- toJSVal fileName
  token_ <- toJSVal token
  file_ <- toJSVal file
  opts_ <- toJSVal opts
  runSupabaseFrom "storage" bucket "uploadToSignedUrl" [fileName_, token_, file_, opts_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-getpublicurl
getPublicUrl
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ File name
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
getPublicUrl bucket fileName opts successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  fileName_ <- toJSVal fileName
  opts_ <- toJSVal opts
  runSupabaseFrom "storage" bucket "getPublicUrl" [fileName_, opts_] successful_ errorful_
-----------------------------------------------------------------------------

