{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE MultilineStrings          #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE MultilineStrings          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE CPP                       #-}
module Main where

import           Control.Monad
import           Data.Aeson hiding (Object)
import           Data.Proxy
import           GHC.Records
import           GHC.TypeLits
import           Language.Javascript.JSaddle
import           Miso
import qualified Miso.Html.Element as H

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run $ startApp (component () Main.update Main.view)
#ifndef WASM
  { scripts =
    [ Module 
       """
       import { createClient } from 'https://cdn.jsdelivr.net/npm/@supabase/supabase-js/+esm'
       const supabase = createClient('https://bufjmcerlanfijatbwxu.supabase.co', 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImJ1ZmptY2VybGFuZmlqYXRid3h1Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NTUzMTgwNzgsImV4cCI6MjA3MDg5NDA3OH0.Cv8xhZyZTQuLVphhQ-fxbf6C4zTzu85I7leNyTMggKU')
       globalThis['supabase'] = supabase;
       console.log('Supabase Instance: ', supabase)
       """
    ]
  }
#endif

data Action = NoOp | Connect
type Model = ()

update
  :: Action
  -> Transition () Action
update NoOp =
  pure ()
update Connect =
  io_ (consoleLog "connecting")

view :: () -> View model action
view () = H.div_ [ ] [ "foo" ]

data Direction = Asc | Desc

data Query a where
  BIND :: Query a -> (a -> Query b) -> Query b
  PURE :: a -> Query a
  SELECT :: [Field] -> Query ()
  FROM :: Query (Proxy p)
  WHERE :: [Where] -> Query ()
  ORDER :: MisoString -> Direction -> Query ()
  LIMIT :: Int -> Query ()
  RANGE :: Int -> Int -> Query ()
  ABORTSIGNAL :: Query ()
  SINGLE :: Query ()
  MAYBESINGLE :: Query ()
  CSV :: Query ()
  EXPLAIN :: Query ()

data Where
  = GTE MisoString Value
  | GT MisoString Value
  | LTE MisoString Value
  | LT MisoString Value
  | EQ MisoString Value
  | NEQ MisoString Value
  | LIKE MisoString Value
  | ILIKE MisoString Value
  | IS MisoString Value
  | IN MisoString Value
  | CONTAINS MisoString Value
  | CONTAINEDBY MisoString Value
  | RANGEGT MisoString Value
  | RANGEGTE MisoString Value
  | RANGELT MisoString Value
  | RANGELTE MisoString Value
  | RANGEADJACENT MisoString Value
  | OVERLAPS MisoString Pattern Object Value
  | MATCH Value
  | NOT MisoString MisoString Value

type Pattern = MisoString


data Person
  = Person
  { name :: String
  , age :: Int
  }

field
  :: forall name field person
   . KnownSymbol name
  => HasField name person field
  => Proxy person
  -> Field
field Proxy = Field $ ms $ symbolVal (Proxy @name)

newtype Field = Field MisoString

upsert_
  :: ToJSON value
  => MisoString
  -- UPSERT INTO
  -> [value]
  -- VALUES
  -> JSM ()
upsert_ = undefined

insert_
  :: ToJSON value
  => MisoString
  -- INSERT INTO
  -> [value]
  -- VALUES
  -> JSM ()
insert_ = undefined

delete_
  -- ^ DELETE
  :: Proxy a
  -- ^ FROM
  -> [Where]
  -- ^ WHERE
  -> JSM ()
delete_ = undefined

update_ :: MisoString -> [Where] -> [Where] -> JSM ()
update_ = undefined

adults :: Query ()
adults = do
  person <- from @Person
  select
    [ field @"name" person
    ]
  where_
    [ gte @"age" (21 :: Int) person
    ]

k :: JSVal -> JSM JSVal
k supabase = foldl1 (<=<)
  [] supabase

compileFrom :: MisoString -> JSVal -> JSM JSVal
compileFrom names o = o # ("from" :: MisoString) $ [names]

gte
  :: forall (name :: Symbol) person field
   . (KnownSymbol name, ToJSON field, HasField name person field)
  => field
  -> Proxy person
  -> Where
gte field_ Proxy =
  GTE (ms (symbolVal (Proxy @name))) (toJSON field_)

select :: [Field] -> Query ()
select = SELECT

from :: forall person . Query (Proxy person)
from = FROM

where_ :: [Where] -> Query ()
where_ = WHERE

instance Functor Query where
  fmap = liftM

instance Applicative Query where
  pure = PURE
  (<*>) = ap

instance Monad Query where
  (>>=) = BIND
