
{-# LANGUAGE MultilineStrings          #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

import Data.Aeson (Value)
import Miso
import Miso.CSS qualified as CSS
import Miso.Lens
import Miso.Html.Element as H
import Miso.Html.Event as E
-- import Miso.Html.Property as P

import Supabase.Miso.Storage

-------------------------------------------------------------------------------
-- model
-------------------------------------------------------------------------------

data Model = Model
  { _modelData :: MisoString
  , _modelError :: MisoString
  } deriving (Eq)

mkModel :: Model
mkModel = Model " " " "

modelData :: Lens Model MisoString
modelData = lens _modelData (\ record field -> record {_modelData = field})

modelError :: Lens Model MisoString
modelError = lens _modelError (\ record field -> record {_modelError = field})

-------------------------------------------------------------------------------
-- action
-------------------------------------------------------------------------------

data Action
  = ActionError MisoString
  | ActionHandleValue Value
  | ActionHandleValues [Value]
  | ActionAskListBuckets 
  | ActionAskListAllFiles MisoString SearchOptions

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect parent Model Action
updateModel = \case

  ActionError errorMessage -> do
    modelData .= " "
    modelError .= errorMessage
    io_ $ consoleError errorMessage

  ActionHandleValue v -> do
    let msg = ms $ show v
    modelError .= " "
    modelData .= msg
    io_ $ consoleLog msg

  ActionHandleValues vs -> do
    let msg = ms $ show vs
    modelError .= " "
    modelData .= msg
    io_ $ consoleLog msg

  ActionAskListBuckets ->
    listBuckets ActionHandleValues ActionError

  ActionAskListAllFiles fp opts ->
    listAllFiles "avatars" fp opts ActionHandleValues ActionError

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel Model{..} = div_ []
  [ h2_ [] [ "Storage" ]
  , p_ [] 
      [ button_ [ onClick ActionAskListBuckets ] [ "listBuckets" ]
      ]
  , p_ [] 
      [ button_ 
          [ onClick (ActionAskListAllFiles "" mempty) ]
          -- [ onClick (ActionAskListAllFiles "" mempty) ]
          [ "listAllFiles '' mempty" ]
      , button_ 
          [ onClick (ActionAskListAllFiles "test" mempty) ]
          [ "listAllFiles 'test' mempty" ]
      , button_ 
          -- [ onClick (ActionAskListAllFiles "" (limit 10)) ]
          [ onClick (ActionAskListAllFiles "" (limit 10 <> search "windsurf")) ]
          [ "listAllFiles ' {limit: 10, search: 'windsurf'}" ]
      ]
  , p_ []
      [ "data: "
      , pre_ 
          [ CSS.style_
              [ CSS.border "1px solid black"
              , CSS.backgroundColor #dddddd
              , CSS.width "600px"
              , CSS.whiteSpace "pre-wrap"
              ]
          ]
          [ code_ [] [ text _modelData ] ]
      , "error: "
      , pre_ 
          [ CSS.style_
              [ CSS.border "1px solid black"
              , CSS.backgroundColor #ffcccc
              , CSS.width "600px"
              , CSS.whiteSpace "pre-wrap"
              ]
          ]
          [ code_ [] [ text _modelError ] ]
      ]
  ]

-------------------------------------------------------------------------------
--  main
-------------------------------------------------------------------------------

#ifndef WASM
main :: IO ()
main = do
  supabasemisojs <- ms <$> readFile "js/supabase-miso.js"
  run $ startApp (component mkModel updateModel viewModel)
    { scripts =
       [ Module
          """
          import { createClient } from 'https://cdn.jsdelivr.net/npm/@supabase/supabase-js/+esm'
          const supabase_url = 'https://cmeicmtkrdbrelovyssz.supabase.co';
          const supabase_key = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImNtZWljbXRrcmRicmVsb3Z5c3N6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NTY0NTM2MTMsImV4cCI6MjA3MjAyOTYxM30._ga2HbuYt8JJTKYEQZc5ACAP2VT3KyjcbbV1Og0wEG0' ;
          const supabase = createClient(supabase_url, supabase_key);
          globalThis['supabase'] = supabase;
          console.log('Supabase Instance: ', supabase)
          """
       , Script supabasemisojs
       ]
    }
#else
main :: IO ()
main = run $ startApp (component mkModel updateModel viewModel)
#endif

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif


