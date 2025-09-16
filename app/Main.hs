
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

import Data.Aeson hiding ((.=))
import Miso
import Miso.CSS qualified as CSS
import Miso.Lens
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P

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
  | ActionHandle Value
  | ActionAskListBuckets 

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect parent Model Action
updateModel = \case

  ActionError errorMessage -> do
    modelData .= " "
    modelError .= errorMessage
    io_ $ consoleError errorMessage

  ActionHandle v -> do
    let msg = ms $ show v
    modelError .= " "
    modelData .= msg
    io_ $ consoleLog msg

  ActionAskListBuckets ->
    listBuckets ActionHandle ActionError

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel Model{..} = div_ []
  [ h2_ [] [ "Storage" ]
  , p_ [] [ button_ [ onClick ActionAskListBuckets ] [ "listBuckets" ] ]
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

main :: IO ()
main = 
  run $ startApp (component mkModel updateModel viewModel)
#ifndef WASM
    { scripts =
       [ Module
          """

          import { createClient } from 'https://cdn.jsdelivr.net/npm/@supabase/supabase-js/+esm'
          const supabase_url = 'https://cmeicmtkrdbrelovyssz.supabase.co';
          const supabase_key = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImNtZWljbXRrcmRicmVsb3Z5c3N6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NTY0NTM2MTMsImV4cCI6MjA3MjAyOTYxM30._ga2HbuYt8JJTKYEQZc5ACAP2VT3KyjcbbV1Og0wEG0' ;
          const supabase = createClient(supabase_url, supabase_key);
          globalThis['supabase'] = supabase;
          console.log('Supabase Instance: ', supabase)

          // dmj: usage like: runSupabase('auth','signUp', args, successCallback, errorCallback);
          globalThis['runSupabase'] = function (namespace, fnName, args, successful, errorful) {
            const p = ({ data, error }) => {
                if (data) successful(data);
                if (error) errorful(error);
              };
            if (Array.isArray(args) && !args.length>0) {
              globalThis['supabase'][namespace][fnName](args).then(p);
            } else {
              globalThis['supabase'][namespace][fnName]().then(p);
            }
          }

          globalThis['runSupabaseFrom'] = function (namespace, fromArg, fnName, args, successful, errorful) {
            const p = ({ data, error }) => {
                if (data) successful(data);
                if (error) errorful(error);
              };
            if (Array.isArray(args) && args.length>0) {
              globalThis['supabase'][namespace].from(fromArg)[fnName](args).then(p);
            } else {
              globalThis['supabase'][namespace].from(fromArg)[fnName]().then(p);
            }
          }

          """
       ]
    }
#endif

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif


