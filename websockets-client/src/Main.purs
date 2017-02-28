module Main where

import Prelude
import Control.Coroutine as CR
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Var (($=))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Log as Log
import WebSocket as WS
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)

messageSender
  :: forall eff
   . WS.Connection
  -> CR.Consumer Log.Message (Aff (HA.HalogenEffects (console :: CONSOLE, ws :: WS.WEBSOCKET | eff))) Unit
messageSender (WS.Connection socket) = CR.consumer \msg -> do
  case msg of
    Log.SendMessage' msg' -> do
      log $ "Sending message " <> msg'
      liftEff $ socket.send (WS.Message msg')
  pure Nothing


messageListener :: forall eff
     . WS.Connection
    -> (Log.Query ~> Aff (HA.HalogenEffects ( ws :: WS.WEBSOCKET | eff)))
    -> Eff (HA.HalogenEffects (ws :: WS.WEBSOCKET | eff)) Unit
messageListener (WS.Connection socket) query =
  socket.onmessage $= \event -> do
    let msg = WS.runMessage <<< WS.runMessageEvent $ event
    HA.runHalogenAff <<< query <<< H.action <<< Log.AddMessage $  msg


main :: Eff (HA.HalogenEffects (ws :: WS.WEBSOCKET, console :: CONSOLE)) Unit
main = do
  connection <- WS.newWebSocket (WS.URL "ws://127.0.0.1:1855") []
  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- runUI Log.component unit body
  
    io.subscribe $ messageSender connection
    liftEff $ messageListener connection io.query       
