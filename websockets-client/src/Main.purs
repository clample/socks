module Main where

import Prelude
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Log as Log
import WebSocket as WS
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
    
-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer
  :: forall eff
   . WS.Connection -> CR.Producer String (Aff (avar :: AVAR, err :: EXCEPTION, ws :: WS.WEBSOCKET | eff)) Unit
wsProducer (WS.Connection socket) = CRA.produce \emit -> do
  
  socket.onmessage $= \event -> do
    emit $ Left $ WS.runMessage (WS.runMessageEvent event)

  -- This part would be unnecessary in the real world, but since we're just
  -- using the echo service we need to send something on init so that we have
  -- something to receive!
  socket.onopen $= \_ -> do
    socket.send (WS.Message "hello")
    socket.send (WS.Message "something")
    socket.send (WS.Message "goodbye")

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `AddMessage` queries in when it receives inputs from the
-- producer.
wsConsumer
  :: forall eff
   . (Log.Query ~> Aff (HA.HalogenEffects eff))
  -> CR.Consumer String (Aff (HA.HalogenEffects eff)) Unit
wsConsumer query = CR.consumer \msg -> do
  query $ H.action $ Log.AddMessage msg
  pure Nothing

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

main :: Eff (HA.HalogenEffects (ws :: WS.WEBSOCKET, console :: CONSOLE)) Unit
main = do
  connection <- WS.newWebSocket (WS.URL "ws://127.0.0.1:1855") []
  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- runUI Log.component unit body
  
    io.subscribe $ messageSender connection
    -- Connecting the consumer to the producer initializes both, opening the
    -- websocket connection and feeding queries back to our component as messages
    -- are received.
    CR.runProcess (wsProducer connection CR.$$ wsConsumer io.query)
