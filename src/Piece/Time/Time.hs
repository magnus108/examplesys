module Piece.Time.Time
  ( timer,
  )
where

import qualified Data.Time.LocalTime as Time
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Timer as UI
import qualified Piece.App.Monad as Monad
import qualified Piece.Effects.Time as Time
import qualified Reactive.Threepenny as R

timer :: Monad.AppEnv -> UI.UI (R.Event Time.ZonedTime)
timer env = do
  t <- UI.timer
  (tE, tH) <- liftIO UI.newEvent
  _ <- liftIO $ R.register (UI.tick t) $ \_ -> do
    time <- liftIO $ Monad.runApp env Time.time
    tH time
  _ <- return t UI.# UI.set UI.interval 1000 UI.# UI.set UI.running True
  return tE
