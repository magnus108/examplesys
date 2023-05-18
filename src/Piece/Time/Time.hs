module Piece.Time.Time
  ( timer,
  )
where

import qualified Data.Time.LocalTime as Time
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Timer as UI
import qualified Piece.App.Monad as Monad
import qualified Piece.Core.Time as Time (Time)
import qualified Piece.Effects.Time as Time
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import UnliftIO

timer :: (Time.MonadTime m, MonadUnliftIO m) => UI.Window -> m (R.Event Time.Time)
timer window = do
  t <- liftIO $ UI.runUI window UI.timer
  (tE, tH) <- liftIO UI.newEvent

  _ <- UnliftIO.withRunInIO $ \run -> R.register (UI.tick t) $ \_ -> run $ do
    time <- Time.currentTime
    liftIO $ tH (Unsafe.fromJust (rightToMaybe time))
  _ <- liftIO $ UI.runUI window $ return t UI.# UI.set UI.interval 1000 UI.# UI.set UI.running True
  return tE
