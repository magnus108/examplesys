module Piece.Effects.Token
  ( validate,
  )
where

import qualified Control.Monad.IO.Unlift as MonadUnliftIO
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import qualified Data.Time.LocalTime as Time
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Time as Time
import qualified Piece.Core.Token as Token
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Token as Token
import qualified Piece.Effects.Time as Time
import Reactive.Threepenny (onChange)
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe

validate :: (MonadUnliftIO.MonadUnliftIO m, MonadIO m, Time.MonadParseTime m, Env.WithTokenEnv env m) => m (R.Behavior (Maybe Time.UTCTime))
validate = do
  {-
  (e, h) <- liftIO $ R.newEvent
  tokenEnv <- Has.grab @Env.TokenEnv
  let bSelectionToken = Env.bSelectionToken tokenEnv
  lookup <- Token.lookup
  let bToken = (=<<) <$> lookup <*> bSelectionToken
  MonadUnliftIO.withRunInIO $ \run -> R.onChange bToken $ \token -> run $ do
    parse <- mapM (Time.parseTime . Token.time) token
    liftIO $ h (parse)

  bParseTime <- R.stepper Nothing $ Unsafe.head <$> R.unions [e]
  let bDiffTime = liftA2 Time.diffUTCTime <$> bParseTime

  let difft = Env.bTTL tokenEnv
  let it = pure (\x y z -> maybe False (x >) (liftA2 Time.diffUTCTime y z)) :: R.Behavior (Time.NominalDiffTime -> Maybe Time.UTCTime -> Maybe Time.UTCTime -> Bool)
  -}

  return undefined -- bParseTime
