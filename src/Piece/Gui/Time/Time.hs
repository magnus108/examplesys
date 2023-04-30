{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Time.Time
  ( setup,
    Time,
    userText,
    errText,
  )
where

import Control.Exception (try)
import qualified Data.Map as Map
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as Elements
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Widgets as UI
import qualified Graphics.UI.Threepenny.Widgets as Widgets
import qualified Piece.App.Env as Env
import Piece.App.Monad (runApp)
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Config as Config
import qualified Piece.Core.Time as Time (Time, unTime)
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Token as Token
import qualified Piece.Effects.Read as Read
import qualified Piece.Effects.Time as Time
import qualified Piece.Effects.Write as Write
import qualified Piece.Gui.Loan.Behavior as Behavior
import qualified Piece.Gui.Loan.Create as LoanCreate
import qualified Piece.Gui.Tab.Tab as Tab
import qualified Piece.Time.Time as Time
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import qualified UnliftIO as UnliftIO

data Time = Time
  { _view :: UI.Element,
    _userTE :: UI.Tidings Time.Time,
    _errTE :: UI.Tidings (Maybe String)
  }

instance UI.Widget Time where
  getElement = _view

userText :: Time -> UI.Tidings Time.Time
userText = _userTE

errText :: Time -> UI.Tidings (Maybe String)
errText = _errTE

setup :: Monad.AppEnv -> R.Behavior (Maybe String) -> R.Behavior Time.Time -> UI.UI Time
setup env berr bsucc = mdo
  textEntry <- UI.entry (Time.formatTime Time.defaultTimeLocale "%F, %T" . Time.unTime <$> bsucc)

  _err <- UI.div UI.# UI.sink UI.text (fromMaybe "" <$> berr)
  _view <- Elements.div UI.# UI.set UI.children [UI.getElement textEntry, _err]

  eParse <- liftIO $ Monad.runApp env $ UnliftIO.withRunInIO $ \run -> do
    let e = R.unsafeMapIO (run . Error.tryError . Time.parseTime "%F, %T") eEntry
    return e

  let tEntry = UI.userText textEntry
      eEntry = UI.rumors tEntry
      (eErr, eSucc) = UI.split eParse
      _userTE = UI.tidings bsucc eSucc
      _errTE = UI.tidings berr $ Just . show <$> eErr

  return Time {..}
