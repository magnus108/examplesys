module Piece.Effects.Change
  ( MonadChanges,
    listen,
    MonadRead,
    read,
  )
where

import Control.Monad.Catch (MonadCatch, tryJust)
import GHC.IO.Exception
import GHC.IO.Exception (IOException (IOError))
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env as Env
import Piece.App.Error (AppError (..), As (..), UserError (..), WithError)
import qualified Piece.App.Monad as Monad
import Piece.CakeSlayer.Error (catchError, throwError)
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.CakeSlayer.Monad as CakeSlayer
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R
import System.IO.Error (isDoesNotExistError, isPermissionError)

class Monad m => MonadChanges m where
  listen :: String -> m ()

instance MonadChanges Monad.App where
  listen = listenImpl
  {-# INLINE listen #-}

listenImpl ::
  ( CakeSlayer.MonadUnliftUILater m,
    As err UserError,
    WithError err m,
    UI.MonadUI m,
    MonadIO m,
    Env.WithLoanEnv env m
  ) =>
  String ->
  m ()
listenImpl datastoreLoan = do
  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  window <- UI.liftUI UI.askWindow
  CakeSlayer.withRunInUILater $
    \x -> x $ liftIO $ R.onChange bDatabaseLoan $ \s ->
      UI.runUI window $ do
        dataWrite <- Db.writeJson2 datastoreLoan s
        case dataWrite of
          Left y -> x (throwError (as NotFound))
          Right y -> return y

class Monad m => MonadRead m where
  read :: String -> m (Db.Database Loan.Loan)

instance MonadRead Monad.App where
  read = readImpl
  {-# INLINE read #-}

readImpl :: (MonadIO m, As err UserError, WithError err m, MonadCatch m) => String -> m (Db.Database Loan.Loan)
readImpl datastoreLoan = do
  databaseLoan <- tryJust handleReadFile $ Db.readJson datastoreLoan
  case databaseLoan of
    Left x -> throwError (as NotFound)
    Right x -> return x
  where
    handleReadFile :: IOError -> Maybe String
    handleReadFile er
      | isDoesNotExistError er = Just "readFile: does not exist"
      | isPermissionError er = Just "readFile: permission denied"
      | otherwise = Nothing
