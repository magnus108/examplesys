module Piece.Effects.Change
  ( MonadChanges,
    listen,
    MonadRead,
    read,
  )
where

import Control.Concurrent (Chan, writeChan)
import GHC.IO (unsafeInterleaveIO)
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db

class Monad m => MonadChanges m where
  listen :: UI.Window -> Chan (IO ()) -> String -> m ()

instance MonadChanges Monad.App where
  listen = listenImpl
  {-# INLINE listen #-}

listenImpl :: (MonadIO m, Env.WithLoanEnv env m) => UI.Window -> Chan (IO ()) -> String -> m ()
listenImpl window chan datastoreLoan = do
  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  traceShowM "gggggaaaa"
  runM chan window $ UI.onChanges bDatabaseLoan $ do
    traceShowM "ggggg"
    Db.writeJson datastoreLoan

runM :: (MonadIO m) => Chan (IO ()) -> UI.Window -> UI.UI a -> m a
runM chan window ui = do
  i <- liftIO $ UI.runUI window ui
  liftIO $ writeChan chan $ do
    let a = (const ()) $! i
    return a
  return i

class Monad m => MonadRead m where
  read :: String -> m (Db.Database Loan.Loan)

instance MonadRead Monad.App where
  read = readImpl
  {-# INLINE read #-}

readImpl :: (MonadIO m) => String -> m (Db.Database Loan.Loan)
readImpl datastoreLoan = do
  databaseLoan <- Db.readJson datastoreLoan
  return databaseLoan
