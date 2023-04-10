module Piece.Effects.Change2
  ( readImpl,
    Monad,
    MonadRead,
    read,
  )
where

import qualified Control.Lens.Operators as LOperators
import qualified Control.Monad.Except as Except
import qualified Piece.App.Env2 as Env
import qualified Piece.App.Error2 as Error
import qualified Piece.App.Monad2 as Monad
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db

{-
class Monad m => MonadChanges m where
  listen :: String -> m ()

instance MonadChanges Monad.App where
  listen = listenImpl
  {-# INLINE listen #-}

listenImpl :: (UI.MonadUI m, MonadIO m, Env.WithLoanEnv env m) => String -> m ()
listenImpl datastoreLoan = do
  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  window <- UI.liftUI UI.askWindow
  UI.liftUI $ UI.liftIOLater $ R.onChange bDatabaseLoan $ \s -> UI.runUI window $ do
    Db.writeJson datastoreLoan s
    -}

class Monad m => MonadRead m where
  read :: String -> m (Db.Database Loan.Loan)

instance MonadRead Monad.App where
  read = readImpl
  {-# INLINE read #-}

readImpl ::
  ( MonadReader r m,
    MonadIO m,
    Except.MonadError e m,
    Error.AsUserError e
  ) =>
  String ->
  m (Db.Database Loan.Loan)
readImpl datastoreLoan = do
  databaseLoan <- Db.readJson2 datastoreLoan
  case databaseLoan of
    Left x -> Except.throwError $ (Error._NotFound LOperators.#) ()
    Right x -> return x
