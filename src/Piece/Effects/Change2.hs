module Piece.Effects.Change2
  ( readImpl,
    Monad,
    MonadRead,
    read,
    -- listenImpl,
    -- MonadChanges,
    -- listen,
  )
where

import qualified Control.Lens.Operators as LOperators
import qualified Control.Monad.Except as Except
import GHC.IO.Exception (IOError)
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env2 as Env
import qualified Piece.App.Error2 as Error
import qualified Piece.App.Monad2 as Monad
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

{-
class Monad m => MonadChanges m where
  listen :: String -> m ()

instance MonadChanges Monad.App where
  listen = listenImpl
  {-# INLINE listen #-}

listenImpl ::
  ( MonadReader r m,
    Env.HasLoanBehavior r,
    UI.MonadUI m
  ) =>
  String ->
  m ()
listenImpl datastoreLoan = do
  r <- ask
  let bDatabaseLoan = r LOperators.^. Env.loanBehavior . Env.bDatabaseLoan
  window <- UI.liftUI UI.askWindow
  -- SHOULD HAVE HAD UNLIFTIO
  -- VI SKAL LAVE EN KØ DER FIXER DEM HER.
  UI.liftUI $ UI.liftIOLater $ R.onChange bDatabaseLoan $ \s -> UI.runUI window $ do
    Db.writeJson datastoreLoan s
    -}

class Monad m => MonadRead m where
  read :: String -> m (Db.Database Loan.Loan)

instance MonadRead Monad.App where
  read = readImpl
  {-# INLINE read #-}

readImpl ::
  ( MonadIO m,
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
