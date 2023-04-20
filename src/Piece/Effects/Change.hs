module Piece.Effects.Change
  ( MonadChanges (..),
  )
where

import qualified Control.Monad.IO.Unlift as Unlift
import qualified Piece.App.Env as Env
import qualified Piece.App.Error as E
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

class Monad m => MonadChanges m where
  listen :: String -> m ()

instance MonadChanges Monad.App where
  listen = listenImpl
  {-# INLINE listen #-}

listenImpl ::
  ( E.As err E.UserError,
    E.WithError err m,
    Unlift.MonadUnliftIO m,
    Env.WithLoanEnv env m
  ) =>
  String ->
  m ()
listenImpl datastoreLoan = do
  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  Unlift.withRunInIO $ \run -> do
    R.onChange bDatabaseLoan $ \s -> do
      -- TODO Not idemnpotent. this is good for atleast once read systems. but bad for test
      dataWrite <- Db.writeJson2 datastoreLoan s
      case dataWrite of
        Left _ -> run $ Error.throwError (E.as E.NotFound)
        Right y -> return y
