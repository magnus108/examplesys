{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Piece.Db.Token.Tests
  ( tests,
  )
where

import qualified Data.Time as Time
import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer as CakeSlayer
import qualified Piece.Core.Loan as Loan
import qualified Piece.Core.Time as Time
import qualified Piece.Core.Token as Token
import qualified Piece.Db.Db as DB
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Token as Token
import qualified Piece.Effects.Time as Time
import qualified Reactive.Threepenny as R
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Mock
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Piece.Db.Token.Tests" $
    concat
      [ fromAssertions
          "lookup"
          [ lookupToken
          ],
        fromAssertions
          "getTime"
          [getTime],
        fromAssertions
          "validate"
          [ valid,
            invalid,
            invalid2
          ]
      ]
  where
    lookupToken = do
      now <- runMockApp $ do
        Time.currentTime
      lookup <-
        runMockApp $
          local
            ( const
                ( MockEnv
                    { tokenEnv =
                        Env.TokenEnv
                          { bDatabaseToken = pure (Db.create (Token.token 0 now) Db.empty),
                            bSelectionToken = pure (Just 0),
                            bTTL = pure (Just (Time.secondsToNominalDiffTime 500))
                          }
                    }
                )
            )
            ( do
                x <- Token.lookup
                R.currentValue (x ?? 0)
            )
      let value = Just (Token.token 0 now)
      value @=? lookup

    getTime = do
      now <- runMockApp $ do
        Time.currentTime
      lookup <-
        runMockApp $
          local
            ( const
                ( MockEnv
                    { tokenEnv =
                        Env.TokenEnv
                          { bDatabaseToken = pure (Db.create (Token.token 0 now) Db.empty),
                            bSelectionToken = pure (Just 0),
                            bTTL = pure (Just (Time.secondsToNominalDiffTime 500))
                          }
                    }
                )
            )
            ( do
                x <- Token.getTime
                R.currentValue (x ?? 0)
            )
      let value = Just now
      value @=? lookup

    valid = do
      now <- runMockApp $ do
        Time.currentTime
      lookup <-
        runMockApp $
          local
            ( const
                ( MockEnv
                    { tokenEnv =
                        Env.TokenEnv
                          { bDatabaseToken = pure (Db.create (Token.token 0 now) Db.empty),
                            bSelectionToken = pure (Just 0),
                            bTTL = pure (Just (Time.secondsToNominalDiffTime 500))
                          }
                    }
                )
            )
            ( do
                x <- Token.validate
                R.currentValue (x ?? now)
            )
      let value = Right 0
      value @=? lookup

    invalid = do
      now <- runMockApp $ do
        Time.currentTime
      lookup <-
        runMockApp $
          local
            ( const
                ( MockEnv
                    { tokenEnv =
                        Env.TokenEnv
                          { bDatabaseToken = pure (Db.create (Token.token 0 now) Db.empty),
                            bSelectionToken = pure (Just 0),
                            bTTL = pure (Just (Time.secondsToNominalDiffTime 500))
                          }
                    }
                )
            )
            ( do
                f <- Token.validate
                R.currentValue (f ?? (Time.time (Time.addUTCTime (Time.secondsToNominalDiffTime 1000) (Time.unTime now))))
            )
      let value = Left ()
      value @=? lookup

    invalid2 = do
      now <- runMockApp $ do
        Time.currentTime
      lookup <-
        runMockApp $
          local
            ( const
                ( MockEnv
                    { tokenEnv =
                        Env.TokenEnv
                          { bDatabaseToken = pure (Db.create (Token.token 0 now) Db.empty),
                            bSelectionToken = pure Nothing,
                            bTTL = pure (Just (Time.secondsToNominalDiffTime 500))
                          }
                    }
                )
            )
            ( do
                f <- Token.validate
                R.currentValue (f ?? now)
            )
      let value = Left ()
      value @=? lookup
