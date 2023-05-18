{-# LANGUAGE DataKinds #-}

module Piece.Db.Token.Tests
  ( tests,
  )
where

import qualified Data.Time as Time
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Time as Time
import qualified Piece.Core.Token as Token
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Token as Token
import qualified Piece.Effects.Time as Time
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
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
          [ runMockApp $ do
              hUserLogin <- Has.grab @(R.Handler (Maybe Db.DatabaseKey))
              liftIO $ hUserLogin (Just 0)
              x <- Token.lookup
              currentToken <- R.currentValue (x ?? 0)
              let value = Just 0
              liftIO $ value @=? (Token.user <$> currentToken)
          ],
        fromAssertions
          "getTime"
          [ runMockApp $ do
              hUserLogin <- Has.grab @(R.Handler (Maybe Db.DatabaseKey))
              liftIO $ hUserLogin (Just 0)
              x <- Token.getTime
              lookup <- Token.lookup
              currentToken <- R.currentValue (lookup ?? 0)
              currentTime <- R.currentValue (x ?? 0)
              let value = Token.time <$> currentToken
              liftIO $ value @=? currentTime
          ],
        fromAssertions
          "getUserId"
          [ runMockApp $ do
              hUserLogin <- Has.grab @(R.Handler (Maybe Db.DatabaseKey))
              liftIO $ hUserLogin (Just 0)
              x <- Token.getUserId
              lookup <- Token.lookup
              currentToken <- R.currentValue (lookup ?? 0)
              currentUserId <- R.currentValue (x ?? 0)
              let value = Token.user <$> currentToken
              liftIO $ value @=? currentUserId
          ],
        {-
        fromAssertions
          "getUser"
          [ runMockApp $ do
              -- createUser jeg skal vide db key
              user <- User.create $ UserCreateForm.user "lol" (Password.PasswordPlainText (pack "lol")) False
              hUser <- Has.grab @(R.Handler (Maybe User.User))
              liftIO $ hUser user

              -- createToken. skal bruge user db key
              token <- Token.createNow
              hToken <- Has.grab @(R.Handler (Maybe Token.Token))
              liftIO $ hToken (Just token)

              -- createToken
              x <- Token.getUser
              currentUser <- R.currentValue (x ?? 0)
              let value = user
              liftIO $ value @=? currentUser
          ],
        fromAssertions
          "getRoleIds"
          [ runMockApp $ do
              -- createUser jeg skal vide db key
              user <- User.create $ UserCreateForm.user "lol" (Password.PasswordPlainText (pack "lol")) False
              hUser <- Has.grab @(R.Handler (Maybe User.User))
              liftIO $ hUser user

              -- createToken. skal bruge user db key
              token <- Token.createNow
              hToken <- Has.grab @(R.Handler (Maybe Token.Token))
              liftIO $ hToken (Just token)

              -- createToken
              x <- Token.getRoleIds
              currentRoles <- R.currentValue (x ?? 0)
              let value = Just [0]
              liftIO $ value @=? currentRoles,
            runMockApp $ do
              -- createUser jeg skal vide db key
              user <- User.create $ UserCreateForm.user "lol" (Password.PasswordPlainText (pack "lol")) True
              hUser <- Has.grab @(R.Handler (Maybe User.User))
              liftIO $ hUser user

              -- createToken. skal bruge user db key
              token <- Token.createNow
              hToken <- Has.grab @(R.Handler (Maybe Token.Token))
              liftIO $ hToken (Just token)

              -- createToken
              x <- Token.getRoleIds
              currentRoles <- R.currentValue (x ?? 0)
              let value = Just [0, 1]
              liftIO $ value @=? currentRoles
          ],
        fromAssertions
          "getRoles"
          [ runMockApp $ do
              -- createUser jeg skal vide db key
              user <- User.create $ UserCreateForm.user "lol" (Password.PasswordPlainText (pack "lol")) False
              hUser <- Has.grab @(R.Handler (Maybe User.User))
              liftIO $ hUser user

              -- createToken. skal bruge user db key
              token <- Token.createNow
              hToken <- Has.grab @(R.Handler (Maybe Token.Token))
              liftIO $ hToken (Just token)

              -- Create roles
              let role = Role.role "admin" [0, 1]
              hRole <- Has.grab @(R.Handler Role.Role)
              liftIO $ hRole role

              x <- Token.getRoles
              currentRoles <- R.currentValue (x ?? 0)

              let value = Just [role]
              liftIO $ value @=? currentRoles,
            runMockApp $ do
              -- createUser jeg skal vide db key
              user <- User.create $ UserCreateForm.user "lol" (Password.PasswordPlainText (pack "lol")) True
              hUser <- Has.grab @(R.Handler (Maybe User.User))
              liftIO $ hUser user

              -- createToken. skal bruge user db key
              token <- Token.createNow
              hToken <- Has.grab @(R.Handler (Maybe Token.Token))
              liftIO $ hToken (Just token)

              -- Create roles
              let role1 = Role.role "role1" [0, 1]
              let role2 = Role.role "role2" [0, 1]
              hRole <- Has.grab @(R.Handler Role.Role)
              liftIO $ hRole role1
              liftIO $ hRole role2

              x <- Token.getRoles
              currentRoles <- R.currentValue (x ?? 0)

              let value = Just [role1, role2]
              liftIO $ value @=? currentRoles
          ],
        fromAssertions
          "getPrivilege"
          [ runMockApp $ do
              -- createUser jeg skal vide db key
              user <- User.create $ UserCreateForm.user "lol" (Password.PasswordPlainText (pack "lol")) False
              hUser <- Has.grab @(R.Handler (Maybe User.User))
              liftIO $ hUser user

              -- createToken. skal bruge user db key
              token <- Token.createNow
              hToken <- Has.grab @(R.Handler (Maybe Token.Token))
              liftIO $ hToken (Just token)

              -- Create roles
              let role = Role.role "role" [0, 1]
              hRole <- Has.grab @(R.Handler Role.Role)
              liftIO $ hRole role

              -- Create privilege
              let privilege = Privilege.privilege "full"
              hPrivilege <- Has.grab @(R.Handler Privilege.Privilege)
              liftIO $ hPrivilege privilege

              x <- Token.getPrivilege
              currentPrivilege <- R.currentValue (x ?? 0)

              let value = Just [0, 1]
              liftIO $ value @=? currentPrivilege
          ],
          -}
        fromAssertions
          "validateTT"
          [ runMockApp $ do
              x <- Token.lessThanTTL
              isValid <- R.currentValue (x ?? Time.secondsToNominalDiffTime 150)
              liftIO $ False @=? isValid,
            runMockApp $ do
              x <- Token.lessThanTTL
              isValid <- R.currentValue (x ?? Time.secondsToNominalDiffTime 50)
              liftIO $ True @=? isValid
          ],
        fromAssertions
          "validate"
          [ runMockApp $ do
              hUserLogin <- Has.grab @(R.Handler (Maybe Db.DatabaseKey))
              liftIO $ hUserLogin (Just 0)
              validate <- Token.validate
              getTime <- Token.getTime
              time <- R.currentValue (getTime ?? 0)
              currentTokenKey <- R.currentValue ((=<<) <$> validate <*> pure time)
              let value = Just 0
              liftIO $ value @=? currentTokenKey,
            runMockApp $ do
              validate <- Token.validate
              getTime <- Token.getTime
              time <- R.currentValue (getTime ?? 0)
              currentTokenKey <- R.currentValue ((=<<) <$> validate <*> pure time)
              let value = Nothing
              liftIO $ value @=? currentTokenKey,
            runMockApp $ do
              hUserLogin <- Has.grab @(R.Handler (Maybe Db.DatabaseKey))
              liftIO $ hUserLogin (Just 0)
              validate <- Token.validate
              getTime <- Token.getTime
              time <- fmap (\t -> Time.time (Time.addUTCTime (Time.secondsToNominalDiffTime 1000) (Time.unTime t))) <$> R.currentValue (getTime ?? 0)
              currentTokenKey <- R.currentValue ((=<<) <$> validate <*> pure time)
              let value = Nothing
              liftIO $ value @=? currentTokenKey
          ]
      ]
