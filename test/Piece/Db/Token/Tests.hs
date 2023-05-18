{-# LANGUAGE DataKinds #-}

module Piece.Db.Token.Tests
  ( tests,
  )
where

import Data.Text (pack)
import qualified Data.Time as Time
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.Privilege as Privilege
import Piece.Core.Role (Role (privilege))
import qualified Piece.Core.Role as Role
import qualified Piece.Core.Time as Time
import qualified Piece.Core.Token as Token
import qualified Piece.Core.User as User
import qualified Piece.Core.UserCreateForm as UserCreateForm
import qualified Piece.Db.Token as Token
import qualified Piece.Db.User as User
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
              token <- Token.createNow
              hToken <- Has.grab @(R.Handler (Maybe Token.Token))
              liftIO $ hToken (Just token)
              x <- Token.lookup
              currentToken <- R.currentValue (x ?? 0)
              let value = Just token
              liftIO $ value @=? currentToken
          ],
        fromAssertions
          "getTime"
          [ runMockApp $ do
              token <- Token.createNow
              hToken <- Has.grab @(R.Handler (Maybe Token.Token))
              liftIO $ hToken (Just token)
              x <- Token.getTime
              currentTime <- R.currentValue (x ?? 0)
              let value = Just (Token.time token)
              liftIO $ value @=? currentTime
          ],
        fromAssertions
          "getUserId"
          [ runMockApp $ do
              token <- Token.createNow
              hToken <- Has.grab @(R.Handler (Maybe Token.Token))
              liftIO $ hToken (Just token)
              x <- Token.getUserId
              currentUserId <- R.currentValue (x ?? 0)
              let value = Just (Token.user token)
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
              now <- Unsafe.fromJust . rightToMaybe <$> Time.currentTime
              let token = Token.token 0 now
              hToken <- Has.grab @(R.Handler (Maybe Token.Token))
              liftIO $ hToken (Just token)
              x <- Token.validate
              currentTokenKey <- R.currentValue (x ?? now)
              let value = Just (now, 0)
              liftIO $ value @=? currentTokenKey,
            runMockApp $ do
              now <- Unsafe.fromJust . rightToMaybe <$> Time.currentTime
              x <- Token.validate
              currentTokenKey <- R.currentValue (x ?? now)
              let value = Nothing
              liftIO $ value @=? currentTokenKey,
            runMockApp $ do
              now <- Unsafe.fromJust . rightToMaybe <$> Time.currentTime
              let token = Token.token 0 now
              hToken <- Has.grab @(R.Handler (Maybe Token.Token))
              liftIO $ hToken (Just token)
              x <- Token.validate
              currentTokenKey <- R.currentValue (x ?? Time.time (Time.addUTCTime (Time.secondsToNominalDiffTime 1000) (Time.unTime now)))
              let value = Nothing
              liftIO $ value @=? currentTokenKey
          ]
      ]
