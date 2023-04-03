module Piece
  ( mkAppEnv,
    runServer,
    main,
  )
where

import Piece.App.Env (Env)
import Piece.App.Monad (AppEnv, runApp)
import Piece.Config (loadConfig)
import qualified Graphics.UI.Threepenny.Core as Threepenny


main :: Int -> IO ()
main port = do
    config <- loadConfig
    Threepenny.startGUI Threepenny.defaultConfig {
                            jsPort       = Just port
                           , jsStatic     = Just "./static"
                           , jsCustomHTML = Just "index.html"
                           }
        $ \ window -> mdo
                env <- runApp env $ app window config
                return ()


app :: forall m . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> Config
    -> m Env
app window Config {..} = do
---READ
    databaseLoan <- readJson datastoreLoan :: m (Database Loan)

---BEHAVIOR
    bDatabaseLoan <- stepper databaseLoan $ Unsafe.head <$> unions []
    bSelectionUser stepper Nothing $ Unsafe.head <$> unions []
    bSelectionItem stepper Nothing $ Unsafe.head <$> unions []
    bSelectionLoan stepper Nothing $ Unsafe.head <$> unions []
    bFilterUser stepper "" $ Unsafe.head <$> unions []
    bFilterItem stepper "" $ Unsafe.head <$> unions []
    bModalState stepper False $ Unsafe.head <$> unions []

---ENV

    let env = Env.Env { Env.loanEnv = {
                                bDatabaseLoan = bDatabaseLoan,
                                bSelectionUser = bSelectionUser,
                                bSelectionItem = bSelectionItem,
                                bSelectionLoan = bSelectionLoan,
                                bFilterUser  = bFilterUser,
                                bFilterItem = bFilterItem,
                                bModalState = bModalState,
                        }
                    }

--CHANGES
    liftUI $ onChanges bDatabaseLoan $ writeJson path

    return env
