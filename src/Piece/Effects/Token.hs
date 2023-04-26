module Piece.Effects.Token
  ( validate,
  )
where

import qualified Data.Time.Format as Time
import qualified Data.Time.LocalTime as Time
import qualified Piece.Core.Time as Time
import qualified Piece.Core.Token as Token

validate :: Time.Time -> Token.Token -> m Token.Token
validate time token = undefined

--  currentTime <- parse (Time.unTime time)
-- tokenTime <- parse (Token.time token)
