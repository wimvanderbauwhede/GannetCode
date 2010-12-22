-- type class for emitting Gannet code
module GannetC.EmitG where

import Control.Monad.State
import GannetC.State.Context
--import GannetC.State.Scope
class (Show a) => EmitG a where
    emit :: a -> State Context String
    emit = return . show