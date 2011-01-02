-- type class for emitting Gannet code
module GannetPerl.EmitG where

import Control.Monad.State
import GannetPerl.State.Context
--import GannetPerl.State.Scope
class (Show a) => EmitG a where
    emit :: a -> State Context String
    emit = return . show