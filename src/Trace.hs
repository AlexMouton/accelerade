module Trace where

import Debug.Trace as T

trace :: Show a => String -> a -> a
trace a = T.trace (a <> " " <> show a)
