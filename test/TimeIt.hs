module TimeIt where

import qualified System.TimeIt as T
import Control.Exception (evaluate)
import Control.Monad (join)

timeItT = T.timeItT . evaluate
timeItNamed s = (T.timeItNamed s) . evaluate
timeItNamedM s = (T.timeItNamed s) . join . evaluate
