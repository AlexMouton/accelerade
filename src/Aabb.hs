module Aabb where

import Linear.V3
import Control.Arrow ((&&&))

type Bounds = (V3 Float, V3 Float) 

aabb :: [(V3 Float)] -> Bounds
aabb = (minimum &&& maximum)