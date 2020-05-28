{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE DeriveAnyClass #-}

module Data where


import           Control.DeepSeq

import           Data.Function
import           Data.IntMap                   as IM
import           Data.Maybe

import           Class
import           Type


data Instance = I
  { instanceID :: ID
  , internalTime :: Maybe InternalTime
  , timeSlot :: {-# UNPACK #-} !Time
  , jumpingType :: {-# UNPACK #-} !InstanceType
  , jumpingCount :: {-# UNPACK #-} !Count
  } deriving (Eq, Show)

instance Ord Instance where
  compare = compare `on` internalTime

instance NFData Instance where
  rnf I {..} =
    rnf instanceID
      `seq` rnf internalTime
      `seq` rnf timeSlot
      `seq` rnf jumpingType
      `seq` rnf jumpingCount


timeLimit :: Time
timeLimit = 64

timeSlotSize :: InternalTime
timeSlotSize = 256

jumpingLimit :: Time
jumpingLimit = 4

countingLimit :: Int
countingLimit = 16

jumpingTypeSize :: Int
jumpingTypeSize = 3

getInternalTime = fromMaybe 0 . internalTime

makeInstance i w x y z = I
  { instanceID   = i
  , internalTime = if iT == 0 then Nothing else Just iT
  , timeSlot     = rem x timeLimit
  , jumpingType  = rem y jumpingTypeSize
  , jumpingCount = rem z countingLimit
  }
  where iT = rem w timeSlotSize

makeInstances (i : w : x : y : z : zs) =
  makeInstance i w x y z : makeInstances zs
