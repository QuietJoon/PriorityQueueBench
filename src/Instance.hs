{-# LANGUAGE RecordWildCards #-}
module Instance where


import           Data.Maybe

import           Data
import           Type


runInstance :: Instance -> Int -> Either Instance Instance
runInstance i@I {..} x = if jumpingCount == 0
      then Left i
      else case jumpingType of
          -- In-place Jumping Instance
            1 -> Right $ i { timeSlot     = timeSlot + timeSlotOffset
                           , internalTime = Just newInternalTime
                           , jumpingCount = jumpingCount - 1
                           }
                  -- Over Time-Slot Jumping Instance
            2 -> Right $ i { timeSlot     = timeSlot + rem x jumpingLimit
                           , internalTime = Nothing
                           , jumpingCount = jumpingCount - 1
                           }
                  -- No Jumping Instance
            _ -> Left i
   where
      jumpingIT = rem x timeSlotSize
      (timeSlotOffset, newInternalTime) =
            quotRem ((fromMaybe 0 internalTime) + jumpingIT) timeSlotSize
