import           Test.Tasty

import           Control.Monad.ScheduleTest  (tests)
import           Control.Static.ScheduleTest (tests)

main :: IO ()
main = do
  defaultMain $ testGroup
    "Schedule *"
    [Control.Monad.ScheduleTest.tests, Control.Static.ScheduleTest.tests]
