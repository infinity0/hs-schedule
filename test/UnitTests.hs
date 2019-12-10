import           Test.Tasty

import           Control.Monad.ScheduleTest (tests)

main :: IO ()
main = do
  defaultMain $ testGroup "Schedule *" [Control.Monad.ScheduleTest.tests]
