import           Test.Tasty

import           Control.Monad.Trans.ScheduleTest (tests)

main :: IO ()
main = do
  defaultMain $ testGroup "Schedule *" [Control.Monad.Trans.ScheduleTest.tests]
