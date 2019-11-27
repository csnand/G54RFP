import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Random

type Fork = TVar Int

newFork :: Int -> IO Fork
newFork i = newTVarIO i

takeFork :: Fork -> STM Int
takeFork fork = readTVar fork

releaseFork :: Int -> Fork -> STM ()
releaseFork i fork = writeTVar fork i

runPhilosopher :: String -> (Fork, Fork) -> IO ()
runPhilosopher name (left, right) = do
  putStrLn (name ++ " is hungry.")
  (leftNum, rightNum) <- atomically $ do
    leftNum <- takeFork left
    rightNum <- takeFork right
    return (leftNum, rightNum)
  putStrLn $ name ++ " got forks " ++ show leftNum ++ " and " ++ show rightNum ++ " and is now eating"
  delay <- randomRIO (2,10)
  threadDelay (delay * 1000000)
  putStrLn (name ++ " is done eating. Going back to thinking.")
  atomically $ do
    releaseFork leftNum left
    releaseFork rightNum right
  delay <- randomRIO (2, 10)
  threadDelay (delay * 1000000)


philosophers :: [String]
philosophers = ["Aristotle", "Kant", "Spinoza", "Marx", "Russel"]

main = do
  forks <- mapM newFork [1..5]
  let namedPhilosophers     = map runPhilosopher philosophers
      forkPairs             = zip forks (tail . cycle $ forks)
      philosophersWithForks = zipWith ($) namedPhilosophers forkPairs
  putStrLn "Running the philosophers. Press enter to quit."
  mapM_ forkIO philosophersWithForks

  -- All threads exit when the main thread exits.
  getLine
