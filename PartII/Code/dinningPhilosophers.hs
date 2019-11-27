import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Random

data Fork = MkFork (TMVar Int)

newFork :: Int -> IO Fork
newFork i = do
  fork <- newTMVarIO i
  return (MkFork fork)

takeFork :: Fork -> STM Int
takeFork (MkFork fork) = takeTMVar fork

putFork :: Int -> Fork -> STM ()
putFork i (MkFork fork) = putTMVar fork i

hungry :: String -> IO ()
hungry name = putStrLn $ name ++ " is hungry."

eating :: String -> Int -> Int -> IO ()
eating name fork1 fork2 = putStrLn $ name ++
  " with forks " ++ show fork1 ++
  " and " ++ show fork2 ++
  " is eating."

thinking :: String -> IO ()
thinking name =   putStrLn $ name ++ " is thinking."

philosophers :: [String]
philosophers = ["Aristotle", "Kant", "Spinoza", "Marx", "Russel"]


randomDelay :: IO ()
randomDelay = do
  waitTime <- randomRIO (5,20)
  threadDelay (waitTime * 1000000)


runPhilosopher :: String -> (Fork, Fork) -> IO ()
runPhilosopher name (left, right) = forever $ do
  hungry name
  (leftFork, rightFork) <- atomically $ do
    leftFork <- takeFork left
    rightFork <- takeFork right
    return (leftFork, rightFork)
  eating name leftFork rightFork
  randomDelay

  atomically $ do
    putFork leftFork left
    putFork rightFork right
  thinking name
  randomDelay


main = do
  forks <- mapM newFork [1..5]
  let namedPhilosophers     = map runPhilosopher philosophers
      forkPairs             = zip forks (tail . cycle $ forks)
      philosophersWithForks = zipWith ($) namedPhilosophers forkPairs
  putStrLn "Running the philosophers. Press enter to quit."
  mapM_ forkIO philosophersWithForks

  getLine
