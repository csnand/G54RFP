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

hungry :: String -> String
hungry name = name ++ " is hungry."

eating :: String -> Int -> Int -> String
eating name fork1 fork2 = name ++
  " with forks " ++ show fork1 ++
  " and " ++ show fork2 ++
  " is eating."

thinking :: String -> String
thinking name =  name ++ " is thinking."

philosophers :: [String]
philosophers = ["Aristotle", "Kant", "Spinoza", "Marx", "Russel"]


randomDelay :: IO ()
randomDelay = do
  waitTime <- randomRIO (1,3)
  threadDelay (waitTime * 1000000)

putBuf :: TMVar [String] -> String -> STM ()
putBuf buf str = do
  xs <- readTMVar buf
  putTMVar buf (xs ++ [str])

getBuf :: TMVar [String] -> STM String
getBuf buf = do
  xs <- readTMVar buf
  case xs of
    [] -> retry
    (x:xs') -> do
      putTMVar buf xs'
      return x

printBuf :: TMVar[String] -> IO ()
printBuf buf = do
  str <- atomically $ getBuf buf
  putStrLn str
  printBuf buf

newInfoBuf :: IO (TMVar [String])
newInfoBuf = newTMVarIO []

dinning :: TMVar [String] -> String -> (Fork, Fork) -> IO ()
dinning buf name (left, right) = forever $ do
  atomically $ putBuf buf (hungry name)
  (leftFork, rightFork) <- atomically $ do
    leftFork <- takeFork left
    rightFork <- takeFork right
    return (leftFork, rightFork)
  atomically $ putBuf buf (eating name leftFork rightFork)
  randomDelay

  atomically $ do
    putFork leftFork left
    putFork rightFork right
  atomically $ putBuf buf (thinking name)
  randomDelay

main = do
  forks <- mapM newFork [1..5]
  infoBuf <- newInfoBuf
  let dinningPhil       = map (dinning infoBuf) philosophers
      forkPairs         = zip forks (tail . cycle $ forks)
      dinningWithForks  = zipWith ($) dinningPhil (take 5 forkPairs)
  mapM_ forkIO dinningWithForks
  printBuf infoBuf

