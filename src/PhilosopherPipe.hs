{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module PhilosopherPipe where

import           Control.Concurrent     (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude          as P
import           System.Random.MWC

data ThreadState = Think | Eat deriving Show

data Philosopher = Philosopher { name  :: String
                               , rfork :: TVar Bool
                               , lfork :: TVar Bool
                               }

data Chair = Chair { phil :: Maybe Philosopher
                   , inbox :: Input Philosopher
                   , outbox :: Output Philosopher
                   }

type Fork = TVar Bool

philosopherNames :: [String]
philosopherNames = ["Aristotle", "Locke", "Paine"]

randomDelay :: IO ()
randomDelay =
  do vs <- (withSystemRandom . asGenST $ \gen -> uniform gen) :: IO Double
     threadDelay $ floor (500000 * vs)

takeFork :: Fork -> STM ()
takeFork fork = do forkFree <- readTVar fork
                   if forkFree then writeTVar fork False
                   else retry

releaseFork :: Fork -> STM ()
releaseFork fork = writeTVar fork True

dinePipe :: MonadIO m => Pipe Philosopher (Philosopher, ThreadState) m ()
dinePipe =
  do ph <- await
     forever $ do yield (ph, Think)
                  liftIO randomDelay
                  startEating ph
                  yield (ph, Eat)
                  liftIO randomDelay
                  finishEating ph

startEating :: MonadIO m => Philosopher -> m ()
startEating ph = liftIO . atomically $ do takeFork (rfork ph)
                                          takeFork (lfork ph)

finishEating :: MonadIO m => Philosopher -> m ()
finishEating ph = liftIO . atomically $ do releaseFork (rfork ph)
                                           releaseFork (lfork ph)

pipeline :: Chair -> IO (Async ())
pipeline chair =
  async $ do liftIO $ print "Pipeline started"
             runEffect $ fromInput (inbox chair) >->
                         dinePipe >->
                         P.map (\(p,ts) -> (name p, ts)) >-> P.print
             performGC

sitDown :: Chair -> Philosopher -> IO (Async ())
sitDown chair ph =
  async $ do runEffect $ yield ph >-> toOutput (outbox chair)
             liftIO . print $ (name ph) ++ " sits down to eat."
             performGC

run :: IO ()
run = do let n = length philosopherNames
         forks <- replicateM n (newTVarIO True)
         (output, input) <- spawn Unbounded
         let philosophers = flip map [0..n-1] $ \i ->
               Philosopher (philosopherNames !! i)
                           (forks !! i)
                           (forks !! ((i+1) `mod` n))
         let chairs = replicate n (Chair Nothing input output)
         zipWithM_ sitDown chairs philosophers
         as <- forM chairs $ \c -> pipeline c
         replicateM_ 15 randomDelay
         mapM_ cancel as
         print "Dinner is over"
         return ()
