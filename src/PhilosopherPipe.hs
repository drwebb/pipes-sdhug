{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module PhilosopherPipe where

import           Control.Concurrent     (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude          as P
import           System.Random.MWC

data ThreadState = Think | Eat deriving Show

data Philosopher = Philosopher { _name  :: String
                               , _rfork :: TVar Bool
                               , _lfork :: TVar Bool
                               }
makeLenses ''Philosopher

data Chair = Chair { _phil :: Maybe Philosopher
                   , _inbox :: Input Philosopher
                   , _outbox :: Output Philosopher 
                   }
makeLenses ''Chair

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
startEating ph = liftIO . atomically $ do takeFork (ph^.rfork) 
                                          takeFork (ph^.lfork)

finishEating :: MonadIO m => Philosopher -> m ()
finishEating ph = liftIO . atomically $ do releaseFork (ph^.rfork)
                                           releaseFork (ph^.lfork)

pipeline :: Chair -> IO (Async ())
pipeline chair = 
  async $ do liftIO $ print "Pipeline started"
             runEffect $ fromInput (chair^.inbox) >-> 
                         dinePipe >-> 
                         P.map (\(p,ts) -> (p^.name, ts)) >-> P.print
             performGC           

sitDown :: Chair -> Philosopher -> IO (Async ())
sitDown chair ph =  
  async $ do runEffect $ yield ph >-> toOutput (chair^.outbox)
             liftIO . print $ (ph^.name) ++ " sits down to eat."
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
