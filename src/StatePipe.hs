module StatePipe where

import Control.Monad.State.Strict
import Pipes hiding (ListT)
import Pipes.Lift
import qualified Pipes.Prelude as P

pipeline :: IO ()
pipeline = runEffect $ void (execStateP 0 incrementP) >-> P.take 10 >-> P.print

action :: IO ()
action = mapM_ print $ evalState (replicateM 10 increment) 0 

withPipe :: IO ()
withPipe = do print "printing counter using a pipe"
              pipeline

noPipe :: IO ()
noPipe = do print "printing counter without a pipe" 
            action 

incrementP :: Producer Int (StateT Int IO) ()
incrementP = forever $ do
  get >>= yield
  modify (+1)
  
  
increment :: State Int Int
increment = do
   n <- get
   modify (1+)
   return n

run :: IO ()
run = do noPipe
         withPipe 
