module Main where

import qualified PhilosopherPipe as PP
import qualified StatePipe as SP

main = PP.run >>
       SP.run >>
       return ()
