{-# LANGUAGE DeriveFunctor #-}
module FreeMonadExamples where

import Control.Monad.Free
import System.Exit

data OptionF a r = Some a r | None deriving (Functor)

type Option = Free (OptionF String)

put' :: String -> Option ()
put' str = liftF $ Some str ()

none' :: Option ()
none' = liftF $ None

run :: Option r -> IO r
run (Pure r) = return r
run (Free (Some s k)) = putStrLn s >> run k
run (Free  None     ) = exitSuccess

runPure :: Option r -> [String]
runPure (Pure r) = []
runPure (Free (Some s k)) = s : runPure k
runPure (Free  None     ) = []

program = do
  put' "Hello"
  none'
  put' "World"

program' =
  put' "Hello" *>
  put' "World" *>
  none' *>
  put' "!!!!!"

