{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
module FreeTransExample where

import Control.Monad.Trans.Free (FreeT, FreeF(..), liftF, runFreeT, runFree, retractT)
import System.Exit hiding (ExitSuccess)
import Control.Monad.Trans.Class (lift)

data TeletypeF a
  = PutString String a
  | GetString (String -> a)
  deriving Functor

putString :: (Monad m) => String -> FreeT TeletypeF m ()
putString str = liftF $ PutString str ()

getString :: (Monad m) => FreeT TeletypeF m String
getString = liftF $ GetString id

prompt :: FreeT TeletypeF IO ()
prompt = do
  lift $ putStrLn "Supply the next step:"
  cmd <- lift getLine
  case cmd of
    "forward" -> do
        str <- getString
        putString str
        prompt
    "greet" -> do
        putString "Hello, world!"
        prompt
    _ -> return ()

runTIO :: FreeT TeletypeF IO r -> IO r
runTIO t = do
  x <- runFreeT t
  case x of
    Pure r -> return r
    Free (PutString str t') -> do
      putStrLn str
      runTIO t'
    Free (GetString k) -> do
      str <- getLine
      runTIO (k str)
