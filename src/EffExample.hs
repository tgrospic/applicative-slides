-- https://gitlab.com/queertypes/freer

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
module EffExample where

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import System.Exit hiding (ExitSuccess)

data Teletype s where
  PutStrLn    :: String -> Teletype ()
  GetLine     :: Teletype String
  ExitSuccess :: Teletype ()

putStrLn' :: Member Teletype r => String -> Eff r ()
putStrLn' = send . PutStrLn

getLine'  :: Member Teletype r => Eff r String
getLine' = send GetLine

exitSuccess' :: Member Teletype r => Eff r ()
exitSuccess' = send ExitSuccess

--------------------------------------------------------------------------------
                     -- Effectful Interpreter --
--------------------------------------------------------------------------------
runTeletype :: Eff '[Teletype] w -> IO w
runTeletype (Val x) = return x
runTeletype (E u q) = case extract u of
              (PutStrLn msg) -> putStrLn msg  >> runTeletype (qApp q ())
              GetLine        -> getLine      >>= \s -> runTeletype (qApp q s)
              ExitSuccess    -> exitSuccess

--------------------------------------------------------------------------------
                        -- Pure Interpreter --
--------------------------------------------------------------------------------
runTeletypePure :: [String] -> Eff '[Teletype] w -> [String]
runTeletypePure inputs req =
  reverse . snd $ run (handleRelayS (inputs, []) (\s _ -> pure s) go req)
  where
    go :: ([String], [String])
       -> Teletype v
       -> (([String], [String]) -> Arr '[] v ([String], [String]))
       -> Eff '[] ([String], [String])
    go (is, os) (PutStrLn msg) q = q (is, msg : os) ()
    go (i:is, os) GetLine q = q (is, os) i
    go ([], _) GetLine _ = error "Not enough lines"
    go (_, os) ExitSuccess _ = pure ([], os)

program :: (Member Teletype r) => Eff r ()
program = do
  putStrLn' "Hello Eff monad!"
  str <- getLine'
  str1 <- getLine'
  putStrLn' $ str ++  str1
