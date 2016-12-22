module FreeApplicativeExample where

import Control.Applicative
import Control.Applicative.Free
import Control.Monad.IO.Class
import Text.Printf
import Text.Read

data Option a = Option
  { optName :: String
  , optDefault :: Maybe a
  , optReader :: String -> Maybe a }

data Image = Image { width :: Int, height :: Int } deriving (Show, Eq)

-- creates our Option and lifts to free applicative
opt :: Read a => String -> Maybe a -> Ap Option a
opt n d = liftAp $ Option n d readMaybe

-- parser for our Image
parseImage :: Ap Option Image
parseImage = Image <$> opt "Width" (Just 666) <*> opt "Height" Nothing

-- interpreter for our little language
-- here is the place for side-effects
interp :: Ap Option a -> IO a
interp (Pure x) = return x
interp (Ap o f) = interp' o
  where
    interp' (Option n d r) = do
      x <- liftIO $ do
        putStr $ printf "%s: " n
        getLine
      case r x <|> d of
        Just x' -> interp $ f <*> pure x'
        Nothing -> interp' o

interp2 :: Ap Option a -> IO a
interp2 m = runAp interp' m
  where
    interp' o@(Option n d r) = do
      x <- liftIO $ do
        putStr $ printf "%s: " n
        getLine
      case r x <|> d of
        Just x' -> return x'
        Nothing -> interp' o

-- pure version of interpreter
-- logic of our language can be checked without effects
interpPure :: [String] -> Ap Option a -> [a]
interpPure _ (Pure x) = return x
interpPure inputs (Ap (Option n d r) f) = interp' inputs
  where
    interp' (x:xs) =
      case r x <|> d of
        Just x' -> interpPure xs $ f <*> pure x'
        Nothing -> interp' xs

test = interpPure ["not int", "42"] parseImage
-- [Image {width = 666, height = 42}]

-- global default value can be inspected
parserDefault :: Ap Option a -> Maybe a
parserDefault (Pure x) = Just x
parserDefault (Ap x g) =
  parserDefault g <*> optDefault x
  
