{-# LANGUAGE ApplicativeDo #-}
module ApplicativeDoExample where

import Control.Applicative.Free

data Option a = Some a | None deriving (Show)

instance  Functor Option where
    _ `fmap` None     = None
    f `fmap` Some a   = Some (f a)

instance Applicative Option where
    pure = Some
    Some f <*> m    = f <$> m
    None   <*> _m   = None

data Image = Image { width :: Int, height :: Int } deriving (Show)

createImage :: Int -> Int -> Image
createImage width height =
  Image width height

printImage :: Image -> String
printImage image =
  "Printer: " ++ show image

widthM  = Some 100
heightM = Some 200

-- syntactic sugar `do`
-- Option is Applicative but not a Monad!
imageM = do
  width  <- widthM -- here `<-` is not monadic bind
  height <- heightM -- here we can't depend on `width`, flow is static
  return (createImage width height)

-- translates to
imageM' = createImage <$> widthM <*> heightM
