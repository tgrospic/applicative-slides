module ApplicativeExample where

data Image = Image { width :: Int, height :: Int } deriving (Show)

createImage :: Int -> Int -> Image
createImage width height =
  Image width height

printImage :: Image -> String
printImage image =
  "Printer: " ++ show image

createImageM :: Maybe Int -> Maybe Int -> Maybe Image
createImageM widthM heightM =
  case widthM of
    Just width ->
      case heightM of
        Just height -> Just $ Image width height
        Nothing -> Nothing
    Nothing -> Nothing

printImageM :: Maybe Image -> Maybe String
printImageM imageM =
  case imageM of
    Just image ->
      Just $ "Printer: " ++ show image
    Nothing -> Nothing

widthM  = Just 100
heightM = Just 200

imageM1 = createImageM widthM heightM

imageM2 = do
  width  <- widthM
  height <- heightM
  return (createImage width height)

imageM3 = pure createImage <*> widthM <*> heightM

imageM4 = createImage <$> widthM <*> heightM

-- Option applicative
data Option a = Some a | None

instance  Functor Option  where
    _ `fmap` None     = None
    f `fmap` Some a   = Some (f a)

instance Applicative Option where
    pure = Some

    Some f <*> m    = f <$> m
    None   <*> _m   = None
