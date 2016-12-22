layout: true

class: middle

background-size: cover
background-image: url(assets/applicative-bg.png)
---

class: center

![](/assets/southpark.gif)

# >>= Monad? No, it's applicative! <\*>

Tomislav Grospić

Lambda Meetup - 21 December 2016, Zagreb

---

# We don't need applicative functor

```hs
data Image = Image { width :: Int, height :: Int } deriving (Show)

createImage :: `Int` -> `Int` -> Image
createImage width height =
  `Image width height`

printImage :: `Image` -> String
printImage image =
  `"Printer: " ++ show image`
```

- if we have the `width` and `height` we can create an `Image`
- if we have the `Image` we can print it
- with pure functions what can go wrong?

---

# What about Maybe? :)

```hs
data Maybe  a = Just a | Nothing
data Option a = Some a | None -- F#

widthM  :: `Maybe` Int
heightM :: `Maybe` Int
```

- values are wrapped in some container type
- e.g. `Maybe`, `List`, `Promise`, `Tree`, `State`, ...

--

```hs
createImageM :: `Maybe` Int -> `Maybe` Int -> `Maybe` Image
```

- we must accept wrapped inputs
- return value is also wrapped

???
how to change our pure `createImage`

---

# Let's do it!

```hs
createImageM :: `Maybe` Int -> `Maybe` Int -> `Maybe` Image
createImageM widthM heightM =
  case widthM of
    Just width ->
      case heightM of
        Just height -> Just $ `Image width height`
        Nothing -> Nothing
    Nothing -> Nothing

printImageM :: `Maybe` Image -> `Maybe` String
printImageM imageM =
  case imageM of
    Just image ->
      Just $ `"Printer: " ++ show image`
    Nothing -> Nothing
```

- pattern match, pattern match, pattern match. uh!
- no new information, just boilerplate

---

# OK, we found the elephant

- `createImageM` is what we need
- what we want to implement is `createImage`

```hs
createImageM ::              Maybe Int -> Maybe Int -> Maybe Image

imageM       = `createImageM`   `widthM`      `heightM`
```

- can we have some magic, please!
--

- yes! magic is fun!
- we can use our pure function `createImage` with wrapped values :)

```hs
createImage  ::                 Int   ->   Int  ->  Image

imageM       = `createImage` 合 `widthM` 喦 `heightM`
```

---

# Monad help us! Is `Maybe` a Monad?

- yes! we can use `do` notation (as LINQ _query_ syntax in C#)
- `<-` is really `bind` in disguise (as LINQ `from width in widthM`)
- `return` lifts pure values (back) to the **effectful** world (as LINQ `select width`)

```hs
imageM = do
  width  <- widthM
  height <- heightM
  return $ `createImage` `width` `height`
```

.right-abs[
![](/assets/pwadler-super-lam.jpg)]

---

### `Maybe` definition for functor, applicative functor and monad

- http://hackage.haskell.org/package/base-4.9.0.0/docs/src/GHC.Base.html#line-652

```hs
-- (<$>) == fmap
instance Functor Maybe where
  _ `<$>` Nothing       = Nothing
  f `<$>` (Just a)      = Just (f a)

instance Applicative Maybe where
  pure = Just
  (Just f) `<*>` a      = f <$> a
  Nothing  `<*>` _      = Nothing

instance Monad Maybe where
  return = pure -- from Monad class definition
  (Just x) `>>=` k      = k x
  Nothing  `>>=` _      = Nothing
```

---

# There are some laws involved

- without these laws abstraction doesn't make sense
- just like `1 + (2 + 3) = (1 + 2) + 3` for numbers

```
 [identity]                  pure id <*> x = x
 [composition]  pure (.) <*> x <*> y <*> z = x <*> (y <*> z)
 [homomorphism]          pure f <*> pure x = pure (f x)
 [interchange]                x <*> pure y = pure ($ y) <*> x
```

---

# Main functions of our interest

- all binary functions that accept some **operation** and an **effectful value**

```hs
-- map (fmap)
(<$>) :: Functor f     =>   `(a -> b)` -> f a -> f b

-- apply
(<*>) :: Applicative f => `f (a -> b)` -> f a -> f b

-- (>>=) bind - flipped
(=<<) :: Monad f       => `(a -> f b)` -> f a -> f b
```

---

# Every monad is an applicative functor!

- `ap m1 m2 = do { f <- m1; x <- m2; return (f x) }`
- with the definition of `<*>` _apply_ (and _pure_) function
- **this is the essence of Applicative programming**: computations have a **fixed structure**, given by the pure function, and a **sequence** of subcomputations, given by the **effectful arguments**

```hs
imageM = pure `createImage` <*> `widthM` <*> `heightM`

imageM =      `createImage` <$> `widthM` <*> `heightM`
```

---

# Why we want Applicative functor or **Idiom**?

> The moral is this: if you’ve got an Applicative functor, that’s good; if you’ve also got a Monad, that’s even better!  
And the dual of the moral is this: if you want a Monad, that’s good; if you only want an Applicative functor, that’s even better!

> .src[[Applicative Programming with Effects - Conor McBride, Ross Paterson][applicative-with-effects] - 2008]

---

# Meet **Idiom brackets**

- provide a concise syntax for writing applicative expressions
- indicate a shift into the idiom of an Applicative functor
- it is used heavily in Idris and is also implemented in the She Haskell preprocessor

```hs
imageM = `createImage` <$> `widthM` <*> `heightM`

imageM =〚 createImage widthM heightM 〛
```

- more of Conor McBride's fun with idiom brackets - [she's effectful](https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/idiom.html)
- Alternative operator _like a sword with two handles and no point_ :)

```hs
(| blah1
 | ..
 | blahn
 |)
```

---

# Idiom and monad difference in execution

- `iffy` and `miffy` define **flow of operation** and their evaluation will determine the data flow

```hs
-- Idiom syntax
iffy :: Applicative f => f Bool -> f a -> f a -> f a
iffy fp fa fb = 〚 cond fp fa fb 〛 where
  cond p a b = `if` p `then` a `else` b

-- Monadic syntax
miffy :: Monad m => m Bool -> m a -> m a -> m a
miffy mp ma mb = do
  p <- mp
  `if` p `then` ma `else` mb

-- all arguments are executed
iffy  〚 True 〛 〚 t 〛 Nothing = Nothing

-- only first argument is executed
miffy 〚 True 〛 〚 t 〛 Nothing =〚 t 〛
```

---

# Flow for Idioms, Arrows and Monads

- **control flow** is dynamic if operation to invoke can depend on the results of prior operations
- **data flow** is dynamic if the value passed to an operation can depend on the results of prior operations
- idioms can provide more space and time efficient implementations than monadic alternatives

<table>
  <thead>
  <tr>
    <th></th>
    <th>control flow</th>
    <th>data flow</th>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td><b>idiom</b></td><td>static</td><td>static</td>
  </tr>
  <tr>
    <td>arrow</td><td>static</td><td>dynamic</td>
  </tr>
  <tr>
    <td>monad</td><td>dynamic</td><td>dynamic</td>
  </tr>
  </tbody>
</table>

---

# DEMO F# option parser

- one situation where the full power of monads is not always required is **parsing**

.small-code[
```ml
let readOpt : unit OptionParser =
  i_ *> first "read" <* br <*
  i_ *> text "Read settings from database to a file (--path)."

let filePathOpt : string OptionParser =
  i_ *> (strShort "p" <|> strLong "path") <* br <*
  i__ *> text "Path to XML settings file."

let commandRead (func: string -> 'a) =
  func
  <!> br *> text "Usage: csman read [options]"
   *> readOpt *> br
   *> filePathOpt <* br

run (commandRead `my-read-command`) ["read"; "-p"; "C:\config.xml"];;

run filePathOpt ["read";"-p"; "C:\config.xml"];;
```
]

---

# Applicative **Five minutes** of fame

- **Haskell** new **ApplicativeDo** extension (GHC 8)
- syntactic sugar **do** for monads and now applicative functors
- algorithm is automatic

```hs
{-# LANGUAGE ApplicativeDo #-}
imageM = do
  width  <- widthM -- here `<-` might not be monadic bind
  height <- heightM -- here we can't depend on `width`, data flow is static
  return (`createImage width height`)
```

- **F#** has proposal for applicative computation inside **computation expression**
- algorithm is _handwritten_

```ml
let imageM = option {
  let! width  = widthM
  `and` height = heightM   (* applicative sequence, `width` is not accessible after `and` *)
  return (`createImage width height`)
}
```

???
this doesn't work for applicative do  
`return $ createImage width height` ??

---

# **ApplicativeDo** test results

> 11,884 Haskell packages ... contained **38,850 do expressions**, of which 16,293 (**41.9%**) included at least one use of **<*>** when translated by ApplicativeDo. Furthermore, **10,899 (28.0%) were fully desugared into Applicative and Functor combinators and thus would not require a Monad constraint**.  
...   
> The heuristic algorithm is currently the default in GHC, while the optimal one is available as an option.

- not requiring monad constraints means operations are static and can be parallelized

---

class: center

# We want to be **Free**

Monad, applicative and functor for free

![](/assets/southpark-free.gif)

---

# **Free** applicative functor

```hs
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
```

---

# **Free** applicative functor (DEMO)

```hs
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
```

---

# And even more **Freer** or **Eff** monad

- single monad which type reflects possible effects of a computation
- more extensible effects
- solves the problem with monad transformer stacks composition (the effects are not ordered)

```hs
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
```

---

# And even more **Freer** or **Eff** monad (DEMO)

- we can have different interpreters for the same effectful structure
- we don't need to define any monad, idiom or functor

```hs
runTeletype :: Eff '[Teletype] w -> IO w
runTeletype (Val x) = return x
runTeletype (E u q) = case extract u of
              (PutStrLn msg) -> putStrLn msg  >> runTeletype (qApp q ())
              GetLine        -> getLine      >>= \s -> runTeletype (qApp q s)
              ExitSuccess    -> exitSuccess
```

---

# Links

[Applicative Programming with Effects][applicative-with-effects] [2008] Conor McBride, Ross Paterson

[Algebraic Effects and Effect Handlers for Idioms and Arrows][algebraic-effects] [2014] Sam Lindley

[Free Applicative Functors](https://arxiv.org/abs/1403.0749) [2014] Paolo Capriotti, Ambrus Kaposi - example option parser!

[Syntax Matters: Writing abstract computations in F#](http://tomasp.net/academic/papers/computation-zoo/syntax-matters.pdf) [2014] Tomas Petricek, Don Syme

[Free and Freer Monads: Putting Monads Back into Closet](http://okmij.org/ftp/Computation/free-monad.html) Oleg Kiselyov

[Freer Monad, More Extensible Effects](https://youtu.be/3Ltgkjpme-Y) Oleg Kiselyov (VIDEO)

[Flavours of free applicative functors](https://ro-che.info/articles/2013-03-31-flavours-of-free-applicative-functors)

[Applicative functors](http://tomasp.net/blog/applicative-functors.aspx/)

[Idioms in LINQ - C# examples](http://tomasp.net/blog/idioms-in-linq.aspx/)

[Understanding map and apply](https://fsharpforfunandprofit.com/posts/elevated-world/)

[applicative-with-effects]: http://www.staff.city.ac.uk/~ross/papers/Applicative.html
[algebraic-effects]: http://homepages.inf.ed.ac.uk/slindley/papers/aeia.pdf

---

class: center, middle

# THANK YOU
