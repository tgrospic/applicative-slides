module erecruit.OptionParser

(*
  """ This implementation of a parser for command-line options (arguments) uses "applicative functor"
  abstraction which is suitable to specify fixed structure of a computation that can be statically
  analyzed without evaluation.
  Applicative functors are generalization of monads. They are less powerful than monads but
  allows writing sequence of computations more succintly when it is not neccessary that computation
  step is dependent of previous step (parsing one option does not depend of parsing other). """

    This "fancy" explanation is the whole picture. Following comments try to explain how this is
    possible with few functions that we already use and why it is so useful. Since we can easily use
    functors and monads in C# with LINQ they are good support for comparison and explanation of
    applicative functors who are their close relatives. They are also called idioms or monoidal functors.

  This interface definitions for Functor, Applicative and Monad are just for easier understanding and they
  don't exist in this form but we can imagine when creating abstraction we are defining our type with
  implementation of these functions (interfaces).

    type 'a Functor =
      abstract member map: ('a -> 'b) -> 'a Functor -> 'b Functor

    type 'a Applicative =
      abstract member  pure: 'a -> 'a Applicative
      abstract member apply: ('a -> 'b) Applicative -> 'a Applicative -> 'b Applicative

    type 'a Monad =
      abstract member return: 'a -> 'a Monad
      abstract member   bind: 'a Monad -> ('a -> 'b Monad) -> 'b Monad

    pure = return // this is the same function

  These functions are special because they represent minimal possible implementation needed to create all
  other useful functions [1][2]. Implementing these functions allows writing and using generic functions for
  building and manipulating different structures (Maybe, List, OptionParser, ...).
  Library like https://github.com/gmpl/FsControl already exists which enhances use of this paradigm in F#.

  In C# we have i.e. `List<T>` which is a Functor because it has `Select` (map) function implemented. With
  `SelectMany` (bind) function it is also a Monad (also needs `return`/`pure` - `new [] { t }.ToList()`).
  And for Applicative functor `List<T>` has `Zip` function which in combination with `map` have the
  same effect as `apply` with `pure` [2], more detailed with C# examples [3]!

  Our `OptionParser` is defined as Applicative functor with functions `apply` (<*>) and `pure` which are used
  to define other combinators - <!>, <*, *>.
  It internally uses `Result` monad that defines `bind` (>>=) and `return` functions.

  Quote from famous paper [4]:
    "The moral is this: if you’ve got an Applicative functor, that’s good; if you’ve also got a Monad,
    that’s even better! And the dual of the moral is this: if you want a Monad, that’s good;
    if you only want an Applicative functor, that’s even better!".

  ## Quick info about infix functions
    - we all know them from the first grade, but we were abused by C :)
    - only makes sense for binary functions (accepts 2 arguments)
    - when defining must be inside brackets e.g. let (+) a b = plus a b
    - in normal (prefix) notation must be inside brackets e.g. let s = (+) 1 2

  Operator order of precedence and associativity.
  https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/symbol-and-operator-reference-%5bfsharp%5d#operator-precedence

  F# uses first letter of an operator to determine precedence. Most operators which we define start with "<"
  and have the same precedence so analogy with simple arithmetics is valid.

    m  +  n       = (+)   m n       = plus m n
    a <*> b       = (<*>) a b       = apply a b
    m + n + i     = (m + n) + i     = plus (plus m n) i
    f <!> a <*> b = (f <!> a) <*> b = (pure f <*> a) <*> b = apply (apply (pure f) a) b

  It is most important to see another pattern which is the main goal of this abstraction.
  In elevated world when values are wrapped (in functors, applicatives, monads) we want similar syntax
  for applying functions.
  When we have `f <!> a <*> b` we mean `a` and `b` are wrapped values and we can't write just `f a b`.
  For example getting user from database involves possible error so `user` is wrapped in the `option` type.

    let sendMail (u: User) : bool = ...
    let maybeUser : User option = getUserFromDb()

    // We can't call `sendMail` directly with `user` because it is wrapped in an `option`
    // but we can put `<!>` in between
    let r : bool option = sendMail <!> maybeUser

    // If function has more arguments just add more `<*>`
    let sendMail' (u: User) (s: SmtpClient) (t: HtmlTemplate) : bool = ...
    let r : bool option = sendMail' <!> maybeUser <*> maybeSmtp <*> maybeHtml

  Instead of the `option` (type) monad we can imagine richer monad with e.g. error handling, logging,
  performance monitoring etc.
  In case of our option (args) parser we are building description of commands and options in `string list`.
  We can imagine more complex structures like trees (XML, HTML), graphs etc.

  More exotic examples are presented in [4] with special syntax for "idioms" (aplicatives).
  They use idiom brackets `(| f a b |)` so the syntax is even closer to regular functions.

  In F# we can use computation expressions which are another syntactic sugar for working with
  monads and other abstract computations [5].
  Also can be helpful (and fun!) to see applicative functors written in LINQ [3].

  References:
    [1] https://fsharpforfunandprofit.com/posts/elevated-world/
    [2] http://tomasp.net/blog/applicative-functors.aspx/
    [3] http://tomasp.net/blog/idioms-in-linq.aspx/ - C# examples
    [4] Applicative Programming with Effects - [2008] Conor McBride, Ross Paterson
        http://www.staff.city.ac.uk/~ross/papers/Applicative.html
    [5] Syntax Matters: Writing abstract computations in F# - [2014] Tomas Petricek, Don Syme
        http://tomasp.net/academic/papers/computation-zoo/syntax-matters.pdf
    [6] Free Applicative Functors - [2014] Paolo Capriotti, Ambrus Kaposi - example option parser!
        https://arxiv.org/pdf/1403.0749.pdf
*)

/// Environment represents input arguments of the program and also input arguments of the
/// individual parser.
type Env = string list

/// State holds a value for the parsed command-line option, it can be valid with a value or in error state.
/// We do not collect parse errors so `Error` constructor is empty. But we could also have `string`,
/// `string list` or some specialized `ParseError` type to store parser errors (`Error of ParseError`).
type 'a State = Ok of 'a | Error

/// Parser result for each individual option parser. It wraps state so parser can
/// continue although it contains an error.
/// Parser either consumes input or just continues parsing with the same input.
type 'a Result = Consumed of 'a State
               | Empty of 'a State

/// `Result` type is becomming a monad with an implementation of `return` and `bind` functions.
/// Wraps a value in `Result`. This is like `new Just( T )` constructor in our well known
/// `erecruit.Utils.Maybe<T>` monad.
/// https://fsharpforfunandprofit.com/posts/elevated-world/#return
let returnn (x: 'a) : 'a Result = Empty (Ok x)
/// `bind` function combines two monads together with "monadic" function (returns wrapped value).
/// You can guess for Maybe this is `Maybe<V> Then<V>( Func<T, Maybe<V>> fn )`.
/// https://fsharpforfunandprofit.com/posts/elevated-world-2/#bind
let (>>=) (a: 'a Result) (f: 'a -> 'b Result) : 'b Result =
  match a with
  | Empty r1
  | Consumed r1 ->
    match r1 with
    | Ok x  -> f x
    | Error -> Empty Error

/// `Result` type (now also a monad) is also an applicative if defines `pure` and `apply` functions.
/// `pure` function is the same as `return` from monad definition.
/// https://fsharpforfunandprofit.com/posts/elevated-world/#return
let puree = returnn
/// Because every monad is an applicative we can use `>>=` (bind) and `return` to define `<*>` (apply).
/// Vice versa is not always posible because not every applicative is a monad.
/// https://fsharpforfunandprofit.com/posts/elevated-world/#apply
let (<*>) (f: ('a -> 'b) Result) (a: 'a Result) : 'b Result =
  f >>= (fun f' -> a >>= (fun a' -> returnn (f' a')))

/// This type represents parser for option. It holds tuple with info message (string list) and runner function
/// which accepts input environment (arguments) and returns parser result with unconsumed environment.
type 'a OptionParser = string list * (Env -> 'a Result * Env)

[<AutoOpen>]
module OptionParser =
  open System

  (*
    Defining `<*>` (apply) and `pure` functions for `OptionParser` makes it our applicative parser. This enables us
    to define some other useful functions (combinators) for defining whole commands parser from smaller pieces.
    So at the end we will end up with a `OptionParser` which is composed (chained) from other `OptionParser`s.
  *)

  /// Creates parser which returns specified value for any input (wraps a value).
  let puree (v: 'a) : 'a OptionParser = [], (fun env -> returnn v, env)
  /// This function creates new runner function
  /// who will, when executed, unwrap function from first and value from second parser and apply it.
  /// Also concatenate info messages from both parsers.
  let (<*>) ((fxs,f): ('a -> 'b) OptionParser) ((axs,a): 'a OptionParser) : 'b OptionParser =
    fxs @ axs, fun env ->
      let f',env1 = f env
      let a',env2 = a env1
      // Here we are just using `<*>` from `Result` applicative because we have two layers of wrapping.
      // (In F# functions are not recursive by default so currently defining function is not is scope.)
      f' <*> a', env2

  /// Functor `map` or `lift` function. It is used for apply normal function to a wrapped value.
  /// Because every functor is applicative functor it can be defined with 'pure' and 'apply'.
  /// https://fsharpforfunandprofit.com/posts/elevated-world/#map
  let (<!>) f a = puree f <*> a
  /// Sequence parsers, discarding the value of the first parser.
  let ( *>) a b : _ OptionParser = (fun _ x -> x) <!> a <*> b
  /// Sequence parsers, discarding the value of the second parser.
  let ( <*) a b : _ OptionParser = (fun x _ -> x) <!> a <*> b

  /// Alternative (or) combinator can be used when we need choice between two parsers.
  /// Logic is defined as pattern matching on the `Result` type. Parser which consumes
  /// input has preference.
  /// Also concatenate info messages from both parsers with space separator.
  let (<|>) ((axs,fa): 'a OptionParser) ((bxs,fb): 'a OptionParser) : 'a OptionParser =
    axs @ [" "] @ bxs, fun (env: Env) ->
      match fa env with
      | Empty Error,_ -> fb env
      | Empty ok,env1 ->
        match fb env1 with
        | Empty _,_ -> Empty ok,env1
        | consumed  -> consumed
      | consumed -> consumed

  /// Empty parser not consuming input.
  let parseUnit env = returnn (), env
  /// Empty parser not consuming input but in error state.
  /// It is used to signal parser error.
  let parseError env = Empty Error, env

  /// This function just returns runner function from supplied parser. Because parsers
  /// can be chained together this runner function runs a hole chain.
  let run (a: 'a OptionParser) : Env -> 'a Result * Env = snd a

  /// This function is used to print info messages for commands and options.
  /// Each parser has `string list` which is used to store info message.
  let render (a: 'a OptionParser) : string = String.concat "" (fst a)

  /// Match strings with pattern for option name/value pair.
  let isKeyVal sep (key: string) (value: string) s = key = sep + s && not(value.StartsWith("-"))
  /// Recursively find/split key/value sequence from list of strings.
  let rec find sep s xs rest =
    match xs with
    | x::y::_ when isKeyVal sep x y s -> xs, rest
    | x::xss                          -> find sep s xss (rest@[x])
    | []                              -> [], rest

  /// Empty parser just adds custom info message.
  let text (t: string) : unit OptionParser = [t], parseUnit
  /// Empty parser represents new line.
  let br : unit OptionParser = ["\n"], parseUnit
  /// Empty parser represents indentation.
  let i_ : unit OptionParser = ["  "], parseUnit
  /// Empty parser represents double indentation.
  let i__ : unit OptionParser = i_ *> i_

  /// String option parser. Match and consume [key; value] from the input (environment).
  let opt sep s : string OptionParser =
    [sep; s], fun env ->
      match find sep s env [] with
      | _::x::t, ys -> Consumed (Ok x), ys@t
      | _      , ys -> parseError ys

  /// String option, long format.
  let strLong = opt "--"
  /// String option, short format.
  let strShort = opt "-"

  /// Option value in `Guid` format.
  let guid ((axs,f): string OptionParser) : Guid OptionParser =
    axs, fun env ->
      match f env with
      | Empty _,_       -> parseError env
      | Consumed r,env1 ->
        match r with
        | Ok x ->
          match Guid.TryParse x with
          | true, v -> Consumed (Ok v),env1
          | _       -> parseError env1
        | Error -> parseError env

  /// Matches and consumes first string from the input (environment).
  let first (s: string) : unit OptionParser =
    [], function
    | x::t when x = s -> Consumed (Ok ()),t
    | env             -> parseError env

  let none: _ option OptionParser = ["(Optional)"], fun env -> returnn None, env

  let optional (a: 'a OptionParser) : 'a option OptionParser =
    Some <!> a <|> none


