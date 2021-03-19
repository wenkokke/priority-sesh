# Priority Sesh ☕

Welcome to the artefact associated with *Deadlock-Free Session Types in Linear Haskell*!

## Building and testing

First off, we'll talk you through some instructions for building and testing Priority Sesh.

Priority Sesh requires *at least* GHC 9.0.1, as it uses the `LinearTypes` extension. We build Priority Sesh with [Stack][stack], and building and testing is be as simple as running:

```haskell
cd /path/to/priority-sesh/
stack test
```

After a bunch of build information, you should see:

```haskell
priority-sesh> test (suite: test-priority-sesh)

Cases: 16  Tried: 16  Errors: 0  Failures: 0

priority-sesh> Test suite test-priority-sesh passed
```

Congrats, you did it! :tada:

### What if it says `Prelude.chr: bad argument: 3925868637`?

Unfortunately, Stack hasn't been thoroughly tested with the latest versions of GHC and Cabal, and running `stack build` sometimes results in `Prelude.chr: bad argument: 3925868637`. (That one's not on us, we think!) Oddly, `stack test` does not run into this problem, and builds the entire library! We hope that this quirk disappears once Stack implements *official* support for the latest GHC versions. If you do run into this problem, running `stack clean` followed by `stack test` should solve it.


## Paper to practice

In this section, we'll discuss the library, and how it relates to the code in the paper. (We're assuming you've read the paper, and you'd like to see how the actual library fits together.) We'll proceed along the same lines as the paper does—one-shot channels (§2.1), session-typed channels (§2.2), deadlock freedom via process structure (§2.3), and deadlock freedom via priorities (§2.4). However, before we can get started, there's a couple of bits and pieces we need to get started. We'll call that the preamble.


### §1 Preamble

Priority Sesh is built on Linear Haskell and its little cousin, [`linear-base`][linear-base]. There's a couple things that are used quite heavily throughout `linear-base` that the average Haskell programmer perhaps isn't familiar with. We'll discuss `linear-base` first, and then we'll move on to the parts of `priority-sesh` which are, essentially, extensions to `linear-base`, providing the basics which don't *yet* exist in `linear-base`.


#### §1.1 `linear-base` at a glance

##### `Data.Unrestricted.Linear`

The most important module to understand is `Data.Unrestricted.Linear`, which defines the type classes `Consumable` and `Dupable`, which essentially correspond to the ability to drop and copy values:

```haskell
-- https://github.com/tweag/linear-base/blob/master/src/Data/Unrestricted/Internal/Consumable.hs
class Consumable a where
  consume :: a %1-> ()

-- https://github.com/tweag/linear-base/blob/master/src/Data/Unrestricted/Internal/Dupable.hs
class Consumable a => Dupable a where
  dup2 :: a %1-> (a, a)
```

(The class for `Dupable` is a little more complicated than that—it also defines `dupV`, which allows you to make some set number of copies at the same time—but we don't have to worry about that right now.)

The module provides instances for `Consumable` and `Dupable` for most builtin data types—numbers, `Bool`, `()`, `Void`, etc. The module also defines the `Ur` data type, which wraps *unrestricted* values, i.e., values which you can use as many as you like *without* having to resort to `consume` and `dup2`:

```haskell
-- https://github.com/tweag/linear-base/blob/master/src/Data/Unrestricted/Internal/Ur.hs
-- > someLinear :: Ur a %1-> (a,a)
-- > someLinear (Ur a) = (a,a)
data Ur a where
  Ur :: a -> Ur a
```

We've avoided `Ur` in the paper, since understanding it requires a slightly deeper understanding of the workings of Linear Haskell, but you may see a few here and there across the library.

##### `Unsafe.Linear`

One other important module is `Unsafe.Linear`, which defines unsafe casts from unrestricted functions to linear functions. For instance,

```haskell
-- https://github.com/tweag/linear-base/blob/master/src/Unsafe/Linear.hs
toLinear
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) p.
     (a %p-> b) %1-> (a %1-> b)
```

The functions in this module are all restricted versions of `unsafeCoerce`—or rather, `unsafeCoerce`'d `unsafeCoerce`, as the builtin version is non-linear! These functions are used throughout `linear-base` to cast functions from `base` to linear types when this is safe, rather than reimplementing them.

##### `Control.Functor.Linear`

The `Monad` class provided by `base` isn't linear. Nor is `Functor`, `Applicative`, etc. The module `Control.Functor` defines linear variants of these classes:

```haskell
-- https://github.com/tweag/linear-base/blob/master/src/Control/Functor/Linear/Internal/Class.hs
class Applicative m => Monad m where
  -- | @x >>= g@ applies a /linear/ function @g@ linearly (i.e., using it
  -- exactly once) on the value of type @a@ inside the value of type @m a@
  (>>=) :: m a %1-> (a %1-> m b) %1-> m b
```

##### `System.IO.Linear`

Finally, `System.IO.Linear` defines a *linear* `IO` monad (for which it's safe to expose the `RealWorld` token) and several conversions from "system" `IO` to linear `IO`. It also defines `toSystemIO`, but does not export it, as it's unsafe. Therefore, you'll see us redefine it at several places in our code.


#### §1.2 What's missing in `linear-base`?

The `linear-base` library is still somewhat immature, and only covers a small fraction of the *actual* `base` library. We'll briefly cover the things we found missing, and where we define these. One thing you've probably caught onto by now is that every module path in `linear-base` ends in `Linear`, which is a naming convention we've taken over in our library.

##### `Control.Concurrent.Linear`

The first thing that's missing is, essentially, all concurrency primitives. To get started, we cast `forkIO` to a linear type, and make `ThreadId` consumable.

##### `Control.Concurrent.MVar.Linear`

The second thing that's missing is `MVar`. Priority Sesh relies quite heavily on `MVar` as a primitive for building one-shot channels. In this module, we cast `newEmptyMVar`, `takeMVar`, and `putMVar` to linear types, and lift them into the *linear* `IO` monad:

```haskell
newEmptyMVar :: Linear.IO (Ur (MVar a))
takeMVar     :: MVar a %1-> Linear.IO a
putMVar      :: MVar a %1-> a %1-> Linear.IO ()
```

We also make `MVar` consumable. The rationale for this is explained in §2.1 under "Cancellation". In short, throwing away an `MVar` happens to have exactly the semantics we need from one-shot channels to get the semantics we need for session cancellation.

##### `Data.Type.Nat` and `Data.Type.Priority`

The priority-based variant of the library relies quite heavily on type-level naturals. GHC ships with `GHC.TypeNats` out of the box, which is what we are *currently* using. If you check `Data.Type.Nat`, you'll see that the file contains *two* implementations type-level naturals—one which re-exports `GHC.TypeNats` along with a few other primitives, and one which builds type-level naturals up from scratch. This allowed us to quickly explore different designs, and compare how many of the constraints they could automatically discharge, which was especially relevant while exploring the interaction between priorities and recursion (see §2.4 under "Recursion"). The `Data.Type.Priority` module extends the basic operations on naturals to priorities.


### §2.1 One-shot channels

The module `Control.Concurrent.OneShot.Linear` implements the one-shot channels described in §2.1 of the paper. The names were change a little for the paper, but otherwise the implementation follows the paper.

| Paper      | Code       |
| ---------- | ---------- |
| `Send₁`    | `SendOnce` |
| `Recv₁`    | `RecvOnce` |
| `new₁`     | `new`      |
| `send₁`    | `send`     |
| `recv₁`    | `recv`     |
| `Sync₁`    | `SyncOnce` |
| `newSync₁` | `newSync`  |
| `sync₁`    | `sync`     |

When imported, the names from the `OneShot` module are generally referred qualified to as `OneShot.new`, `OneShot.send`, `OneShot.recv`, *etc.*

The module `Test.OneShot` has some simple tests for the one-shot channels. The first test checks that a simple ping works, and doesn't throw an exception:

```haskell
pingWorks :: Test
pingWorks = TestLabel "ping" $ TestCase (assert ping)
  where
    ping = do
      (chan_s, chan_r) <- new
      void $ forkIO (send chan_s ())
      recv chan_r
```

The `assert` primitive simply checks that no exception is thrown. (If the argument passed returns a `Bool`, it also checks that the value is `True`.) The second test check that `cancelAndRecv` and `cancelAndSend` behave appropriately (see §2.1 under "Cancellation"):

```haskell
cancelWorks :: Test
cancelWorks = TestLabel "cancel" $ TestList
  [ TestLabel "recv" $ TestCase (assertBlockedIndefinitelyOnMVar @() cancelRecv)
  , TestLabel "send" $ TestCase (assert cancelSend)
  ]
  where
    -- Server cancels, client tries to receive.
    cancelAndRecv = do
      (chan_s, chan_r) <- new
      void $ forkIO (return (consume chan_s))
      recv chan_r

    -- Server cancels, client tries to send.
    cancelAndSend = do
      (chan_s, chan_r) <- new
      void $ forkIO (return (consume chan_r))
      send chan_s ()
```

### §2.2 Session-typed channels

The module `Control.Concurrent.Session.Linear` implements the session-typed channels described in §2.2 of the paper. The names should match those in the paper, and the implementations should generally match as well, though the implementations of `new` in the code are in point-free style.

The module `Test.Session` contains some tests for the session-typed channels. For instance, it tests a calculator service:

```haskel
type NegServer = Recv Int (Send Int End)
type AddServer = Recv Int (Recv Int (Send Int End))

type CalcServer = Offer NegServer AddServer
type CalcClient = Dual CalcServer

-- |Test using the calculator server for negation.
calcWorks :: Test
calcWorks = TestLabel "calc" $ TestList
  [ TestLabel "neg" $ TestCase (assert negMain)
  , TestLabel "add" $ TestCase (assert addMain)
  ]
  where
    negMain = connect calcServer negClient
    addMain = connect calcServer addClient

    -- Calculator server which offers negation and addition.
    calcServer :: CalcServer %1 -> Linear.IO ()
    calcServer s = offerEither s match
      where
        match :: Either NegServer AddServer %1 -> Linear.IO ()
        match (Left s)  = do
          (x, s) <- recv s
          s <- send (negate x, s)
          close s
        match (Right s) = do
          (x, s) <- recv s
          (y, s) <- recv s
          s <- send (x + y, s)
          close s

    -- Calculator client which chooses (negate 42).
    negClient :: CalcClient %1 -> Linear.IO Bool
    negClient s = do
      s <- selectLeft s
      s <- send (42, s)
      (x, s) <- recv s
      close s
      return $ x == -42

    -- Calculator client which chooses (4 + 5).
    addClient :: CalcClient %1 -> Linear.IO Bool
    addClient s = do
      s <- selectRight s
      s <- send (4, s)
      s <- send (5, s)
      (x, s) <- recv s
      close s
      return $ x == 9
```

And a recursive summation service, as described in §2.1 under "Recursion":

```haskell
newtype SumServer
  = SumServer (Offer (Recv Int SumServer) (Send Int End))

newtype SumClient
  = SumClient (Select (Send Int SumClient) (Recv Int End))

instance Consumable SumServer where
  consume (SumServer s) = consume s

instance Consumable SumClient where
  consume (SumClient s) = consume s

instance Session SumServer where
  type Dual SumServer = SumClient
  new = bimap SumServer SumClient <$> new

instance Session SumClient where
  type Dual SumClient = SumServer
  new = bimap SumClient SumServer <$> new

sumWorks :: Test
sumWorks = TestLabel "sum" $ TestCase (assert main)
  where
    main = connect (sumServer 0) sumClient

    -- Server which offers summation.
    sumServer :: Int %1 -> SumServer %1 -> Linear.IO ()
    sumServer tot (SumServer s) = offerEither s (match tot)
      where
        match :: Int %1 -> Either (Recv Int SumServer) (Send Int End) %1 -> Linear.IO ()
        match tot (Left s) = do
          (x, s) <- recv s
          sumServer (tot + x) s
        match tot (Right s) = do
          s <- send (tot, s)
          close s

    -- Client which sums [1..6].
    sumClient :: SumClient %1 -> Linear.IO Bool
    sumClient (SumClient s) = do
      s <- selectLeft s
      SumClient s <- send (1, s)
      s <- selectLeft s
      SumClient s <- send (2, s)
      s <- selectLeft s
      SumClient s <- send (3, s)
      s <- selectLeft s
      SumClient s <- send (4, s)
      s <- selectLeft s
      SumClient s <- send (5, s)
      s <- selectLeft s
      SumClient s <- send (6, s)
      s <- selectRight s
      (tot, s) <- recv s
      close s
      return $ tot == 21
```

It also tests the behaviour for cancellation, and implements a more complex, recursive variant of the cyclic scheduler presented in §2.4 under "Cyclic Scheduler", but without guaranteeing deadlock freedom.

### §2.3 Deadlock freedom via process structure

The module `Control.Concurrent.Session.DF.Tree.Linear` exports the deadlock-free interface described in §2.3 of the paper. This modules doesn't define any new function, as `connect` is already defined in `Control.Concurrent.Session.Linear`. Rather, it re-exports the definitions from `Control.Concurrent.Session.Linear`, but hides `new`.

There are no tests associated with this module, as it doesn't implement any new functionality.

### §2.4 Deadlock freedom via priorities

Finally, `Control.Concurrent.Session.DF.Priority.Linear` defines the priority-based session-typed channels described in §2.4 of the paper. This modules defines the `Sesh` monad *with* the extension described in §2.4 under "Safe IO", but the order of the arguments was changed for the paper, as mentioned, for typographical reasons.

```haskell
newtype Sesh
  (t :: Type)     -- ^ Session token.
  (p :: Priority) -- ^ Lower priority bound.
  (q :: Priority) -- ^ Upper priority bound.
  (a :: Type)     -- ^ Underlying type.
  = Sesh (Linear.IO a)
```

(The function `unsafeRunSesh` is defined separately, rather than via record syntax, to ensure it has a linear type.)

The module defines `>>>=` and `ireturn`—the names derive from those frequently used by *indexed monads*—as well as `=<<<`, `>>>`, and `<<<`. The `Sesh` monad implements `Data.Functor` as well as `Control.Functor`—these are two different `Functor` classes provided by `linear-base`, which differ in their linearity. Unfortunately, the types for `>>>=` and `ireturn` don't fit the standard `Monad` class.

The implementation of the `Session` monad and the communication primitives follow the implementations in `Control.Concurrent.Session.Linear`, but wrapped in the `Sesh` monad. A previous version of the library wrapped the primitives from `Control.Concurrent.Session.Linear`, but this made it needlessly complex to write user-defined instances of the `Session` class, as is needed for recursive sessions.

The module `Test.Session.Priority` contains some tests for the priority-based channels. Most importantly, it starts by rebinding do-notation to the `Sesh` monad:

```haskell
{-# LANGUAGE RebindableSyntax #-}

-- ... other stuff ...

import Control.Concurrent.Session.DF.Priority.Linear as Session

-- ... other stuff ...

return :: a %1 -> Sesh t 'Top 'Bot a
return = Session.ireturn

(>>=) :: (q < p') =>
  Sesh t p q a %1 ->
  (a %1 -> Sesh t p' q' b) %1 ->
  Sesh t (Min p p') (Max q q') b
(>>=) = (Session.>>>=)

(>>) :: (q < p') =>
  Sesh t p q () %1 ->
  Sesh t p' q' b %1 ->
  Sesh t (Min p p') (Max q q') b
(>>) = (Session.>>>)

fail :: String -> Sesh t p q a
fail = error
```

It implements largely the same tests as before: a simple ping, a calculator service, cancellation, *etc.*

It contains commented-out code for a deadlocking process, a slight variant of `woops` from §2.3 in the paper:

```haskell
deadlockFails :: Test
deadlockFails = TestLabel "deadlock" $ TestCase (assert (runSeshIO woops))
  where
    woops :: Sesh t _ _ ()
    woops = do
      (s1, r1) <- new :: Sesh t _ _ (Send t _ Void (), Recv t _ Void ())
      (s2, r2) <- new :: Sesh t _ _ (Send t _ Void (), Recv t _ Void ())
      fork $ do (void, ()) <- recv @0 r1
                send @1 (void, s2)
      (void, ()) <- recv @0 r2
      send @1 (void, s1)
```

This code fails to compile with the error that `0 ~ 1` doesn't hold, which happens because our deadlocking code forced us to annotate dual actions with `@0` and `@1`.

Finally, the module contains the code for the *finite* cyclic scheduler from §2.4 under "Cyclic Scheduler".


[stack]: https://docs.haskellstack.org/en/stable/README/
[linear-base]: https://github.com/tweag/linear-base/
