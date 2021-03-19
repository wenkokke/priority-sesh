# Priority Sesh ☕

Welcome to the artefact associated with *Deadlock-Free Session Types in Linear Haskell*!

## Installation

First off, we'll talk you through some installation instructions. Priority Sesh is built with [Stack][stack].

TODO


## Paper to practice

In this section, we'll discuss the library, and how it relates to the code in the paper. (We're assuming you've read the paper, and you'd like to see how the actual library fits together.) We'll proceed along the same lines as the paper does---one-shot channels (§2.1), session-typed channels (§2.2), deadlock freedom via process structure (§2.3), and deadlock freedom via priorities (§2.4). However, before we can get started, there's a couple of bits and pieces we need to get started. We'll call that the preamble.


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

(The class for `Dupable` is a little more complicated than that---it also defines `dupV`, which allows you to make some set number of copies at the same time---but we don't have to worry about that right now.)

The module provides instances for `Consumable` and `Dupable` for most builtin data types---numbers, `Bool`, `()`, `Void`, etc. The module also defines the `Ur` data type, which wraps *unrestricted* values, i.e., values which you can use as many as you like *without* having to resort to `consume` and `dup2`:

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

The functions in this module are all restricted versions of `unsafeCoerce`---or rather, `unsafeCoerce`'d `unsafeCoerce`, as the builtin version is non-linear! These functions are used throughout `linear-base` to cast functions from `base` to linear types when this is safe, rather than reimplementing them.

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

The priority-based variant of the library relies quite heavily on type-level naturals. GHC ships with `GHC.TypeNats` out of the box, which is what we are *currently* using. If you check `Data.Type.Nat`, you'll see that the file contains *two* implementations type-level naturals---one which re-exports `GHC.TypeNats` along with a few other primitives, and one which builds type-level naturals up from scratch. This allowed us to quickly explore different designs, and compare how many of the constraints they could automatically discharge, which was especially relevant while exploring the interaction between priorities and recursion (see §2.4 under "Recursion"). The `Data.Type.Priority` module extends the basic operations on naturals to priorities.


### §2.1 One-shot channels

### §2.2 Session-typed channels

### §2.3 Deadlock freedom via process structure

### §2.4 Deadlock freedom via priorities

[stack]: https://docs.haskellstack.org/en/stable/README/
[linear-base]: https://github.com/tweag/linear-base/
