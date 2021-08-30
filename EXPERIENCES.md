The following code does not type check, as GHC cannot see that `sync1` and `sync2` use `sender1`, `sender2`, `receiver1`, and `receiver2` are used linearly.

```haskell
newSyncOnce :: Linear.IO (SyncOnce, SyncOnce)
newSyncOnce =
  let (>>=) = (Linear.>>=) in do
    (sender1, receiver1) <- new
    (sender2, receiver2) <- new
    let sync1 = SyncOnce (sender1, receiver2)
    let sync2 = SyncOnce (sender2, receiver1)
    Linear.return (sync1, sync2)
```

The following code, inlining these definition, *does* type check, which leads me to believe there is something fundamental about let bindings which are not inspected by GHC for usage:

```haskell
newSyncOnce :: Linear.IO (SyncOnce, SyncOnce)
newSyncOnce =
  let (>>=) = (Linear.>>=) in do
    (sender1, receiver1) <- new
    (sender2, receiver2) <- new
    Linear.return
        ( SyncOnce (sender1, receiver2)
        , SyncOnce (sender2, receiver1)
        )
```
