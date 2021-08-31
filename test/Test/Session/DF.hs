{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Session.DF where

import Control.Concurrent.Channel.Session.DF
import Control.Concurrent.Linear (forkIO_)
import Control.Monad qualified as Unrestricted
import Data.Proxy (Proxy (..))
import Data.Type.Period (At, Empty, Period (..), type (<), type (<>))
import Data.Type.Priority (Priority (..))
import GHC.TypeNats (type (+))
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Extension
import Prelude qualified
import Prelude.Linear hiding (Dual)
import System.IO.Linear qualified as Linear
import Test.HUnit (Assertable (..), Assertion, Test (..), assertFailure)
import Test.HUnit.Linear (assertException, assertOutput)
import Unsafe.Linear qualified as Unsafe
import GHC.Exception (Exception(displayException))

-- * Ping

-- | Test sending a ping across threads.
pingWorks :: Test
pingWorks = TestLabel "ping" $ TestCase (assert (runSeshIO main))
  where
    main = do
      (here, there) <- new

      fork $ do
        there <- send @0 ((), there)
        close @1 there

      ((), here) <- recv @0 here
      close @1 here

-- * Calculator Server

data CalcOp t o
  = Neg (Recv t o Int (Send t (o + 2) Int (End t (o + 3))))
  | Add (Recv t o Int (Recv t (o + 1) Int (Send t (o + 2) Int (End t (o + 3)))))

instance Consumable (CalcOp t o) where
  consume (Neg s) = consume s
  consume (Add s) = consume s

-- | The calculator server offers a choice between various calculations.
calcServer :: Offer t 0 CalcOp %1 -> Sesh t ('Val 0 :-: 'Val 4) ()
calcServer s = offer s $ \case
  -- Offer negation:
  Neg s -> do
    (x, s) <- recv s
    s <- send (negate x, s)
    close s

  -- Offer addition:
  Add s -> do
    (x, s) <- recv s
    (y, s) <- recv s
    s <- send (x + y, s)
    close s

-- | The negation client uses the negation action of the calculator server.
negClient :: Select t 0 CalcOp %1 -> Sesh t ('Val 0 :-: 'Val 4) Bool
negClient s0 = do
  s1 <- select Neg s0
  s2 <- send (42, s1)
  (r, s3) <- recv s2
  close s3
  return (r == -42)

-- | The addition client uses the addition action of the calculator server.
addClient :: Select t 0 CalcOp %1 -> Sesh t ('Val 0 :-: 'Val 4) Bool
addClient s = do
  s <- select Add s
  s <- send (4, s)
  s <- send (5, s)
  (r, s) <- recv s
  close s
  return (r == 9)

calcWorks :: Test
calcWorks =
  TestLabel "calc" $
    TestList
      [ TestLabel "neg" $ TestCase (assert (runSesh negMain)),
        TestLabel "add" $ TestCase (assert (runSesh addMain))
      ]
  where
    negMain = do
      (here, there) <- new
      fork (calcServer there)
      negClient here
    addMain = do
      (here, there) <- new
      fork (calcServer there)
      addClient here


-- * Cancellation

-- | Test the interaction of cancel with send and receive.
cancelWorks :: Test
cancelWorks =
  TestLabel "cancel" $
    TestList
      [ TestLabel "recv" $ TestCase (assertException (Proxy @CommunicationException) (runSeshIO cancelAndRecv)),
        TestLabel "send" $ TestCase (assert (runSeshIO cancelAndSend))
      ]
  where
    -- Server cancels, client tries to receive.
    cancelAndRecv :: Sesh t (At 0) ()
    cancelAndRecv = do
      s <- connect cancel
      ((), ()) <- recv s
      return ()

    -- Server cancels, client tries to send.
    cancelAndSend :: Sesh t (At 0) ()
    cancelAndSend = do
      s <- connect cancel
      () <- send ((), s)
      return ()


-- * Deadlock (does not compile, rightfully)

tryTypeCheckDeadlock :: MonadInterpreter m => m ()
tryTypeCheckDeadlock = do
  set [languageExtensions := extensions]
  setImports imports
  rebindSyntax
  runStmt
    "let woops :: Sesh t _ ();\
    \    woops = do;\
    \      (s1, r1) <- new;\
    \      (s2, r2) <- new;\
    \      fork $ do (void, ()) <- recv @0 r1;\
    \                send @1 (void, s2);\
    \      (void, ()) <- recv @0 r2;\
    \      send @1 (void, s1)"
  where
    -- Use default Monad class in what follows:
    (>>) = (Unrestricted.>>)

    imports =
      [ "Control.Concurrent.Channel.Session.DF",
        "Data.Type.Period",
        "Data.Void",
        "Prelude.Linear"
      ]

    -- Language extensions used in test code:
    --
    -- NOTE: hint-9.0.4 does not provide a constructor for LinearTypes
    --
    extensions =
      [ DataKinds,
        PartialTypeSignatures,
        RebindableSyntax,
        TypeApplications,
        TypeOperators,
        UnknownExtension "LinearTypes"
      ]

    -- Rebind do-notation to 'Sesh' monad:
    rebindSyntax = do
      runStmt
        "let fail :: String -> Sesh t p a;\
        \    fail = error"
      runStmt
        "let return :: a %1 -> Sesh t Empty a;\
        \    return = ireturn"
      runStmt
        "let (>>=) :: (p1 < p2) => Sesh t p1 a %1 -> (a %1 -> Sesh t p2 b) %1 -> Sesh t (p1 <> p2) b;\
        \    (>>=) = (>>>=)"
      runStmt
        "let (>>) :: (p1 < p2) => Sesh t p1 () %1 -> Sesh t p2 b %1 -> Sesh t (p1 <> p2) b;\
        \    (>>) = (>>>)"


deadlockFails :: Test
deadlockFails = TestLabel "deadlock" $ TestCase (assert main)
  where
    -- Use default Monad class in what follows:
    (>>=) = (Unrestricted.>>=)
    return = Unrestricted.return

    -- Run the interpreter and check the result:
    main :: IO ()
    main = do
      result <- runInterpreter tryTypeCheckDeadlock
      case result of
        Left intpError
          | isExpected intpError -> return ()
          | otherwise -> assertFailure (displayException intpError)
        Right () ->
          assertFailure "Deadlocking computation successfully passed the type checker."

    -- Check if the error message returned is as expected:
    isExpected :: InterpreterError -> Bool
    isExpected (WontCompile ghcErrors) =
      and [ errMsg ghcError == expectedMsg
          | (ghcError, expectedMsg) <- Prelude.zip ghcErrors expectedMsgs
          ]
    isExpected _ = False

    -- The expected error message:
    expectedMsgs =
      [ "<interactive>:1:101: error:\n\
        \    \8226 Couldn't match type \8216'EQ\8217 with \8216'LT\8217 arising from a do statement\n\
        \    \8226 In a stmt of a 'do' block: (void, ()) <- recv @0 r1\n\
        \      In the second argument of \8216($)\8217, namely\n\
        \        \8216do (void, ()) <- recv @0 r1\n\
        \            send @1 (void, s2)\n\
        \            (void, ()) <- recv @0 r2\n\
        \            send @1 (void, s1)\8217\n\
        \      In a stmt of a 'do' block:\n\
        \        fork\n\
        \          $ do (void, ()) <- recv @0 r1\n\
        \               send @1 (void, s2)\n\
        \               (void, ()) <- recv @0 r2\n\
        \               send @1 (void, s1)",
        "<interactive>:1:142: error:\n\
        \    \8226 Couldn't match type \8216'GT\8217 with \8216'LT\8217 arising from a do statement\n\
        \    \8226 In a stmt of a 'do' block: send @1 (void, s2)\n\
        \      In the second argument of \8216($)\8217, namely\n\
        \        \8216do (void, ()) <- recv @0 r1\n\
        \            send @1 (void, s2)\n\
        \            (void, ()) <- recv @0 r2\n\
        \            send @1 (void, s1)\8217\n\
        \      In a stmt of a 'do' block:\n\
        \        fork\n\
        \          $ do (void, ()) <- recv @0 r1\n\
        \               send @1 (void, s2)\n\
        \               (void, ()) <- recv @0 r2\n\
        \               send @1 (void, s1)"
      ]



-- * Rebindable Syntax

fail :: String -> Sesh t p a
fail = error

return :: a %1 -> Sesh t Empty a
return = ireturn

(>>=) :: (p1 < p2) => Sesh t p1 a %1 -> (a %1 -> Sesh t p2 b) %1 -> Sesh t (p1 <> p2) b
(>>=) = (>>>=)

(>>) :: (p1 < p2) => Sesh t p1 () %1 -> Sesh t p2 b %1 -> Sesh t (p1 <> p2) b
(>>) = (>>>)
