{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}

import           Test.HUnit
import           Test.HUnit.Linear
import qualified Prelude
import           Prelude.Linear hiding (Dual)
import           Control.Concurrent.Session.Linear
import           Control.Monad.Linear
import qualified System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe


main :: IO Counts
main = runTestTT tests
  where
    tests :: Test
    tests = TestList [pingWorks, calcWorks]


-- * Ping

-- |Test sending a ping across threads.
pingWorks :: Test
pingWorks = TestLabel "ping" $ TestCase (assert ping)
  where
    ping :: Linear.IO (Ur ())
    ping = do
      connect
        (\s -> do
            s <- send () s
            close s
        )
        (\s -> do
            ((), s) <- recv s
            close s
        )
      return $ Ur ()


-- * Calculator

type NegServer n = Recv n (Send n End)
type AddServer n = Recv n (Recv n (Send n End))

type CalcServer n = Offer (NegServer n) (AddServer n)
type CalcClient n = Dual (CalcServer n)


-- |Test using the calculator server for negation.
calcWorks :: Test
calcWorks = TestLabel "calc" $ TestList
  [ TestLabel "neg" $ TestCase (assert neg)
  , TestLabel "add" $ TestCase (assert add)
  ]
  where
    calcServer :: CalcServer Int %1 -> Linear.IO ()
    calcServer = offerEither match
      where
        match :: Either (NegServer Int) (AddServer Int) %1 -> Linear.IO ()
        match (Left s) = do
          (x, s) <- recv s
          s <- send (negate x) s
          close s
        match (Right s) = do
          (x, s) <- recv s
          (y, s) <- recv s
          s <- send (x + y) s
          close s

    neg :: Linear.IO (Ur Bool)
    neg = do
      x <- connect calcServer
        (\s -> do
            s <- selectLeft s
            s <- send 42 s
            (x, s) <- recv s
            close s
            return x
        )
      return $ move (x == -42)

    add :: Linear.IO (Ur Bool)
    add = do
      x <- connect calcServer
        (\s -> do
            s <- selectRight s
            s <- send 4 s
            s <- send 5 s
            (x, s) <- recv s
            close s
            return x
        )
      return $ move (x == 9)
