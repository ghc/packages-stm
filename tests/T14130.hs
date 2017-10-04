module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad            (when)
import           System.Environment

data State = State
  { inp       :: TVar [Int]
  , inbetween :: TChan Int
  , out       :: TChan Int
  , numbers   :: TVar Numbers
  }

data Numbers = Numbers
  { nInpTot          :: Int -- total number of input elements
  , nInpAvail        :: Int -- number of input elements still available
  , nInpProc         :: Int -- number of input elements already processed
  , nInbetweensTot   :: Int -- likewise...
  , nInbetweensAvail :: Int
  , nInbetweensProc  :: Int
  , nOutTot          :: Int
  , nOutAvail        :: Int
  , nOutProc         :: Int
  } deriving Show

noMoreInput :: State -> STM Bool
noMoreInput s = null <$> readTVar (inp s)

noMoreInbetweens :: State -> STM Bool
noMoreInbetweens s = (&&) <$> isEmptyTChan (inbetween s) <*> noMoreInput s

noMoreOutput :: State -> STM Bool
noMoreOutput s = (&&) <$> isEmptyTChan (out s) <*> noMoreInbetweens s

invariants :: State -> STM ()
invariants s = do
  ns@(Numbers itot iavail iproc ibtot ibavail ibproc otot oavail oproc) <-
    readTVar (numbers s)
  when (iavail + iproc /= itot) $ error $ "invariant violated (1) " ++ show ns
  when (ibavail + ibproc /= ibtot) $
    error $ "invariant violated (2) " ++ show ns
  when (oavail + oproc /= otot) $ error $ "invariant violated (3) " ++ show ns
  when
    (any
       (< 0)
       [itot, iavail, iproc, ibtot, ibavail, ibproc, otot, oavail, oproc]) $
    error $ "invariant violated (4) " ++ show ns

initState :: String -> [Int] -> STM State
initState str ns = do
  nsT <- newTVar ns
  c1 <- newTChan
  c2 <- newTChan
  let n = length ns
  nums <- newTVar (Numbers n n 0 0 0 0 0 0 0)
  let newS = State nsT c1 c2 nums
  when (str == "check") (alwaysSucceeds (invariants newS))
  return newS

inputLoop :: State -> IO ()
inputLoop s = do
  done <- atomically . noMoreInput $ s
  if done
    then putStrLn "Done!" >> return ()
    else do
      atomically $ do
        is <- readTVar (inp s)
        writeTChan (inbetween s) ((* 2) . head $ is)
        ns <- readTVar (numbers s)
        let iavail = nInpAvail ns
            iproc = nInpProc ns
            ibtot = nInbetweensTot ns
            ibavail = nInbetweensAvail ns
            ns' =
              ns
              { nInpAvail = pred iavail
              , nInpProc = succ iproc
              , nInbetweensTot = succ ibtot
              , nInbetweensAvail = succ ibavail
              }
        writeTVar (numbers s) ns'
        writeTVar (inp s) (tail is)
      inputLoop s

inbetweenLoop :: State -> IO ()
inbetweenLoop s = do
  done <- atomically . noMoreInbetweens $ s
  if done
    then return ()
    else do
      atomically $ do
        ib <- readTChan (inbetween s)
        writeTChan (out s) (ib `div` 2)
        ns <- readTVar (numbers s)
        let ibavail = nInbetweensAvail ns
            ibproc = nInbetweensProc ns
            otot = nOutTot ns
            oavail = nOutAvail ns
            ns' =
              ns
              { nInbetweensAvail = pred ibavail
              , nInbetweensProc = succ ibproc
              , nOutTot = succ otot
              , nOutAvail = succ oavail
              }
        writeTVar (numbers s) ns'
      inbetweenLoop s

outputLoop :: State -> IO ()
outputLoop s = do
  done <- atomically . noMoreOutput $ s
  if done
    then return ()
    else do
      io <-
        atomically $ do
          o <- readTChan (out s)
          ns <- readTVar (numbers s)
          let oavail = nOutAvail ns
              oproc = nOutProc ns
              ns' = ns {nOutAvail = pred oavail, nOutProc = succ oproc}
          writeTVar (numbers s) ns'
          return (print ns' >> print o)
      io
      outputLoop s

main :: IO ()
main = do
  inpStr <- concat <$> getArgs
  s <- atomically $ initState inpStr [1 .. 1000]
  forkIO $ inputLoop s 
  forkIO $ inbetweenLoop s 
  forkIO $ outputLoop s
  threadDelay $ 1000*1000*1000
