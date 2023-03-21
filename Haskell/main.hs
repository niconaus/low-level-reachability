import Examples as P ( program, post, longDiv, faultyQsort,indirectJump)
import Types
import Preconditions ( preconditions, precondition)
import System.IO ( hSetBuffering, BufferMode(LineBuffering), stdout )
import System.Environment ( getArgs )
import Data.Time.Clock (diffUTCTime, getCurrentTime)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let limit = if null args then id else
              case reads (head args) of [(n,"")] -> take n
                                        _        -> id

  let exploitTree = extend (leaf.simp) $ preconditions P.program P.post :: Mirkwood
  --print exploitTree
  --print $ forceTree $ takeTree 50 exploitTree

  start <- getCurrentTime
  mapM_ print $ limit $ filter (/=Expr (Not true)) $ deforest (fastheur (++)) exploitTree
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."

-- Long Division example

example1 :: Maybe Predicate
example1 = uncurry precondition longDiv

-- for interactive use
exploits :: (Prog,Predicate) -> [Predicate]
exploits (prog,p) = filter (/=Expr (Not true)) $ deforest (fastheur (++)) exploitTree
  where exploitTree =  extend (leaf.simp) $ preconditions prog p :: Mirkwood


-- To run the paper examples: preconditions 





-- CURRENT STATE OF THIS APPLICATION

-- Solver does not work for memory

-- Simplifier does not work as thorrow as could be

-- LLVM module is depricated
