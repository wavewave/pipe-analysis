import Control.Monad
import Control.Proxy
import System.IO

lines' :: (Proxy p) => Handle -> () -> Producer p String IO ()
lines' h () = runIdentityP loop where
  loop = do 
    eof <- lift $ hIsEOF h 
    if eof 
    then return () 
    else do 
      str <- lift $ hGetLine h 
      respond str 
      loop 


promptInt :: (Proxy p) => () -> Producer p Int IO r 
promptInt () = runIdentityP $ forever $ do 
  lift $ putStrLn "Enter an Integer " 
  n <- lift readLn 
  respond n 


printer :: (Proxy p, Show a) => () -> Consumer p a IO r 
printer () = runIdentityP $ forever $ do 
  a <- request () 
  lift $ putStrLn "Received a value:"
  lift $ print a

main = do 
  withFile "test.txt" ReadMode $ \h -> runProxy $ lines' h >-> printer 
  runProxy $ promptInt >-> printer :: IO ()


