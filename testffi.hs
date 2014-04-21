import Control.Monad (when, unless, forever)
import Data.Char 
import Pipes
import Pipes.Prelude (tee)
import System.IO (isEOF)

import Data.IORef



import Graphics.UI.Gtk 
import Control.Monad.Trans
 






callbacker :: IORef Int -> (Int -> Int -> IO ()) -> Int -> IO ()
callbacker xref f y = do 
  x <- readIORef xref
  f x y 
 

callbackerPipe :: (Int -> IO ()) -> Consumer Int IO ()
callbackerPipe cb = forever $ do 
  x <- await 
  liftIO $ cb x  
  


stdinLn :: Int -> Producer String IO ()
stdinLn n = do 
  when (n /= 0) $ do 
    eof <- lift isEOF 
    unless eof $ do 
      str <- lift getLine
      if str == "" then stdinLn (n-1) else do 
        yield str
        stdinLn n 

makeCAPITAL = map toUpper

capitalize :: Pipe String String IO ()
capitalize = do 
  x <- await
  yield (makeCAPITAL x)
  capitalize

limiting :: Int -> Pipe a a IO ()
limiting n = do 
  x <- await 
  if n <= 0 then return () else yield x >> limiting (n-1) 


stdoutLn :: Consumer String IO ()
stdoutLn = do 
  str <- await 
  x <- lift $ putStrLn str 
  stdoutLn 

stdoutLnNum :: Consumer Int IO ()
stdoutLnNum = do 
  n <- await 
  x <- lift $ putStrLn (show n)
  stdoutLnNum 


counter :: Int -> Pipe a Int IO ()
counter n = do 
  x <- await 
  yield (n+1)
  counter (n+1)


loop :: Effect IO ()
loop = for (stdinLn 2) $ lift . putStrLn . makeCAPITAL 
       -- (stdinLn 2) >~ stdoutLn  

main' :: IO ()
main' = do 
  xref <- newIORef 3
  let f x y = print (x + y)
  runEffect $ 
    stdinLn 2 
    >-> limiting 5 
    >-> tee (counter 0 >-> callbackerPipe (callbacker xref f)) -- stdoutLnNum)
    >-> capitalize 
    >-> stdoutLn

main :: IO ()
main = do 
  initGUI
  window <- windowNew 
  button <- buttonNewWithLabel "test"
  vbox <- vBoxNew False 0 
  boxPackStart vbox button PackNatural 0 
  containerAdd window vbox 
  button `on` buttonActivated $ do 
    liftIO $ putStrLn "button press"
  window `on` deleteEvent $ tryEvent $ do 
    liftIO $ mainQuit 
  widgetShowAll window 
 
  mainGUI
 

