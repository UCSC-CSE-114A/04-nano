import           Control.Monad.Trans
import           System.Console.Haskeline
import qualified Language.Nano.Types  as Nano
import qualified Language.Nano.Eval   as Nano

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "nano> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> liftIO (exec input) >> loop

exec :: String -> IO ()
exec s = (print =<< Nano.execString s) `catch` (print . Nano.errMsg)

-- process input = do
  -- let tokens = Nano.parseTokens input
  -- putStrLn ("Tokens: " ++ show tokens)
  -- let ast = Nano.parseExpr input
  -- putStrLn ("Syntax: " ++ show ast)
  -- case ast of
    -- Left err -> do
      -- putStrLn "Parse Error:"
      -- print err
    -- Right ast -> exec ast
