module Main where

import Asterius.Types
import Omnirev.AbsOmnirev
import Omnirev.CheckOmnirev
import Omnirev.ErrM
import Omnirev.EvalOmnirev
import Omnirev.Exhaustive
import Omnirev.ParOmnirev
import Omnirev.PrintOmnirev
import Omnirev.LexOmnirev


interpret' :: String -> String
interpret' src = let ts = myLexer src in case pProgram ts of
  Bad s -> unlines ["Parse              Failed...", s]
  Ok tree -> case check tree of
    Bad s -> unlines ["Inference          Failed...", s]
    Ok (env, clog) -> case eval env of
      Bad s -> unlines ["Eval               Failed...", s]
      Ok (res, elog) -> unlines $ map (\(i,tm) -> printTree i ++ " = " ++ printTree tm) res

interpret :: JSString -> JSString
interpret = toJSString . interpret' . fromJSString

foreign export javascript "interpret" interpret :: JSString -> JSString

main ::IO ()
main = pure ()