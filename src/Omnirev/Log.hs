module Omnirev.Log where


type Logs = [String]

data LogLevel
  = Debug   -- 重要では無いが，記録しておきたい場合
  | Info    -- 処理に問題は無いが，重要な情報を持つ場合
  | Warn    -- 言語仕様の範囲内だが，計算や処理に問題がある場合
  | Error   -- 言語仕様と明確に異なる動作をする場合
  deriving (Eq, Ord, Show, Read)

debugLog :: String -> String
debugLog str = "[debug] " ++ str

infoLog :: String -> String
infoLog str = "[info]  " ++ str

warnLog :: String -> String
warnLog str = "[warn]  " ++ str

errorLog :: String -> String
errorLog str = "[error] " ++ str

logAt :: LogLevel -> String -> String
logAt Debug   = debugLog
logAt Info    = infoLog
logAt Warn    = warnLog
logAt Error   = errorLog