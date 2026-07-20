module Display
  ( bold,
    dim,
    red,
    green,
    yellow,
    cyan,
    reset,
    printColored,
    printErr,
    printWarn,
    printOk,
    printStep,
    stripAnsi,
  )
where

import System.IO (hIsTerminalDevice, stdout)

esc :: String -> String
esc code = "\ESC[" ++ code ++ "m"

reset, bold, dim, red, green, yellow, cyan :: String
reset = esc "0"
bold = esc "1"
dim = esc "2"
red = esc "31"
green = esc "32"
yellow = esc "33"
cyan = esc "36"

stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC' : '[' : rest) = stripAnsi (drop 1 (dropWhile (/= 'm') rest))
stripAnsi (c : cs) = c : stripAnsi cs

printColored :: String -> IO ()
printColored s = do
  isTTY <- hIsTerminalDevice stdout
  putStr (if isTTY then s else stripAnsi s)

printErr :: String -> IO ()
printErr = printColored

printWarn :: String -> IO ()
printWarn = printColored

printOk :: String -> IO ()
printOk msg =
  printColored $ bold ++ green ++ "  ok" ++ reset ++ "  " ++ msg ++ "\n"

printStep :: String -> String -> IO ()
printStep tag msg =
  printColored $ dim ++ "  " ++ padR 10 tag ++ reset ++ "  " ++ msg ++ "\n"
  where
    padR n s = s ++ replicate (max 0 (n - length s)) ' '
