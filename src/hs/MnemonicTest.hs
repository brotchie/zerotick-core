module Main where

import ZeroTick.Mnemonic

main :: IO ()
main = do print $ map parseMnemonic 
                    [ "STK BHP ASX AUD"
                    , "FUT ES 20100215 GLOBEX USD"
                    , "OPT AAPL 20120815 P 300 SMART USD"
                    ]
