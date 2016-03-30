module Main where
import Probe
import Data.List
import Data.Maybe
import System.Random
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import Text.Printf


main:: IO()
main =
        do
            putStrLn "Enter 1st hand (Введите руку первого игрока):"
            s1 <- getLine
            let gambler1 = parseHand s1
            putStrLn "Enter 2nd hand:"
            s2 <- getLine
            let gambler2 = parseHand s2
            gen <- getStdGen

{-
            let gambler1 = [Card Queen Clubs, Card Queen Diamonds]
            let gambler2 = [Card Ace Hearts, Card King Hearts]
-}
            let tailDeck = generateDeck \\ (gambler1 ++ gambler2)
            let index = genRandomIndex (length tailDeck - 1) gen
            let count = 100000:: Int
            let oneHalf = 5
            let cnt1 = 3 * count
            let cnt2 = 3 * count
            let cnt3 = 3 * count
            let cnt4 = 3 * count
            let cnt5 = 3 * count

            let in1 = index
            let in2 = drop 1 index
            let in3 = drop 2 index
            let in4 = drop 3 index
            let in5 = drop 4 index

            putStr "Total amount:"
            print (count * 15)
            putStr "Threads load:"
            print [cnt1,cnt2,cnt3,cnt4,cnt5]
            putStr "1st hand:"
            print gambler1
            putStr "2nd hand:"
            print gambler2

            let myRes = runEval $
                            do
                            [i1,i2,i3,i4,i5] <- sequence [
                                                rpar $ force (letsPlayStats tailDeck gambler1 gambler2 cnt1 in1),
                                                rpar $ force (letsPlayStats tailDeck gambler1 gambler2 cnt2 in2),
                                                rpar $ force (letsPlayStats tailDeck gambler1 gambler2 cnt3 in3),
                                                rpar $ force (letsPlayStats tailDeck gambler1 gambler2 cnt4 in4),
                                                rpar $ force (letsPlayStats tailDeck gambler1 gambler2 cnt5 in5)
                                                         ]
                            return (zipWith (+) i1 $ zipWith (+) i2 $ zipWith (+) i3 $ zipWith (+) i4 i5)

            let divL' =  (*100) . (/fromIntegral(count * 15)) . fromIntegral

            putStrLn $ printf "Wins = %d%%"  (round $ divL' $ head myRes :: Int)
            putStrLn $ printf "Royal flashes = %d(%.4f%%)"  (myRes!!10) (divL' $ myRes!!10 :: Double)
            putStrLn $ printf "Straight flashes = %d(%.4f%%)" (myRes!!9) (divL' $ myRes!!9 :: Double)
            putStrLn $ printf "Quads = %d(%.4f%%)"  (myRes!!8) (divL' $ myRes!!8 :: Double)
            putStrLn $ printf "FullHouses = %d(%.4f%%)"  (myRes!!7) (divL' $ myRes!!7 :: Double)
            putStrLn $ printf "Flashes = %d(%.4f%%)"  (myRes!!6) (divL' $ myRes!!6 :: Double)
            putStrLn $ printf "Straights = %d(%.4f%%)"  (myRes!!5) (divL' $ myRes!!5 :: Double)
            putStrLn $ printf "Sets = %d(%.4f%%)"  (myRes!!4) (divL' $ myRes!!4 :: Double)
            putStrLn $ printf "2 pairs = %d(%.4f%%)"  (myRes!!3) (divL' $ myRes!!3 :: Double)
            putStrLn $ printf "Pairs = %d(%.4f%%)"  (myRes!!2) (divL' $ myRes!!2 :: Double)
            putStrLn $ printf "No combo = %d(%.4f%%)"  (myRes!!1) (divL' $ myRes!!1 :: Double)




