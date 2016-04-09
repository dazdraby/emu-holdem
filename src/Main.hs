module Main where
import Probe
import Data.List
import Data.Maybe
import System.Random
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import Text.Printf

splitListN :: Int -> [Int] -> [[Int]]
splitListN _ [] = []
splitListN n xs = take n xs:splitListN n (drop n xs)

{-parMap f xs = map f xs `using ` parList rdeepseq-}


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

 {-           let gambler1 = [Card Queen Clubs, Card Queen Diamonds]
            let gambler2 = [Card Ace Hearts, Card King Hearts]-}
            let tailDeck = generateDeck \\ (gambler1 ++ gambler2)
            let count = 1000000 :: Int

            putStr "Total amount:"
            print count
            putStr "1st hand:"
            print gambler1
            putStr "2nd hand:"
            print gambler2

            let play = letsPlayStats tailDeck gambler1 gambler2 -- force

            let newIndex = take count . splitListN 15 . genRandomIndex (length tailDeck - 1) $ gen

            let myRes = foldl1' (zipWith (+)) $ parMap rdeepseq play newIndex

            let divL' =  (*100) . (/fromIntegral count) . fromIntegral

            putStrLn $ printf "Wins = %d%%"  (round $ divL' $ head myRes :: Int)
            putStrLn $ printf "Draws = %d(%.4f%%)"  (myRes!!11) (divL' $ myRes!!11 :: Double)
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



