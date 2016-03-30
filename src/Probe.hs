module Probe
(Rank(..),
 Card(..),
 Hand(..),
 Combo(..),
 Suit(..),
 ComboType(..),
 FullRank(..),
 Result(..),
 getCombo,
 letsPlayProto,
 letsPlay,
 letsPlayStats,
 generateDeck,
 genRandomIndex,
 parseHand)

where

import Data.List
import Data.Maybe
import System.Random
import Control.Parallel
import GHC.Generics


-- Тип "Ранг карты"
data Rank =  Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack |  Queen | King | Ace
     deriving ( Read, Eq, Ord, Enum)

instance Show Rank where
    show Two    = "2"
    show Three  = "3"
    show Four   = "4"
    show Five   = "5"
    show Six    = "6"
    show Seven  = "7"
    show Eight  = "8"
    show Nine   = "9"
    show Ten    = "T"
    show Jack   = "J"
    show Queen  = "Q"
    show King   = "K"
    show Ace    = "A"

-- Тип "Масть карты"
data Suit = Hearts | Diamonds | Clubs | Spades
     deriving (Read, Eq, Ord, Enum)

instance Show Suit where
    show Hearts     = "h"
    show Diamonds   = "d"
    show Clubs      = "c"
    show Spades     = "s"

-- Тип "Карта"
data Card = Card {rank::Rank,
                  suit::Suit}
      deriving (Read)
instance Show Card where
    show c = show(rank c) ++ show(suit c)

 -- Переписываем классы Eq и Ord для адекватного сравнения двух карт
instance Eq Card where
    Card r1 s1  == Card r2 s2 = (r1 == r2) && (s1 == s2)

instance Ord Card where
    compare x y
     |  rank x == rank y = EQ
     |  rank x <  rank y = LT
     |  otherwise = GT

-- Тип "Рука" или "Набор карт"
type Hand = [Card]

-- Тип "Полный ранг покерной комбинации"
type FullRank = [Rank]

-- Тип "Виды покерных комбинаций"
data ComboType = NoCombo | Pair | TwoPair | Set | Straight | Flash | FullHouse | Quads | StraightFlash | RoyalFlash
     deriving (Show, Read, Eq, Ord, Enum)

-- Тип "Покерная комбинация с весом"
data Combo = Combo {comboType::ComboType,
                    weight :: FullRank,
                    record :: Hand,
                    kicker :: Hand}
     deriving (Show, Read)


 -- Переписываем классы Eq и Ord для адекватного сравнения двух комбинаций
instance Eq Combo where
    Combo ct1 w1 r1 k1  == Combo ct2 w2 r2 k2 = (r1 == r2) && (ct1 == ct2) && (k1 == k2)

instance Ord Combo where
    compare x y
     |  (weight x == weight y) && (comboType x == comboType y) && (kicker x `compare` kicker y == EQ) = EQ
     |  (comboType x < comboType y) || ((comboType x == comboType y) && (weight x < weight y)) ||
                                       ((comboType x == comboType y) && (weight x == weight y) && (kicker x < kicker y)) = LT
     |  otherwise = GT

type Result = (Bool, Combo , Combo, Hand)

nullCombo = Combo NoCombo [] [] []


isNotNullCombo :: Combo -> Bool
isNotNullCombo c = comboType c /= NoCombo

------------------------------- Вспомогательные функции ---------------------------------------------------
-- Кривой вывод результатов
typeResult :: Result -> String
typeResult (b,c1,c2,h)
            = " Board: " ++ show h ++ ['\n'] ++ "Combo 1st gambler: " ++ show c1 ++ "\n Combo 2nd gambler: " ++ show c2
              ++ " 1st player win:" ++ show b ++ "\n"

-- Анализ статистики розыгрышей

getRank :: Char -> String
getRank c = fromJust $ lookup c $ zip [head (show x) | x <-[(Two)..(Ace)]] ["Two", "Three","Four","Five","Six","Seven","Eight","Nine","Ten","Jack"," Queen","King","Ace"]


getSuit :: Char -> String
getSuit c = fromJust $ lookup c $ zip [head (show x) | x <-[(Hearts)..(Spades)]] ["Hearts","Diamonds","Clubs","Spades"]


parseCard :: String -> Card
parseCard [] = error "Empty card!"
parseCard [a] = error "Invalid card!"
parseCard (a:b:_)  =
                        let s = "Card {rank=" ++ (getRank a) ++ ",suit=" ++ (getSuit b) ++ "}"
                        in
                        read s :: Card



parseHand :: String -> Hand
parseHand s =
            let r = words s
            in
            [parseCard (head r), parseCard (last r)]


-- Полная сортировка по масти и по рангу
sortFull :: Hand -> Hand
sortFull h = concat [sortSuit s h | s <- [(Hearts)..(Spades)] ]
                where sortSuit sl hl =  [x | x <- sort hl, suit x == sl]

-- Функция, возвращающая "Колоду для холдема"
generateDeck :: Hand
generateDeck = [Card r s | r <- [(Two)..(Ace)], s <- [(Hearts)..(Spades)]]


-- Генератор бесконечного случайного списка индексов для боарда
genRandomIndex :: Int -> StdGen -> [Int]
genRandomIndex len sg = randomRs (0,len) sg :: [Int]

-- Получить список с неповторяющимися n-картами из бесконечного списка
getBoard :: [Int] -> [Int] -> Int -> [Int]
getBoard [] _ _  = error "Empty random list!!!"
getBoard _ acc 0 = acc
getBoard (h:ts) acc n = if h `elem` acc then getBoard ts acc n
                          else getBoard ts (acc ++ [h]) (n-1)
------------------------------- Функции, определяющие наличие покерной комбинации --------------------------

--------------- 1. Проверка на стрит
------- Функция-прототип
isStraightProto :: Hand -> Hand -> Int -> Combo
isStraightProto [] _ _ = error "Empty hand!!!"
isStraightProto (h:xs) rs n
    | n == 4  = Combo Straight [rank h] (rs ++ [h]) []
    | length xs + n < 4 = nullCombo
    | null xs = nullCombo
    | rank h == Ace = isStraightProto xs [] 0
    | succ(rank h) == rank(head xs) = isStraightProto xs (rs ++ [h]) (n + 1)
    | rank h == rank(head xs) = isStraightProto xs rs n
    | otherwise = isStraightProto xs [] 0
------- Интерфейсная функция
isStraight :: Hand -> Combo
isStraight h = isStraightProto (sort h) [] 0

--------------- 2. Проверка на флеш
------- Функция-прототип
isFlashProto ::  Hand -> Int -> Hand -> Combo
isFlashProto _ _ [] = error "Empty hand!!!"
isFlashProto rs n (h:xs)
    | n == 4  = Combo Flash [rank h] (rs ++ [h]) []
    | length xs + n < 4 = nullCombo
    | null xs  = nullCombo
    | suit h == suit(head xs) = isFlashProto (rs ++ [h]) (n + 1) xs
    | otherwise = isFlashProto [] 0 xs

------- Интерфейсная функция
isFlash :: Hand -> Combo
isFlash  h = isFlashProto [] 0 (sortFull h)

--------------- 3. Проверка на пару или 2 пары

------- Функция-прототип
--- Параметры: Рука - Запись комбинации - Кикер - Полный ранг - Число найденных пар
isPairProto :: Hand -> Hand -> Hand -> FullRank -> Int -> Combo
isPairProto [] rs ks fr c
    | c == 1 =  Combo Pair fr rs (take 3 ks)
    | c > 1 = Combo TwoPair fr rs [head ks]
    | otherwise = nullCombo
isPairProto (h:n:xs) rs ks fr c
    | (rank h == rank n) && (c < 2)  =  isPairProto xs (h:n:rs) ks (fr ++ [rank n]) (c+1)
    | otherwise = isPairProto (n:xs) rs (ks ++ [h]) fr c
isPairProto (h:xs) rs ks fr c = isPairProto xs rs (ks ++ [h]) fr c

------- Интерфейсная функция
isPair :: Hand -> Combo
isPair h = isPairProto (sortBy (flip compare) h) [] [] [] 0

--------------- 4. Проверка на сет

------- Функция-прототип
isSetProto :: Hand -> Hand -> FullRank -> Hand -> Bool -> Combo
isSetProto [] _ _ _ _ = nullCombo
isSetProto (c1:c2:c3:xs) rs fr ks b                 --isSetProto xs [c1,c2,c3] [rank c1] ks True
    | (rank c1 == rank c2) && (rank c2 == rank c3) = Combo Set [rank c1] [c1,c2,c3]  (take 2 $ sortBy (flip compare) (ks ++ xs))
    | otherwise = isSetProto (c2:c3:xs) rs fr (c1:ks) b
isSetProto (c1:xs) rs fr ks b    = nullCombo

------- Интерфейсная функция
isSet :: Hand -> Combo
isSet h = isSetProto (sort h) [] [] [] False

--------------- 5. Проверка на каре

------- Функция-прототип
isQuadsProto :: Hand -> Hand -> FullRank -> Hand -> Bool -> Combo
isQuadsProto [] _ _ _ _  = nullCombo
isQuadsProto (c1:c2:c3:c4:xs) rs fr ks b
    | (rank c1 == rank c2) && (rank c2 == rank c3) && (rank c3 == rank c4)  =
                                            Combo Quads [rank c1] [c1,c2,c3,c4] [maximum(ks ++ xs)]
    | otherwise = isQuadsProto (c2:c3:c4:xs) rs fr (c1:ks) b
isQuadsProto (c1:xs) rs fr ks b = nullCombo


------- Интерфейсная функция
isQuads :: Hand -> Combo
isQuads h = isQuadsProto (sort h) [] [] [] False

--------------- 6. Проверка на фуллхаус

------- Интерфейсная функция
isFullHouse :: Hand -> Combo
isFullHouse h =
                let setCombo = isSetProto (sortBy (flip compare) h) [] [] [] False
                    pairCombo = isPair (h \\ record setCombo) in
                       if isNotNullCombo setCombo && isNotNullCombo pairCombo then
                          Combo FullHouse (weight setCombo ++ weight pairCombo) (record setCombo ++ record pairCombo) []
                          else nullCombo

--------------- 7. Проверка на стрит-флеш

------- Функция-прототип
isStraightFlashProto :: Hand -> Hand -> Int -> Combo
isStraightFlashProto [] _ _ = error "Empty hand!!!"
isStraightFlashProto (h:xs) rs n
    | n == 4  =   if rank h == Ace then
                    Combo RoyalFlash [] (rs ++ [h]) []
                  else
                    Combo StraightFlash [rank h] (rs ++ [h]) []
    | length xs + n < 4 = nullCombo
    | null xs = nullCombo
    | rank h == Ace = isStraightFlashProto xs [] 0
    | (succ(rank h) == rank(head xs)) && (suit h == suit (head xs)) = isStraightFlashProto xs (rs ++ [h]) (n + 1)
    | otherwise = isStraightFlashProto xs [] 0
------- Интерфейсная функция
isStraightFlash :: Hand -> Combo
isStraightFlash h = isStraightFlashProto (sortFull h) [] 0



-------------- Экспериментаьная функция определения пар, сетов, фуллхаусов и каре
isRepCombo :: Hand -> Combo
isRepCombo h
    | length h < 7 = error "Invalid hand!"
    | otherwise = let
                    sfc = isStraightFlash h
                    sc = isStraight h
                    fs = filter (\x-> length x >= 5) $ groupBy (\x y -> (suit x == suit y)) $ sortFull h
                    reps = map length $ groupBy (\x y -> rank x == rank y) $ sort h in
                        if fs /= [] then
                            if isNotNullCombo sfc  then sfc
                                else isFlash h
                          else
                            if 4 `elem` reps then isQuads h
                                else
                                    if [2,3] `isSubsequenceOf` sort reps then isFullHouse h
                                        else
                                            if isNotNullCombo sc then sc
                                                else
                                                    if 3 `elem` reps then isSet h
                                                        else
                                                            if 2 `elem` reps then isPair h
                                                                else Combo NoCombo [] (drop 2 $ sort h) (take 5 $ reverse h)

------------------------------ Основная функция определения ранга
getCombo :: Hand -> Combo
getCombo h =
            let fc = filter isNotNullCombo [isStraightFlash h, isQuads h, isFullHouse h, isFlash h, isStraight h, isSet h, isPair h]
                sorted = sort h
            in
                if null fc then Combo NoCombo [] (drop 2 sorted) (take 5 (reverse sorted))
                   else head fc

------------------------------------  Функция, осуществляет n - розыгрышей
-- Колода - Первая рука - Вторая рука  - Случайные индексы -> Число розыгрышей - Список результатов
letsPlayProto :: Hand -> Hand -> Hand ->[Int] -> [Result]
letsPlayProto  td h1 h2 i =
   --- | n == 0 = []
   -- | otherwise =
                    let
                        board = [td!!x| x <- getBoard i [] 5]
                        c1 = isRepCombo (h1 ++ board)
                        c2 = isRepCombo (h2 ++ board)

                     in
                        (c1 >= c2, c1, c2, board) : letsPlayProto  td h1 h2 (drop 10 i)

------------------------------------  Функция, осуществляет n - розыгрышей
-- Колода - Первая рука - Вторая рука  - Случайные индексы -> Число розыгрышей - Список результатов
letsPlay :: Hand -> Hand -> Hand -> [Int] -> [Bool]
letsPlay  td h1 h2 i =
   -- | cn == 0 = []
   -- | otherwise =
                    let
                        board = [td!!x| x <- getBoard i [] 5]
                        c1 = isRepCombo (h1 ++ board)
                        c2 = isRepCombo (h2 ++ board)
                     in
                        (c1 >= c2):letsPlay td h1 h2 (drop 5 i)

letsPlayStats :: Hand -> Hand -> Hand -> Int -> [Int] -> [Int]
letsPlayStats  td h1 h2 cn i
    | cn == 0 = replicate 12 0
    | otherwise =
                    let
                        board = [td!!x| x <- getBoard i [] 5]
                        c1 = isRepCombo (h1 ++ board)
                        c2 = isRepCombo (h2 ++ board)
                        pos = fromJust $ lookup (comboType c1) $ zip [(NoCombo)..(RoyalFlash)] [1..10]

                        result = [sum $ 0:[1| c1 >= c2]]  ++ replicate (pos - 1) 0 ++ [1] ++ replicate (10 - pos) 0
                                                            ++ [sum $ 0:[1| c1 == c2]]
                     in
                        if c1 > c2 then
                           zipWith (+) result (letsPlayStats td h1 h2 (cn-1) (drop 5 i))
                        else if c1 == c2 then
                             zipWith (+) ([head result] ++ replicate 10 0 ++ [last result]) (letsPlayStats td h1 h2 (cn-1) (drop 5 i))
                             else
                                letsPlayStats td h1 h2 (cn-1) (drop 5 i)

{-
myFlash = [Card Five Clubs ,Card Five Hearts , Card Six Hearts , Card Six Clubs , Card Seven Hearts , Card Eight Hearts , Card Nine Hearts]
myHand2 = [Card Five Hearts , Card Six Hearts , Card Seven Hearts , Card Eight Hearts , Card Nine Diamonds, Card Two Diamonds, Card Ace Spades]
myHand3 = [Card Five Hearts , Card Six Hearts , Card Seven Hearts , Card Eight Hearts , Card Ten Hearts , Card Nine Clubs,Card King Clubs]

my2Pair = [Card Five Hearts , Card Five Diamonds , Card Seven Hearts , Card Eight Hearts , Card Eight Diamonds, Card King Hearts , Card Ace Diamonds ]
myPair = [Card Five Hearts , Card Five Diamonds , Card Seven Hearts , Card Eight Diamonds, Card King Hearts , Card Ace Diamonds]
my3Pair = [Card Five Hearts , Card Five Diamonds , Card Ace Hearts , Card Eight Hearts , Card Eight Diamonds, Card King Hearts , Card Ace Diamonds ]

mySet1 = [Card Five Hearts , Card Five Diamonds , Card Five Clubs, Card Eight Diamonds, Card King Hearts , Card Ace Diamonds]
mySet2 = [Card Three Diamonds, Card Jack Clubs, Card Eight Diamonds, Card King Hearts ,  Card Five Hearts , Card Five Diamonds , Card Five Clubs]

myQuads1 = [Card Five Hearts , Card Five Diamonds , Card Five Clubs, Card Eight Diamonds, Card Five Spades , Card Ace Diamonds]
myQuads2 = [Card Ace Diamonds, Card Jack Clubs, Card Eight Diamonds, Card Five Spades ,  Card Five Hearts , Card Five Diamonds , Card Five Clubs]

myFullHouse1 = [Card King Diamonds, Card Jack Clubs, Card Eight Diamonds, Card King Hearts ,  Card Five Hearts , Card Five Diamonds , Card Five Clubs]
myFullHouse2 = [Card King Diamonds, Card Jack Clubs, Card Eight Diamonds, Card King Hearts ,  Card Five Hearts , Card Five Diamonds , Card King Clubs]

mySF = [Card Five Hearts , Card Six Clubs , Card Six Hearts , Card Seven Hearts , Card Eight Hearts , Card Nine Hearts]
mySF2 = [Card Ten Hearts , Card Six Clubs , Card Six Diamonds , Card Ace Hearts , Card King Hearts , Card Jack Hearts , Card Queen Hearts]

deck = [Card Ace Clubs,Card King Clubs, Card Queen Clubs, Card Jack Clubs, Card Ten Clubs]

mainDeck = generateDeck
gambler1 = [Card Queen Hearts, Card Queen Diamonds]
gambler2 = [Card Ace Hearts, Card King Hearts]

-- Исключим из колоды карты игроков
tailDeck = mainDeck \\ (gambler1 ++ gambler2)

-- [Card Jack Diamonds,Card Jack Spades,Card Jack Hearts,Card Six Diamonds, Card Jack Clubs]
g1 = [Card Ace Hearts, Card Queen Hearts, Card Eight Clubs, Card Seven Spades, Card Jack Spades, Card Five Hearts,Card Three Hearts]
g2 = [Card Ace Spades, Card Queen Spades, Card Eight Diamonds, Card Six Hearts, Card Jack Hearts,Card Five Hearts,Card Three Hearts]
c1 = getCombo g1
c2 = getCombo g2
-}


