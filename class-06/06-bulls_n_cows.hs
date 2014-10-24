{-
   Запрограммируйте игру «Быки и коровы» (https://ru.wikipedia.org/wiki/Быки_и_коровы)
   в варианте «компьютер загадывает — пользователь отгадывает».
-}
import System.Environment
import System.IO
import Data.Char
import System.Random
import Data.List.Split
import Data.List

randN gen = do
					take 4 $ randomRs (1,9) gen ::[Integer]

playGame = do 
					gen <- newStdGen
					let list = randN gen
					if (length $ group $ sort list) /=4 then playGame else doGame list
snd'(x:xx:xs)=xx
snd'(x:[])=if head x == 1 then x else []
					
checkBuls x x2 = length $ snd' $ group $ sort  $ snd $ mapAccumL(\xx y-> if ((!!)x xx) == y then (xx+1,1) else(xx+1,0) ) 0 x2
checkCows x x2 = length $ snd' $ group $ sort  $ map(\xx-> if elem xx x then 1 else 0) x2	

doGame x = do 
					print "Enter your Num"
					s<-getLine
					let x2 = (map (\x-> fromIntegral $ digitToInt x) s)
					let cows = checkCows x x2
					let buls = checkBuls x x2
					print "Cows"
					print (cows-buls)
					print "Buls"
					print buls
					if buls ==4 then print("You Won, gg wp , the number is " ++ s) else doGame x
					

main = playGame
