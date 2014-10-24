{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}
import System.Environment
import System.IO
import Data.Char
import System.Random
import Data.List.Split
import Data.List
doRandCord gen = do
					take 100 $ randomRs (-100,100) gen
					
writeToFile f (x:xx:[]) = do 
							appendFile f (show(x :: Int)++[' ']++show(xx :: Int)++['\n'])
							--appendFile f (show(xx)++['\n'])
writeToFile f (x:xx:xs) = do 
					appendFile f (show(x)++[' ']++show(xx)++['\n'])
					writeToFile f xs 


generateFileCoord f = do	
					gen <- newStdGen
					let list = doRandCord gen
					writeFile f ""						
					writeToFile f list
					
watqart' (x,y)
		|x>0 && y>0 = 1 
		|x>0 && y<0 = 4 
		|x<0 && y>0 = 2  
		|otherwise = 3	
		
fst' (x:xs)=x
snd' (x:xx:xs)=xx	
occurencies = map (\xs -> (head xs, length xs)) . group . sort

	
howMuch([])=[]	
howMuch (x:xs) = do 
			let x1=splitOn " " x
			watqart' (read(fst' x1),read(snd' x1)):howMuch xs
			
countOfCoords f=do
			contents <-readFile f
			let list = lines contents
			let list2 = howMuch list
			let list3 = occurencies list2
			print (list3)
			
			
main = undefined
