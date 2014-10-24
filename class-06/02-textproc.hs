{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}
import System.Environment
import System.IO
import Data.Char
import System.Random
import Data.List.Split

strCount f = do
	handle <- openFile f ReadMode
	contents <- hGetContents handle	
	print (length(lines contents))
	hClose handle

insStart f s = do
	contents <- readFile f
    	length contents `seq` (writeFile f $ s++ contents)

insEnd f s = do
	appendFile f s
	--contents <- readFile f
    	--length contents `seq` (writeFile f $ contents ++ s)

toUppFile f = do
	contents <- readFile f
	putStr (map toUpper contents)

mergeFiles f1 f2 = do
	contents<-readFile f1
	contents2<-readFile f2
	let r =myMerge (lines contents) (lines contents2)
	putStr (r)
	
myMerge (x:xs)(y:ys) = x++y++(myMerge xs ys)

doRandStrs cstr csymb gen = do
						take cstr $ splitEvery csymb $ randomRs ('a','z') gen

writeToFile f (x:[]) = appendFile f x
writeToFile f (x:xs) = do 
					appendFile f (x++['\n'])
					writeToFile f xs 

randomFile f cstr csymb =  do	
						gen <- newStdGen
						writeFile f "leeeets get it started \n"
						let list = doRandStrs cstr csymb gen
						writeToFile f list
main = do 
	[fileT1, fileT2,strT2, fileT3,file1T4,file2T4,fileT5,strMax,symbMax] <- getArgs
	strCount fileT1
	insStart fileT2 strT2
	insEnd fileT2 strT2
	toUppFile fileT3
	mergeFiles file1T4 file2T4
	let str=read (strMax):: Int
	let symb= read (symbMax) :: Int
	randomFile fileT5 str symb