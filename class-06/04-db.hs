{-
  Дан текстовый файл с информацией о студентах факультета в следующем формате:

    ФАМИЛИЯ ИМЯ ОТЧЕСТВО;ВОЗРАСТ;КУРС;ГРУППА

  Имя этого файла задаётся параметром командной строки. Остальные параметры определяют,
  какое действие следует выполнить:

  1) Вычислить средний возраст студентов заданной группы заданного курса.
  2) Вычислить количество студентов в каждой группе каждого курса.
  3) Создать файлы (с именами "<КУРС>_<ГРУППА>.txt") со списками всех студентов групп в формате
        ФАМИЛИЯ И.О.

-}
import Data.List
import Data.List.Split
import System.Environment
import System.IO

			
avgAge2 xx = read (snd' $ splitOn ";" xx) ::Integer

avgAge([])=0
avgAge(x:xs)=avgAge2 x + avgAge xs
	
fst' (x:xs) = x
snd' (x:[]) = []		
snd' (x:xx:xs) = xx
thr (x:[])=[]
thr (x:xx:xxx:xs)=xxx
fr (x:xx:xxx:xxxx:xs)=xxxx
				
onlyCoGr co gr xx = do 
					let x = splitOn ";" xx
					read(thr x) == co && read(fr x)==gr


studAvgAge f co gr = do 
			contents <-readFile f
			let list=lines contents
			let list2 = filter (onlyCoGr co gr) list
			let m = avgAge list2
			print m

fCourseG x y = (read (thr $ splitOn ";" x) ::Integer) == (read (thr $ splitOn ";" y) :: Integer)


fCourseS x y  
			| (read(thr $ splitOn ";" x)::Integer) < (read(thr $ splitOn ";" y)::Integer) =LT
			| otherwise =GT
fGroupG x y = (read (fr $ splitOn ";" x) ::Integer) == (read (fr $ splitOn ";" y) :: Integer)

occurencies= map (\xs -> groupBy (fGroupG) xs) . groupBy (fCourseG).sortBy (fCourseS)

countStud f = do
				contents <-readFile f
				let list=lines contents
				let list2 = occurencies list
				let list3 =map(\xs->map(\xs->(thr $ splitOn ";"$ head xs,fr $ splitOn ";" $ head xs, length xs))xs) list2
				print "Stud Count "
				print list3		

studStr xs = map(\x->(fst' $ splitOn " " $ fst' $ splitOn ";" x) ++" " ++ [(fst' $ snd' $ splitOn " " $ fst' $ splitOn ";" x)]++". " ++ [(fst' $ thr $ splitOn " " $ fst' $ splitOn ";" x)]++"." )xs

allStud f = do 
				contents <-readFile f
				let list=lines contents
				let list2 = occurencies list
				mapM(\xs->mapM(\xs->appendFile ((thr $ splitOn ";"$ head xs)++"_"++(fr $ splitOn ";" $ head xs)++".txt") (unlines $ studStr xs) )xs) list2
				return()

				
main = do 
		[file,avg,co,gr,count,crt] <- getArgs
		if avg == "avg" then (studAvgAge file (read(co)::Integer) (read(gr)::Integer)) else return()
		if count == "count" then countStud file else return()
		if crt == "crt" then allStud file else return()
		return()