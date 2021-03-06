-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms t = (div t 3600,div (mod t 3600) 60,mod (mod t 3600) 60)

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h*3600+m*60+s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec(h,m,s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt((x1+x2)^2 + (y1+y2)^2)

-- triangle :: ??? -> (Double, Double)
triangle (x1, y1) (x2, y2) (x3,y3) = (p, s)
  where
    p = distance(x1, y1) (x2, y2)+distance(x2, y2) (x3, y3)+distance(x1, y1) (x3, y3)
    s = undefined

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs) = if mod x 2==0 then 1+ nEven(xs) else nEven(xs)

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems []=[]
doubleElems(x:xs) =(x*2) : doubleElems(xs)

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs) = if mod x 2/=0 then x : fltOdd(xs) else fltOdd(xs)

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
delNeg[]=[]
delNeg(x:xs)=if x<0 then delNeg(xs) else x:delNeg(xs)
-- б) увеличить элементы с чётными значениями в два раза;
evenDouble []=[]
evenDouble (x:xs)=if mod x 2==0 then x*2 :evenDouble(xs) else x:evenDouble(xs)
-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
rev[]=[]
rev(x:[])=[]
rev(x:xx:xs)= xx : x : rev(xs) 

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = (x+y) : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
pairList [] _=[]
pairList _ [] =[]
pairList (x:xs)(y:ys)=(x,y):pairList xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
firstNat 0=[]
firstNat n=n: firstNat(n-1)
-- б) в порядке возрастания.
--firstNat' 0=[]
--firstNat' n=[1..n]
reverse' [] acc = acc
reverse' (x:xs) acc = reverse' xs (x:acc) 
rev' a=reverse' a []

firstNat' 0=[] 
firstNat' n = rev' $ firstNat n
-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
pasteElem a (x:[])=[x]
pasteElem a(x:xs)=x:a:pasteElem a xs
-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).

twoList (x:[])=([x],[])
twoList x=(firstPart x,secPart x)
	
firstPart[]=[]
firstPart(x:xx:xs)
		|x==xx = x:firstPart(xx:xs)
		|otherwise = [x]
secPart[]=[]
secPart(x:xx:xs)
		|x==xx = secPart(xx:xs)
		|otherwise = xx:xs
		
--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a взять эелемент с номером
	
-- б) Eq a => [a] -> a -> Bool существует ли в списке элемент

-- в) [a] -> Int -> [a]	 выдать первые/последние элементы списка
firstN _ 0=[]
firstN [] n=error ("error")
firstN (x:xs) n =x:firstN xs (n-1)

lastN x n=firstN ( rev' x) n 
-- г) a -> Int -> [a]   создать список из N эелементов типа a

-- д) [a] -> [a] -> [a]  Сформировать список, каждый элемент которого равен сумме соответствующих   элементов исходных списков.  Задание 2.5

-- е) Eq a => [a] -> [[a]] создать списки по условию,  задание 2.9 подходит

-- ж) [a] -> [(Int, a)]   дан список создать список пар с характеристикой елемента исходного списка(длина слова)

-- з) Eq a => [a] -> [a] обработать все элементы сравнивая их 
--если два ближних равны то ничего не делать,если нет то удалить второй	
delEq [x]=[x]
delEq (x:xx:xs)
	|x ==xx  = x:(delEq $ xx:xs)
	|otherwise = x : delEq xs
