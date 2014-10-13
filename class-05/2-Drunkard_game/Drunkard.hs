{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = Spades | Clubs | Diamonds | Hearts
	deriving (Show, Eq, Ord)


data Value = Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
	deriving (Show, Eq, Ord)

data Card = Card {value::Value, suit::Suit}
	deriving (Show, Eq, Ord)

-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit c1 c2 = suit c1==suit c2 

{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
c1 `beats` c2 = compare (value c1) (value c2)

{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры): 
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

--game_round :: ([Card], [Card]) -> ([Card], [Card])	
game_round(p,p2)=do 
				fstscn $ game_round1(p,p2,[])
				where fstscn (a,b,c) = (a,b)

game_round1((c1:xs),(c2:xxs),acc)  
	| c1 `beats` c2 == GT = (xs++acc++[c1,c2],xxs,acc++[c1,c2])
	| c1 `beats` c2 == LT = (xs,xxs++acc++[c1,c2],acc++[c1,c2])
	| otherwise =game_round1(xs,xxs,acc++[c1,c2])

game_round1([],[],acc)=(acc,[],acc)
game_round1(x,[],acc)=(x++acc,[],acc)
game_round1([],x,acc)=([],x++acc,acc)

{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second
	deriving (Show)
	
game :: ([Card], [Card]) -> IO()
game (x,xx)=game1((x,xx,[]),100000) -- 100 это кол-во повторений считающееся циклом

game1((x,[],_),_)= print(First,1)
game1(([],x,_),_)= print(Second,2)
game1((x,xx,acc),counter) = do
		let f= game_round1(x,xx,[])
		print (acc)
		if counter >0 then game1(f,(counter-1)) else print("Maybe cycle")
		where thr (a,b,c)=c
{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}
doit = do
			let cT = Card {suit=Spades, value=Ten}
			let cA = Card {suit=Spades, value=Ace}
			let cK = Card {suit=Spades, value=King}
			let cQ = Card {suit=Spades, value=Queen}
			let cJ = Card {suit=Spades, value=Jack}
			let cN = Card {suit=Spades, value=Nine}
			let cE = Card {suit=Spades, value=Eight}
			let cS = Card {suit=Spades, value=Seven}
			--game([cT,cA,cK,cS,cE,cN,cJ,cQ,cA,cK],[cK,cA,cQ,cN,cK,cS,cJ,cJ,cA,cE])
			print()
			print()
			--game([cK,cJ,cA,cT,cS,cE,cN,cQ,cJ,cE],[cQ,cJ,cT,cA,cJ,cN,cS,cE,cQ,cK]) -- зацикливается
			print()
			print()
			game([cQ,cN,cE,cK,cT,cJ,cQ,cK,cT,cJ],[cA,cA,cS,cA,cA,cS,cS,cN,cE,cE])
{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
  
  Непонятно, когда game_round станет зацикливаться
-}
