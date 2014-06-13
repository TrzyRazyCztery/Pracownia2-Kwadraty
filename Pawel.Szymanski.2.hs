-- ----------------------------------------------------------------------------------------------
-- Pracownia oceniona na 17/20 czyli 85%
-- pkt obniżone za użycie if a then True else False bo to to samo co a
-- ---------------------------------------------------------------------------------------------
--
--                         Metody Programowania 2014 - Pracownia
--						   Pawel Szymanski, nr. indeksu: 248082
--						   Pracownia nr.2, wersja 2: "Kwadraty"
--
-- ---------------------------------------------------------------------------------------------
-- Rozwiazanie:
-- 		Rozwiazanie polega na generowaniu dla kazdej liczby z kolei kwadratu w kazdym z 
-- 		czterech kierunkow (sprawdzajac tez czy sa poprawne) i dodawaniu do daniu pierwszego
--		z nich do listy a nastepnie geneerowaniu kwadratow dla nastepnej cyrfy na podstawie
--      wczesniej wygenerowanych kwadratów.
--      w przypadku gdy wygenerujemy juz kwadraty dla np 4 cyfr a dla 5 cyfry jest to nie mozliwe
--		z powodu jakiś kolizji (z istniejacymi kwadratami czy cyframi)
--      program wraca spowrotem do 4-tej cyfry i proboje wygenerowac dla niej inny kwadrat
--
-- ---------------------------------------------------------------------------------------------
--
-- Nazewnictwo:
--		Staralem sie trzymac CamelCase i ponazywac wszystko tak by bylo zrozumiale, 
--      jednak chcialbym dodac ze jesli w predykacie pojawia sie
-- 		I lub J sa to zawsze zmienne zawierajace wspolrzedne
--		odpowiednio: I - Wspolrzedna Pionowa, J- wspolrzedna Pozioma 
--      Plansza( m x n):
--      	m  - iRozmiarTablicy
--			n  - jRozmiarTablicy
--		kwadrat (i,j,d) lub cyfra (i,j,l):
--			i  - odpakujPierwszy kwadrat/cyfra 
--			j  - odpakujDrugi kwadrat/cyfra
--			d/l- odpakujTrzeci kwadrat/cyfra
--		listaCyfr - wejsciowa lista z cyframi podana w solve
--		listaKwadratow - lista kwadratow ktore sa juz obecne na planszy 
--
-- ---------------------------------------------------------------------------------------------
--
-- Czas:
-- 		Pracownia dla wszystkich testow z KNO ( latwe testy + trudny test) 
--      I moich 6-sciu testow ( jednego latwego, jednego na poziomie trudnego z KNO
--      oraz trudniejszego  20x20 z 30 cyframi) konczy dzialanie pozytywnie w 
--      mniej niż sekundę (wyniki na końcu pliku)
--
-- ---------------------------------------------------------------------------------------------


import Puzzle
import Checker


main :: IO ()
main = checkerMain solve tests

solve :: Int -> Int -> [(Int, Int, Int)] -> [[(Int, Int, Int)]]
solve iRozmiarTablicy jRozmiarTablicy listaCyfr = 
	rozwiazanie' iRozmiarTablicy jRozmiarTablicy listaCyfr listaCyfr []

-- ------------------------------------------------------------------------------
-- rozwiazanie' jest pomocnicza funkcja ktora do solve dodaje jeszcze liste cyfr
-- po ktorej bede sie poruszał generujac kwadraty oraz akumulator
-- -------------------------------------------------------------------------------
rozwiazanie' :: Int -> Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
				 -> [(Int, Int, Int)] -> [[(Int, Int, Int)]]
rozwiazanie' _ _ _ [] xs = [reverse xs]
rozwiazanie' iRozmiarTablicy jRozmiarTablicy listaCyfr (cyfra:resztaCyfr) wczesniejszeKwadraty = do
	kwadrat <- generowanieKwadratuZeSprawdzaniemPoprawnosci cyfra iRozmiarTablicy jRozmiarTablicy wczesniejszeKwadraty listaCyfr
	rozwiazanie' iRozmiarTablicy jRozmiarTablicy listaCyfr resztaCyfr (kwadrat : wczesniejszeKwadraty) 









-- ------------------------------------- Generowanie kwadratow ------------------------------------
-- Generowanie polega na wygenerowaniu kwadratow w 4 roznych kierunkach za pomoca 4 predykatow
-- i sprawdzeniu ich za pomoca funkcji z sekcji sprawdzania kwadratow
-- ------------------------------------------------------------------------------------------------
generowanieKwadratuZeSprawdzaniemPoprawnosci :: (Int, Int, Int) -> Int -> Int -> 
												[(Int, Int, Int)] -> [(Int, Int, Int)] -> 
												[(Int, Int, Int)]
generowanieKwadratuZeSprawdzaniemPoprawnosci cyfra iRozmiarTablicy jRozmiarTablicy listaKwadratow listaCyfr = 
	[ x | x <- listaWygenerowanychKwadratow, sprawdzanieKwadratu x cyfra listaCyfr listaKwadratow ]
	where
		listaWygenerowanychKwadratow = generowanieKwadratuLewoGora cyfra iRozmiarTablicy jRozmiarTablicy ++
						 generowanieKwadratuPrawoGora cyfra iRozmiarTablicy jRozmiarTablicy ++
						 generowanieKwadratuLewoDol cyfra iRozmiarTablicy jRozmiarTablicy ++
						 generowanieKwadratuPrawoDol cyfra iRozmiarTablicy jRozmiarTablicy 

generowanieKwadratuPrawoDol :: (Int, Int, Int) -> Int -> Int -> [(Int, Int, Int)]
generowanieKwadratuPrawoDol cyfra iRozmiarTablicy jRozmiarTablicy = 
	[(odpakujPierwszy cyfra, odpakujDrugi cyfra, y) | y <- ys] 
	where
		ys = [1..if (iRozmiarTablicy - odpakujPierwszy cyfra) <= (jRozmiarTablicy - odpakujDrugi cyfra)
			then (iRozmiarTablicy - odpakujPierwszy cyfra)
			else (jRozmiarTablicy - odpakujDrugi cyfra)]


generowanieKwadratuLewoGora :: (Int, Int, Int) -> Int -> Int -> [(Int, Int, Int)]
generowanieKwadratuLewoGora cyfra iRozmiarTablicy jRozmiarTablicy =
	[(odpakujPierwszy cyfra-y, odpakujDrugi cyfra-y, y) | y <- ys]
	where
		ys = [1..if (odpakujPierwszy cyfra) <= (odpakujDrugi cyfra)
			then (odpakujPierwszy cyfra-1)
			else (odpakujDrugi cyfra-1)]


generowanieKwadratuLewoDol :: (Int, Int, Int) -> Int -> Int -> [(Int, Int, Int)]
generowanieKwadratuLewoDol cyfra iRozmiarTablicy jRozmiarTablicy = 
	[(odpakujPierwszy cyfra, odpakujDrugi cyfra - y, y) | y <- ys] 
	where
		ys = [1..if (odpakujDrugi cyfra) <= (iRozmiarTablicy - odpakujPierwszy cyfra)
			then (odpakujDrugi cyfra -1)
			else (iRozmiarTablicy - odpakujPierwszy cyfra)	]	



generowanieKwadratuPrawoGora :: (Int, Int, Int) -> Int -> Int -> [(Int, Int, Int)]
generowanieKwadratuPrawoGora cyfra iRozmiarTablicy jRozmiarTablicy = 
	[(odpakujPierwszy cyfra - y, odpakujDrugi cyfra, y) | y <- ys] 
	where
		ys = [1..if (odpakujPierwszy cyfra) <= (jRozmiarTablicy - odpakujDrugi cyfra)
			then (odpakujPierwszy cyfra - 1)
			else (jRozmiarTablicy - odpakujDrugi cyfra)	]	


-- ------------------------------------------ Sprawdzanie Kwadratow  -------------------------------------------
-- sprawdzanie polega na przejsciu 3 testow, and użyty jest po to by nie wykonywac pozostalych testow
-- (zostaly ulozone w sposob (wedlug mnie) od najlatwiejszego do najtrudniejszego (czasowo)).
-- Wiec jesli kwadrat nie przejdzie najprostszego z testow, czyli sprawdzenia czy na obwodzie istnieje
-- tylko cyfra dla ktorej jest generowany, bedie automatycznie odrzucany bez sprawdzania kolizji
-- oraz tego czy ilosc cyfr w srodku kwadratu sie zgadza.
-- -------------------------------------------------------------------------------------------------
sprawdzanieKwadratu :: (Int, Int, Int) -> (Int, Int, Int) -> 
						[(Int, Int, Int)] -> [(Int, Int, Int)] -> Bool
sprawdzanieKwadratu kwadrat cyfra listaCyfr listaKwadratow = 
	and 
	   [if listaCyfrNaObwodzie kwadrat listaCyfr == [cyfra] then True else False,
	    if length (sprawdzanieIlosciCyfrWewnatrz kwadrat listaCyfr) == (odpakujTrzeci cyfra) then True else False,
	    if sprawdzanieKolizji kwadrat listaKwadratow == [] then True else False]


-- funkcja zwraca liste punktow na ktorych sa cyfry w srodku kwadratu
sprawdzanieIlosciCyfrWewnatrz :: (Int, Int, Int) -> [(Int, Int, Int)] -> [(Int, Int)]
sprawdzanieIlosciCyfrWewnatrz kwadrat listaCyfr = [ x | x <- xs, elem x (zamienNaPolaWewnetrzne kwadrat)]
	where
		xs = [(odpakujPierwszy x ,odpakujDrugi x) | x <- listaCyfr]




-- ---------------------------------------------------------------------------
-- funkcja sprawdza czy nie zachodzi kolizja miedzy kwadratem a kwadratami 
-- juz obecnymi na planszy.
-- sprawdzanie kolizji sprawdza czy rogi kwadratow z planszy nie leza na obwodzie
-- sprawdzanego kawadratu
-- sprawdzanie kolizji odwrotnej sprawdza czy rogi kwadratu nie leza na obwodach
-- kwadratow obecnych na planszy
-- -------------------------------------------------------------------------------
sprawdzanieKolizji :: (Int, Int, Int) -> [(Int, Int, Int)] -> [(Int, Int)] 
sprawdzanieKolizji kwadrat listaKwadratow =  
	if warunekKolizji1 /= [] 
		then warunekKolizji1 
	    else warunekKolizji1 ++ sprawdzanieKolizjiOdwrotnej' kwadrat listaKwadratow
	where 
		warunekKolizji1 = sprawdzanieKolizji' kwadrat listaKwadratow
	




sprawdzanieKolizji' :: (Int, Int, Int) -> [(Int, Int, Int)] -> [(Int,Int)]
sprawdzanieKolizji' kwadrat listaKwadratow = [ x | x <- listaRogow, elem x listaPolObwodu]
	where
		listaRogow = listaRogowKwadratow listaKwadratow
		listaPolObwodu = zamienNaObwod kwadrat

sprawdzanieKolizjiOdwrotnej' :: (Int, Int, Int) -> [(Int, Int, Int)] -> [(Int, Int)]
sprawdzanieKolizjiOdwrotnej' kwadrat listaKwadratow = [ x | x <- listaRogowKwadratu, elem x listaPolObwodow ]
	where
		listaRogowKwadratu = zamienNaListeRogow kwadrat
		listaPolObwodow = listaPolObwoduKwadratow listaKwadratow







-- zwraca cyfry ktore znajduja sie na obwodzie podanego kwadratu ------------------------------------------------
listaCyfrNaObwodzie :: (Int, Int, Int) -> [(Int, Int,Int)] -> [(Int, Int, Int)]
listaCyfrNaObwodzie kwadrat listaCyfr = 
	[x | x <- listaCyfr, 
		elem (odpakujPierwszy x, odpakujDrugi x) listaPolObwodu]
	where
		listaPolObwodu = zamienNaObwod kwadrat 






-- ---------------------------------------------- Zamiany kwadratow na Listy pol -----------------------------------------



-- listy pol wszystkich kwadratow obecnych na planszy -------------------------------------------------------------------
listaPolObwoduKwadratow :: [(Int, Int, Int)] -> [(Int, Int)]
listaPolObwoduKwadratow listaKwadratow = concat [[ x | x <- zamienNaObwod xs] | xs <- listaKwadratow ]

listaRogowKwadratow :: [(Int, Int, Int)] -> [(Int, Int)]
listaRogowKwadratow listaKwadratow = concat [[ x | x <- zamienNaListeRogow xs] | xs <- listaKwadratow ]
-- -----------------------------------------------------------------------------------------------------------------------





-- -----------------------------------------------------------------------------------------------------------------------
-- ponizsze funkcje po podaniu kwadratu zwroca liste pol zajmowanych przez wnetrze tego kwadratu, jego obwod 
-- lub jego narozniki.
-- ----------------------------------------------------------------------------------------------------------------------
zamienNaListeRogow :: (Int, Int, Int) -> [(Int,Int)]
zamienNaListeRogow kwadrat = [ (odpakujPierwszy kwadrat,odpakujDrugi kwadrat),
								(odpakujPierwszy kwadrat + odpakujTrzeci kwadrat ,odpakujDrugi kwadrat),
								(odpakujPierwszy kwadrat + odpakujTrzeci kwadrat ,odpakujDrugi kwadrat + odpakujTrzeci kwadrat),
								(odpakujPierwszy kwadrat, odpakujDrugi kwadrat + odpakujTrzeci kwadrat)]

zamienNaPolaWewnetrzne :: (Int, Int, Int) -> [(Int,Int)]
zamienNaPolaWewnetrzne kwadrat = [(x,y) | x <- pomocnicza, y <- pomocnicza2]
	where 
		pomocnicza = [(odpakujPierwszy kwadrat + 1)..(odpakujPierwszy kwadrat + odpakujTrzeci kwadrat - 1)]
		pomocnicza2 = [(odpakujDrugi kwadrat + 1)..(odpakujDrugi kwadrat + odpakujTrzeci kwadrat - 1)]

zamienNaObwod :: (Int, Int, Int) -> [(Int,Int)]
zamienNaObwod kwadrat = pomocnicza2 ++ pomocnicza3 
	where
		pomocnicza = [(odpakujPierwszy kwadrat)..(odpakujPierwszy kwadrat + odpakujTrzeci kwadrat)]
		pomocnicza' = [(odpakujDrugi kwadrat)..(odpakujDrugi kwadrat + odpakujTrzeci kwadrat)]
		pomocnicza2 = [(x,y) | x <- [head pomocnicza, last pomocnicza], y <- pomocnicza']
		pomocnicza3 = [(x,y) | x <- pomocnicza, y <- [last pomocnicza', head pomocnicza']]





-- ---- funkcja do wyciagania elementu z krotki 3 elementowej) -------------------------------------------------------------
odpakujPierwszy :: (a, b, c) -> a
odpakujPierwszy (a,_,_) = a

odpakujDrugi :: (a, b, c) -> b
odpakujDrugi (_,b,_) = b

odpakujTrzeci :: (a, b, c) -> c
odpakujTrzeci (_,_,c) = c



-- ---------------------------------------------------- Moje Testy ----------------------------------------------------
tests :: [Test]
tests = [ SimpleTest
        (Puzzle 14 14
			[(1,1,6),(1,12,0),(2,2,0),(2,4,1),(2,8,0),(3,5,1),(4,2,0),(4,12,0),
			(6,7,1),(7,14,1),(9,12,0),(10,3,2),(11,9,0),(11,12,1),(12,4,0),
			(12,5,0),(12,9,0),(12,11,0)])
			[ (1,1,8),(1,12,2),(2,2,1),(2,4,3),(2,8,3),(3,5,5),(4,2,1),(4,12,2),
			(6,2,5),(7,11,3),(8,12,1),(10,3,3),(10,8,1),(11,10,2),(12,2,2),
			(12,5,2),(12,8,1),(12,11,2)]
    , SimpleTest 
        (Puzzle 10 2 
			[(1,1,0),(3,2,0),(5,2,0),(7,1,0),(9,1,0)] )
			 [(1,1,1),(3,1,1),(5,1,1),(7,1,1),(9,1,1)]
    , SimpleTest
        (Puzzle 20 20
			[(1,1,4),(1,9,1),(1,19,1),(2,2,4),(2,13,0),(2,18,0),(3,3,0),(3,6,1),(6,3,0),
			(7,11,0),(7,15,2),(8,17,0),(9,3,0),(9,17,0),(12,1,1),(12,14,2),(13,2,3),
			(13,17,0),(14,8,1),(15,3,0),(15,4,0),(15,6,1),(15,12,1),(15,17,0),(16,9,1),
			(18,11,0),(18,16,0),(19,1,0),(19,5,0),(19,18,0)])
			[(1,1,6),(1,9,5),(1,17,2),(2,2,8),(2,11,2),(2,16,2),(3,3,1),(3,6,6),
			 (5,3,1),(5,11,2),(7,15,3),(6,17,2),(8,3,1),(9,17,2),(12,1,2),(12,14,5),
			  (13,2,5),(13,17,1),(14,8,3),(15,1,2),(14,4,1),(12,6,3),(11,12,4), 
			  (15,17,1),(16,9,3),(18,11,2),(18,14,2),(19,1,1),(19,4,1),(18,17,1)]
	, SimpleTest
		(Puzzle 2 6 
			[(1,2,0),(1,3,0),(2,6,0)]) 
			[(1,1,1), (1,3,1), (1,5,1)]	  
    , CountTest 
        (Puzzle 5 5  [(1,1,1),(2,2,0)]) 6
    
    , CountTest
        (Puzzle 9 5 [(2,2,1),(3,3,1),(5,5,1),(6,4,0)]) 6
    , CountTest
        (Puzzle 3 10 [(1,1,0),(1,3,0),(2,5,0),(1,7,0),(2,8,0)]) 0
    

    ]




-- ---------------------------------------------------------
-- Moje Testy:
-- ======================
-- Test: 1/7
-- CPU time:   0.04s
-- Accept!
-- ======================
-- Test: 2/7
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 3/7
-- CPU time:   0.04s
-- Accept!
-- ======================
-- Test: 4/7
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 5/7
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 6/7
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 7/7
-- CPU time:   0.00s
-- Accept!
--
-- Accepted: 7/7
--
-- Testy z KNO :
-- ======================
-- Test: 1/8
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 2/8
-- CPU time:   0.01s
-- Accept!
-- ======================
-- Test: 3/8
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 4/8
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 5/8
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 6/8
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 7/8
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 8/8
-- CPU time:   0.04s
-- Accept!
-- Accepted: 8/8  
