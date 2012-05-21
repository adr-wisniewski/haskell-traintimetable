import System.Console.ANSI
import DummyData
import Domain
import Data.Char
import Users
import Print
import Styles
import UI

main = do	

	initUI
	putStrLn "Train timetable v1.00" 
	--rozklad <- loadContext "timetable.dat"
	mainMenu rozklad
	writeContext rozklad "timetable.dat"
	releaseUI
		

writeContext :: Timetable -> String -> IO ()
writeContext context fname = do writeFile fname (show context)

loadContext fname = do 
                     line <- readFile fname
                     let context = read line :: Timetable
                     return context
					 

					 
					 
					 

-- Menu glowne - dla kazdego uzytkownika
mainMenu context = do
	putStrLn ""
	putStrLn "Menu:"
	menu [
		(Choice '1' "Znajdz polaczenie" (Action znajdzPolaczenie context)),
		(Choice '2' "Pokaz stacje" (Action uiPokazStacje context)),
		(Choice '3' "Pokaz trasy" (Action uiPokazTrasy context)),
		(Choice '4' "Pokaz kursy" (Action uiPokazKursy context)),
		(Choice '5' "Administracja" (Action administracja context)),
		(Choice 'q' "Koniec" ExitAction)
		] context
	return context

adminMenu context = do
	putStrLn ""
	putStrLn "Logowanie pomyslne"
	menu [
		(Choice '1' "Znajdz polaczenie" (Action znajdzPolaczenie context)),
		(Choice '2' "Pokaz stacje" (Action uiPokazStacje context)),
		(Choice '3' "Pokaz trasy" (Action uiPokazTrasy context)),
		(Choice '4' "Pokaz kursy" (Action uiPokazKursy context)),
		(Choice '5' "Dodaj stacje" (Action uiDodajStacje context)),
		(Choice '6' "Usun stacje" (Action uiUsunStacje context)),
		(Choice '7' "Dodaj trase" (Action uiDodajTrase context)),
		(Choice '8' "Usun trase" (Action uiUsunPolaczenie context)),
		(Choice '9' "Dodaj kurs" (Action uiDodajKurs context)),
		(Choice 'q' "Koniec" ExitAction)	
		] context
	return context
							 


-- Funkcja wyswietlajaca wynik dzialania findQuickestRoute
wyswietlTrase :: [TravelRoute] -> IO ()
wyswietlTrase ((TravelRoute (TravelLeg stopId _ _ _ _ _)):xs) = do
	putStrLn "Trasa1"
	wyswietlTrase xs
	
wyswietlTrase ((TooFewStops):xs) = do
	putStrLn "Zbyt malo przystankow"
	wyswietlTrase xs
	
wyswietlTrase ((DestinationUnreachable):xs) = do
	putStrLn "Brak polaczen"
	wyswietlTrase xs

isNum str = case reads str :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False
 

 
	
sprawdzDzien [] _ = False
sprawdzDzien ((Dzien sid nazwa):xs) nr = do
										if((isNum(nr) == True)) then do
											let nrN = read nr::Int
											if(nrN == sid) then True
												else sprawdzDzien xs nr												
										else False
pobierzDzien dni = do		
		nr <- getStyledLine choiceStyle				
		if((sprawdzDzien dni nr) == False ) then do
			putStrLn "Podaj dzien wyjazdu:"
			pobierzDzien dni
		else do
			let nrN = read nr::Int
			return (nrN)
	
sprawdzNumerStacji [] _ = False
sprawdzNumerStacji ((Stop sid nazwa):xs) nr = do
										if((isNum(nr) == True)) then do
											let nrN = read nr::Int
											if(sid == nrN || nrN == -1) then True
											else sprawdzNumerStacji xs nr
										else False
										
sprawdzNumer :: [Char] -> Int -> Int -> Bool
sprawdzNumer nr min max = do
							if((isNum(nr) == True)) then do
											let nrN = read nr::Int
											if(nrN > min && nrN < max) then True
											else False
							else False
				
pobierzGodzine = do
	nr <- getStyledLine choiceStyle
	if ((sprawdzNumer nr (-1) 24) == True) then do
			let nrN = read nr::Int
			return nrN
	else do
		putStrLn "Podaj prawidlowa godzine z przedzialu 0 - 23"
		pobierzGodzine
		
pobierzMinute = do
	nr <- getStyledLine choiceStyle
	if ((sprawdzNumer nr (-1) 60) == True) then do
			let nrN = read nr::Int
			return nrN
	else do
		putStrLn "Podaj prawidlowa minute z przedzialu 0 - 59"
		pobierzMinute
		
pobierzNumer text= do
	nr <- getStyledLine choiceStyle
	if ((sprawdzNumer nr (-1) 100000) == True) then do
			let nrN = read nr::Int
			return nrN
	else do
		putStrLn text
		pobierzNumer text
		
pobierzNumerStacji stacje = do
		nr <- getStyledLine choiceStyle
		
		if((sprawdzNumerStacji stacje nr) == False) then do
			putStrLn "Podaj prawidlowy numer stacji:"
			pobierzNumerStacji stacje
		else do
			let nrN = read nr::Int
			return (nrN)
			

pobierzNazwe text = do
			putStrLn text
			nazwa <- getStyledLine choiceStyle
			if(nazwa == "") then 
				pobierzNazwe text
			else
				return nazwa

		
znajdzPolaczenie context = do
		putStrLn "Wybierz stacje poczatkowa:"
		let stacje = getTimetableStops context --getTimetableStops (getTimetable context)
		printStops stacje
		stop1n <- pobierzNumerStacji stacje
		putStrLn "Wybierz stacje koncowa:"
		printStops (usunStacje stacje stop1n)
		stop2n <- pobierzNumerStacji (usunStacje stacje stop1n)
		putStrLn "Podaj dzien wyjazdu:"
		printDays dniTygodnia				
		day <- pobierzDzien dniTygodnia
		putStrLn "Podaj godzine wyjazdu z przedzialu 0 - 23:"
		hour <- pobierzGodzine	
		let godzina = fromHourMinute hour 0
		putStrLn "Podaj maksymalna ilosc przystankow:"
		maxStops <- pobierzNumer "Podaj maksymalna ilosc przystankow:"
		wyswietlTrase(findQuickestRoute context stop1n stop2n (Datetime (toEnum(day)) godzina) maxStops)
		return context
		mainMenu context
		
uiPokazStacje context = do
	let stacje = getTimetableStops context
	printStops stacje
	mainMenu context
				
uiPokazTrasy context = do
	let trasy = getTimetableRoutes context
	printRoutes trasy context
	mainMenu context

uiPokazKursy context = do
	let courses = getTimetableCourses context 
	printCourses courses context
	mainMenu context	
	
administracja context = do
		putStrLn "Podaj nazwe uzytkownika:"
		login <- getStyledLine choiceStyle
		putStrLn "Podaj haslo"
	 	pass <- getStyledLine choiceStyle
		ok <- Users.login (Users.User login pass)
		if (ok == True) then 						
			adminMenu context					
		else do
			putStrLn "Bledny login lub haslo"
			mainMenu context
		mainMenu context
		

-- Pobiera sekwencje, zeby automatycznie nadac numer nowej stacji
getStopSequence [] max = (max + 1)
getStopSequence ((Stop sid nazwa):xs) max = do
	if(sid > max) then 
		getStopSequence xs sid
	else
		getStopSequence xs max 

getRouteSequence [] max = (max + 1)
getRouteSequence ((Route rid _ _):xs) max = do
	if(rid > max) then 
		getRouteSequence xs rid
	else
		getRouteSequence xs max 
	
getCourseSequence [] max = (max + 1)
getCourseSequence ((Course cid _ _ _ _):xs) max = do
	if(cid > max) then 
		getCourseSequence xs cid
	else
		getCourseSequence xs max 	
		
		
uiDodajStacje context = do

	nazwa <- pobierzNazwe "Podaj nazwe stacji"
	let tt = rozklad --getTimetable context
	let stops = getTimetableStops tt
	let numer = getStopSequence stops 0
	--let stopid = StopId numer
	let newStop = Stop numer nazwa		
	let newStops = stops ++ [newStop]
	let newContext = Timetable (getTimetableCourses(tt)) (getTimetableRoutes(tt)) newStops
	printStyledStr defaultStyle "Dodano stacje "
	printStyledStr defaultStyle  nazwa
	printStyledStr defaultStyle  " id: " 
	let numerStr = show numer
	printStyledStr defaultStyle numerStr
	putStrLn ""
	adminMenu newContext
	
	
	

usunStacje [] _ = []
usunStacje ((Stop id name):xs) sid = do 
				if(sid == id) then  ((usunStacje xs sid))
				else  (Stop id name) : (usunStacje xs sid)

uiUsunStacje context = do
	putStrLn "Wybierz stacje do usuniecia:"
	let rozklad = context
	let stacje = getTimetableStops rozklad --getTimetableStops (getTimetable context)
	printStops stacje
	stop <- pobierzNumerStacji stacje
	let newStops = usunStacje stacje stop
	let newContext = Timetable (getTimetableCourses(rozklad)) (getTimetableRoutes(rozklad)) newStops
	putStrLn "Usunieto stacje"
	adminMenu newContext
	
	

uiDodajTrase context = do
	let stacje = getTimetableStops context
	let routes = getTimetableRoutes context
	nazwa <- pobierzNazwe "Podaj nazwe trasy:"
	let numer = getRouteSequence routes 0
	polaczenia <- uiDodajTraseLoop stacje [] context 0	
	let rt = Route numer nazwa polaczenia
	putStrLn "Dodano nowa trase "
	printStyledStr defaultStyle nazwa
	printStyledStr defaultStyle " id: "
	let numerStr = show numer
	printStyledStr defaultStyle numerStr
	putStrLn ""
	let newContext = Timetable (getTimetableCourses context) (routes ++ [rt]) (getTimetableStops context)
	adminMenu newContext
	

	
uiDodajTraseLoop stacje wybraneStacje context st = do
	
	if(st /= -1) then do
		putStrLn "Podaj stacje wchodzaca w sklad kursu, lub -1, jesli koniec:"
		printStops (usunStacje stacje st)
		st <- pobierzNumerStacji stacje		
		uiDodajTraseLoop (usunStacje stacje st) ([st] ++ wybraneStacje) context st
	else
		return (wybraneStacje)

		
pobierzCzasyOdjazdow [] wybrane = do
		return wybrane 
		
pobierzCzasyOdjazdow ((Stop sid nazwa):xs) wybrane = do
		printStyledStr defaultStyle "Podaj czas odjazdu ze stacji: "
		printStyledStr defaultStyle nazwa
		putStrLn ""
		m <- getStyledLine choiceStyle
		let mN = read m::Int
		pobierzCzasyOdjazdow xs ([(CourseStop sid mN)] ++ wybrane)
		

sprawdzNumerTrasy [] _ = False
sprawdzNumerTrasy ((Route rid _ _ ):xs) nr = if(rid == nr || nr == -1) then True
										else sprawdzNumerTrasy xs nr
		
pobierzNumerTrasy trasy context = do
		printRoutes trasy context
		nr <- getStyledLine choiceStyle
		let nrN = read nr::Int
		if((sprawdzNumerTrasy trasy nrN) == False) then do
			putStrLn "Podaj prawidlowy numer trasy:"
			pobierzNumerTrasy trasy context
		else
			return (nrN)



			 --Course CourseId RouteId Time [Day] [CourseStop]  deriving (Show, Read)
	
uiDodajKurs context = do
	let trasy = getTimetableRoutes rozklad
	let courses = getTimetableCourses context
	putStrLn "Podaj trase:"
	rId <- pobierzNumerTrasy trasy context
	putStrLn "Podaj bazowa godzine odjazdu:"
	h <- pobierzGodzine
	putStrLn "Podaj bazowa minute odjazdu:"	
	m <- pobierzMinute
	let cid = getCourseSequence (getTimetableCourses context) 0
	let czasy = pobierzCzasyOdjazdow stacje []
	
	printStyledStr defaultStyle "Dodano nowy kurs"
	printStyledStr defaultStyle " id: "
	let numerStr = show cid
	printStyledStr defaultStyle numerStr
	adminMenu context
	

	--let newContext = Timetable (courses ++ [course]) (getTimetableRoutes context) (getTimetableStops context)
	adminMenu context
	
uiUsunPolaczenie context = do
	return context

	
	

	
	
