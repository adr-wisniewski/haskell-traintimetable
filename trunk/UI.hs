module UI where
import System.Console.ANSI
import DummyData
import Domain
import Data.Char
import Users
import Database


		
		
-------------------------------------------------------------------------------
-- INIT & RELEASE
-------------------------------------------------------------------------------
initUI = do
	setSGR defaultStyle
		
releaseUI = do
	setSGR [Reset]

-------------------------------------------------------------------------------
-- STYLES
-------------------------------------------------------------------------------
defaultStyle = [color]
	where
		color = SetColor Foreground Vivid White

choiceStyle = [color]
	where
		color = SetColor Foreground Vivid Green

specialStyle = [color]
	where
		color = SetColor Foreground Vivid Yellow
		
errorStyle = [color]
	where
		color = SetColor Foreground Vivid Red
			
printStyledStr style text = do
	setSGR style
	putStr text
	setSGR defaultStyle
	
printStyledStrLn style text = do
	setSGR style
	putStrLn text
	setSGR defaultStyle
	
getStyledLine style = do
	setSGR style
	line <- getLine
	setSGR defaultStyle
	return line

	
getStyledNumber style = do
	setSGR style
	line <- getLine
	setSGR defaultStyle
	let lineN = read line::Int
	return lineN
-------------------------------------------------------------------------------
-- MENU COMPONENT
-------------------------------------------------------------------------------


data Action action = Action action (Timetable) | ExitAction 
data Choice action = Choice Char [Char] action | InvalidChoice
	
	
	
--menu :: [Choice (Action)] -> (IO DB) -> IO String

printStops [] = do return ()
printStops((Stop key name):cs) = do
	printStyledStr choiceStyle (show key)
	printStyledStr specialStyle ") "
	putStrLn name
	printStops cs
	
	
	
printDays [] = do return ()
printDays((Dzien key name):cs) = do
	printStyledStr choiceStyle (show key)
	printStyledStr specialStyle ") "
	putStrLn name
	printDays cs
			
menu choices context = do
	printChoices choices
	putStr "Enter choice: "
	input <- getStyledLine choiceStyle
	clearScreen
	setCursorPosition 0 0
	processChoice (findChoice input)
	where
		printChoices [] = do return ()
		printChoices ((Choice key desc _):cs) = do
			printStyledStr choiceStyle [key]
			printStyledStr specialStyle ") "
			putStrLn desc
			printChoices cs

		findChoice input
			| null input || length input > 1 || null candidates = InvalidChoice
			| otherwise = head candidates
			where
				candidates = filter (keyCompare $ head input) choices
				keyCompare key (Choice k _ _) = key == k
				
		processChoice InvalidChoice = do
			printStyledStrLn errorStyle "Invalid choice. Try again!"
			menu choices context
			
		processChoice (Choice _ _ (Action action context)) = do
			action context
			return ""
		processChoice (Choice _ _ ExitAction) = do
			return ""
		
dummyAction text = do
	putStrLn text
	return ""
	
-- Prosta funkcja zwracajaca wartosc pobrana w menu
getChoiceValue sid = do
	return sid

-- Funkcja zamieniajaca liste stacji na menu
--stacjeMenu _ [] = []
--stacjeMenu context ((Stop sid name):xs)  = (Choice (chr(sid + 96)) name (Action (getChoiceValue chr(sid)))) : (stacjeMenu context xs)



		

-- Menu glowne - dla kazdego uzytkownika
mainMenu context = do
	menu [
		(Choice '1' "Znajdz polaczenie" (Action znajdzPolaczenie context)),
		(Choice '2' "Administracja" (Action administracja context)),
		(Choice 'q' "Koniec" ExitAction)
		] context
	return context

adminMenu context = do
	putStrLn "Logowanie pomyslne"
	menu [
		(Choice '1' "Znajdz polaczenie" (Action znajdzPolaczenie context)),
		(Choice '2' "Dodaj stacje" (Action uiDodajStacje context)),
		(Choice '3' "Usun stacje" (Action uiUsunStacje context)),
		(Choice '4' "Dodaj trase" (Action uiDodajTrase context)),
		(Choice '5' "Usun trase" (Action uiUsunPolaczenie context)),
		(Choice '6' "Dodaj kurs" (Action uiDodajKurs context)),
		(Choice 'q' "Koniec" ExitAction)	
		] context
	return context
		
-- Lista dni tygodnia do menu. Niestety, nie udalo mi sie tego zrobic przez wyciagniecie z Enum

data Dzien = Dzien Int String
dniTygodnia  = [(Dzien 1 "Poniedzialek"),
			   (Dzien 2 "Wtorek" ),
			   (Dzien 3 "Sroda" ),
			   (Dzien 4 "Czwartek" ),
			   (Dzien 5 "Piatek" ),
			   (Dzien 6 "Sobota" ),
			   (Dzien 7 "Niedziela")]


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
											if(nrN > min || nrN < max) then True
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
		putStrLn "Podaj prawidlowa godzine z przedzialu 0 - 23"
		pobierzGodzine
		
pobierzNumerStacji stacje = do
		nr <- getStyledLine choiceStyle
		
		if((sprawdzNumerStacji stacje nr) == False) then do
			putStrLn "Podaj prawidlowy numer stacji:"
			pobierzNumerStacji stacje
		else do
			let nrN = read nr::Int
			return (nrN)
			

		
		
znajdzPolaczenie context = do
		putStrLn "Wybierz stacje poczatkowa:"
		let stacje = getTimetableStops context --getTimetableStops (getTimetable context)
		printStops stacje
		stop1n <- pobierzNumerStacji stacje
		putStrLn "Wybierz stacje koncowa:"
		printStops stacje
		stop2n <- pobierzNumerStacji stacje
		putStrLn "Podaj dzien wyjazdu:"
		printDays dniTygodnia				
		day <- pobierzDzien dniTygodnia
		putStrLn "Podaj godzine wyjazdu z przedzialu 0 - 23:"
		hour <- pobierzGodzine	
		let godzina = fromHourMinute hour 0
		putStrLn "Podaj maksymalna ilosc przystankow:"
		maxStops <- getStyledNumber choiceStyle
		wyswietlTrase(findQuickestRoute context stop1n stop2n (Datetime (toEnum(day)) godzina) maxStops)
		return context
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
			adminMenu context
		adminMenu context
		

-- Pobiera sekwencje, zeby automatycznie nadac numer nowej stacji
getSequence [] max = (max + 1)
getSequence ((Stop sid nazwa):xs) max = do
	if(sid > max) then 
		getSequence xs sid
	else
		getSequence xs max 
		
uiDodajStacje context = do
	putStrLn "Podaj nazwe stacji: "
	nazwa <- getStyledLine choiceStyle
	let tt = rozklad --getTimetable context
	let stops = getTimetableStops tt
	let numer = getSequence stops 0
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
	--let tt = getTimetable context
	--let stops = getTimetableStops tt
	--stop <- getStyledLine choiceStyle --menu (stacjeMenu context stops)	
	--let stopN = read stop::Int
	--let newStops = usunStacje stops stopN	
	--let newContext = setTimetable context (Timetable (getTimetableCourses(tt)) (getTimetableRoutes(tt)) newStops)
	--return context
	adminMenu newContext
	
uiDodajTrase context = do
	let stacje = getTimetableStops rozklad
	putStrLn "Podaj nazwe trasy:"
	nazwa <- getStyledLine choiceStyle
	putStrLn "Podaj numer trasy:"
	numer <- getStyledLine choiceStyle
	let numerN = read numer::Int
	polaczenia <- uiDodajTraseLoop stacje [] context 0	
	let rt = Route numerN nazwa polaczenia
	putStrLn "Dodano nowa trase"
	adminMenu context
	

	
uiDodajTraseLoop stacje wybraneStacje context st = do
	
	if(st /= -1) then do
		putStrLn "Podaj stacje wchodzaca w sklad kursu, lub -1, jesli koniec:"
		printStops stacje
		st <- pobierzNumerStacji stacje		
		uiDodajTraseLoop stacje ([st] ++ wybraneStacje) context st
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
		
pobierzNumerTrasy trasy = do
		printRoutes trasy
		nr <- getStyledLine choiceStyle
		let nrN = read nr::Int
		if((sprawdzNumerTrasy trasy nrN) == False) then do
			putStrLn "Podaj prawidlowy numer trasy:"
			pobierzNumerTrasy trasy
		else
			return (nrN)


printRoutes [] = do return ()
printRoutes((Route id name _):cs) = do
	printStyledStr choiceStyle (show id)
	printStyledStr specialStyle ") "
	putStrLn name
	printRoutes cs
	
uiDodajKurs context = do
	let trasy = getTimetableRoutes rozklad
	putStrLn "Podaj trase:"
	rId <- pobierzNumerTrasy trasy
	putStrLn "Podaj bazowa godzine odjazdu:"
	h <- getStyledLine choiceStyle
	putStrLn "Podaj bazowa minute odjazdu:"
	let hN = read h::Int
	m <- getStyledLine choiceStyle
	let mN = read m::Int
	pobierzCzasyOdjazdow stacje []
	putStrLn "Dodano nowy kurs"
	adminMenu context
	
	
uiUsunPolaczenie context = do
	return context

	
	

	
	
