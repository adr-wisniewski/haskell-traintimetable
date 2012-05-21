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


data Action action = Action action (IO DB) | ExitAction 
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
		(Choice '4' "Dodaj polaczenie" (Action uiDodajPolaczenie context)),
		(Choice '5' "Usun polaczenie" (Action uiUsunPolaczenie context)),
		(Choice 'q' "Koniec" ExitAction)	
		] context
	return ()
		
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

	
sprawdzDzien [] _ = False
sprawdzDzien ((Dzien sid nazwa):xs) nr = if(sid == nr) then True
										else sprawdzDzien xs nr		
	
pobierzDzien dni = do
		
		nr <- getStyledLine choiceStyle
		let nrN = read nr::Int
		if((sprawdzDzien dni nrN) == False) then do
			putStrLn "Podaj dzien wyjazdu:"
			pobierzDzien dni
		else
			return (nrN)
	
sprawdzNumer [] _ = False
sprawdzNumer ((Stop sid nazwa):xs) nr = if(sid == nr || sid == -1) then True
										else sprawdzNumer xs nr
										
									
pobierzGodzine = do
	nr <- getStyledLine choiceStyle
	let nrN = read nr::Int
	if(nrN > 23 || nrN < 0) then do
		putStrLn "Podaj prawidlowa godzine z przedzialu 0 - 23"
		pobierzGodzine
	else
		return (nrN)
		
pobierzNumerStacji stacje = do
		
		nr <- getStyledLine choiceStyle
		let nrN = read nr::Int
		if((sprawdzNumer stacje nrN) == False) then do
			putStrLn "Podaj prawidlowy numer stacji:"
			pobierzNumerStacji stacje
		else
			return (nrN)
		
znajdzPolaczenie context = do
		putStrLn "Wybierz stacje poczatkowa:"
		let stacje = getTimetableStops rozklad --getTimetableStops (getTimetable context)
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
		wyswietlTrase(findQuickestRoute rozklad stop1n stop2n (Datetime (toEnum(day)) godzina) maxStops)
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
		else
			putStrLn "Bledny login lub haslo"
		return context
		--mainMenu context
		
		
getSequence [] max = max

getSequence ((Stop sid nazwa):xs) max = do
	if(sid > max) then 
		getSequence xs sid
	else
		getSequence xs max
		
uiDodajStacje context = do
	putStrLn "Podaj nazwe stacji: "
	nazwa <- getStyledLine choiceStyle
	let tt = getTimetable context
	let stops = getTimetableStops tt
	let numer = getSequence stops 0
	--let stopid = StopId numer
	let newStop = Stop numer nazwa		
	let newStops = stops ++ [newStop]
	let newContext = setTimetable context (Timetable (getTimetableCourses(tt)) (getTimetableRoutes(tt)) newStops)
	putStrLn "Dodano stacje"
	putStrLn nazwa
	let numerStr = show numer
	putStrLn numerStr
	mainMenu newContext
	
	
	

usunStacje [] _ = []
usunStacje ((Stop id name):xs) sid = do 
				if(sid == id) then  ((usunStacje xs sid))
				else  (Stop id name) : (usunStacje xs sid)

uiUsunStacje context = do
	putStrLn "Wybierz stacje do usuniecia:"
	let rozklad = getTimetable context
	let stacje = getTimetableStops rozklad --getTimetableStops (getTimetable context)
	printStops stacje
	stop <- pobierzNumerStacji stacje
	let newStops = usunStacje stacje stop
	let newContext = setTimetable context (Timetable (getTimetableCourses(rozklad)) (getTimetableRoutes(rozklad)) newStops)
	putStrLn "Usunieto stacje"
	--let tt = getTimetable context
	--let stops = getTimetableStops tt
	--stop <- getStyledLine choiceStyle --menu (stacjeMenu context stops)	
	--let stopN = read stop::Int
	--let newStops = usunStacje stops stopN	
	--let newContext = setTimetable context (Timetable (getTimetableCourses(tt)) (getTimetableRoutes(tt)) newStops)
	--return context
	mainMenu newContext
	
uiDodajPolaczenie context = do
	stacja <- pobierzNumerStacji stacje
	polaczenia <- uiDodajPolaczenieLoop stacje [] context
	
	
	--let tt = getTimetable context
	
	return context
	
	
uiDodajPolaczenieLoop stacje wybraneStacje context = do
	putStrLn "Podaj stacje wchodzaca w sklad kursu, lub -1, jesli koniec:"
	let stacje = getTimetableStops rozklad
	let st = 0
	if(st /= -1) then do
		let st = pobierzNumerStacji stacje
		return uiDodajPolaczenieLoop stacje [st] ++ wybraneStacje context
	else
		wybraneStacje
	
	
uiUsunPolaczenie context = do
	return context

	
	

	
	
