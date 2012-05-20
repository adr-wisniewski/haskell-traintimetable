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
data Action = Action (IO DB) | SimpleAction (IO [Char]) | ExitAction 
data Choice = Choice Char [Char] Action | InvalidChoice
	
menu :: [Choice] -> IO String
menu choices = do
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
			menu choices
			
		processChoice (Choice _ _ (Action action)) = do
			action
			return ""
			--menu choices
		processChoice (Choice _ _ ExitAction) = do
			return ""
		
dummyAction text = do
	putStrLn text
	return ""
	
-- Prosta funkcja zwracajaca wartosc pobrana w menu
getChoiceValue sid = do
	return sid

-- Funkcja zamieniajaca liste stacji na menu
stacjeMenu :: DB -> [Stop] -> [Choice]
stacjeMenu _ [] = []
stacjeMenu context ((Stop sid name):xs)  = (Choice (chr(sid + 96)) name (SimpleAction (getChoiceValue [(chr(sid))]))) : (stacjeMenu context xs)



-- Menu glowne - dla kazdego uzytkownika
mainMenu context = do
	menu [
		(Choice '1' "Znajdz polaczenie" (Action (znajdzPolaczenie context))),
		(Choice '2' "Administracja" (Action (administracja context))),
		(Choice 'q' "Koniec" ExitAction)
		]
	return context

adminMenu context = do
	putStrLn "Logowanie pomyslne"
	menu [
		(Choice '1' "Znajdz polaczenie" (Action (znajdzPolaczenie context))),
		(Choice '2' "Dodaj stacje" (Action (uiDodajStacje context))),
		(Choice '3' "Usun stacje" (Action (uiUsunStacje context))),
		(Choice '4' "Dodaj polaczenie" (Action (uiDodajPolaczenie context))),
		(Choice '5' "Usun polaczenie" (Action (uiUsunPolaczenie context))),
		(Choice 'q' "Koniec" ExitAction)	
		]
	return ()
		
-- Lista dni tygodnia do menu. Niestety, nie udalo mi sie tego zrobic przez wyciagniecie z Enum
dniTygodnia  = [(Choice '1' "Poniedzialek" (SimpleAction (getChoiceValue  "1" ))),
			   (Choice '2' "Wtorek" (SimpleAction  (getChoiceValue  "2"))),
			   (Choice '3' "Sroda" (SimpleAction  (getChoiceValue  "3"))),
			   (Choice '4' "Czwartek" (SimpleAction  (getChoiceValue  "4"))),
			   (Choice '5' "Piatek" (SimpleAction  (getChoiceValue  "5"))),
			   (Choice '6' "Sobota" (SimpleAction  (getChoiceValue  "6"))),
			   (Choice '7' "Niedziela" (SimpleAction  (getChoiceValue  "7")))]


-- Funkcja wyswietlajaca wynik dzialania findQuickestRoute
wyswietlTrase :: [TravelRoute] -> IO ()
wyswietlTrase _ = do
			putStrLn "Brak trasy"
--wyswietlTrase TooFewStops = do 
--		putStrLn "Zbyt malo przystankow"
--wyswietlTrase DestinationUnreachable = do
--		putStrLn "Brak polaczen"
--wyswietlTrase [(TravelRoute (TravelLeg stopId _ _ _ _ _))] = do
--		putStrLn "Znaleziono trase"
		
		
		
znajdzPolaczenie context = do
		putStrLn "Wybierz stacje poczatkowa:"
		stop1 <- menu (stacjeMenu context stacje)	
		let stop1n = read stop1::Int		
		putStrLn "Wybierz stacje koncowa:"
		stop2 <- menu (stacjeMenu context stacje)
		let stop2n = read stop2::Int
		putStrLn "Podaj dzien wyjazdu:"
		rDate <- menu(dniTygodnia)
		putStrLn "Podaj godzine wyjazdu:"
		rTime <- getStyledNumber choiceStyle		
		let godzina = fromHourMinute rTime 0
		putStrLn "Podaj maksymalna ilosc przystankow:"
		maxStops <- getStyledNumber choiceStyle
		--wyswietlTrase(findQuickestRoute rozklad stop1n stop2n (Datetime (toEnum(ord(head(rDate))- 49)) godzina) maxStops)
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
		
		
uiDodajStacje context = do
	putStrLn "Podaj nazwe stacji: "
	nazwa <- getStyledLine choiceStyle
	let numer = sekwencja
	let newStop = Stop sekwencja nazwa
	
	let tt = getTimetable context
	let stops = getTimetableStops tt
	let newStops = stops ++ [newStop]
	let newContext = setTimetable context (Timetable (getTimetableCourses(tt)) (getTimetableRoutes(tt)) newStops)
	return newContext

	
uiUsunStacje context = do
	putStrLn "Wybierz stacje do usuniecia:"
	let tt = getTimetable context
	let stops = getTimetableStops tt
	stop <- menu (stacjeMenu context stops)	
	let stopN = read stop::Int
	let newStops = usunStacje stops stopN	
	let newContext = setTimetable context (Timetable (getTimetableCourses(tt)) (getTimetableRoutes(tt)) newStops)
	return newContext

	
uiDodajPolaczenie context = do
	--polaczenia <- uiDodajPolaczenieLoop []
	let tt = getTimetable context
	
	return context
	
	
uiDodajPolaczenieLoop wybraneStacje context = do
	putStrLn "Podaj stacje wchodzaca w sklad kursu, lub q, jesli koniec:"
	stacja <- menu ((stacjeMenu context stacje) ++ [(Choice 'q' "Koniec" (SimpleAction (getChoiceValue  "q")))] )
	let stacjaN = read stacja::Int	
	if(stacja == "q") then
		return wybraneStacje
	else do
		--temp <- (uiDodajPolaczenieLoop [(Stop stacjaN)] ) ++ IO wybraneStacje
		return [] --temp
	return []
	
	
uiUsunPolaczenie context = do
	return context

	
	
usunStacje [] _ = []
usunStacje ((Stop id name):xs) sid = if(sid == id) then  ((usunStacje xs sid))
												   else  (Stop id name) : (usunStacje xs sid)
	
	
