import System.Console.ANSI
import DummyData
import Domain
import Data.Char
import Users
import UI
import Data.Maybe


-------------------------------------------------------------------------------
-- ENTRY POINT
-------------------------------------------------------------------------------
main = do	
	initUI
	putStrLn "Train timetable v1.00" 
	rozklad <- loadContext "timetable.dat"
	mainMenu (MainMenuContext Anonymous (emptyTimetable))
	writeContext rozklad "timetable.dat"
	releaseUI
		
-------------------------------------------------------------------------------
-- SAVING / LOADING
-------------------------------------------------------------------------------		
writeContext context fname = do 
	writeFile fname (show context)

loadContext fname = do 
	line <- readFile fname
	let context = read line :: Timetable
	return context
					 				 
-------------------------------------------------------------------------------
-- MAIN MENU
-------------------------------------------------------------------------------	
data MainMenuContext = MainMenuContext User Timetable
isAnonymous (MainMenuContext Anonymous _) = True
isAnonymous _ = False

getContextTimetable (MainMenuContext _ timetable) = timetable
setContextTimetable (MainMenuContext user _) timetable = (MainMenuContext user timetable)
	
mainMenu mainMenuContext = do
	let menu = if isAnonymous mainMenuContext 
		then unauthorizedMainMenu 
		else authorizedMainMenu

	maybeUpdatedContext <- menu mainMenuContext

	if isJust maybeUpdatedContext then
		mainMenu (fromJust maybeUpdatedContext) -- loop again
	else
		return mainMenuContext -- exit action fired
	
		
unauthorizedMainMenu mainMenuContext = do 
	menu [
		(Choice '1' "Znajdz polaczenie" (Action (znajdzPolaczenie mainMenuContext))),
		(Spacer),
		(Choice 'l' "Zaloguj" (Action (login mainMenuContext))),
		(Choice 'q' "Koniec" ExitAction)
		]
	
authorizedMainMenu mainMenuContext = do
	menu [
		(Choice '1' "Znajdz polaczenie" (Action (znajdzPolaczenie mainMenuContext))),
		(Spacer),
		(Choice '2' "Pokaz stacje" (Action (pokazStacje mainMenuContext))),
		(Choice '3' "Pokaz trasy" (Action (pokazTrasy mainMenuContext))),
		(Choice '4' "Pokaz kursy" (Action (pokazKursy mainMenuContext))),
		(Spacer),
		(Choice '5' "Dodaj stacje" (Action (akcjaDodajStacje mainMenuContext))),
		(Choice '6' "Usun stacje" (Action (akcjaUsunStacje mainMenuContext))),
		(Spacer),
		(Choice '7' "Dodaj trase" (Action (akcjaDodajTrase mainMenuContext))),
		(Choice '8' "Usun trase" (Action (akcjaUsunTrase mainMenuContext))),
		(Spacer),
		(Choice '9' "Dodaj kurs" (Action (akcjaDodajKurs mainMenuContext))),
		(Choice '0' "Usuń kurs" (Action (akcjaUsunKurs mainMenuContext))),
		(Spacer),
		(Choice 'l' "Wyloguj" (Action (logout mainMenuContext))),
		(Choice 'q' "Koniec" ExitAction)
		]
	
-------------------------------------------------------------------------------
-- ACTIONS - AUTHENTICATION
-------------------------------------------------------------------------------		
login (MainMenuContext user timetable) = do
	putStrLn "Podaj nazwe uzytkownika:"
	loginStr <- getStyledLine choiceStyle
	putStrLn "Podaj haslo"
	passStr <- getStyledLine choiceStyle
	
	let supposedUser = User loginStr passStr
	if tryLogin supposedUser then do
		putStrLn "Zalogowano"
		return (MainMenuContext supposedUser timetable)
	else do
		putStrLn "Bledny login lub haslo"
		return (MainMenuContext user timetable)
	
logout (MainMenuContext _ timetable) = do
	putStrLn "Wylogowano"
	return (MainMenuContext Anonymous timetable)

-------------------------------------------------------------------------------
-- ACTIONS - QUERIES
-------------------------------------------------------------------------------			
pokazStacje context = do
	let timetable = (getContextTimetable context)
	let stacje = getTimetableStops timetable
	
	if null stacje then do
		putStrLn "Nie ma stacji!"
	else do
		putStrLn "Stacje:"
		printStops stacje
		
	return context
				
pokazTrasy context = do
	let timetable = (getContextTimetable context)
	let trasy = getTimetableRoutes timetable
	
	if null trasy then do
		putStrLn "Nie ma tras!"
	else do
		putStrLn "Trasy:"
		printRoutes trasy timetable
	
	return context

pokazKursy context = do
	let timetable = (getContextTimetable context)
	let courses = getTimetableCourses timetable
	
	if null courses then do
		putStrLn "Nie ma kursow!"
	else do
		putStrLn "Kursy:"
		printCourses courses timetable
		
	return context		
	
znajdzPolaczenie context = do
		let timetable = getContextTimetable context
		putStrLn "Wybierz stacje poczatkowa:"
		let stacje = getTimetableStops timetable
		printStops stacje
		stop1n <- pobierzNumerStacji stacje
		putStrLn "Wybierz stacje koncowa:"
		printStops (filtrujStacje stacje stop1n)
		stop2n <- pobierzNumerStacji (filtrujStacje stacje stop1n)
		putStrLn "Podaj dzien wyjazdu:"
		printDays dniTygodnia				
		day <- pobierzDzien dniTygodnia
		putStrLn "Podaj godzine wyjazdu z przedzialu 0 - 23:"
		hour <- pobierzGodzine	
		let godzina = fromHourMinute hour 0
		putStrLn "Podaj maksymalna ilosc przystankow:"
		maxStops <- pobierzNumer "Podaj maksymalna ilosc przystankow:"
		wyswietlTrase(findQuickestRoute timetable stop1n stop2n (Datetime (toEnum(day)) godzina) maxStops)
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

	
-------------------------------------------------------------------------------
-- ACTIONS - ADMINISTRATION - STOPS
-------------------------------------------------------------------------------	
akcjaDodajStacje context = do
	let timetable = getContextTimetable context
	nazwa <- pobierzNazwe "Podaj nazwe stacji"
	let stops = getTimetableStops timetable
	let numer = getStopSequence stops 0
	let newStop = Stop numer nazwa		
	let newStops = stops ++ [newStop]
	let newTimetable = setTimetableStops timetable newStops
	printStyledStr defaultStyle "Dodano stacje "
	printStyledStr defaultStyle  nazwa
	printStyledStr defaultStyle  " id: " 
	let numerStr = show numer
	printStyledStr defaultStyle numerStr
	putStrLn ""
	return (setContextTimetable context newTimetable)
	
akcjaUsunStacje context = do
	putStrLn "Wybierz stacje do usuniecia:"
	let rozklad = getContextTimetable context
	let stacje = getTimetableStops rozklad
	
	printStops stacje
	stop <- pobierzNumerStacji stacje

	if((moznaUsunacStacje (getTimetableCourses rozklad) stop) == True) then do
		let newStops = filtrujStacje stacje stop
		let newTimetable = setTimetableStops rozklad newStops	
		putStrLn "Usunieto stacje"
		return (setContextTimetable context newTimetable)
	else do
		putStrLn "Nie mozna usunac stacji, poniewaz wchodzi w sklad aktywnych kursow"
		return context
		

filtrujStacje stacje idStacji = filter (\s -> getStopId s /= idStacji) stacje

moznaUsunacStacje [] _ = True
moznaUsunacStacje ((Course  _ _ _ _ stops):xs) id = (sprawdzKurs stops id) && (moznaUsunacStacje xs id)

-------------------------------------------------------------------------------
-- ACTIONS - ADMINISTRATION - ROUTES
-------------------------------------------------------------------------------	
akcjaDodajTrase context = do
	let rozklad = getContextTimetable context
	let stacje = getTimetableStops rozklad
	let routes = getTimetableRoutes rozklad
	nazwa <- pobierzNazwe "Podaj nazwe trasy:"
	let numer = getRouteSequence routes 0
	polaczenia <- dodajTraseLoop stacje [] rozklad 0	
	let rt = Route numer nazwa polaczenia
	putStrLn "Dodano nowa trase "
	printStyledStr defaultStyle nazwa
	printStyledStr defaultStyle " id: "
	let numerStr = show numer
	printStyledStr defaultStyle numerStr
	putStrLn ""
	let newTimetable = setTimetableRoutes rozklad (routes ++ [rt])
	return (setContextTimetable context newTimetable)
	
dodajTraseLoop stacje wybraneStacje context st = do
	if(st /= -1) then do
		putStrLn "Podaj stacje wchodzaca w sklad kursu, lub -1, jesli koniec:"
		printStops (filtrujStacje stacje st)
		st <- pobierzNumerStacji stacje		
		dodajTraseLoop (filtrujStacje stacje st) ([st] ++ wybraneStacje) context st
	else
		return (wybraneStacje)
		
akcjaUsunTrase context = do
	let rozklad = getContextTimetable context
	let routes = getTimetableRoutes rozklad	
	rid <- pobierzNumerTrasy routes rozklad
	if((moznaUsunacTrase (getTimetableCourses rozklad) rid) == True) then do
		let newRoutes = usunTrase routes rid
		let newTimetable = setTimetableRoutes rozklad newRoutes
		putStrLn "Usunieto stacje"
		return (setContextTimetable context newTimetable)
	else do
		putStrLn "Nie mozna usunac trasy, poniewaz wchodzi w sklad aktywnych kursow"
		return context

usunTrase [] _ = []
usunTrase((Route id nazwa stops):xs) sid = do 
				if(sid == id) then  ((usunTrase xs sid))
				else  (Route id nazwa stops) : (usunTrase xs sid)
		
moznaUsunacTrase [] _ = True
moznaUsunacTrase ((Course  _ rid _ _ _):xs) id = if(rid == id) then False
											   else (moznaUsunacTrase xs id) 
	
	
-------------------------------------------------------------------------------
-- ACTIONS - ADMINISTRATION - COURSES
-------------------------------------------------------------------------------	
akcjaDodajKurs context = do
	let rozklad = getContextTimetable context
	let routes = getTimetableRoutes rozklad
	let courses = getTimetableCourses rozklad
	let stops = getTimetableStops rozklad
	putStrLn "Podaj trase:"
	rId <- pobierzNumerTrasy routes rozklad
	putStrLn "Podaj bazowa godzine odjazdu:"
	h <- pobierzGodzine
	putStrLn "Podaj bazowa minute odjazdu:"	
	m <- pobierzMinute
	let cid = getCourseSequence (getTimetableCourses rozklad) 0
	cstops <- pobierzCzasyOdjazdow stops []
	let newCourse = Course cid rId (fromHourMinute h m) [Mon, Tue] cstops
	
	printStyledStr defaultStyle "Dodano nowy kurs"
	printStyledStr defaultStyle " id: "
	let numerStr = show cid
	printStyledStr defaultStyle numerStr	
	let newTimetable = setTimetableCourses rozklad (courses ++ [newCourse])
	return (setContextTimetable context newTimetable)	
	
	
usunKurs [] _ = []
usunKurs((Course id rid t d stops):xs) sid = do 
				if(sid == id) then  ((usunKurs xs sid))
				else  (Course id rid t d stops) : (usunKurs xs sid)
		
akcjaUsunKurs context = do
	let rozklad = getContextTimetable context
	let courses = getTimetableCourses rozklad
	printCourses courses rozklad
	course <- pobierzNumerKursu courses
	let newCourses = usunKurs courses course
	let newTimetable = setTimetableCourses rozklad newCourses
	return (setContextTimetable context newTimetable)	


sprawdzKurs [] _ = True
sprawdzKurs ((CourseStop sid _):xs) id = if(sid == id) then False
											else sprawdzKurs xs id	
	
-------------------------------------------------------------------------------
-- ID GENERATION
-------------------------------------------------------------------------------	
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
	
