module UI where
import System.Console.ANSI
import DummyData
import Domain
import Data.Char


mainMenu = do
	menu [
		(Choice '1' "Znajdz polaczenie" (Action (znajdzPolaczenie))),
		(Choice '2' "Dodaj stacje" (Action (dummyAction "lal"))),
		(Choice '3' "Usun stacje" (Action (dummyAction "lal"))),
		(Choice '4' "Dodaj polaczenie" (Action (dummyAction "lal"))),
		(Choice '5' "Usun polaczenie" (Action (dummyAction "lal"))),
		(Choice 'q' "Koniec" ExitAction)
		]
		
		
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

-------------------------------------------------------------------------------
-- MENU COMPONENT
-------------------------------------------------------------------------------
data Action = Action (IO String) | ExitAction 
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
stacje :: [Stop] -> [Choice]
stacje [] = []
stacje ((Stop sid name):xs) = (Choice (chr(sid + 96)) name (Action (getChoiceValue [(chr(sid))]))) : (stacje xs)

stacjeLst = [s1,s2,s3,s4]

-- Lista dni tygodnia do menu. Niestety, nie udalo mi sie tego zrobic przez wyciagniecie z Enum
dniTygodnia = [(Choice '1' "Poniedzialek" (Action (getChoiceValue  "1"))),
			   (Choice '2' "Wtorek" (Action (getChoiceValue  "2"))),
			   (Choice '3' "Sroda" (Action (getChoiceValue  "3"))),
			   (Choice '4' "Czwartek" (Action (getChoiceValue  "4"))),
			   (Choice '5' "Piatek" (Action (getChoiceValue  "5"))),
			   (Choice '6' "Sobota" (Action (getChoiceValue  "6"))),
			   (Choice '7' "Niedziela" (Action (getChoiceValue  "7")))]


-- Funkcja wyswietlajaca wynik dzialania findQuickestRoute
wyswietlTrase :: TravelRoute -> IO ()
wyswietlTrase TooFewStops = do 
		putStrLn "Zbyt malo przystankow"
wyswietlTrase DestinationUnreachable = do
		putStrLn "Brak polaczen"
wyswietlTrase (TravelRoute (TravelLeg stopId _ _ _ _ _)) = do
		putStrLn "Znaleziono trase"
		
		
znajdzPolaczenie = do
		putStrLn "Wybierz stacje poczatkowa:"
		stop1 <- menu (stacje(stacjeLst))		
		putStrLn "Wybierz stacje koncowa:"
		stop2 <- menu (stacje(stacjeLst))
		putStrLn "Podaj dzien wyjazdu:"
		rDate <- menu(dniTygodnia)
		putStrLn "Podaj godzine wyjazdu:"
		rTime <- getStyledLine choiceStyle		
		putStrLn "Podaj maksymalna ilosc przystankow:"
		maxStops <- getStyledLine choiceStyle
		wyswietlTrase(findQuickestRoute tt1 (ord(head(stop1))) (ord(head(stop2))) (Datetime (toEnum(ord(head(rDate))- 49)) (Time (ord(head(rTime))))) (ord(head(maxStops))))
		return ""
		mainMenu

		