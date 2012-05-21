module UI where
import System.Console.ANSI
import DummyData
import Domain
import Data.Char
import Users
import Data.Maybe
		
-------------------------------------------------------------------------------
-- INIT & RELEASE
-------------------------------------------------------------------------------
initUI = do
	setSGR defaultStyle
		
releaseUI = do
	setSGR [Reset]	
		
-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
newScreen = do
	clearScreen
	setCursorPosition 0 0		
	
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
	
-------------------------------------------------------------------------------
-- PRINTING WITH COLORS
-------------------------------------------------------------------------------	
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
-- MENU WIDGET
-------------------------------------------------------------------------------
data Action context = Action (IO context) | ExitAction 
data Choice context = Choice Char [Char] context | Spacer | InvalidChoice
	
menu choices = do
	putStr "\n"
	printChoices choices
	putStr "\nWybierz opcje: "
	input <- getStyledLine choiceStyle
	newScreen
	processChoice (findChoice input)
	where
		printChoices [] = do return ()
		
		printChoices (Spacer:cs) = do
			printStyledStrLn specialStyle "========================================"
			printChoices cs
		
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
				keyCompare key Spacer = False
				
		processChoice InvalidChoice = do
			printStyledStrLn errorStyle "Invalid choice. Try again!"
			menu choices
			
		processChoice (Choice _ _ (Action action)) = do
			value <- action
			return (Just value)
			
		processChoice (Choice _ _ ExitAction) = do
			return Nothing
	
-------------------------------------------------------------------------------
-- LISTING WIDGETS
-------------------------------------------------------------------------------	
printStops [] = do return ()
printStops((Stop key name):cs)  = do
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
	
printCourses [] _  = do return ()
printCourses ((Course cid rid time days cstops):cs) context  = do
	printStyledStr defaultStyle (show cid)
	printStyledStr defaultStyle ": "
	printStyledStr defaultStyle (show time)
	putStrLn ""
	printCourseStops cstops context
	putStrLn ""
	printCourses cs context 

printCourseStops [] _ = do return ()
printCourseStops ((CourseStop cid t):cs) context = do	
	let name = (getStopNameById cid (getTimetableStops context))
	printStyledStr defaultStyle name
	printStyledStr defaultStyle " +"
	printStyledStr defaultStyle (show t)
	putStrLn ""
	printCourseStops cs context
	
printRoutes [] _ = do return ()
printRoutes((Route id name stops):cs) context = do
	printStyledStr choiceStyle (show id)
	printStyledStr specialStyle ") "
	putStrLn name
	printStopsByIds stops context
	putStrLn "Koniec trasy"
	printRoutes cs context
	
printStopsByIds [] _ = do return ()
printStopsByIds ((sid):stopids) context = do
	let name = (getStopNameById sid (getTimetableStops context))
	printStyledStr defaultStyle name
	printStyledStr defaultStyle " -> "
	printStopsByIds stopids context
	
-------------------------------------------------------------------------------
-- INPUT WIDGETS
-------------------------------------------------------------------------------	
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
	
	
--pobierzDni wybrane dni = do		
--		printDays dniTygo
--		nr <- getStyledLine choiceStyle				
--		if((sprawdzDzien dni nr) == False ) then do
--			putStrLn "Podaj dzien wyjazdu:"
--			pobierzDzien dni
--		else do
--			let nrN = read nr::Int
--			return (nrN)
			
			
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
	
	
sprawdzNumerKursu [] _ = False
sprawdzNumerKursu ((Course cid _ _ _ _):xs) nr = do
										if((isNum(nr) == True)) then do
											let nrN = read nr::Int
											if(cid == nrN || nrN == -1) then True
											else sprawdzNumerKursu xs nr
										else False
	
pobierzNumerKursu kursy = do
		nr <- getStyledLine choiceStyle		
		if((sprawdzNumerKursu kursy nr) == False) then do
			putStrLn "Podaj prawidlowy numer kursu:"
			pobierzNumerKursu kursy
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
				
isNum str = case reads str :: [(Integer, String)] of
	[(_, "")] -> True
	_         -> False