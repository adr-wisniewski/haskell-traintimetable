module UI where
import System.Console.ANSI
import DummyData
import Domain
import Data.Char
import Users
import Print
import Styles
		
		

-------------------------------------------------------------------------------
-- MENU COMPONENT
-------------------------------------------------------------------------------


data Action action = Action action (Timetable) | ExitAction 
data Choice action = Choice Char [Char] action | InvalidChoice
	
	
	
--menu :: [Choice (Action)] -> (IO DB) -> IO String


	
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
		

