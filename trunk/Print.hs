module Print where
import System.Console.ANSI
import DummyData
import Domain
import Data.Char
import Users
import Styles


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