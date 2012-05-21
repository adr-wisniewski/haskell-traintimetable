import Database
import Domain
import UI
import DummyData

emp = empty::DB
context = setTimetable empty rozklad
main = do	

	--context <- loadContext "timetable.dat"
	initUI
	putStrLn "Train timetable v1.00" 
	let context2 = getDb context
	mainMenu context2
--	writeContext context2 "timetable.dat"
	releaseUI
		

--writeContext :: IO DB -> String -> IO ()
--writeContext context fname = do writeFile fname (show context)

loadContext fname = do 
                     line <- readFile fname
                     let context = read line :: DB 
                     return context
					 
getAct action = do
			return action
