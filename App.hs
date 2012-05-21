import Database
import Domain
import UI
import DummyData


main = do	

	--context <- loadContext "timetable.dat"
	initUI
	putStrLn "Train timetable v1.00" 
	
	mainMenu rozklad
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
