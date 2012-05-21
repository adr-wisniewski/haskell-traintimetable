import Database
import Domain
import UI
import DummyData
import Styles

main = do	

	initUI
	putStrLn "Train timetable v1.00" 
	--let rozklad = loadContext "timetable.dat"
	mainMenu rozklad
	writeContext rozklad "timetable.dat"
	releaseUI
		

writeContext :: Timetable -> String -> IO ()
writeContext context fname = do writeFile fname (show context)

loadContext fname = do 
                     line <- readFile fname
                     let context = read line :: Timetable
                     return context
					 
getAct action = do
			return action
