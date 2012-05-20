import Database
import Domain
import UI
import DummyData

--context = empty::DB
main = do	
	let context = loadContext "timetable.dat"
	initUI
	putStrLn "Train timetable v1.00" 
	mainMenu context
	writeContext context "timetable.dat"
	releaseUI
		

writeContext :: IO DB -> String -> IO ()
writeContext context fname = do writeFile fname (show context)

loadContext fname = do 
                     line <- readFile fname
                     let context = read line :: DB 
                     return context