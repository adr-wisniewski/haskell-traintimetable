import Database
import Domain
import UI
import DummyData

context = empty::DB
main = do	
	initUI
	putStrLn "Train timetable v1.00" 
	mainMenu context
	releaseUI
		

