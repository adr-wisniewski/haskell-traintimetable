import Database
import Domain
import UI

main = do
	initUI
	putStrLn "Train timetable v1.00" 
	mainMenu
	releaseUI
		
mainMenu = do
	menu [
		(Choice '1' "lol" (Action (dummyAction "lol"))),
		(Choice '2' "lal" (Action (dummyAction "lal"))),
		(Choice 'e' "exit" ExitAction)
		]
