module Database where
import Domain
import DummyData

data DB = DB Timetable deriving (Show, Read)

class Databases db where
	empty :: db
	setTimetable :: db -> Timetable -> db
	getTimetable :: db -> Timetable
	addTimetable :: db -> Timetable -> db
	remTimetable :: db -> Timetable -> db
	
	--addTimetable db timetable = setTimetable db (timetable:getTimetable db)
	--remTimetable db timetable = setTimetable db $ filter (/= timetable) $ getTimetable db

instance Databases DB where
	empty = DB rozklad
	setTimetable (DB timetable) timetable' = DB timetable'
	getTimetable (DB timetable) = timetable
	