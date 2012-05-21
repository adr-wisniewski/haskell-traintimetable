module Database where
import Domain
import DummyData
import Random
{-# LANGUAGE FlexibleInstances #-}
data DB = DB Timetable deriving (Show, Read)



class Databases db where
	empty :: db
	setTimetable :: db -> Timetable -> db
	getTimetable :: db -> Timetable
	addTimetable :: IO db -> Timetable -> IO db
	remTimetable :: IO db -> Timetable -> IO db
	--addTimetable db timetable = setTimetable db (timetable:getTimetable db)
	--remTimetable db timetable = setTimetable db $ filter (/= timetable) $ getTimetable db

instance Databases (DB) where
	getTimetable (DB timetable) = timetable
	setTimetable timetable timetable' = DB timetable'
	
instance Databases (IO DB) where
	empty = do 
		return (DB rozklad)
	setTimetable timetable timetable' = do 
		return (DB timetable')
		

--	getTimetable (DB timetable) = timetable
	
--instance Databases (DB) IO where
--	empty = DB rozklad
--	setTimetable (DB timetable) timetable' = DB timetable'
--	getTimetable (DB timetable) = timetable
	
	
getDb db = do
		return (db)