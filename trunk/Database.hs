module Database where
import Domain

data DB = DB [Stop] deriving (Show)

class Databases db where
	empty :: db
	setStops :: db -> [Stop] -> db
	getStops :: db -> [Stop]
	addStop :: db -> Stop -> db
	remStop :: db -> Stop -> db
	
	addStop db stop = setStops db (stop:getStops db)
	remStop db stop = setStops db $ filter (/= stop) $ getStops db

instance Databases DB where
	empty = DB []
	setStops (DB stops) stops' = DB stops'
	getStops (DB stops) = stops