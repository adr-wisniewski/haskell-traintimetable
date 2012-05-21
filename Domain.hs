module Domain where
import Data.List
import Data.Map
import Data.Maybe
import Text.Printf
import Debug.Trace

-------------------------------------------------------------------------------
-- TIME
-------------------------------------------------------------------------------
-- TIME
data Day = Mon|Tue|Wed|Thu|Fri|Sat|Sun deriving (Show, Enum, Read)
next day = case day of
	Sun -> Mon
	_ -> succ day
	
addDay day n = toEnum $ mod (fromEnum day + n) 7

data Time = Time Int deriving (Eq, Ord, Read)

instance Show Time where
	show time = printf "%02d:%02d" (fst hm) (snd hm)
		where
			hm = toHourMinute time

fromHourMinute h m 
	| h < 0 || h >= 24 = error "invalid hour"
	| m < 0 || m >= 60 = error "invalid minute"
	| otherwise = Time (h * 60 + m)

toHourMinute time = (getHours time, getMinutes time)

getHours (Time total) = total `div` 60  
getMinutes (Time total) = total `mod` 60
addMinutesTime (Time total) minutes = Time ((total + minutes) `mod` (60*24))
	
-- DATETIME
data Datetime = Datetime Day Time
instance Show Datetime where
	show (Datetime day time) = show day ++ " " ++ show time


addMinutes datetime minutes = fromMinutes (toMinutes datetime + minutes)	
toMinutes (Datetime day time) = fromEnum day * 24 * 60 + (getHours time * 60) + (getMinutes time)
fromMinutes minutes = Datetime (toEnum dayTime) (Time hmTime)
	where
		dayTime = mod (div minutes (24 * 60)) 7
		hmTime = mod minutes (24 * 60)
		
elapsedMinutesFromTo from to
	| minutesFrom > minutesTo = weekMinutes - diff
	| otherwise = diff
	where
		minutesFrom = toMinutes from
		minutesTo = toMinutes to
		diff = abs (minutesTo - minutesFrom)
		weekMinutes = 7 * 24 * 60	
		
earliest start traveltime initialDeparture days = earliestTime
	where
		times = [let 
					departure = addMinutes (Datetime day initialDeparture) traveltime
					waitingTime = elapsedMinutesFromTo start departure 
					in (departure, waitingTime) | day <- days]
		--earliestTime = trace ("lol" ++ show times) (minimumBy earliestComparator times)
		earliestTime = minimumBy earliestComparator times
		earliestComparator left right = compare (snd left) (snd right)
		
		
		
data Dzien = Dzien Int String
dniTygodnia  = [(Dzien 1 "Poniedzialek"),
			   (Dzien 2 "Wtorek" ),
			   (Dzien 3 "Sroda" ),
			   (Dzien 4 "Czwartek" ),
			   (Dzien 5 "Piatek" ),
			   (Dzien 6 "Sobota" ),
			   (Dzien 7 "Niedziela")]
-------------------------------------------------------------------------------
-- TIMETABLE
-------------------------------------------------------------------------------
-- STOP
type StopId = Int
data Stop = Stop StopId String deriving (Show, Eq, Read)

getStopId :: Stop -> StopId
getStopId (Stop stopId _) = stopId

getStopName :: Stop -> String
getStopName (Stop _ stopName) = stopName

getStopNameById _ [] = ""
getStopNameById id ((Stop sid name):stops) = 
			if(id == sid) then name
			else getStopNameById id stops

-- ROUTE
type RouteId = Int
data Route = Route RouteId String [StopId] deriving (Show, Read)
getRouteId :: Route -> RouteId
getRouteId (Route routeId _ _) = routeId

getRouteStops  :: Route -> [StopId]
getRouteStops (Route _ _ stops) = stops

getRouteNameById _ [] = ""
getRouteNameById id ((Route rid name _):routes) = 
			if(id == rid) then name
			else getRouteNameById id routes

-- COURSE
data CourseStop = CourseStop StopId Int deriving (Show, Read)



		



getCourseStopId :: CourseStop -> StopId
getCourseStopId (CourseStop stopId _) = stopId

getTravelTime :: CourseStop -> Int
getTravelTime (CourseStop _ travelTime) = travelTime

type CourseId = Int
data Course = Course CourseId RouteId Time [Day] [CourseStop]  deriving (Show, Read)
getCourseId (Course courseId _ _ _ _) = courseId

getCourseRouteId :: Course -> RouteId
getCourseRouteId (Course _ routeId _ _ _) = routeId

getCourseDepartureTime :: Course -> Time
getCourseDepartureTime (Course _ _ time _ _) = time

getCourseDays :: Course -> [Day]
getCourseDays (Course _ _ _ days _) = days

getCourseStops :: Course -> [CourseStop]
getCourseStops (Course _ _ _ _ stops) = stops

getCourseStop :: Course -> StopId -> CourseStop
getCourseStop course stopId
	| isNothing maybeStop = error ("no stop " ++ show stopId ++ "in course " ++ show courseId)
	| otherwise = fromJust maybeStop
	where
		stops = getCourseStops course
		courseId = getCourseId course
		maybeStop = find (\stop -> getCourseStopId stop == stopId) stops

getCourseStopsAfter :: Course -> StopId -> [CourseStop]
getCourseStopsAfter course stopId = dropWhile (\x -> getCourseStopId x == stopId) $ dropWhile (\x -> getCourseStopId x /= stopId) $  getCourseStops course

data Timetable = Timetable [Course] [Route] [Stop] deriving (Show, Read)

emptyTimetable = Timetable [] [] []

getTimetableCourses :: Timetable -> [Course]
getTimetableCourses (Timetable courses _ _) = courses

getTimetableRoutes :: Timetable -> [Route]
getTimetableRoutes (Timetable _ routes _) = routes

getTimetableStops :: Timetable -> [Stop]
getTimetableStops (Timetable _ _ stops) = stops

getTimetableStopsCount :: Timetable -> Int
getTimetableStopsCount (Timetable _ _ stops) = length stops

setTimetableCourses :: Timetable -> [Course] -> Timetable
setTimetableCourses (Timetable _ routes stops) courses = (Timetable courses routes stops)

setTimetableRoutes :: Timetable -> [Route] -> Timetable
setTimetableRoutes (Timetable courses _ stops) routes = (Timetable courses routes stops)

setTimetableStops :: Timetable -> [Stop] -> Timetable
setTimetableStops (Timetable courses routes _) stops = (Timetable courses routes stops)

isValidStop timatable stopId = isJust $ find (\s -> getStopId s == stopId) $ getTimetableStops timatable

-- helper structure for next course
-- NextCourse 
--		Course - reference to course
--		Datetime - departure time
--		Int - waiting time in minutes
data NextCourse = NextCourse Course Datetime Int
instance Show NextCourse where
	show (NextCourse course departure waitingTime) = "(NextCourse c:" ++ show (getCourseId course) ++ " d:" ++ show departure ++ " w:" ++ show (Time waitingTime) ++ ")"

getNextCourseCourse :: NextCourse -> Course
getNextCourseCourse (NextCourse course _ _) = course

getNextCourseDepartureTime :: NextCourse -> Datetime
getNextCourseDepartureTime (NextCourse _ departureTime _) = departureTime

getNextCourseWaitingTime :: NextCourse -> Int
getNextCourseWaitingTime (NextCourse _ _ waitingTime) = waitingTime

fromTimetable :: Course -> Datetime -> StopId -> NextCourse
--fromTimetable course startDatetime stopId | trace ("fromTimetable " ++ (show course) ++ " " ++ (show startDatetime) ++ (show stopId)) False = undefined
fromTimetable course startDatetime stopId = NextCourse course departureTime waitingTime 
	where
		courseStop = getCourseStop course stopId
		transferTime = addMinutes startDatetime transferTimeMinutes
		earliestPair = earliest transferTime (getTravelTime courseStop) (getCourseDepartureTime course) (getCourseDays course)
		departureTime = fst earliestPair
		waitingTime = snd earliestPair + transferTimeMinutes

pickBetterNextCourse :: NextCourse -> NextCourse -> NextCourse
pickBetterNextCourse left right = if getNextCourseWaitingTime left <= getNextCourseWaitingTime right then left else right

findNextCourse :: Timetable -> RouteId -> StopId -> Datetime -> Maybe NextCourse
--findNextCourse timetable routeId stopId startTime | trace ("findNextCourse route:" ++ show routeId ++ " stopId:" ++ show stopId ++ " time:" ++ show startTime) False = undefined
findNextCourse timetable routeId stopId startTime = recursiveFindNextCourse (getTimetableCourses timetable) Nothing
	where
		recursiveFindNextCourse [] best = best
		recursiveFindNextCourse (course:rest) best -- | trace ("recursiveFindNextCourse: " ++ show (getCourseRouteId course)) False = undefined
			| getCourseRouteId course /= routeId = recursiveFindNextCourse rest best
			| isNothing best = recursiveFindNextCourse rest (Just nextCourse)
			| otherwise = recursiveFindNextCourse rest (Just newBest)
			where 
				nextCourse = fromTimetable course startTime stopId 
				newBest = pickBetterNextCourse (fromJust best) nextCourse
			
-------------------------------------------------------------------------------
-- TRAVEL ROUTE
-------------------------------------------------------------------------------
-- OPTIONS
transferTimeMinutes = 5

-- TravelLeg -  helper structure denoting visitable stop and travel history
--		StopId
--		Int - travel time
--		Int - stops count
--		Datetime - arrival date/time
--		TravelLeg -- previous stop								  
data TravelLeg = TravelLeg StopId Int Int Datetime Course TravelLeg | InitialTravelLeg StopId Datetime

instance Show TravelLeg where
	show (InitialTravelLeg stopId arrivalTime) = "InitialTravelLeg stopId:" ++ show stopId ++ " arrivalTime: " ++ show arrivalTime
	show (TravelLeg stopId travelTime stops arrivalTime course previous) = "TravelLeg stopId:" ++ show stopId ++ " travelTime:" ++ show (Time travelTime) ++ " stops:" ++ show stops ++ " arrivalTime:" ++ show arrivalTime

getLegStopId :: TravelLeg -> StopId
getLegStopId (TravelLeg stopId _ _ _ _ _) = stopId
getLegStopId (InitialTravelLeg stopId _) = stopId

getLegTravelTime :: TravelLeg -> Int
getLegTravelTime (TravelLeg _ travelTime _ _ _ _) = travelTime
getLegTravelTime (InitialTravelLeg _ _) = 0

getLegStopsCount :: TravelLeg -> Int
getLegStopsCount (TravelLeg _ _ stops _ _ _) = stops
getLegStopsCount (InitialTravelLeg _ _) = 0

getLegArrivalTime :: TravelLeg -> Datetime
getLegArrivalTime (TravelLeg _ _ _ arrivalTime _ _) = arrivalTime
getLegArrivalTime (InitialTravelLeg _ arrivalTime) = arrivalTime

getLegCourse :: TravelLeg -> Maybe Course
getLegCourse (TravelLeg _ _ _ _ course _) = Just course
getLegCourse (InitialTravelLeg _ _) = Nothing

getLegPreviousLeg :: TravelLeg -> Maybe TravelLeg
getLegPreviousLeg (TravelLeg _ _ _ _ _ previous) = Just previous
getLegPreviousLeg (InitialTravelLeg _ _) = Nothing

-- function that compares two legs (assuming they both have same stopId)
-- returns Maybe Ordering, because legs can be uncompareable (longer with fewer stops or shorter with more stops) 
legCompare left right
	| lTravelTime == rTravelTime && lStopsCount == rStopsCount = Just EQ
	| lTravelTime <= rTravelTime && lStopsCount <= rStopsCount = Just LT
	| lTravelTime >= rTravelTime && lStopsCount >= rStopsCount = Just GT
	| otherwise = Nothing
	where
		lStopsCount = getLegStopsCount left
		lTravelTime = getLegTravelTime left
		rStopsCount = getLegStopsCount right
		rTravelTime = getLegTravelTime right


-- TravelRoute - route of travel. References last travel leg with complete travel history
-- DestinationUnreachable - route doesnt exist
-- TooFewStops - couldnt find any route within given stop limit
data TravelRoute = TravelRoute TravelLeg | DestinationUnreachable | TooFewStops deriving (Show)

type FeasibilityMap = Map StopId [TravelLeg]

-- facade function implementing validation and starting recursive procedure
findQuickestRoute :: Timetable -> StopId -> StopId -> Datetime -> Int -> [TravelRoute]
findQuickestRoute timetable beginStopId endStopId travelStartDatetime maxStops
	| invalidBegin = error "invalid starting stop"
	| invalidEnd = error "invalid ending stop"
	| invalidStops = error "invalid maxStops"
	| beginStopId == endStopId = error "begin must be different than end"
	| otherwise = recursiveFindQuickestRoute stopQueue feasibilityMap timetable endStopId maxStops
	where
		invalidBegin = not $ isValidStop timetable beginStopId
		invalidEnd = not $ isValidStop timetable endStopId
		invalidStops = maxStops <= 0
		initialStop = InitialTravelLeg beginStopId travelStartDatetime
		stopQueue = [initialStop]
		feasibilityMap = singleton 1 [initialStop]
	
-- recursive worker function
recursiveFindQuickestRoute :: [TravelLeg] -> FeasibilityMap -> Timetable -> StopId -> Int -> [TravelRoute]
recursiveFindQuickestRoute [] _ _ _ _ = [DestinationUnreachable]
--recursiveFindQuickestRoute (next:rest) feasibilityMap timetable endStopId maxStops | trace ("recursiveFindQuickestRoute:\n\t" ++ show next) False = undefined
recursiveFindQuickestRoute (next:rest) feasibilityMap timetable endStopId maxStops
	| getLegStopId next == endStopId = (TravelRoute next):recursiveFindQuickestRoute rest feasibilityMap timetable endStopId maxStops -- add this leg and check next options
	| getLegStopsCount next == maxStops = recursiveFindQuickestRoute rest feasibilityMap timetable endStopId maxStops -- when stops == maxstops there is no sense of extending this path, so only check next leg
	| getLegStopsCount next > maxStops = [TooFewStops]
	| otherwise = recursiveFindQuickestRoute updatedStopQueue updatedFeasibilityMap timetable endStopId maxStops
	where
		temp = findNextLegs next feasibilityMap timetable
		rechableLegs = fst temp
		updatedFeasibilityMap = snd temp
		--updatedStopQueue = trace ("found new legs: " ++ show rechableLegs) (insertElementsIntoPriorityQueue rest rechableLegs isLegPriorityHigher)
		updatedStopQueue = insertElementsIntoPriorityQueue rest rechableLegs isLegPriorityHigher
		isLegPriorityHigher candidate leg = fromMaybe GT (legCompare candidate leg) /= GT
		
-- helper function
-- finds all next route legs from given leg according to timetable 
-- with exception of infeasible legs (longer than previously visited)
-- TODO: update feasibilityMap
findNextLegs :: TravelLeg -> FeasibilityMap -> Timetable -> ([TravelLeg], FeasibilityMap)
findNextLegs leg feasibilityMap timetable = (nextLegs, feasibilityMap)
	where
		stopId = getLegStopId leg
		arrivalTime = getLegArrivalTime leg
		initialTravelTime = getLegTravelTime leg
		stopsCount = getLegStopsCount leg
		nextLegs = getAllReachableLegs stopId arrivalTime (getTimetableRoutes timetable)
		getAllReachableLegs stopId startTime [] = []
		getAllReachableLegs stopId startTime (x:xs)
			| not routeGoesThroughThatStop || isNothing maybeNextCourse = getAllReachableLegs stopId startTime xs
			| otherwise = feasibleLegs ++ (getAllReachableLegs stopId startTime xs)
			where
				routeGoesThroughThatStop = any (== stopId) (getRouteStops x)
				maybeNextCourse = findNextCourse timetable (getRouteId x) stopId startTime
				--nextCourse = trace ("next course: " ++ show maybeNextCourse) (fromJust maybeNextCourse)
				nextCourse = fromJust maybeNextCourse
				rawCourse = getNextCourseCourse nextCourse
				--stopTravelTime = trace ("found course:" ++ show (getCourseId rawCourse)) (getTravelTime (getCourseStop rawCourse stopId))
				stopTravelTime = getTravelTime (getCourseStop rawCourse stopId)
				departureTime = getNextCourseDepartureTime nextCourse
				waitingTime = getNextCourseWaitingTime nextCourse
				legs = produceLegs (getCourseStopsAfter rawCourse stopId)
				feasibleLegs = filterInfeasible legs feasibilityMap
				filterInfeasible legs feasibilityMap = legs -- TODO: finish this
				produceLegs stops = Data.List.map stopToLeg stops
					where
						--stopToLeg s = trace ("nextleg:" ++ show nextTravelTime ++ "i:" ++ show initialTravelTime ++ "w:" ++ show waitingTime ++ "t:" ++ show travelTime) (TravelLeg nextStopId nextTravelTime nextStopsCount nextArrivaltime rawCourse leg)
						stopToLeg s = TravelLeg nextStopId nextTravelTime nextStopsCount nextArrivaltime rawCourse leg
							where
								travelTime = getTravelTime s - stopTravelTime
								nextStopId = getCourseStopId s
								nextStopsCount = stopsCount + 1
								nextTravelTime = initialTravelTime + waitingTime + travelTime
								nextArrivaltime = addMinutes departureTime travelTime
		
--getAllStopRoutes :: Timetable -> StopId -> [Route]
--getAllStopRoutes timetable stopId = filter (routeContainsStop stopId) 
--	where
--		routeContainsStop stopId route = any 

-- helper function
-- adds array of elements into priority queue, preserving order resulting from priority
insertElementsIntoPriorityQueue :: (Show a) => [a] -> [a] -> (a -> a -> Bool) -> [a]
--insertElementsIntoPriorityQueue queue elements f | trace ("insertElementsIntoPriorityQueue " ++ (show queue) ++ " " ++ (show elements)) False = undefined
insertElementsIntoPriorityQueue queue [] _ = queue
insertElementsIntoPriorityQueue queue (x:xs) f = insertElementsIntoPriorityQueue (insertIntoPriorityQueue queue x f) xs f
insertIntoPriorityQueue [] element _ = [element]
insertIntoPriorityQueue (x:xs) element f
	| f element x = element:x:xs
	| otherwise = x:insertIntoPriorityQueue xs element f
