module DummyData where
import Domain




sekwencja = (length stacje) + 1



-- Stacje
stacje = [
		(Stop 1 "Gdansk"),
		(Stop 2 "Warszawa"),
		(Stop 3 "Poznan"),
		(Stop 4 "Wroclaw")
	]
	



-- Trasy
trasy = [
		(Route 1 "Jan Kiepura" [1, 2, 3]),
		(Route 2 "Jan Kiepura" [3, 2, 1]),
		(Route 3 "Stefan Batory" [1, 3, 4])
	]

-- Jan Kiepura
janKiepuraStacje = [
		(CourseStop 1 0), 
		(CourseStop 2 60), 
		(CourseStop 3 120)
	]
	
janKiepuraKursy = [
		(Course 1 1 (fromHourMinute 09 00) [Tue,Wed,Fri,Sat,Sun] janKiepuraStacje),
		(Course 2 1 (fromHourMinute 15 00) [Tue,Wed,Fri,Sat,Sun] janKiepuraStacje)
	]
	
janKiepuraStacje2 = [
		(CourseStop 3 0), 
		(CourseStop 2 60), 
		(CourseStop 1 120)
	]
	
janKiepuraKursy2 = [
		(Course 3 2 (fromHourMinute 11 00) [Tue,Wed,Fri,Sat,Sun] janKiepuraStacje2),
		(Course 4 2 (fromHourMinute 17 00) [Tue,Wed,Fri,Sat,Sun] janKiepuraStacje2)
	]

-- Stefan Batory
stefanBatoryStacje = [
		(CourseStop 1 0), 
		(CourseStop 3 15), 
		(CourseStop 4 30)
	]
	
stefanBatoryKursy = [
		(Course 5 3 (fromHourMinute 15 30) [Sat,Sun] stefanBatoryStacje)
	]



-- Rozklad
kursy = janKiepuraKursy ++ stefanBatoryKursy ++ janKiepuraKursy2
rozklad = Timetable kursy trasy stacje


-- Testy
dt1 = Datetime Mon (fromHourMinute 14 30)
test = findQuickestRoute rozklad 1 2 dt1 1