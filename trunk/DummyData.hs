module DummyData where
import Domain
s1 = Stop 1 "Gdansk"
s2 = Stop 2 "Warszawa"
s3 = Stop 3 "Poznan"
s4 = Stop 4 "Wroclaw"
r1 = Route 1 "Jan Kiepura" [1, 2, 3]
cs1 = CourseStop 1 11
cs2 = CourseStop 2 12
cs3 = CourseStop 3 13
cs4 = CourseStop 4 15
t1 = Time 10
c1 = Course 1 1 t1 [Mon,Tue,Wed,Thu,Fri,Sat,Sun] [cs1,cs2,cs3]
tt1 = Timetable [c1] [r1] [s1,s2,s3,s4]
dt1 = Datetime Mon t1

