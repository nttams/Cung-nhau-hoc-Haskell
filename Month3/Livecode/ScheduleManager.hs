
-- 👉 Đề Bài : Chương trình quản lý lịch làm việc 

-- 👉Nội Dung : Xây dựng một hệ thống để theo dõi và quản lý lịch làm việc hàng ngày, hàng tuần và hàng tháng. 
-- Mỗi ngày có thể chứa nhiều sự kiện, và mỗi tuần hoặc tháng có thể chứa nhiều ngày hoặc tuần con tương ứng.

-- 👉Các chức năng :
--       - Thêm sự kiện: Thêm sự kiện vào một ngày cụ thể.
--       -Xóa sự kiện: Xóa sự kiện khỏi một ngày cụ thể.
--       -Tìm kiếm sự kiện: Tìm kiếm sự kiện dựa trên tên hoặc thời gian.
--       -Cập nhật sự kiện: Cập nhật thông tin của một sự kiện cụ thể.

-- 👉 Các kiến thức về haskell được sử dụng để xây dựng chương trình : Creating Parameterized and Recursive Types,Recursion and Folds,Higher-order functions,Pattern Matching.....





-- Định nghĩa kiểu Event biểu diễn một sự kiện với tên và thời gian (giờ bắt đầu, giờ kết thúc).

data Event = Event { eventName :: String, eventTime :: (Int,Int) } deriving Show

event1 = Event { eventName = "Gym", eventTime = (8,9)}
event2 = Event { eventName = "Lunch", eventTime = (12,13)}
event3 = Event { eventName = "Study Haskell", eventTime = (15,17)}
event4 = Event { eventName = "Dinner", eventTime = (18,19)}


-- Định nghĩa kiểu Schedule để biểu diễn lịch làm việc hàng ngày, hàng tuần và hàng tháng.

data Schedule = Daily [Event] | Weekly [Schedule] | Monthly [Schedule] deriving Show

dailySchedule = Daily []
weeklySchedule = Weekly (replicate 7 (Daily []))

monthlySchedule = Monthly (replicate 4 (Weekly (replicate 7 (Daily []))))


-- Hàm trợ giúp để cập nhật một phần tử trong danh sách tại vị trí cụ thể

updateList :: Int -> (a->a)->[a]->[a]
updateList 0 f (x:xs )= f x : xs
updateList n f (x :xs )= x : updateList (n-1) f xs


-- Hàm thêm sự kiện

addEvent :: Event ->(Int,Int)->Schedule->Schedule
addEvent event (_,_) (Daily events)= Daily (event:events)
addEvent event (_,day) (Weekly days) = Weekly (updateList (day-2) (addEvent event (0,0)) days)
addEvent event (day,week) (Monthly weeks) = Monthly (updateList (week-1) (addEvent event (0,day))weeks)

addEvent1 = addEvent event1 (0,0) dailySchedule
addEvent2 = addEvent event2 (0,0) addEvent1
addEvent3 = addEvent event3 (0,0) addEvent2
addEvent4 = addEvent event4 (0,0) addEvent3


addEventWeek1 = addEvent event1 (0,5) weeklySchedule
addEventWeek2 = addEvent event2 (0,8) addEventWeek1

addEventMonthly1 = addEvent event1 (3,2) monthlySchedule
addEventMonthly2 = addEvent event2 (8,3) addEventMonthly1


-- Hàm xóa sự kiện

removeEvent :: String ->(Int,Int)->Schedule->Schedule
removeEvent name (_,_) (Daily events)= Daily (filter (\e->eventName e /= name) events)
removeEvent name  (_,day) (Weekly days)= Weekly (updateList (day-2) (removeEvent name (0,0))days)
removeEvent name (day,week) (Monthly weeks) = Monthly (updateList (week-1 )(removeEvent name (0,day))weeks)

removeEvent1 = removeEvent "Study Haskell" (0,0) addEvent4
removeEvent2 = removeEvent "Dinner" (0,0) removeEvent1


removeEventWeekly1 = removeEvent "Lunch" (0,8) addEventWeek2

removeEventMonthly1 = removeEvent "Study Haskell" (8,3) addEventMonthly2



-- Hàm tìm kiếm sự kiện
searchEvent :: String->Schedule->[Event]
searchEvent name (Daily events)= filter (\e->eventName e==name) events
searchEvent name (Weekly days) = concatMap (searchEvent name) days
searchEvent name (Monthly weeks) = concatMap (searchEvent name) weeks


searchEventMonthly1 = searchEvent "l" addEventMonthly2


-- Hàm cập nhật sự kiện

updateEvent :: String->Event->(Int,Int)->Schedule->Schedule
updateEvent oldName newEvent (_,_) (Daily events)= Daily (map (\e->if eventName e==oldName then newEvent else e) events)
updateEvent oldName newEvent (_,day) (Weekly days) = Weekly (updateList (day-2) (updateEvent oldName newEvent (0,0))days)
updateEvent oldName newEvent (day,week) (Monthly weeks) = Monthly (updateList (week-1) (updateEvent oldName newEvent (0,day))weeks)

updateEventMonthly1 :: Schedule
updateEventMonthly1 = updateEvent "Lunch" event3 (8,3) addEventMonthly2

