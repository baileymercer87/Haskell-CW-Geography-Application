import Data.List
import Data.Char
import Text.Printf
import Text.Read

type Place = String
type DegreesNorth = Double
type DegreesEast = Double

type AverageRainList = [Integer]

type PlaceData = (Place, DegreesNorth, DegreesEast, AverageRainList)



-- Q1 - Returns a string of all places
getAllPlaces :: [PlaceData] -> String
getAllPlaces((place,_,_,_):places)
 | places == []       = place ++ "\n"
 | otherwise         = place ++ "\n" ++ getAllPlaces places


-- Q2 - Returns the average rainfall for a place given its name
averageRainfall :: String -> [PlaceData] -> String
averageRainfall st ((place,_,_,averages):places)
 | st == place   = printf"%.2f"((fromIntegral(sum averages)) / (fromIntegral 7):: Float)
 | places == []  = ""
 | otherwise     = averageRainfall st places
 
 
-- Q3 - Returns a formatted string with all places and average rainfall for the last 7 days
placesAsString :: [PlaceData] -> String
placesAsString [] = ""
placesAsString ((place,_,_,averages):places) = "\n" ++ place ++ " \t \t" ++ rainAsString averages ++ placesAsString places

rainAsString :: [Integer] -> String
rainAsString [] = ""
rainAsString (x:xs) = (show x)  ++ "\t \t" ++ rainAsString xs


-- Q4 - Returns the places which were completely dry a given number of days ago
dryPlaces :: Int -> [PlaceData] -> String
dryPlaces day ((place,_,_,averages):places)
 | (averages !! (day-1)) == 0 && places == []  = place ++ "\n"
 | (averages !! (day-1)) /= 0 && places == []  = "\n"
 | (averages !! (day-1)) == 0       =  place ++ "\n" ++ dryPlaces day places
 | otherwise            = "" ++ dryPlaces day places

 

-- Q5 - Updates the rainfall of each place given an array of new values
updateRainfall :: AverageRainList -> [PlaceData] -> [PlaceData]
updateRainfall (z:zs) ((place,north,east,averages):places) 
 | places == [] || zs == []       = [(place, north, east, [z] ++ (init averages))]
 | otherwise      = [(place, north, east, [z] ++ (init averages))] ++ updateRainfall zs places


-- Q6 - Removes a given place and adds a new place given all of its data
newPlace :: String -> PlaceData -> [PlaceData] -> [PlaceData]
newPlace oldPlace newPlaceData ((place,a,b,c):places)
 | oldPlace == place && places == []   = [newPlaceData]
 | oldPlace /= place && places == []   = [(place,a,b,c)]
 | oldPlace == place   = [newPlaceData] ++ newPlace oldPlace newPlaceData places
 | otherwise     = [(place,a,b,c)] ++ newPlace oldPlace newPlaceData places
 
 
 -- Q7 - Finds the nearest place to a given point which was completely dry yesterday
getDistances :: Double -> Double -> [PlaceData] -> [Double]
getDistances northPoint eastPoint ((place,north,east,average):places)
 | places == []               = [sqrt(((northPoint - north)^2) + ((eastPoint - east)^2))]
 | head (average) == 0      = [sqrt(((northPoint - north)^2) + ((eastPoint - east)^2))] ++ getDistances northPoint eastPoint places
 | otherwise                = getDistances northPoint eastPoint places
 
check :: Double -> Double -> Double -> [PlaceData] -> String
check distance northPoint eastPoint((place,north,east,_):rest)
 | (sqrt(((northPoint - north)^2) + ((eastPoint - east)^2))) == distance   = "Place: " ++ place ++ "    " ++ "Distance: " ++ printf"%.2f"(distance)
 | otherwise = check distance northPoint eastPoint rest


closestDryPlace :: Double -> Double -> [PlaceData] -> String
closestDryPlace northPoint eastPoint places = (check distance northPoint eastPoint places)
	where distance = head (sort(getDistances northPoint eastPoint places))

	
	
-- (Q8) - Function for drawing the map
type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
	
-- Main function to draw the map, calling other functions to plot points
drawMap :: [PlaceData] -> IO()
drawMap ((place,north,east,averages):places) = do
 writeAt ((10 + (floor east))*10,((100 -(ceiling north))*4)-155) ("+ " ++ place ++ "  " ++ averageRainfall place dataStore)
 if (length places) > 0 
 then do
  drawMap places
 else do
  writeAt (0,80) ""
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	


	
-- Q9 Onwards

-- Starter function to start IO system and load data from txt file
main :: IO ()
main = do 
 putStr("========================================================")
 loadPlaces <- readFile "testdata.txt"
 let places = read loadPlaces
 putStr("\nSuccesfully loaded the Place data!\n")
 putStr("========================================================\n\n")
 mainMenu places
	
	
	
-- Main menu function which displays the menu tot the user and gets their option
mainMenu :: [PlaceData] -> IO()
mainMenu places = do 
 putStrLn("========================================================")
 putStrLn("Enter the number for the option:")
 putStrLn(" 1 | Show all places")
 putStrLn(" 2 | Display average rainfall for a place")
 putStrLn(" 3 | Display all places and 7 day rainfall figures")
 putStrLn(" 4 | Display places which were dry a umber of days ago")
 putStrLn(" 5 | Update rainfall figures")
 putStrLn(" 6 | Add a new place")
 putStrLn(" 7 | Display closest dry place to position")
 putStrLn(" 8 | Display a map of the places and their rainfall")
 putStrLn(" 0 | Save and Exit")
 putStrLn("========================================================\n")
 putStr("Please insert your option: ")
 option <- getLine
 putStrLn("\n")
 runFunctions option places
  
  
  

  
  
-- Function to run the selected option
runFunctions :: String -> [PlaceData] -> IO ()

runFunctions "1" places = ioGetAllPlaces places

runFunctions "2" places = ioAverageRainfall places

runFunctions "3" places = ioPlaceString places

runFunctions "4" places = ioDryPlaces places

runFunctions "5" places = ioUpdateRainfall places

runFunctions "6" places = ioNewPlace places

runFunctions "7" places = ioClosestDryPlace places

runFunctions "8" places = ioRainfallMap places

runFunctions "0" places = saveExit places

-- Validation of option input
runFunctions _ places = do
	putStr("*******Please only enter your choice from 0-9!*******\n\n\n")
	mainMenu places

-- Function to save and exit the system
saveExit :: [PlaceData] -> IO()
saveExit places =
 do
  putStrLn("========================================================\n")
  putStrLn("Saving the test data....")
  writeFile "testdata.txt" ( show places )
  putStrLn("Saved the Database successfully!")
  putStrLn("Exiting the system\n")
  putStrLn("========================================================\n")
  
  
  
-- (Q1) IO function to display all places
ioGetAllPlaces :: [PlaceData] -> IO()
ioGetAllPlaces places =
 do
  putStr ("------------------------------------------------------------\n")
  putStr ("You have chosen to display all of the places.\n")
  putStr ("------------------------------------------------------------\n" ++ getAllPlaces places ++ "\n") 
  putStr ("------------------------------------------------------------\n\n")
  mainMenu places

-- (Q2) IO function to display average rainfall for a place
ioAverageRainfall :: [PlaceData] -> IO()
ioAverageRainfall places = do 
 putStr("------------------------------------------------------------\n\n")
 putStr("Enter place: ")
 place <- getLine
 putStr("\n")
 if (checkPlace place places) == "Valid"
 then do
  putStrLn("\nAverage Rainfall in " ++ place ++ " is:  " ++ (averageRainfall place places) ++ "\n\n")
  putStr("------------------------------------------------------------\n")
  mainMenu places
 else do 
  if place == ""
  then do
   putStr ("Returning to main menu....\n\n")
   putStr("------------------------------------------------------------\n\n")
   mainMenu places
  else do
   putStrLn ("*****Invalid place, please try again or press ENTER to return to the main menu*****\n")
   ioAverageRainfall places
  

-- (Q3) IO function to display places with their 7 day rainfall figures
ioPlaceString :: [PlaceData] -> IO()
ioPlaceString places =
 do
  putStr ("------------------------------------------------------------\n\n")
  putStr ("You have chosen to display all of the places with the average rainfall.\n")
  putStr ("------------------------------------------------------------\n" ++ placesAsString places)
  putStr ("\n\n------------------------------------------------------------\n\n\n")
  mainMenu places	

-- (Q4) IO function to display all places that were dry a given number of days ago
ioDryPlaces :: [PlaceData] -> IO()
ioDryPlaces places = do
 putStr("------------------------------------------------------------\n\n")
 putStr("Enter number of days ago: ")
 days <- getLine
 putStr("\n\n")
 if days == ""
 then do
  putStr ("Returning to main menu....\n\n")
  putStr("------------------------------------------------------------\n\n")
  mainMenu places
 else do
  if (all isDigit days) == True
  then do
   if (read days :: Int) > -1 && (read days :: Int) < 8
   then do
    putStr ("You have chosen to see what places were dry " ++ show (days) ++ " days ago\n\n")
    putStr (dryPlaces (read days::Int) places ++ "\n\n")
    putStr ("------------------------------------------------------------\n\n")
    mainMenu places
   else do
    putStr("****Number is out of range 0-7, Please try again or press ENTER to go back to the main menu*****\n")
    ioDryPlaces places
  else do
   putStr("****Number is out of range 0-7, Please try again or press ENTER to go back to the main menu*****\n")
   ioDryPlaces places
  
  
-- (Q5) - IO for updating the rainfall figures
ioUpdateRainfall :: [PlaceData] -> IO()
ioUpdateRainfall places = do
 putStr ("------------------------------------------------------------\n\n")
 putStr("Enter new average rainfall list (14): ")
 averages <- getLine
 let newAverageRainList = read averages::AverageRainList
 let updatedPlaces = (updateRainfall newAverageRainList places)
 putStr ("\n\nSuccessful! \n\n")
 putStr ("------------------------------------------------------------\n\n")
 mainMenu updatedPlaces

-- (Q6) - IO for adding a new place
ioNewPlace :: [PlaceData] -> IO()
ioNewPlace places = do
 putStr ("------------------------------------------------------------\n\n")
 putStr("Enter old place name: ")
 oldName <- getLine
 if (checkPlace oldName places) == "Valid"
 then do
  putStr("Enter new place name: ")
  newName <- getLine
  putStr("Enter new north position: ")
  north <- getLine
  putStr("Enter new east position: ")
  east <- getLine
  putStr("Enter new average rainfall list (7): ")
  averages <- getLine
  let eastDegrees = read east::Double
  let northDegrees = read north::Double
  let newAverageRainList = read averages::AverageRainList
  let newPlaceData =(newName, northDegrees, eastDegrees, newAverageRainList)
  let updatedPlaces = (newPlace oldName newPlaceData places)
  putStr("\nSuccess!!!\n")
  putStr ("------------------------------------------------------------\n\n")
  mainMenu updatedPlaces
 else do 
  if oldName == ""
  then do
   putStr ("Returning to main menu....\n\n")
   putStr("------------------------------------------------------------\n\n")
   mainMenu places
  else do
   putStrLn ("*****Invalid place, please try again or press ENTER to return to the main menu*****\n")
   ioNewPlace places
 
 
 
 -- (Q7) - IO function to display closest dry place
ioClosestDryPlace :: [PlaceData] -> IO()
ioClosestDryPlace places = do
 putStr("Enter north co-ord: ")
 north <- getLine
 putStr("Enter east co-ord: ")
 east <- getLine
 putStr ("------------------------------------------------------------\n\n")
 putStr ("You have chosen to see the closest dry place to: " ++ show(north) ++ " " ++ show(east) ++ "\n")
 putStr ("\n------------------------------------------------------------\n\n")
 putStr ( closestDryPlace(read north::Double) (read east::Double) places ++ "\n\n")
 putStr ("------------------------------------------------------------\n\n")
 mainMenu places
 
 
-- (Q8) - IO function to draw the rainfall map
ioRainfallMap :: [PlaceData] -> IO()
ioRainfallMap places = do
 clearScreen
 drawMap places
 mainMenu places
 
-- Function used to validate a place name
checkPlace :: String -> [PlaceData] -> String
checkPlace inputPlace ((place,_,_,_):places)
 | inputPlace == place   = "Valid"
 | places == []          = "Invalid"
 | otherwise             = checkPlace inputPlace places
 
 
dataStore = [("London", 51.5, -0.1, [0,0,5,8,8,0,0]),
	("Cardiff", 51.5, -3.2, [12,8,15,0,0,0,2]),
	("Norwich", 52.6, 1.3, [0,6,5,0,0,0,3]),
	("Birmingham", 52.5, -1.9, [0,2,10,7,8,2,2]),
	("Liverpool", 53.4, -3.0, [8,16,20,3,4,9,2]),
	("Hull ", 53.8, -0.3, [0,6,5,0,0,0,4]),
	("Newcastle", 55.0, -1.6, [0,0,8,3,6,7,5]),
	("Belfast", 54.6, -5.9, [10,18,14,0,6,5,2]),
	("Glasgow", 55.9, -4.3, [7,5,3,0,6,5,0]),
	("Plymouth", 50.4, -4.1, [4,9,0,0,0,6,5]),
	("Aberdeen", 57.1, -2.1, [0,0,6,5,8,2,0]),
	("Stornoway", 58.2, -6.4, [15,6,15,0,0,4,2]),
	("Lerwick", 60.2, -1.1, [8,10,5,5,0,0,3]),
	("St Helier", 49.2, -2.1, [0,0,0,0,6,10,0])]
 
 
 
 
 
 
 
 
 