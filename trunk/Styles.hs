module Styles where
import System.Console.ANSI
import DummyData
import Domain
import Data.Char
import Users

-------------------------------------------------------------------------------
-- INIT & RELEASE
-------------------------------------------------------------------------------
initUI = do
	setSGR defaultStyle
		
releaseUI = do
	setSGR [Reset]

-------------------------------------------------------------------------------
-- STYLES
-------------------------------------------------------------------------------
defaultStyle = [color]
	where
		color = SetColor Foreground Vivid White

choiceStyle = [color]
	where
		color = SetColor Foreground Vivid Green

specialStyle = [color]
	where
		color = SetColor Foreground Vivid Yellow
		
errorStyle = [color]
	where
		color = SetColor Foreground Vivid Red
			
printStyledStr style text = do
	setSGR style
	putStr text
	setSGR defaultStyle
	
printStyledStrLn style text = do
	setSGR style
	putStrLn text
	setSGR defaultStyle
	
getStyledLine style = do
	setSGR style
	line <- getLine
	setSGR defaultStyle
	return line

	
getStyledNumber style = do
	setSGR style
	line <- getLine
	setSGR defaultStyle
	let lineN = read line::Int
	return lineN