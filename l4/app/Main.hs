{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- usage:
-- :main -v combined_income=32103 dependants=0 rates_total=2000 additional_per_dependant=500 initial_contribution=160 maximum_allowable=620 income_threshold=24790 --goal=rr-br3nda

-- :main -v  additional_per_dependant=500 initial_contribution=160 maximum_allowable=620 income_threshold=24790 --goal=rr-br3nda --nlgstyle=algebra week_numDays=7
-- min((((2.00/3.00)*(rates_total-(initial_contribution=160.00)))-((combined_income-((income_threshold=24790.00)+((additional_per_dependant=500.00)*dependants)))/8.00)), (maximum_allowable=620.00))

import Lib
import Debug.Trace
import Data.Char
import Data.Matrix
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Text.Regex
import Text.PrettyPrint.Boxes as Box

data Flag = Quiet               -- -q
          | Verbose             -- -v
          | Help                -- --help
          | Goal String          -- --goal
          | NLGStyle (Maybe String)          -- --style
          | PySource (Maybe String)        -- --py
          | YAMLParamFile (Maybe String)    -- --params
          deriving (Eq,Ord,Show)

flags =
       [Option ['q'] ["quiet"]    (NoArg Quiet)   "Quiet"
       ,Option ['v'] ["verbose"]  (NoArg Verbose) "Verbose"
       ,Option ['g'] ["goal"]     (ReqArg Goal "Goal")   "Goal"
       ,Option ['g'] ["nlgstyle"] (OptArg NLGStyle "NLGStyle")   "Natural Language Generation Style"
       ,Option ['p'] ["pysource"] (OptArg PySource "PySource")   "Python Source"
       ,Option ['y'] ["params"]   (OptArg YAMLParamFile "Params")   "Params File"
       ,Option ['h'] ["help"]     (NoArg Help)    "Print this help message"
       ]

parseargs argv = case getOpt Permute flags argv of
  (flagargs,fs,[]) -> do
    let dataargs = if Data.List.null fs then ["-"] else fs
    if Help `elem` flagargs
      then do hPutStrLn stderr (usageInfo header flags)
              exitWith ExitSuccess
      else return (nub (concatMap set flagargs), dataargs)

  (_,_,errs)      -> do
    hPutStrLn stderr (concat errs ++ usageInfo header flags)
    exitWith (ExitFailure 1)

  where header = "Usage: l4fisca [-qv] -g concrete|abstract|neutral|algebra --goal=mygoal1 foo=bar baz=quux"
        set f      = [f]

splitOn   = splitRegex . mkRegex
splitEq f = f <$> (Main.splitOn "=")

splitToFSFD = splitEq (\a -> mkFSFD (a !! 0) (read (a !! 1) :: Double))

main :: IO ()
main = do
  (flagargs, dataargs) <- getArgs >>= parseargs
--  putStrLn $ "Flags: " ++ show flagargs
--  putStrLn $ "Dataargs: " ++ show dataargs
  let goal = Data.List.find wantgoal flagargs
      goalname = (Data.Maybe.fromMaybe
                  (error $ "goal not found; choose one of " ++ show (Map.keys goals))
                  (Map.lookup (getgoal goal) goals))
      style = Data.List.find (\arg -> case arg of (NLGStyle _) -> True; _ -> False) flagargs
      styleval = do
        (NLGStyle x) <- style
        y <- x
        (Map.lookup y nlgstyles)

  -- if any input variables in the scenario have a range operator ".." in their value, then
  -- respect [x,y..z] syntax from Haskell.
  -- for example, dependants=0..4 combined_income=0,500..30000 rates_total=0,200..2000
  --    if there is  one such range,  draw a one-dimensional table down the page
  --    if there are two such ranges, draw a two-dimensional table (using the boxes library)
  --    if there are three or more such ranges, just blast out the x1=y1 x2=y2 x3=y3 answer=N
  -- note: you want to leave --nlgstyle unspecified, or you'll get some really verbose output.

  let (rangeArgs,plainArgs) = Data.List.partition (\(k,v) -> any (`isInfixOf` v) [",",".."]) (splitEq (\a -> (a !! 0, a !! 1)) <$> dataargs)
  
  -- show single scenario
  when (rangeArgs == []) $ do
    let scenario = Map.fromList $ (\(a,b) -> mkFSFD a ((read b) :: Double)) <$> plainArgs
    putStrLn $ runScenario (Env styleval scenario variables_dict) goalname

  when (rangeArgs /= []) $ do
    -- putStrLn $ "## input contains range! the range args are " ++ show rangeArgs
    -- well, if the list comprehension fits, wear it
    let rangeScenarios = rangeArgs
    let ranges = do
          rangearg <- rangeScenarios
          let rangekey = fst rangearg
              rangeval = snd rangearg
              myrange = case length $ parseRange rangeval of
                1 -> pure ((read rangeval) :: Int)
                2 -> [(parseRange rangeval !! 0) .. (parseRange rangeval !! 1)]
                3 -> [(parseRange rangeval !! 0) ,  (parseRange rangeval !! 1) .. (parseRange rangeval !! 2)]
          return $ (rangekey, myrange)
    let output = do
          let rangekeys = fst <$> ranges
              rangevals = snd <$> ranges
              -- now we zip back the rangekeys so we know which is which
          -- (\a -> zip (fst a) (snd a) ) <$> (zip (repeat ["A","B","C"]) (sequence [[1,2,3], [20,30],[100]]))
          -- [[("A",1),("B",20),("C",100)],[("A",1),("B",30),("C",100)],[("A",2),("B",20),("C",100)],[("A",2),("B",30),("C",100)],[("A",3),("B",20),("C",100)],[("A",3),("B",30),("C",100)]]

              zipped = (\a -> Data.List.zip (fst a) (snd a)) <$> (Data.List.zip (repeat rangekeys) (sequence rangevals))
          eachthing <- zipped
          let scenario = ( Map.union
                   (Map.fromList $ (\(a,b) -> mkFSFD a (fromIntegral b))   <$> eachthing)
                   (Map.fromList $ (\(a,b) -> mkFSFD a ((read b)::Double)) <$> plainArgs) )
          let myenv = (Env Nothing scenario variables_dict)
          let answer = runScenario myenv goalname
          return $ Answer scenario answer $ eachthing ++ pure ("goal", (round ((read answer)::Double))::Int)
--          return $ (scenario, answer)
--    mapM_ putStrLn (details <$> output)
    let mymatrix = Data.Matrix.fromLists $ ((fmap snd) . details) <$> output

    case length ranges of
      -- data table format. each details contains exactly three elements: the x, the y, and the answer-goal.
      2 ->
        -- coltitles go across the top, but in practice these get glued on top of each individual column of answers, so we don't fold-join them here just yet
        let rowtitles = (text "") : ((text . show) <$> (nub $ snd <$> (!! 1) <$> (details <$> output)))
      -- rowtitles go on the left
            coltitles = (text . show) <$> (nub $ snd <$> (!! 0) <$> (details <$> output))
            datacols = Data.List.foldl (/>/) nullBox <$> chunksOf (length rowtitles - 1) ((text . show) <$> (snd <$> (!! 2) <$> (details <$> output)))
--            headeredcols = (\datarows -> Data.List.foldl (/>/) nullBox $ Data.List.zipWith (/>/) coltitles datarows) <$> datacols
--        in surround $ Data.List.foldl (Box.<+>) nullBox (Data.List.foldl (/>/) nullBox coltitles : headeredcols)
      -- full detail format
        in prettyPrintBox coltitles (Data.List.foldl (/>/) nullBox rowtitles) datacols
      _ ->
        let colnames  = (text . fst) <$> (details $ head output)
            datacols  = fmap (\col -> Data.List.foldl (/>/) nullBox (fmap (text . show) col)) (Data.Matrix.toLists $ Data.Matrix.transpose mymatrix)
        in prettyPrintBox colnames nullBox datacols
--    mapM_ putStrLn (details <$> output)
    putStrLn $ "## done with run; " ++ (show $ length output) ++ " answers across " ++ ( Data.List.intercalate " * " $ (\(rk, myr) -> (show $ length $ myr) ++ " variations of " ++ rk)  <$> ranges )
        
      where
        wantgoal arg = case arg of
          (Goal goal) -> True
          _ ->           False
        getgoal goalarg = case goalarg of
          (Just (Goal goal)) -> goal
          _ -> (error $ "specify a goal name with --goal=mygoalname; choose one of " ++ show (Map.keys goals))
        parseRange :: String -> [Int]
        parseRange x = read <$> Main.splitOn ",|\\.\\." x
        prettyPrintBox toprow leftcol datacols =
          let 
            headeredcols = Data.List.zipWith (/>/) toprow datacols
            tablerows = Data.List.foldl (Box.<+>) leftcol headeredcols
          in printBox $ surround tablerows


-- | Paste two boxes together vertically, using a default (left)
--   alignment.
(/>/) :: Box -> Box -> Box
t />/ b = vcat right [t,b]


data Answer = Answer { scn :: RatesRebateWorld
                     , ans :: String
                     , details :: [(String,Int)]
                     }


-- TOOD: move this out to an output rendering library
-- all this is from the boxes library
-- https://www.reddit.com/r/haskell/comments/4222im/how_to_make_this_tableheaders_work_with_the_boxes/
-- https://github.com/treeowl/boxes/blob/master/Text/PrettyPrint/Boxes.hs

vDiv :: Int -> Box
vDiv n = vcat left (replicate n (char '|'))

hDiv :: Int -> Box
hDiv n = hcat left (replicate n (char '-'))

surround :: Box -> Box
surround box = let r = rows box
                   c = cols box + 1
               in  (char '+' Box.<> hDiv c  Box.<> char '+')
                // (vDiv r   Box.<> box     Box.<+> vDiv r  )
                // (char '+' Box.<> hDiv c  Box.<> char '+')


