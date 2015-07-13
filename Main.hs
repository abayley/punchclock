#!/usr/bin/env runhaskell

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Eq as Eq
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Ord as Ord
import qualified Data.Time.Format as FormatTime
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Calendar.WeekDate as WeekDate
import qualified Data.Time.Clock as Clock
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Directory as Dir
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified Text.Printf as Printf
import qualified Text.Read as Read
import qualified Text.Regex as Regex

{-
sundial
pendulum
metronome
crocodile

timesheet files - one per week?
store in ~/.timesheets
store tasks in ~/.timesheets/task.txt

commands
list : show tasks via pager, like git log
add : create new task (at top of list?)
del : remove task from list
t in [task]  record starting on task. Ends current task (if any).
t out/off  stop working altogether.

We recommend that the task list is in todo.txt format:
https://github.com/ginatrapani/todo.txt-cli/wiki/The-Todo.txt-Format

but this is not required (we just copy the entire task to
the timesheet), but it is likely to make reporting easier
e.g producing a summary by project.
-}


-- Utilities and a coupld of orphan instances
-- that will be useful later.

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x


thd3 :: (a, b, c) -> c
thd3 (_,_,c) = c


rpad :: Int -> String -> String
rpad n str =
    str ++ replicate (n - List.length str) ' '


-- delete item n from a list
removeItem :: Int -> [a] -> [a]
removeItem n l = take (n-1) l ++ drop n l


instance Eq.Eq LocalTime.ZonedTime where
    (==) a b = (==) (LocalTime.zonedTimeToUTC a)  (LocalTime.zonedTimeToUTC b)


instance Ord.Ord LocalTime.ZonedTime where
    compare a b = compare (LocalTime.zonedTimeToUTC a)  (LocalTime.zonedTimeToUTC b)


-- A single timed event.
type TimeEvent = (LocalTime.ZonedTime, Text.Text)
-- A duration where a single task was performed.
type TimeEntry = (LocalTime.ZonedTime, LocalTime.ZonedTime, Text.Text)

-- Time-sheet table is to accumulate time for reporting.
-- It is a map of time-code -> rows.
-- Each row is indexed by date.
type TimeSheetRow = Map.Map Calendar.Day Double
type TimeSheetTable = Map.Map String TimeSheetRow

data ReportType = Code | Task


timeformat :: String
timeformat = "%FT%T%z"
dayformat :: String
dayformat = "%F"


timeDir :: FilePath
timeDir = "./"
-- timeDir = "/c/Users/alistair/Documents/haskell/punchclock/"
-- timeDir = "/home/abayley/.todo/"

-- taskFile = "~/.timesheets/tasks.txt"
taskFile :: String
taskFile = timeDir ++ "todo.txt"


readLines :: FilePath.FilePath -> IO [Text.Text]
readLines filePath = do
    exist <- Dir.doesFileExist filePath
    if exist
    then do
        t <- TextIO.readFile filePath
        -- strip empty lines
        return (List.filter (not . Text.null) (Text.lines t))
    else do
        putStrLn (filePath ++ " not found")
        return []


writeLines :: FilePath.FilePath -> [Text.Text] -> IO ()
writeLines filePath t = TextIO.writeFile filePath (Text.unlines t)


writeTimeSheet :: FilePath.FilePath -> [TimeEvent] -> IO ()
writeTimeSheet filePath timeSheet = do
    -- maybe (return ()) putStrLn (validate timeSheet)
    writeLines filePath (map formatTimeEvent timeSheet)


{-
getEnvMaybe s =


-- If the todo.cfg exists get the todo.txt path from it.
-- If not, assume current dir.
todoFilePath :: IO FilePath.FilePath
todoFilePath = do
    home = Environment.getEnv "HOME"
    let paths = [home, "."]
    let files = ["todo.cfg", ".todo.cfg"]
    todoCfg = Environment.getEnv "TODOTXT_CFG_FILE"
-}

-- construct a ZonedTime from the current day plus user-supplied time
makeUserTime :: String -> IO LocalTime.ZonedTime
makeUserTime str = do
    now <- LocalTime.getZonedTime
    let ds = List.filter Char.isDigit str
    let mbTod = FormatTime.parseTimeM True FormatTime.defaultTimeLocale "%H%M" ds
    let day = LocalTime.localDay (LocalTime.zonedTimeToLocalTime now)
    let newZT = now { LocalTime.zonedTimeToLocalTime = LocalTime.LocalTime day (Maybe.fromJust mbTod) }
    return (maybe now (const newZT) mbTod)


timesheetStart :: Calendar.Day -> Calendar.Day
timesheetStart today =
    let (y, w, _) = WeekDate.toWeekDate today
    in WeekDate.fromWeekDate y w 1


currentTimesheetStart :: IO Calendar.Day
currentTimesheetStart = do
    now <- LocalTime.getZonedTime
    let today = LocalTime.localDay (LocalTime.zonedTimeToLocalTime now)
    return (timesheetStart today)


timesheetFilepath :: Calendar.Day -> String
timesheetFilepath today =
    let monday = timesheetStart today
    in (timeDir ++ "timesheet-" ++ Calendar.showGregorian monday ++ ".txt")


parseTime :: String -> Maybe LocalTime.ZonedTime
parseTime = FormatTime.parseTimeM True FormatTime.defaultTimeLocale timeformat


parseTimeEvent :: Text.Text -> TimeEvent
parseTimeEvent text =
    let fields = Text.splitOn (Text.pack "  ") text
        (f1:f2:_) = fields
        start = Maybe.fromJust (parseTime (Text.unpack f1))
    in (start, f2)


formatTimeEvent :: TimeEvent -> Text.Text
formatTimeEvent (s, t) =
    let toStr = FormatTime.formatTime FormatTime.defaultTimeLocale timeformat
        sep = Text.pack "  "
    in (Text.pack (toStr s) Monoid.<> sep Monoid.<> t)


parseTimesheet :: FilePath.FilePath -> IO [TimeEvent]
parseTimesheet filePath = do
    lns <- readLines filePath
    return (map parseTimeEvent lns)


getTaskByPos :: [Text.Text] -> Int -> Either String Text.Text
getTaskByPos tasks n = if n > 0 && n <= length tasks
    then Right (tasks !! (n-1))
    else Left ("No task matching: " ++ show n)


getTaskByPosM :: [Text.Text] -> Int -> IO Text.Text
getTaskByPosM tasks n = either fail return (getTaskByPos tasks n)


-- If we don't find a matching task return the match text
getTaskByToken :: [Text.Text] -> String -> Text.Text
getTaskByToken tasks str =
    Maybe.fromMaybe pstr (headMaybe matches)
    where
        pstr = Text.pack str
        matches = List.filter (Text.isInfixOf pstr) tasks


-- If we have a number then return that line.
-- If is some text (a tag, maybe) then return
-- the first line matching that text.
getTask :: String -> IO Text.Text
getTask str = do
    tasks <- readLines taskFile
    maybe
        (return (getTaskByToken tasks str))
        (getTaskByPosM tasks)
        (Read.readMaybe str)


getCurrentTask :: [TimeEvent] -> Text.Text
getCurrentTask [] = Text.empty
getCurrentTask ts = snd (last ts)


getCurrentTaskTime :: [TimeEvent] -> Maybe LocalTime.ZonedTime
getCurrentTaskTime [] = Nothing
getCurrentTaskTime ts = Just (fst (last ts))


getCurrentTimesheet :: IO (String, [TimeEvent])
getCurrentTimesheet = do
    monday <- currentTimesheetStart
    let fp = timesheetFilepath monday
    ts <- parseTimesheet fp
    return (fp, ts)


-----------------------------------------------------------
-- Reporting


diffZonedTime :: LocalTime.ZonedTime -> LocalTime.ZonedTime -> Clock.NominalDiffTime
diffZonedTime start end = Clock.diffUTCTime (LocalTime.zonedTimeToUTC end) (LocalTime.zonedTimeToUTC start)


diifTimeToHours :: Clock.NominalDiffTime -> Double
diifTimeToHours i = fromRational (toRational i) / 3600


timeentryDuration :: TimeEntry -> Double
timeentryDuration (start, end, _) = diifTimeToHours (diffZonedTime start end)


timeentryDay :: TimeEntry -> Calendar.Day
timeentryDay (start, _, _) =
    LocalTime.localDay (LocalTime.zonedTimeToLocalTime start)


-- would use [\\S] but regex-compat doesn't recognise it
-- i.e. codeRegex prefix = "\\+" ++ prefix ++ "([\\S]+)"
codeRegex :: String -> String
codeRegex prefix = "\\+" ++ prefix ++ "([^ \\t\\n\\r\\f\\v]+)"


matchTimeCode :: String -> String -> String -> String
matchTimeCode nocode prefix task = maybe nocode head
    (Regex.matchRegex (Regex.mkRegex (codeRegex prefix)) task)


timeentryCodePrefix :: String -> String -> TimeEntry -> String
timeentryCodePrefix nocode prefix (_,_,task) =
    matchTimeCode nocode prefix (Text.unpack task)


accumRow :: TimeEntry -> TimeSheetRow -> TimeSheetRow
accumRow te = Map.alter f (timeentryDay te) where
        f = Just . maybe v (+ v)
        v = timeentryDuration te


accumTime :: (TimeEntry -> String) -> TimeSheetTable -> TimeEntry -> TimeSheetTable
accumTime keyf m te = Map.alter f (keyf te) m
    where f = Just . maybe (accumRow te Map.empty) (accumRow te)


weekDays :: Calendar.Day -> [Calendar.Day]
weekDays monday = map toEnum [start .. start+6]
    where start = fromEnum monday


printReportRow :: Calendar.Day -> String -> TimeSheetRow -> IO ()
printReportRow monday code row = do
    putStr ("\t\t" ++ code)
    Monad.forM_ (weekDays monday) $ \d ->
        putStr (maybe "\t." (Printf.printf "\t%.2f") (Map.lookup d row))
    putStrLn ""


dayMonth :: Calendar.Day -> String
dayMonth date = let (_, m, d) = Calendar.toGregorian date
    in show d ++ "/" ++ show m


printReport :: Calendar.Day -> TimeSheetTable -> IO ()
printReport monday table = do
    putStr "timesheet-code\t"
    Monad.forM_ (weekDays monday) $ \d -> putStr ("\t" ++ dayMonth d)
    putStrLn ""
    Monad.mapM_
        (uncurry (printReportRow monday))
        (List.sortBy (Ord.comparing fst) (Map.toList table))


parseReportType :: String -> Maybe ReportType
parseReportType ('t':_) = Just Task
parseReportType ('c':_) = Just Code
parseReportType _ = Nothing


parseDay :: String -> Maybe Calendar.Day
parseDay = FormatTime.parseTimeM True FormatTime.defaultTimeLocale dayformat


parseReportArgs :: Calendar.Day -> [String] -> Either String (Calendar.Day, ReportType)
parseReportArgs monday [] = return (monday, Code)
parseReportArgs monday [s] =
    case parseReportType s of
        Nothing ->
            case parseDay s of
                Nothing -> fail ("Date parse failed for " ++ s)
                Just d -> return (d, Code)
        Just t -> return (monday, t)
parseReportArgs _ (by:day:_) =
    case parseReportType by of
        Nothing -> fail ("invalid report type: " ++ by)
        Just t ->
            case parseDay day of
                Nothing -> fail ("Date parse failed for " ++ day)
                Just d -> return (d, t)

-- validate:
-- timestamps are always increasing
-- there aren't 2 punch-outs in a row
-- each day ends with a punch-out

validate :: [TimeEvent] -> Maybe String
validate [] = Nothing
validate [_] = Nothing
validate (e1:more1) = go e1 more1 where
    go (_, t) [] = if Text.null t then Nothing
        else Just "Did not finish with punch-out"
    go (s1, t1) (e2@(s2, t2):more) = case () of
      _ | s1 >= s2 -> Just ("entry " ++ show s1 ++ " followed by " ++ show s2)
        | Text.null t1 && Text.null t2 -> Just ("2 empty (punch-out) entries in a row: " ++ show s1 ++ ", " ++ show s2)
        | otherwise -> go e2 more


-- turns a list of events into a list of durations
createTimeEntries :: [TimeEvent] -> [TimeEntry]
createTimeEntries [] = []
createTimeEntries [_] = []
createTimeEntries (e1:more1) = go e1 more1 where
    go _ [] = []
    go (s1, t) [(s2, _)] = if Text.null t then []
        else [(s1, s2, t)]
    go (s1, t) (e@(s2, _):more) = if Text.null t then go e more
        else (s1, s2, t) : go e more


doReport :: Calendar.Day -> ReportType -> String -> IO ()
doReport start by prefix = do
    let fp = timesheetFilepath start
    tevs <- parseTimesheet fp
    let ts = createTimeEntries tevs
    let keyf = case by of
            Code -> timeentryCodePrefix "no-code" prefix
            Task -> Text.unpack . thd3
    Monad.forM_ ts $ \te ->
        putStrLn (show te ++ " " ++ Printf.printf "%.4f" (timeentryDuration te) ++ " " ++ (keyf te))
    let table = List.foldl' (accumTime keyf) Map.empty ts
    -- print table
    printReport start table


-----------------------------------------------------------
-- Commands

-- list,add,del act on the todo.txt file.
-- in,out,validate,report touch the timesheet file.

cmdList :: String -> IO ()
cmdList str = do
    tasks <- readLines taskFile
    let nats :: [Int]
        nats = [1..]
    let nTasks = [Text.append (Text.pack (rpad 3 (show n) ++ " ")) l | (n, l) <- zip nats tasks]
    let filteredTasks = if List.null str
        then nTasks
        else List.filter (Text.isInfixOf (Text.pack str)) nTasks
    mapM_ TextIO.putStrLn filteredTasks


cmdAdd :: String -> IO ()
cmdAdd task = do
    Monad.unless (List.null task) (do
        tasks <- readLines taskFile
        writeLines taskFile (List.sort (Text.pack task : tasks))
        )
    cmdList ""


cmdDel :: String -> IO ()
cmdDel str = do
    let n = maybe 0 id (Read.readMaybe str)
    lns <- readLines taskFile
    if (n > 0 && n <= List.length lns)
    then writeLines taskFile (removeItem n lns)
    else putStrLn ("task index " ++ (show n) ++ " out of range (1-" ++ show (List.length lns) ++ ")")
    cmdList ""


cmdIn :: [String] -> IO ()
cmdIn [] = putStrLn "no task given"
cmdIn [str] = doCommandIn str ""
cmdIn (str:time:_) = doCommandIn str time


doCommandIn :: String -> String -> IO ()
doCommandIn str time = do
    (fp, ts) <- getCurrentTimesheet
    task <- getTask str
    if getCurrentTask ts == task
    then putStrLn "task already in progress"
    else do
        now <- makeUserTime time
        -- check timestamp is after last entry
        let new = (now, task)
        putStrLn ("start: " ++ Text.unpack task)
        writeTimeSheet fp (ts ++ [new])


cmdOut :: String -> IO ()
cmdOut time = do
    (fp, ts) <- getCurrentTimesheet
    -- We end the current task by writing an line with
    -- just the timestamp - no task.
    -- So if the last line (the current task) is already
    -- empty, don't do it again.
    let task = getCurrentTask ts
    putStrLn ("stop: " ++ Text.unpack task)
    now <- makeUserTime time
    either putStrLn (\new -> writeTimeSheet fp (ts ++ [new]))
        (doCommandOut ts now)


doCommandOut :: [TimeEvent] -> LocalTime.ZonedTime -> Either String TimeEvent
doCommandOut ts now = do
    let task = getCurrentTask ts
    Monad.when (Text.null task)
        (fail "no current active task")
    let new = (now, Text.empty)
    case getCurrentTaskTime ts of
        Nothing -> return new
        Just prev -> if prev >= now
            then (fail "Given time precedes last entry")
            else return new



-- what can we validate?
-- that timestamps are always increasing
-- that there aren't 2 punch-outs in a row
-- that the timesheet ends with a punch-out
-- that there isn't more than 10 hours (say) in a day
cmdValidate :: String -> IO ()
cmdValidate str = do
    start <- maybe currentTimesheetStart return (parseDay str)
    let fp = timesheetFilepath start
    ts <- parseTimesheet fp
    maybe (putStrLn (fp ++ " ok")) putStrLn (validate ts)
    -- let ts2 = List.sortBy (Ord.comparing fst) ts


-- FIXME make timecode prefix an argument
cmdReport :: [String] -> IO ()
cmdReport args = do
    monday <- currentTimesheetStart
    either fail (\(start, by) -> doReport start by "timecode-")
        (parseReportArgs monday args)

-----------------------------------------------------------
-- Main

usage :: IO ()
usage = do
    putStrLn "usage: "
    putStrLn " add <task>"
    putStrLn " ls <text>"
    putStrLn " del <n>"
    putStrLn " in <n>|<task>"
    putStrLn " out <time>"
    putStrLn " report [code|task] <date>"
    putStrLn " validate <date>"


main :: IO Int
main = do
    args <- Environment.getArgs
    let tailArgs = unwords (tail args)
    case args of
        [] -> usage
        _ -> case head args of
            ('a':_) -> cmdAdd tailArgs
            ('l':_) -> cmdList tailArgs
            ('d':_) -> cmdDel tailArgs
            ('i':_) -> cmdIn (tail args)
            ('o':_) -> cmdOut tailArgs
            ('r':_) -> cmdReport (tail args)
            ('v':_) -> cmdValidate tailArgs
            _ -> usage
    return 0