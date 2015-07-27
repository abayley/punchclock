#!/usr/bin/env runhaskell

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Eq as Eq
import qualified Data.List as List
import qualified Data.List.Split as Split
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
import qualified System.IO.Error as IOError
import qualified Text.Printf as Printf
import qualified Text.Read as Read
import qualified Text.Regex as Regex

{-
timesheet files - one per week?
store in ~/.todo/
store tasks in ~/.todo/todo.txt

I recommend that the task list is in todo.txt format:
  https://github.com/ginatrapani/todo.txt-cli/wiki/The-Todo.txt-Format
but this is not required (we just copy the entire task to
the timesheet).
It is likely to make reporting easier
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


-- remove chars from each end of a list (string)
trim :: Eq a => [a] -> [a] -> [a]
trim chars = dropChars . reverse . dropChars . reverse
    where dropChars = List.dropWhile ((flip elem) chars)


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
writeTimeSheet filePath timeSheet =
    writeLines filePath (map formatTimeEvent timeSheet)


ensureFileExists :: FilePath.FilePath -> IO ()
ensureFileExists filepath = do
    exists <- Dir.doesFileExist filepath
    Monad.unless exists (TextIO.writeFile filepath Text.empty)


searchFiles :: [String] -> [FilePath] -> IO (Maybe FilePath)
searchFiles files paths = do
    let filepaths = [FilePath.combine p f | p <- paths, f <- files]
    mbDirs <- Monad.forM filepaths $ \fp -> do
        exists <- Dir.doesFileExist fp
        if exists then return (Just fp) else return Nothing
    return (case Maybe.catMaybes mbDirs of
        [] -> Nothing
        (h:_) -> Just h
        )


getEnvMaybe :: String -> IO (Maybe String)
getEnvMaybe s =
    Exception.catchJust
        (\e -> if IOError.isDoesNotExistErrorType (IOError.ioeGetErrorType e) then Just () else Nothing)
        (Environment.getEnv s >>= return . Just)
        (\_ -> return Nothing)


parseConfigVar :: Map.Map String String -> String -> Map.Map String String
parseConfigVar m line =
    let parts = Split.splitOn "=" line
        key = head parts
        value = trim " :=" (if List.length parts > 1 then head (drop 1 parts) else "")
    in Map.insert key value m


parseConfigLine :: Map.Map String String -> String -> Map.Map String String
parseConfigLine m line =
    case line of
        [] -> m
        ('#':_) -> m
        _ | take 7 line == "export " -> parseConfigVar m (drop 7 line)
          | otherwise -> parseConfigVar m line


parseConfigFile :: FilePath.FilePath -> IO (Map.Map String String)
parseConfigFile fp = do
    ls <- readLines fp
    return (List.foldl' parseConfigLine Map.empty (map Text.unpack ls))


expandVars :: Map.Map String String -> String -> String
expandVars m s =
    -- Assumes only one var, at start of string.
    -- Will keep expanding, so check length as a token
    -- way to limit recursive expansion.
    if elem '$' s && List.length s < 10000
    then
        let var = takeWhile (/= '/') . tail . dropWhile (/= '$') $ s
            suffix = dropWhile (/= '/') s
        in expandVars m (maybe s (++ suffix) (Map.lookup var m))
    else s


getTodoEnv :: IO (Map.Map String String)
getTodoEnv = do
    home <- Environment.getEnv "HOME"
    let paths = [home, "."]
    let files = ["todo.cfg", ".todo.cfg"]

    mbTodoCfg <- getEnvMaybe "TODOTXT_CFG_FILE"
    mbTodoCfgFp <- case mbTodoCfg of
        Nothing -> searchFiles files paths
        Just fp -> return (Just fp)
    m <- case mbTodoCfgFp of
        Nothing -> IOError.ioError (IOError.userError "Todo config file not found")
        Just fp -> parseConfigFile fp
    env <- Environment.getEnvironment
    return (Map.union m (Map.fromList env))


todoDirPath :: IO FilePath.FilePath
todoDirPath = do
    env <- getTodoEnv
    return (maybe "./" (expandVars env) (Map.lookup "TODO_DIR" env))


-- If the todo.cfg exists get the todo.txt path from it.
-- If not, assume current dir.
todoFilePath :: IO FilePath.FilePath
todoFilePath = do
    env <- getTodoEnv
    return (maybe "./todo.txt" (expandVars env) (Map.lookup "TODO_FILE" env))


dayPart :: LocalTime.ZonedTime -> Calendar.Day
dayPart d = LocalTime.localDay (LocalTime.zonedTimeToLocalTime d)


-- construct a ZonedTime from the current day plus user-supplied time
makeUserTime :: String -> IO (Maybe LocalTime.ZonedTime)
-- empty string returns current time
makeUserTime [] = LocalTime.getZonedTime >>= return . Just
-- Nothing means the parse failed.
makeUserTime str = do
    now <- LocalTime.getZonedTime
    let ds = List.filter Char.isDigit str
    let mbTod = FormatTime.parseTimeM True FormatTime.defaultTimeLocale "%H%M" ds
    let day = dayPart now
    let newZT = now { LocalTime.zonedTimeToLocalTime = LocalTime.LocalTime day (Maybe.fromJust mbTod) }
    return (maybe Nothing (Just . const newZT) mbTod)


dayOfWeek :: Calendar.Day -> Int
dayOfWeek d =
    let (_, _, n) = WeekDate.toWeekDate d in n


timesheetStart :: Calendar.Day -> Calendar.Day
timesheetStart d =
    let (y, w, _) = WeekDate.toWeekDate d
    in WeekDate.fromWeekDate y w 1


sapTimesheetStart :: Calendar.Day -> Calendar.Day
sapTimesheetStart d =
    let (y, w, _) = WeekDate.toWeekDate (Calendar.addDays (-7) d)
    in WeekDate.fromWeekDate y w 7


currentTimesheetStart :: IO Calendar.Day
currentTimesheetStart = do
    now <- LocalTime.getZonedTime
    return (timesheetStart (dayPart now))


timesheetFilepath :: String -> Calendar.Day -> String
timesheetFilepath dir today =
    let monday = timesheetStart today
    in (dir ++ "/timesheet-" ++ Calendar.showGregorian monday ++ ".txt")


parseTime :: String -> Maybe LocalTime.ZonedTime
parseTime = FormatTime.parseTimeM True FormatTime.defaultTimeLocale timeformat


parseTimeEvent :: Text.Text -> TimeEvent
parseTimeEvent text =
    let fields = Text.splitOn (Text.pack "  ") text
        f1 = head fields
        f2 = if List.length fields > 1 then fields !! 1 else Text.empty
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


-- SAP timesheet goes sunday -> monday, but our timesheets
-- go monday -> sunday, so we need the current plus previous.
sapTimesheets :: String -> Calendar.Day -> IO [TimeEvent]
sapTimesheets dir date = do
    let monday = timesheetStart date
    let monday2 = Calendar.addDays (-7) monday
    let monday3 = Calendar.addDays (7) monday
    let sunday = Calendar.addDays (-1) monday
    let t1Path = timesheetFilepath dir monday
    let t2Path = timesheetFilepath dir monday2
    lns1 <- readLines t1Path
    lns2 <- readLines t2Path
    let evs = (map parseTimeEvent (lns2 ++ lns1))
    -- filter to just the week we want: sunday -> monday
    return (filter (\(d, _) -> dayPart d <= monday3 && dayPart d >= sunday) evs)


stripTaskPriority :: Text.Text -> Text.Text
stripTaskPriority task =
    let re = (Regex.matchRegex (Regex.mkRegex "^\\([A-Z]\\) ") (Text.unpack task))
    in case re of
        Nothing -> task
        Just _ -> Text.drop 4 task


getTaskByPos :: [Text.Text] -> Int -> Either String Text.Text
getTaskByPos tasks n = if n > 0 && n <= length tasks
    then Right (stripTaskPriority (tasks !! (n-1)))
    else Left ("No task matching: " ++ show n)


-- If we don't find a matching task return the match text
getTaskByToken :: [Text.Text] -> String -> Either String Text.Text
getTaskByToken tasks str = maybe
        (Left ("No task matching: " ++ str))
        (Right . stripTaskPriority)
        (headMaybe matches)
    where matches = List.filter (Text.isInfixOf (Text.pack str)) tasks


-- If we have a number then return that line.
-- If is some text (a tag, maybe) then return
-- the first line matching that text.
getTask :: String -> [Text.Text] -> Either String Text.Text
getTask str tasks = maybe
    (getTaskByToken tasks str)
    (getTaskByPos tasks)
    (Read.readMaybe str)


getCurrentTask :: [TimeEvent] -> Text.Text
getCurrentTask [] = Text.empty
getCurrentTask ts = snd (last ts)


-- reverse the list and return the first item with a non-empty task
getLastTask :: [TimeEvent] -> Either String Text.Text
getLastTask [] = Left "No previous task"
getLastTask ts = let t = filter (not . Text.null) . reverse . map snd $ ts in
    if List.null t
    then Left "No previous task"
    else Right (head t)


getCurrentTaskTime :: [TimeEvent] -> Maybe LocalTime.ZonedTime
getCurrentTaskTime [] = Nothing
getCurrentTaskTime ts = Just (fst (last ts))


getCurrentTimesheet :: IO (String, [TimeEvent])
getCurrentTimesheet = do
    monday <- currentTimesheetStart
    dir <- todoDirPath
    let fp = timesheetFilepath dir monday
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
timeentryDay (start, _, _) = dayPart start


timeentryFormat :: TimeEntry -> String
timeentryFormat te@(start, end, task) =
    let toStr = FormatTime.formatTime FormatTime.defaultTimeLocale "%H:%M"
    in Printf.printf "  %s-%s  %5.2f  %s" (toStr start) (toStr end)
        (timeentryDuration te) (Text.unpack task)


-- would use [\\S] but regex-compat doesn't recognise it
-- i.e. codeRegex prefix = "\\+" ++ prefix ++ "-([\\S]+)"
codeRegex :: String -> String
codeRegex prefix = "\\+" ++ prefix ++ "-([^ \t\n\r\f\v]+)"


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
weekDays start = map toEnum [d .. d+6]
    where d = fromEnum start


dayMonth :: Calendar.Day -> String
dayMonth date = let (_, m, d) = Calendar.toGregorian date
    in show d ++ "/" ++ show m


printReportRow :: Calendar.Day -> String -> TimeSheetRow -> IO ()
printReportRow start code row = do
    putStr ("\t\t" ++ code ++ "\t\t\t\t\t")
    Monad.forM_ (weekDays start) $ \d ->
        putStr (maybe "\t" (Printf.printf "\t%.2f") (Map.lookup d row))
    putStrLn ""


printReport :: Calendar.Day -> TimeSheetTable -> IO ()
printReport start table = do
    putStr "\t\ttimesheet-code\t\t\t\t\t"
    Monad.forM_ (weekDays start) $ \d -> putStr ("\t" ++ dayMonth d)
    putStrLn ""
    Monad.mapM_
        (uncurry (printReportRow start))
        (List.sortBy (Ord.comparing fst) (Map.toList table))


parseDay :: String -> Maybe Calendar.Day
parseDay = FormatTime.parseTimeM True FormatTime.defaultTimeLocale dayformat


-- Rules:
-- SAP report:
--  * date given: report week containing date
--  * no date given: if today is monday or tuesday
--    then report last week, otherwise report this week.
-- Day report:
--  * date given: report week containing date
--  * no date given: report this week
-- Task report:
--  * date given: report week containing date
--  * no date given: report this week
parseReportArgs2 :: Calendar.Day -> String -> String -> (Calendar.Day, String)
parseReportArgs2 today date timecode =
    let parseDate d = timesheetStart (Maybe.fromJust (parseDay d))
        monday = timesheetStart today
    in case timecode of
        ":day" -> if date == ""
            then (monday, timecode)
            else (parseDate date, timecode)
        ":task"-> if date == ""
            then (monday, timecode)
            else (parseDate date, timecode)
        _ -> if date == ""
            then if dayOfWeek today `elem` [1,2]
                then (sapTimesheetStart today, timecode)
                else (monday, timecode)
            else (parseDate date, timecode)


parseReportArgs :: Calendar.Day -> [String] -> (Calendar.Day, String)
parseReportArgs start [] = (start, "")
parseReportArgs start [a] = if isDate a
    then parseReportArgs2 start a ""
    else parseReportArgs2 start "" a
parseReportArgs start (a:b:_) = if isDate a
    then parseReportArgs2 start a b
    else if isDate b
        then parseReportArgs2 start b a
        else parseReportArgs2 start "" a


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
      _ | s1 >= s2 -> Just ("entry " ++ show s1 ++ " followed by earlier time: " ++ show s2)
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


doSapReport :: Calendar.Day -> String -> [TimeEntry] -> IO ()
doSapReport start prefix ts = do
    let keyf = timeentryCodePrefix "no-code" prefix
    let table = List.foldl' (accumTime keyf) Map.empty ts
    printReport (sapTimesheetStart start) table


doTaskReport :: [TimeEntry] -> IO ()
doTaskReport ts = do
    let ds = map (\te -> (timeentryDuration te, thd3 te)) ts
    -- sort and group by task (snd tuple element)
    let comp a b = Ord.compare (snd a) (snd b)
    let grp a b = snd a == snd b
    let groups :: [[(Double, Text.Text)]]
        groups = List.groupBy grp . List.sortBy comp $ ds
    let f :: [(Double, Text.Text)] -> (Text.Text, Double)
        f l = (snd (head l), sum (List.map fst l))
    let sums = List.map f groups
    Monad.forM_ sums $ \(task, total) ->
        putStrLn ((Printf.printf "%6.2f" total) ++ "  " ++ Text.unpack task)


doDayReport :: [Calendar.Day] -> [TimeEntry] -> IO ()
doDayReport [] _ = return ()
doDayReport (d:ds) ts = do
    let todays = filter ((d ==) . timeentryDay) ts
    Monad.when (not (List.null todays)) (do
        putStrLn (show d)
        Monad.mapM_ (putStrLn .timeentryFormat) todays
        putStrLn ""
        )
    doDayReport ds ts


-- SAP: a line per code, a column for each day
-- task: a line per task
-- day: a line per entry: shows start, end, task. Header per day.
doReport :: Calendar.Day -> String -> IO ()
doReport start prefix = do
    dir <- todoDirPath
    let fp = timesheetFilepath dir start
    tevs <- parseTimesheet fp
    let ts = createTimeEntries tevs
    case prefix of
        ":day" -> doDayReport (weekDays start) ts
        ":task" -> doTaskReport ts
        _ -> do
            tevs2 <- sapTimesheets dir start
            doSapReport start prefix (createTimeEntries tevs2)


-----------------------------------------------------------
-- Commands

-- list,add,del act on the todo.txt file.
-- in,out,validate,report touch the timesheet file.

cmdList :: String -> IO ()
cmdList str = do
    todoFile <- todoFilePath
    -- ensureFileExists todoFile
    tasks <- readLines todoFile
    listTasks str tasks


listTasks :: String -> [Text.Text] -> IO ()
listTasks str tasks = do
    let nats :: [Int]
        nats = [1..]
    let nTasks = [Text.append (Text.pack (rpad 3 (show n) ++ " ")) l | (n, l) <- zip nats tasks]
    let filteredTasks = if List.null str
        then nTasks
        else List.filter (Text.isInfixOf (Text.pack str)) nTasks
    mapM_ TextIO.putStrLn filteredTasks


cmdAdd :: String -> IO ()
cmdAdd task =
    if List.null task
    then putStrLn "no task given"
    else do
        todoFile <- todoFilePath
        -- ensureFileExists todoFile
        tasks <- readLines todoFile
        writeLines todoFile (List.sort (Text.pack task : tasks))
        listTasks "" tasks


cmdDel :: String -> IO ()
cmdDel str = do
    todoFile <- todoFilePath
    -- ensureFileExists todoFile
    tasks <- readLines todoFile
    let n = maybe 0 id (Read.readMaybe str)
    if (n > 0 && n <= List.length tasks)
    then do
        writeLines todoFile (removeItem n tasks)
        listTasks "" tasks
    else putStrLn ("task index " ++ (show n) ++ " out of range (1-" ++ show (List.length tasks) ++ ")")


-- why can't I used \\d for digit? argh
isTime :: String -> Bool
isTime str = maybe False (const True)
    (Regex.matchRegex (Regex.mkRegex "[0-9][0-9][:\\.][0-9][0-9]") str)


-- ISO format: yyyy-mm-dd
isDate :: String -> Bool
isDate str = maybe False (const True)
    (Regex.matchRegex (Regex.mkRegex "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]") str)


cmdIn :: [String] -> IO ()
cmdIn [] = doCommandIn "" ""
-- Try to detect if either of the first 2 args is a time
cmdIn [str] = if isTime str
    then doCommandIn "" str
    else doCommandIn str ""
cmdIn (str1:str2:_) = if isTime str1
    then doCommandIn str2 str1
    else doCommandIn str1 str2


doCommandIn :: String -> String -> IO ()
doCommandIn str time = do
    (fp, ts) <- getCurrentTimesheet
    todoFile <- todoFilePath
    tasks <- readLines todoFile
    mbnow <- makeUserTime time
    let ete = doIn2 str time ts tasks mbnow
    case ete of
        Left e -> putStrLn e
        Right te -> do
            putStrLn ("start: " ++ Text.unpack (snd te))
            writeTimeSheet fp (ts ++ [te])


doIn2 :: String -> String -> [TimeEvent] -> [Text.Text] -> Maybe LocalTime.ZonedTime -> Either String TimeEvent
doIn2 str time ts tasks mbnow = do
    -- if the task string is empty then reuse the last task
    task <- if List.null str then getLastTask ts
        else getTask str tasks
    Monad.when (getCurrentTask ts == task)
        (fail "task already in progress")
    case mbnow of
        Nothing -> fail ("not a valid start time: " ++ time)
        Just now -> do
            let mbprev = getCurrentTaskTime ts
            case mbprev of
                Nothing -> return (now, task)
                Just prev ->
                    if prev >= now
                    then fail ("Start time must be after previous event: " ++ show prev)
                    else return (now, task)


cmdOut :: String -> IO ()
cmdOut time = do
    (fp, ts) <- getCurrentTimesheet
    mbnow <- makeUserTime time
    case mbnow of
        Nothing -> putStrLn ("end time not valid: " ++ time)
        Just now -> do
            let task = getCurrentTask ts
            either putStrLn
                (\new -> do
                    putStrLn ("stop: " ++ Text.unpack task)
                    writeTimeSheet fp (ts ++ [new])
                )
                (doCommandOut ts now)


doCommandOut :: [TimeEvent] -> LocalTime.ZonedTime -> Either String TimeEvent
doCommandOut ts now = do
    let task = getCurrentTask ts
    -- We end the current task by writing a line with
    -- just the timestamp - no task.
    -- So if the last line (the current task) is already
    -- empty, don't do it again.
    Monad.when (Text.null task) (fail "no current active task")
    let new = (now, Text.empty)
    case getCurrentTaskTime ts of
        Nothing -> return new
        Just prev -> if prev >= now
            then (fail "Given time precedes last entry")
            else return new


cmdCurrent :: String -> IO ()
cmdCurrent _ = do
    (_, ts) <- getCurrentTimesheet
    TextIO.putStrLn (getCurrentTask ts)


-- what can we validate?
-- that timestamps are always increasing
-- that there aren't 2 punch-outs in a row
-- that the timesheet ends with a punch-out
-- that there isn't more than 10 hours (say) in a day
cmdValidate :: String -> IO ()
cmdValidate str = do
    start <- maybe currentTimesheetStart return (parseDay str)
    dir <- todoDirPath
    let fp = timesheetFilepath dir start
    ts <- parseTimesheet fp
    maybe (putStrLn (fp ++ " ok")) putStrLn (validate ts)


cmdReport :: [String] -> IO ()
cmdReport args = do
    now <- LocalTime.getZonedTime
    let today = dayPart now
    (uncurry doReport) (parseReportArgs today args)


-----------------------------------------------------------
-- Main

usage :: String -> IO ()
usage cmd = case cmd of
    ('r':_) -> Monad.mapM_ putStrLn (
        "usage: report <date> :day|:task|<timecode-prefix>" :
        "" :
        "Emits a report. Layout and analysis depend on args:" :
        "  :day : detailed list of weeks's tasks with start and end times and durations" :
        "  :task : summary of hours spend per task for the week" :
        "  <prefix> : SAP timesheet. Line per timecode, column per day of week. Timecodes are identified by prefix." :
        [])
    _ -> Monad.mapM_ putStrLn (
        "usage: " :
        "  add <task>" :
        "  ls <text>" :
        "  del <n>" :
        "  in <n>|<task>" :
        "  out <time>" :
        "  current" :
        "  report <date> :day|:task|<timecode-prefix>" :
        "  validate <date>" :
        [])


main :: IO Int
main = do
    args <- Environment.getArgs
    let tailArgs = unwords (tail args)
    case args of
        [] -> usage ""
        _ -> case head args of
            ('h':_) -> usage tailArgs
            ('a':_) -> cmdAdd tailArgs
            ('l':_) -> cmdList tailArgs
            ('d':_) -> cmdDel tailArgs
            ('i':_) -> cmdIn (tail args)
            ('o':_) -> cmdOut tailArgs
            ('c':_) -> cmdCurrent tailArgs
            ('r':_) -> cmdReport (tail args)
            ('v':_) -> cmdValidate tailArgs
            cmd -> usage cmd
    return 0
