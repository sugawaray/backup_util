module BackupUtil where

import Data.Foldable(toList)
import Data.List(intercalate)
import Data.Maybe(isJust, catMaybes)
import Data.Either()
import System.Directory
import System.Posix.Files
import System.IO
import Control.Exception

type BackupFlag = Bool

data BackupInfo = BackupInfo BackupFlag deriving Show

data BackupFile = BackupFile (Either RegularBackupFile BackupDirectory)

data RegularBackupFile = RegularBackupFile
                         (Maybe BackupFile)
                         [FilePath]
                         (Maybe Integer)
                         BackupInfo

data BackupDirectory = BackupDirectory
                       (Maybe BackupFile)
                       [FilePath]
                       [BackupFile]
                       BackupInfo

class BackupFileClass a where
  getPath :: a -> [FilePath]
  
  pathString :: a -> String
  pathString file = intercalate "/" $ getPath file

  pathWithSize :: a -> (String, Integer)

  toBackup :: a -> Bool
  
  pathWithFlag :: a -> ([FilePath], Bool)
  pathWithFlag file = (getPath file, toBackup file)

instance BackupFileClass BackupFile where
  getPath file = pathOfBackupFile file
  pathWithSize file = 
    let pathString' = case file of
                        BackupFile (Left _) -> pathString file
                        BackupFile (Right _) -> (pathString file) ++ "/"
    in (pathString', sizeOfBackupFile file)
  toBackup file = case backupInfoOfBackupFile file of
                    BackupInfo True -> True
                    _ -> False

instance BackupFileClass BackupDirectory where
  getPath file = pathOfBackupDirectory file
  pathWithSize file = ((pathString file) ++ "/", sizeOfBackupDirectory file)
  toBackup (BackupDirectory _ _ _ (BackupInfo flag)) = flag

instance BackupFileClass RegularBackupFile where
  getPath file = pathOfRegularBackupFile file
  pathWithSize file = ((pathString file), sizeOfRegularBackupFile file)
  toBackup (RegularBackupFile _ _ _ (BackupInfo flag)) = flag

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path =
  handle handler $
  bracket (openFile path ReadMode)
  (hClose)
  (\h -> do size <- hFileSize h
            return $ Just size)
  where handler :: SomeException -> IO (Maybe Integer)
        handler _ = return Nothing

isDirectory :: FilePath -> IO Bool
isDirectory path = do
  linkStatus <- getSymbolicLinkStatus path >>= (\s -> return (System.Posix.Files.isSymbolicLink s))
  if linkStatus
    then return False
    else do
        isADirectory <- doesDirectoryExist path
        return isADirectory

isSpecialFile :: FilePath -> Bool
isSpecialFile path | path == "." || path == ".." = True
                   | otherwise = False

removeSpecialFilesImpl :: [FilePath] -> [FilePath] -> [FilePath]
removeSpecialFilesImpl [] acc = acc
removeSpecialFilesImpl (x:xs) acc | isSpecialFile x = removeSpecialFilesImpl xs acc
                                  | otherwise = removeSpecialFilesImpl xs (x:acc)

removeSpecialFiles :: [FilePath] -> [FilePath]
removeSpecialFiles path = removeSpecialFilesImpl path []

childFiles :: FilePath -> IO [FilePath]
childFiles path =
  do
    files <- getDirectoryContents path
    filesExceptSpecialFiles <- return (removeSpecialFiles files)
    return ((++) <$> [path ++ "/"] <*> filesExceptSpecialFiles)

childFileNames :: FilePath -> IO [FilePath]
childFileNames path =
  do
    files <- getDirectoryContents path
    return (removeSpecialFiles files)

childFiles1 :: FilePath -> IO [(FilePath, FilePath)]
childFiles1 path =
  do
    fileNames <- childFileNames path
    let filePaths = map (\name -> path ++ "/" ++ name) fileNames
    return (zipWith (\a b -> (a, b)) fileNames filePaths)

childFileSizes :: FilePath -> IO [(FilePath, Integer)]
childFileSizes path = do
  files <- childFiles path
  sizes <- mapM getFileSize files
  results <- return (zipWith (\a b -> (a, b)) files sizes)
  return [(a, b) | (a, Just b) <- results]

sizeOfRegularBackupFile :: RegularBackupFile -> Integer
sizeOfRegularBackupFile (RegularBackupFile _ _ Nothing _) = 0
sizeOfRegularBackupFile (RegularBackupFile _ _ (Just size) info) =
  case info of
    BackupInfo True -> size
    _ -> 0

backupInfoOfBackupFile :: BackupFile -> BackupInfo
backupInfoOfBackupFile (BackupFile (Left (RegularBackupFile _ _ _ info))) = info
backupInfoOfBackupFile (BackupFile (Right (BackupDirectory _ _ _ info))) = info
    
sizeOfBackupDirectory :: BackupDirectory -> Integer
sizeOfBackupDirectory (BackupDirectory _ _ theChildren info) =
  case info of
    BackupInfo True ->
      let isBackupTarget = (\backupFile ->
                              let childInfo = backupInfoOfBackupFile
                                              backupFile in
                                case childInfo of
                                  BackupInfo True -> True
                                  _ -> False) in
        let summarize = (foldl (+) 0)
                        . (map sizeOfBackupFile)
                        . (filter isBackupTarget) in
        summarize theChildren
    _ -> 0

sizeOfBackupFile :: BackupFile -> Integer
sizeOfBackupFile (BackupFile x) = either
                                  sizeOfRegularBackupFile
                                  sizeOfBackupDirectory
                                  x

nameOfRegularBackupFile :: RegularBackupFile -> FilePath
nameOfRegularBackupFile (RegularBackupFile _ path _ _) = (last path)

nameOfBackupDirectory :: BackupDirectory -> FilePath
nameOfBackupDirectory (BackupDirectory _ path _ _) = (last path)

nameOfBackupFile :: BackupFile -> FilePath
nameOfBackupFile (BackupFile x) = either
                        nameOfRegularBackupFile
                        nameOfBackupDirectory
                        x

pathOfRegularBackupFile :: RegularBackupFile -> [FilePath]
pathOfRegularBackupFile (RegularBackupFile _ path _ _) = path

pathOfBackupDirectory :: BackupDirectory -> [FilePath]
pathOfBackupDirectory (BackupDirectory _ path _ _) = path

pathOfBackupFile :: BackupFile -> [FilePath]
pathOfBackupFile (BackupFile file) = either
                        pathOfRegularBackupFile
                        pathOfBackupDirectory
                        file

parentOfBackupFile :: BackupFile -> Maybe BackupFile
parentOfBackupFile (BackupFile file) =
  case file of
    Left (RegularBackupFile parent _ _ _) -> parent
    Right (BackupDirectory parent _ _ _) -> parent

setBackupFlag :: BackupFile -> Bool -> BackupFile
setBackupFlag (BackupFile file) flag =
  case file of
    Left (RegularBackupFile parent path size _)
      -> BackupFile (
      Left (RegularBackupFile parent path size (BackupInfo flag)))
    Right (BackupDirectory parent path children' _)
      -> BackupFile (
      Right (BackupDirectory parent path children' (BackupInfo flag)))

toggleBackupFlag :: BackupFile -> BackupFile
toggleBackupFlag file = setBackupFlag file (not (toBackup file))

children :: BackupDirectory -> [BackupFile]
children (BackupDirectory _ _ theChildren _) = theChildren

childrenOfBackupFile :: BackupFile -> [BackupFile]
childrenOfBackupFile (BackupFile backupFile) =
  either (\_ -> []) (\file -> children file) backupFile

collectBackupFiles :: [FilePath] -> IO [BackupFile]
collectBackupFiles path =
  do
    pairs <- childFiles1 $ intercalate "/" path
    let nextPairs = map (\pair -> (path ++ [fst pair]
                                  , snd pair)) pairs
    mapM (\pair -> loadBackupFile (fst pair)) nextPairs

loadBackupFile :: [FilePath] -> IO BackupFile
loadBackupFile path =
  let setParent parent child =
        case child of
          BackupFile (Right (BackupDirectory _ path' children' backupInfo'))
            -> BackupFile (
            Right (BackupDirectory parent path' children' backupInfo'))
          BackupFile (Left (RegularBackupFile _ path' size' backupInfo'))
            -> BackupFile (
            Left (RegularBackupFile parent path' size' backupInfo'))
  in
  do
    let filePath = intercalate "/" path
    itIsDirectory <- BackupUtil.isDirectory filePath
    if itIsDirectory
      then
      do
        childBackupFiles <- collectBackupFiles path
        let selfNode =
              let setParent' = setParent (Just selfNode) in
                BackupFile (
                Right (BackupDirectory
                       Nothing
                       path
                       (map setParent' childBackupFiles)
                       (BackupInfo True)))
        return selfNode
      else
      do
        size <- getFileSize filePath
        return (BackupFile
                (Left (RegularBackupFile
                       Nothing
                       path
                       size
                       (BackupInfo True))))

data MatchResult = NotMatch | DoMatch | ChildrenMaybeMatch deriving (Enum)

matchBackupFileOnPath :: BackupFile -> [FilePath] -> MatchResult
matchBackupFileOnPath _ [] = NotMatch
matchBackupFileOnPath file (path:[]) =
  if path == (nameOfBackupFile file) then DoMatch else NotMatch
matchBackupFileOnPath file (x:_) =
  if x == (nameOfBackupFile file) then ChildrenMaybeMatch else NotMatch

findBackupFile :: [FilePath] -> BackupFile -> Maybe BackupFile
findBackupFile path root =
  case (matchBackupFileOnPath root path) of
    DoMatch -> Just root
    ChildrenMaybeMatch ->
      case root of
        BackupFile (Right backupDirectory) ->
          let findChild = findBackupFile (tail path) in
            let matchResults = filter
                               isJust
                               (map
                               findChild
                               (children backupDirectory)) in
            head matchResults
        BackupFile (Left _) -> Nothing
    _ -> Nothing

updateBackupFile :: [FilePath] -> BackupFile -> BackupFile -> BackupFile
updateBackupFile path newNode root =
  case matchBackupFileOnPath root path of
    DoMatch -> newNode
    ChildrenMaybeMatch ->
      case root of
        BackupFile (Right backupDirectory) ->
          let updateChild = updateBackupFile (tail path) newNode in
            let updatedTree = map updateChild (children backupDirectory) in
              case backupDirectory of
                BackupDirectory parent thePath _ info ->
                  BackupFile (Right (BackupDirectory parent thePath updatedTree info))
        BackupFile (Left _) -> root
    _ -> root

updateFileInTree :: [FilePath] -> (BackupFile -> BackupFile) -> BackupFile
                 -> (Bool, BackupFile)
updateFileInTree path updateFunc root =
  let callUpdateBackupFile newNode = updateBackupFile path newNode root
  in
    let result =
          fmap callUpdateBackupFile
          <$> fmap updateFunc $ findBackupFile path root
    in
      case result of
        Just x -> (True, x)
        Nothing -> (False, root)

showContentOfBackupFile :: BackupFile -> [(String, Integer)]
showContentOfBackupFile backupFile =
  map pathWithSize $ childrenOfBackupFile $ backupFile

listChildDirectories :: BackupFile -> [BackupDirectory]
listChildDirectories backupFile =
  let toDirectory (BackupFile x) =
        case x of
          Right x' -> Just x'
          Left _ -> Nothing
  in
    catMaybes $ map toDirectory $ childrenOfBackupFile backupFile

backupFilePath :: BackupFile -> Maybe String
backupFilePath file =
  case backupInfoOfBackupFile file of
    BackupInfo flag | flag -> Just $ pathString file
                    | otherwise -> Nothing

walkBackupFiles :: Bool -> (BackupFile -> Maybe a) -> BackupFile -> [a]
walkBackupFiles skip callback file =
  let walk files =
        case files of
          (x:xs) -> (walkBackupFiles skip callback x) ++ (walk xs)
          [] -> []
      call file' | toBackup file = callback file'
                 | otherwise = Nothing
      returnCallResult =
        case file of
          BackupFile (Left _) -> toList (call file)
          BackupFile (Right directory)
            ->  let heads = walk $ children directory
                in heads ++ catMaybes [(call file)]
  in
    if not (toBackup file) && skip then [] else returnCallResult

setBackupFlagByList :: BackupFile -> [([FilePath], Bool)] -> BackupFile
setBackupFlagByList file flagSeq =
  foldl update file flagSeq
  where
    update acc v =
      let setFlag file' = setBackupFlag file' (snd v)
          r = updateFileInTree (fst v) setFlag acc
      in
        case fst r of
          True -> (snd r)
          _ -> acc
