import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Maybe
import Data.Either()
import BackupUtil

testGivenRegularBackupFileWhenPathAndNameMatchThenReturnIt ::
  Assertion
testGivenRegularBackupFileWhenPathAndNameMatchThenReturnIt =
  do
    let result = findBackupFile
                 ["theName"]
                 (BackupFile (Left (RegularBackupFile
                        Nothing
                        ["theName"]
                        (Just 0)
                        (BackupInfo True)
                       )))
    assertBool "has a just value" (isJust result)
    case result of Just x -> assertEqual "the names should be equal" "theName" (nameOfBackupFile x)
                   _ -> return ()

testGivenRegularBackupFileWhenPathAndNameDoNotMatchThenReturnNothing ::
  Assertion
testGivenRegularBackupFileWhenPathAndNameDoNotMatchThenReturnNothing =
  do
    let result = findBackupFile
                 ["theName"]
                 (BackupFile (Left (RegularBackupFile
                        Nothing
                        ["theName1"]
                        (Just 0)
                        (BackupInfo True)
                       )))
    assertBool "has nothing" (isNothing result)

testGivenBackupDirectoryOnLeafWhenPathMatchThenReturnIt ::
  Assertion
testGivenBackupDirectoryOnLeafWhenPathMatchThenReturnIt =
  do
    let result = findBackupFile
                 ["theName"]
                 (BackupFile (Right
                  (BackupDirectory
                  Nothing
                  ["theName"]
                  []
                  (BackupInfo True)
                  )))
    assertBool "just BackupFile" (isJust result)

testGivenBackupDirectoryHasChildWhenThePathMatchesThenReturnTheChild ::
  Assertion
testGivenBackupDirectoryHasChildWhenThePathMatchesThenReturnTheChild =
  do
    let child1 = (BackupFile (Left
                     (RegularBackupFile
                      Nothing
                      ["theParent", "theChild1"]
                      (Just 0)
                      (BackupInfo True)
                     )))
    let child2 = (BackupFile (Left
                     (RegularBackupFile
                     Nothing
                     ["theParent", "theChild2"]
                     (Just 0)
                     (BackupInfo True)
                     ))) in
      let tree = (BackupFile (Right
                   (BackupDirectory
                    Nothing
                    ["theParent"]
                    [child1
                    ,child2]
                    (BackupInfo True)
                   ))) in
        let result = findBackupFile
                     ["theParent", "theChild2"]
                     tree in
          case result of
            Just result' -> assertEqual
                            "the name matches"
                            "theChild2"
                            (nameOfBackupFile result')
            _ -> assertFailure "should not happen"

testGivenDirectoryHasChildWhenPathToParentMatchesThenReturnNothing::Assertion
testGivenDirectoryHasChildWhenPathToParentMatchesThenReturnNothing =
  do
    let child1 = (BackupFile (Left
                     (RegularBackupFile
                      Nothing
                      ["theParent", "theChild1"]
                      (Just 0)
                      (BackupInfo True)
                     )))
    let child2 = (BackupFile (Left
                     (RegularBackupFile
                     Nothing
                     ["theParent", "theChild2"]
                     (Just 0)
                     (BackupInfo True)
                     ))) in
      let tree = (BackupFile (Right
                   (BackupDirectory
                    Nothing
                    ["theParent"]
                    [child1
                    ,child2]
                    (BackupInfo True)
                   ))) in
        let result = findBackupFile
                     ["theParent", "theChild3"]
                     tree in
          case result of
            Just result' -> assertFailure "should not happen"
            _ -> return ()

testGivenRegularBackupFileWhenPathToCurrentNodeMatchesButNotForTheRestThenReturnNothing ::
  Assertion
testGivenRegularBackupFileWhenPathToCurrentNodeMatchesButNotForTheRestThenReturnNothing =
  do
    let root = BackupFile (Left
                (RegularBackupFile
                 Nothing
                 ["theNode"]
                 (Just 0)
                 (BackupInfo True)
                )) in 
      let result = findBackupFile
                   ["theNode", "theChild"]
                   root in
        assertBool "should be nothing" (isNothing result)

testGivenRegularBackupFileWhenOrderedToUpdateThenReturnUpdatedValue ::
  Assertion
testGivenRegularBackupFileWhenOrderedToUpdateThenReturnUpdatedValue =
  do
    let result = updateBackupFile
                 ["theName"]
                 (BackupFile (Left
                  (RegularBackupFile
                   Nothing
                   ["theName"]
                   (Just 1)
                   (BackupInfo True)
                  )))
                 (BackupFile (Left
                  (RegularBackupFile
                   Nothing
                   ["theName"]
                   (Just 0)
                   (BackupInfo True)
                  )))
    assertEqual "should be updated" 1 (sizeOfBackupFile result)

testGivenRegularBackupFileWhenOrderedToUpdateNonExistentNodeThenReturnInput ::
  Assertion
testGivenRegularBackupFileWhenOrderedToUpdateNonExistentNodeThenReturnInput =
  do
    let result = updateBackupFile
                 ["theName2"]
                 (BackupFile (Left
                  (RegularBackupFile
                   Nothing
                   ["theName1"]
                   (Just 1)
                   (BackupInfo True)
                  )))
                 (BackupFile (Left
                  (RegularBackupFile
                   Nothing
                   ["theName1"]
                   (Just 0)
                   (BackupInfo True)
                  ))) in
      assertEqual "should be the same" 0 (sizeOfBackupFile result)

testGivenBackupDirectoryWithChildrenWhenUpdateChildThenReturnUpdatedTree ::
  Assertion
testGivenBackupDirectoryWithChildrenWhenUpdateChildThenReturnUpdatedTree =
  do
    let child1 = (BackupFile (Left
                     (RegularBackupFile
                      Nothing
                      ["theParent", "theChild1"]
                      (Just 0)
                      (BackupInfo True)
                     )))
    let child2 = (BackupFile (Left
                     (RegularBackupFile
                     Nothing
                     ["theParent", "theChild2"]
                     (Just 0)
                     (BackupInfo True)
                     )))
    let tree = (BackupFile (Right
                 (BackupDirectory
                   Nothing
                   ["theParent"]
                   [child1
                   ,child2]
                   (BackupInfo True)
                 )))
    let newNode = (BackupFile (Left
                   (RegularBackupFile
                    Nothing
                    ["theParent", "theChild2"]
                    (Just 1)
                    (BackupInfo True)
                   )))
    let result = updateBackupFile
                 ["theParent", "theChild2"]
                 newNode
                 tree
    let updatedNode = findBackupFile
                      ["theParent", "theChild2"]
                      result in
      case updatedNode of
        Just backupFile -> assertEqual
                           "the size should be updated."
                           1
                           (sizeOfBackupFile backupFile)
        _ -> assertFailure "should not happen"

testGivenFileWithBackupFlagFalseWhenGetSizeThenReturnZero ::
  Assertion
testGivenFileWithBackupFlagFalseWhenGetSizeThenReturnZero =
  do
    let file = RegularBackupFile
               Nothing
               ["node"]
               (Just 1)
               (BackupInfo False)
    assertEqual "the size should be 0." 0 (sizeOfRegularBackupFile file)

testGivenDirectoryWithBackupFlagTrueWhenGetSizeThenReturnTotalSizeOfChildren ::
  Assertion
testGivenDirectoryWithBackupFlagTrueWhenGetSizeThenReturnTotalSizeOfChildren =
  do
    let directory = BackupDirectory
                    Nothing
                    ["node"]
                    [(BackupFile (Left
                      (RegularBackupFile
                       Nothing
                       ["node", "theChild1"]
                       (Just 1)
                       (BackupInfo True))))
                    ,(BackupFile (Left
                      (RegularBackupFile
                       Nothing
                       ["node", "theChild2"]
                       (Just 3)
                       (BackupInfo True))))
                    ,(BackupFile (Left
                      (RegularBackupFile
                       Nothing
                       ["node", "theChild3"]
                       (Just 5)
                       (BackupInfo False))))]
                    (BackupInfo True) in
      assertEqual "size should match" 4 (sizeOfBackupDirectory directory)
  
testGivenDirectoryWithBackupFlagFalseWhenGetSizeThenReturnZero ::
  Assertion
testGivenDirectoryWithBackupFlagFalseWhenGetSizeThenReturnZero =
  do
    let directory = BackupDirectory
                    Nothing
                    ["node"]
                    [(BackupFile (Left
                      (RegularBackupFile
                       Nothing
                       ["node", "theChild"]
                       (Just 1)
                       (BackupInfo True))))]
                    (BackupInfo False) in
      assertEqual "the size should be 0." 0 (sizeOfBackupDirectory directory)

testGivenTreeAndUpdateFunctionWhenUpdateThenReturnUpdatedTree::Assertion
testGivenTreeAndUpdateFunctionWhenUpdateThenReturnUpdatedTree =
  let tree = BackupFile
             (Right (BackupDirectory
                     Nothing
                     ["node"]
                     [(BackupFile (Left
                                   (RegularBackupFile
                                    Nothing
                                    ["node", "theChild"]
                                    (Just 1)
                                    (BackupInfo True))))]
                     (BackupInfo False)))
  in
    let result = updateFileInTree ["node", "theChild"] toggleBackupFlag tree
    in
      let size = sizeOfBackupFile (snd result)
      in
        do
          assertEqual "result should be true." True (fst result)
          assertEqual "size should be 0." 0 size
          return ()

testPathWithFlag::Assertion
testPathWithFlag =
  do
    let fileToBackup = BackupFile (Left (RegularBackupFile
                                         Nothing
                                         [".", "dir", "path"]
                                         (Just 1)
                                         (BackupInfo True)))
    let fileNotToBackup = toggleBackupFlag fileToBackup
      in
      do
        assertEqual "pathWithFlag" ([".", "dir", "path"], True)
          (pathWithFlag fileToBackup)
        assertEqual "pathWithFlagNot" ([".", "dir", "path"], False)
          (pathWithFlag fileNotToBackup)
        return ()

testWalkBackupFiles::Assertion
testWalkBackupFiles =
  let tree = BackupFile
             (Right (BackupDirectory
                     Nothing
                     ["."]
                     [BackupFile
                      (Right (BackupDirectory
                              Nothing
                              [".", "dir1"]
                              [BackupFile
                               (Left (RegularBackupFile
                                      Nothing
                                      [".", "dir1", "file1"]
                                      (Just 1)
                                      (BackupInfo True)))]
                               (BackupInfo True)))]
                      (BackupInfo True)))
      expect = [[".", "dir1", "file1"], [".", "dir1"], ["."]]
  in
    let result = walkBackupFiles
                 Skip
                 (\a -> Just (pathOfBackupFile a))
                 tree
    in
      do
        assertEqual "recursive" expect result
        return ()

testWalkBackupFilesGivenFilesNotToBackupWhenWalkThenSkipThose::Assertion
testWalkBackupFilesGivenFilesNotToBackupWhenWalkThenSkipThose =
  let tree = BackupFile
             (Right (BackupDirectory
                     Nothing
                     ["."]
                     [BackupFile
                      (Right (BackupDirectory
                              Nothing
                              [".", "dir1"]
                              [BackupFile
                               (Left (RegularBackupFile
                                      Nothing
                                      [".", "dir1", "file1"]
                                      (Just 1)
                                      (BackupInfo True)))]
                              (BackupInfo False)))]
                     (BackupInfo True)))
      expect = [["."]]
  in
    let result = walkBackupFiles
                 Skip
                 (\a -> Just (pathOfBackupFile a))
                 tree
    in
      do
        assertEqual "skip" expect result
        return ()

testGivenFilesNotToBackupWhenWalkWithoutSkipThenDoNotSkipThose::Assertion
testGivenFilesNotToBackupWhenWalkWithoutSkipThenDoNotSkipThose =   
  let tree = BackupFile
             (Right (BackupDirectory
                     Nothing
                     ["."]
                     [BackupFile
                      (Right (BackupDirectory
                              Nothing
                              [".", "dir1"]
                              [BackupFile
                               (Left (RegularBackupFile
                                      Nothing
                                      [".", "dir1", "file1"]
                                      (Just 1)
                                      (BackupInfo True)))]
                              (BackupInfo False)))]
                      (BackupInfo True)))
      expect = reverse [["."], [".", "dir1"], [".", "dir1", "file1"]]
  in
    let result = walkBackupFiles
                 DoNotSkip
                 (\a -> Just (pathOfBackupFile a))
                 tree
    in
      do
        assertEqual "do not skip" expect result
        return ()
        
testGivenFilesNotToBackupWhenWalkSkippingChildrenThenSkipThose :: Assertion
testGivenFilesNotToBackupWhenWalkSkippingChildrenThenSkipThose = 
  let tree = BackupFile
             (Right (BackupDirectory
                     Nothing
                     ["."]
                     [BackupFile
                      (Right (BackupDirectory
                              Nothing
                              [".", "dir1"]
                              [BackupFile
                               (Left (RegularBackupFile
                                      Nothing
                                      [".", "dir1", "file1"]
                                      (Just 1)
                                      (BackupInfo True)))]
                              (BackupInfo False)))]
                     (BackupInfo True)))
      expect = reverse [["."], [".", "dir1"]]
  in
    let result = walkBackupFiles
                 SkipChildren
                 (\a -> Just (pathOfBackupFile a))
                 tree
    in
      do
        assertEqual "skip children" expect result
        return ()
        
main :: IO ()
main = defaultMainWithOpts
       [ testCase
         "GivenRegularBackupFileWhenPathAndNameMatchThenReturnIt"
         testGivenRegularBackupFileWhenPathAndNameMatchThenReturnIt
       , testCase
         "GivenRegularBackupFileWhenPathAndNameDoNotMatchThenReturnNothing"
         testGivenRegularBackupFileWhenPathAndNameDoNotMatchThenReturnNothing
       , testCase
         "testGivenBackupDirectoryOnLeafWhenPathMatchThenReturnIt"
         testGivenBackupDirectoryOnLeafWhenPathMatchThenReturnIt
       , testCase
         "testGivenBackupDirectoryHasChildWhenThePathMatchesThenReturnTheChild"
         testGivenBackupDirectoryHasChildWhenThePathMatchesThenReturnTheChild
       , testCase
         "testGivenDirectoryHasChildWhenPathToParentMatchesThenReturnNothing"
         testGivenDirectoryHasChildWhenPathToParentMatchesThenReturnNothing
       , testCase
         "testGivenRegularBackupFileWhenPathToCurrentNodeMatchesButNotForTheRestThenReturnNothing"
         testGivenRegularBackupFileWhenPathToCurrentNodeMatchesButNotForTheRestThenReturnNothing
       , testCase
         "testGivenRegularBackupFileWhenOrderedToUpdateThenReturnUpdatedValue"
         testGivenRegularBackupFileWhenOrderedToUpdateThenReturnUpdatedValue
       , testCase
         "testGivenRegularBackupFileWhenOrderedToUpdateNonExistentNodeThenReturnInput"
         testGivenRegularBackupFileWhenOrderedToUpdateNonExistentNodeThenReturnInput
       , testCase
         "testGivenBackupDirectoryWithChildrenWhenUpdateChildThenReturnUpdatedTree"
         testGivenBackupDirectoryWithChildrenWhenUpdateChildThenReturnUpdatedTree
       , testCase
         "testGivenFileWithBackupFlagFalseWhenGetSizeThenReturnZero"
         testGivenFileWithBackupFlagFalseWhenGetSizeThenReturnZero
       , testCase
         "testGivenDirectoryWithBackupFlagTrueWhenGetSizeThenReturnTotalSizeOfChildren"
         testGivenDirectoryWithBackupFlagTrueWhenGetSizeThenReturnTotalSizeOfChildren
       , testCase
         "testGivenDirectoryWithBackupFlagFalseWhenGetSizeThenReturnZero"
         testGivenDirectoryWithBackupFlagFalseWhenGetSizeThenReturnZero
       , testCase
         "testGivenTreeAndUpdateFunctionWhenUpdateThenReturnUpdatedTree"
         testGivenTreeAndUpdateFunctionWhenUpdateThenReturnUpdatedTree
       , testCase
         "testPathWithFlag"
         testPathWithFlag
       , testCase
         "testWalkBackupFiles"
         testWalkBackupFiles
       , testCase
         "testWalkBackupFilesGivenFilesNotToBackupWhenWalkThenSkipThose"
         testWalkBackupFilesGivenFilesNotToBackupWhenWalkThenSkipThose
       , testCase
         "testGivenFilesNotToBackupWhenWalkWithoutSkipThenDoNotSkipThose"
         testGivenFilesNotToBackupWhenWalkWithoutSkipThenDoNotSkipThose
       , testCase
         "testGivenFilesNotToBackupWhenWalkSkippingChildrenThenSkipThose"
         testGivenFilesNotToBackupWhenWalkSkippingChildrenThenSkipThose
       ]
       mempty
