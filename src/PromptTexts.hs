{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PromptTexts where

import Relude
import Text.RawString.QQ (r)

makeUnitTestPrompt :: Text -> Text -> Text -> Text -> Text
makeUnitTestPrompt fileName testFileName testName testSummary = "Please create a unit test for " <> fileName <> " in " <> testFileName <> " called " <> testName <> " that: " <> testSummary <> " (unless it already exists and looks correct). Tests will automatically run each time you modify a unit test file or source file. IF THE TEST FAILS BECAUSE OF A BUG IN THE SOURCE FILE BEING TESTED, YOU ARE RESPONSIBLE FOR FIXING THE SOURCE FILE; feel free to add more logging to the source files to assist in debugging test failures. For testing external APIs you may write tests that connect and wait e.g. 10 seconds to receive a response (and timeout if none is received), asserting that the response parses correctly and the result looks sane. Return true when you're satisfied with the test. Note that if any other tests (even in other files) are broken, you should fix those too before returning  (because you might have accidentally broken one somehow), opening whatever files are necessary. Remember to also open any helpful .txt/.md documentation from the list of available files. Remember the package is 'main', and local imports are project_name/file_name."

makeUnitTestsPrompt :: Text -> Text
makeUnitTestsPrompt fileName = makeUnitTestsPromptInner $ "Please decide upon some key unit tests to write for " <> fileName <> "."

makeUnitTestsForSpecificChangePrompt :: Text -> Text -> Text
makeUnitTestsForSpecificChangePrompt change fileName = makeUnitTestsPromptInner $ "Please decide upon some key unit tests to write for the change(s) you just made: " <> change <> " in fileName " <> fileName <> "."

makeUnitTestsPromptInner :: Text -> Text
makeUnitTestsPromptInner starter = starter <> "Return a list of unit test names and descriptions in the format specified. If there are parts that will be hard to unit test fully (e.g. connecting to an external API), then you should design tests that just connect to the API and verify the downloaded/streamed data parses correctly and passes sanity assertions (with a timeout of e.g. 10 seconds that makes the test fail if no data is received). Remember to also open any helpful .txt/.md documentation from the list of available files."

makeSourcefilePrompt :: Text -> Text -> Text
makeSourcefilePrompt fileName fileSummary = "Please create the file " <> fileName <> " with contents: " <> fileSummary <> ". Try to write it in a testable manner, so unit tests can be written. Prefer a 'functional core, imperative shell' approach, where IO is done at an outer level and passed into pure functions (pure in the sense they do no IO, but they may mutate inputs in a deterministic way, as this is not Haskell). Remember to OpenFile other relevant files (which adds them to your context) when necessary. You can also EditFile existing project files where necessary. After creating the file with AppendFile (and editing with InsertInFile/EditFile as necessary until it compiles), which you'll see from the file being present and complete in OpenFiles, return the names and descriptions of the files created, in the format specified. Remember to also open any helpful .txt/.md documentation from the list of available files that may be useful as reference for the current task. Remember the package is 'main', and local imports are project_name/file_name. YOU MUST NOT RETURN UNTIL THE FULL FILE/IMPLEMENTATION IS COMPLETE, without any placeholders remaining!"

makeFilenamesPrompt :: Text
makeFilenamesPrompt =
  [r|
Please first plan a list of source files that the project will consist of, based on the intended architecture, along with outlines of their contents, and what other files they'll include as dependencies (only files to be created by the project, not including external dependencies, i.e. no need to mention boost etc.). The list of dependencies will be used in topologically sorting the files that need to be created such that each file is created before the files that depend on it (so no circular dependencies are allowed). Each file will have a unit test, so bear this in mind when arranging files; ideally each file should be like an independent module that depends on minimal files and can be tested in isolation. This is a single-step task so you don't need to write to the journal.txt (and cannot, because you don't have append/replace/edit tool permissions for this task). Don't mention unit tests files here; they'll be handled separately later. Please do however do include as a dependency any blahDocSummary.txt someDoc.md you see in availableFiles that might be useful to a particular file.

NOTE: Please put all files in the root directory of the project, not subdirectories, for simplicity. And all in package main.
|]

makeRefactorBackgroundPrompt :: Text -> Text
makeRefactorBackgroundPrompt taskDescription =
  [r|
Your current project is refactoring. First you'll, for each source file, analyse it for any changes needed. Then you'll combine these lists of changes for all files, and go through implementing them file by file, adding unit tests for each change. You'll also create any other files you deem necessary.
|]
    <> taskDescription

makeArchitectureDesignPrompt :: Text
makeArchitectureDesignPrompt = "Please think carefully and design the architecture for the project given the requirements above, returning a detailed description of the planned architecture. Do not write the exact actual file names yet though, that's for a later planning step. The project must have everything in a single directory; no nested paths."

approachSummary :: Text
approachSummary =
  [r|
It's designed to be an LLM-developed and managed application, so the design should involve many small files, allowing the LLM to easily read and update them without needing to keep large files in context. The amount of dependencies among files should be minimised, so that each file can be understood with a minimum number of other files in context (but at the same time, putting all dependencies in a single source file isn't ideal if that file is really big the aim is to minimise context needed, not just number of files).

IMPORTANT NOTE: To keep context size minimal you won't see your full previous responses/history, so I recommend keeping a journal in "journal.txt", which you can append a short summary of what you're trying to do at each step, and what you plan to do next, in case you expect to have future steps in that task and don't want to lose your train of thought between responses.

|]

-- When you make changes to a source file, if compilation and unit tests pass then the file is added and committed to a git repo in the base directory. You may use the Revert tool where necessary to get back to the last version of a file when compilation and all tests passed.
