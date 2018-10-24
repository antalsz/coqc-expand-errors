{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module CoqcExpandErrors.CLI (
  -- * Pretty expanded Coq errors
  annotateCoqcErrors, visualize, colorOutput,
  -- * Run @coqc@
  withCoqcErrs, withCoqcErrs_,
  -- * Display process command lines
  showProcessCommand,
  -- * Help message
  help,
  -- * Main
  main
) where

import Prelude hiding (lines)

import Control.Lens

import Control.Monad.State

import Data.Text (Text)
import qualified Data.Text    as T
import Text.Printf

import Text.Megaparsec
import CoqcExpandErrors.Parse.CoqcErrors

import CoqcExpandErrors.Util.Pipes
import CoqcExpandErrors.Pipes.Report

import GHC.IO.Exception

import System.IO
import System.Process
import System.IO.Error
import System.Environment
import System.Exit

import CoqcExpandErrors.Types
import CoqcExpandErrors.CoqFile

--------------------------------------------------------------------------------

-- |Produce output with line numbers where tagged lines have bold green tagged
-- line markers and the relevant text has a yellow yellow background.
colorOutput :: Marker
colorOutput = Marker { markerOnLine         = line "\ESC[32;1m" "\ESC[39;22m" '>'
                     , markerOtherLine      = line ""           ""            ' '
                     , markerPreChars       = "\ESC[43m"
                     , markerPostChars      = "\ESC[49m"
                     , markerPrePostPerLine = True }
  where
    line hstart hend c def l t = hstart <> lineTag c def l <> hend <> " " <> t
    lineTag c def (Line i) = T.pack $ printf "%c %*d:" c (maxLength def) i
    
    maxLength :: Range Line -> Int
    maxLength def = ceiling (logBase (10 :: Double) . fromIntegral . (+ 1) . unLine $ def^.end)

-- |Produce the highlighted definition text, and then an extra bold line giving
-- the definition where it was found with its name in red.
visualize :: (Maybe Text, [Text]) -> [Text]
visualize (mname, lines) = lines ++ ["", inDefinition, ""] where
  inDefinition = bold $ "Error found in " <> case mname of
                          Just name -> "the definition \"" <> red name <> "\""
                          Nothing   -> red "an unknown definition"
  bold s = "\ESC[1m"  <> s <> "\ESC[22m"
  red  s = "\ESC[31m" <> s <> "\ESC[39m"

-- |Interpose between a handle generating @coqc@ output and @stderr@ or a
-- similar handle, expanding all errors as per 'colorOutput' and 'visualize'.
annotateCoqcErrors :: MonadIO m => Handle -> Handle -> m ()
annotateCoqcErrors source sink =
  flip evalStateT mempty . runEffect
    $ reportingBeforeNext (parseMaybe locationP)
                          (fmap visualize . locate colorOutput)
                          (fromHandleLn source)
                          (toHandleLn sink)

--------------------------------------------------------------------------------

-- |Display the command that was run.
showProcessCommand :: CreateProcess -> String
showProcessCommand = cmdspec <&> \case
  ShellCommand shell    -> shell
  RawCommand   cmd args -> showCommandForUser cmd args

-- |Given a @coqc@ executable and some arguments, run @coqc@ with colorful
-- output and pass a handle to its standard error stream to the given function.
-- Run that command and wait for @oqc@ to terminate, producing both the exit
-- code and the function result.
withCoqcErrs :: FilePath -> [String] -> (Handle -> IO a) -> IO (ExitCode, a)
withCoqcErrs coqc args action =
  let coqcProcess = (proc coqc $ ["-color", "on"] ++ args)
                      { std_err       = CreatePipe
                      , close_fds     = True
                      , delegate_ctlc = True }
  in withCreateProcess coqcProcess $ \_inMH _outMH errMH procH ->
       case errMH of
         Just errH -> do
           result <- action errH
           ec     <- waitForProcess procH
           pure (ec, result)
         Nothing ->
           ioError $ mkIOError
             doesNotExistErrorType
             ("stderr of `coqc` process `" ++ showProcessCommand coqcProcess ++ "`")
             Nothing
             Nothing

-- |Like @withCoqcErrs@, but returns only the exit code of @coqc@.
withCoqcErrs_ :: FilePath -> [String] -> (Handle -> IO ()) -> IO ExitCode
withCoqcErrs_ coqc args action = fst <$> withCoqcErrs coqc args action

-- |Display the help message for the executable.
help :: IO ()
help = do
  name <- getProgName
  putStrLn $ "Usage: " ++ name ++ " COQC [ARGS...]"
  putStrLn ""
  putStrLn "Runs `coqc` and prints out colorized Coq error location information"
  putStrLn ""
  putStrLn "COQC should be a `coqc` executable.  You'll usually want to run this as"
  putStrLn ""
  putStrLn $ "    " ++ name ++ " coqc file.v ..."
  putStrLn ""
  putStrLn "which will function as a drop-in replacement for your system `coqc`."

-- |Wrap @coqc@ with fancy errors.
main :: IO ()
main = getArgs >>= \case
  []        -> help
  coqc:args -> exitWith =<< withCoqcErrs_ coqc args (annotateCoqcErrors ?? stderr)
