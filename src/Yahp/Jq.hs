{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Yahp.Jq (runJq) where

import qualified Data.ByteString as BS
import           Prelude hiding (filter, putStrLn)
import           System.IO.Temp
import           System.Process.ByteString
import           Yahp hiding (filter)


runJq :: ConvertText a String => FilePath -> a -> [String] -> BS.ByteString -> IO (Either Text BS.ByteString)
runJq bin (toS -> filter) args json = readProcessWithExitCode bin (args <> [filter]) json
  >>= \(code, out, err) -> case code of
  ExitSuccess   -> pure2 out
  ExitFailure r -> do
    jsonPath <- emptySystemTempFile "jq_input.json" 
    BS.writeFile jsonPath json
    pure $ Left $ toS (Prelude.unlines ["jq returned ExitCode " <> show r
                                        ,"Binary: " <> bin
                                        ,"Args: " <> show args
                                        ,"Input: " <> jsonPath
                                        ,"Filter: " <> filter
                                        ])
                           <> "Stdout: " <> fromUtf8Lenient out
                           <> "\nStderr: " <> fromUtf8Lenient err

exampleJson :: ByteString
exampleJson = "[{\"name\": \"John Brooks\",\"id\": \"003\"},{\"name\": \"Randy Park\",\"id\": \"053\"},{\"name\": \"Todd Gray\",\"id\": \"009\"}]"

devmain :: IO ()
devmain = do
  either putStrLn putStrLn =<< runJq @Text "jq" ".[].name" [] exampleJson
  either putStrLn putStrLn =<< runJq @Text "jq" ".[asd].name" [] exampleJson


