import System.IO

main :: IO ()
main = do
  myFile <- openFile "cheat-sheet.txt" ReadMode
  firstLine <- hGetLine myFile
  putStrLn firstLine
  secondLine <- hGetLine myFile
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodbyeFile secondLine
  hClose myFile
  hClose goodbyeFile
  putStrLn "done!"

-- main :: IO ()
-- main = do
--   myFile <- openFile "cheat-sheet.txt" ReadMode
--   hasLine <- hIsEOF myFile
--   firstLine <- if not hasLine
--               then hGetLine myFile
--               else return "empty"
--   putStrLn "done!"

--   hasSecondLine <- hIsEOF myFile
--   secondLine <- if not hasSecondLine
--                 then hGetLine myFile
--                 else return ""
--   putStrLn "finished!"
