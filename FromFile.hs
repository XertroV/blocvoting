import Data.Hex (unhex)
import Data.List.Split (splitOn)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M

import BlocVoting.Filter
import BlocVoting.Tally
import BlocVoting.Nulldata


unhexNulldata :: String -> String
unhexNulldata s = case unhex s of Just unhexedScript -> unhexedScript
                                  _ -> "un-unhexable"

toNulldata :: [String] -> Nulldata
toNulldata (script:address:_) = Nulldata (C.pack script) (C.pack address)
toNulldata (script:_) = Nulldata (C.pack script) $ C.pack ""
toNulldata _ = error "toNulldata invalid input"


trimFromNulldata :: Int -> Nulldata -> Nulldata
trimFromNulldata n nd = modNulldataScript (BS.drop n $ ndScript nd) nd


main :: IO ()
main = do
  hexlifiedLines <- getContents
  -- trim 2 from nulldata for OP_RETURN byte, length byte
  let ndOperations = trimFromNulldata 2 . toNulldata . (\(x:xs) -> unhexNulldata x:xs) . splitOn "|"
  let ndList = filterNVB $ map ndOperations $ lines hexlifiedLines
  -- print ndList
  -- print $ listOfInstructionsToGrandTally ndList
  let gt = listOfInstructionsToGrandTally ndList
  print gt
  mapM_ print $ map (\(Tally tres _) -> tres) . M.elems . gtTallies $ gt
