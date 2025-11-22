-- pex6.hs 
-- unKnot Haskell

-- name: Jack Barnett

{- DOCUMENTATION:
-}

unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "unknot"
   | typeIknotExists tripCode = unKnot (drop 2 tripCode)
   | typeIIknotExists tripCode = unKnot (drop 4 tripCode)
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

typeIknotExists :: [(Char, Char)] -> Bool
typeIknotExists tripCode =
   if ( fst( head tripCode ) == fst( head (tail tripCode)) )
      then
         if ( snd( head tripCode ) /= snd(head (tail tripCode)) )
            then True
            else False
   else False

typeIIknotExists :: [(Char, Char)] -> Bool
typeIIknotExists tripCode =
   --if tripCode null 
      --then False
   if ( snd( head tripCode ) == snd( head (tail tripCode)) ) --if we find an adjacent pair in the same direction
      then 
         if ( fst(head tripCode) == fst(head ( tail (tail tripCode) ) )   ) && ( fst( head (tail tripCode)) == fst( head (tail ( tail (tail tripCode) ) ) )  ) --then check if the next one has matching inverted direction and the same spot
            then True
            else False
   else False


 {-
 typeIknotExists psuedo code... (boolean)
   if head( head tripcode ) == head(head (tail tripcode)) -- if the first 2 adjacent points in the list cross the same point
   and tail( head tripcode ) != tail(head (tail tripcode)) --AND they are not equal crossings
   return true -- a type one knot exists
 -}

--typeIIknotExists tripCode = 
 {-
 typeIIknotExists psuedo code... (boolean)
   if any adjacent pairs have the same direction and then invert direction and reapeat spots in the code, then a type II knot exists.
 -}



main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

   let t02 = [('a','u'),('a','o'),('b','u'),('b','o')]
   print("   test case t02 - tripcode: " )
   print(t02)
   print("   result:" ++ unKnot t02)

   let t03 = [('a','u'),('b','o')]
   print("   test case t03 - tripcode: " )
   print(t03)
   print("   result:" ++ unKnot t03)

   let t04 = [('a','u'),('b','u'),('a','o'), ('b','o')]
   print("   test case t04 - tripcode: " )
   print(t04)
   print("   result:" ++ unKnot t04)

