module Repl where
import Color
deltron3030 =
  L.unzip3
replicate n xs = List.foldl (\a _ -> xs ++ a) [] [1..n]
tsol = ()
