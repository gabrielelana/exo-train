module Main where

import Data.List (intersperse)

type Train = [Cart]

data Cart = Head
          | Tail
          | Passenger
          | Restaurant
          | Cargo CargoStatus
          deriving (Eq, Show)

data CargoStatus = Empty
                 | Full
                 deriving (Eq, Show)

parse :: String -> Train
parse s = reverse $ go s []
  where go ('H':r) [] = go r [Head]
        go _ [] = error "must start with head"
        go ('P':r) t = go r (Passenger:t)
        go ('R':r) t = go r (Restaurant:t)
        go ('C':r) t = go r (Cargo Empty:t)
        go ['H'] t = Tail:t
        go [] t = t
        go _ _ = error "wrong input"

cartToASCII :: Cart -> String
cartToASCII Head = "<HHHH"
cartToASCII Tail = "HHHH>"
cartToASCII Passenger = "|OOOO|"
cartToASCII Restaurant = "|hThT|"
cartToASCII (Cargo Empty) = "|____|"
cartToASCII (Cargo Full) = "|^^^^|"

trainToASCII :: Train -> String
trainToASCII = mconcat . intersperse "::" . map cartToASCII

detachEnd :: Train -> Train
detachEnd = init

detachHead :: Train -> Train
detachHead = tail

fill :: Train -> Train
fill [] = error "no cargo cart left to fill"
fill (Cargo Empty:t) = Cargo Full : t
fill (c:t) = c : fill t


main :: IO ()
main = do
  print $ (trainToASCII . parse) "HPP"
  print $ (trainToASCII . parse) "HPRP"
  print $ (trainToASCII . parse) "HPRPH"
  print $ (trainToASCII . detachEnd . parse) "HPRPH"
  print $ (trainToASCII . detachHead . detachEnd . parse) "HPRPH"
  print $ (trainToASCII . parse) "HCCC"
  print $ (trainToASCII . fill . parse) "HCCC"
  print $ (trainToASCII . fill . fill . parse) "HCCC"
  print $ (trainToASCII . fill . fill . fill . parse) "HCCC"
