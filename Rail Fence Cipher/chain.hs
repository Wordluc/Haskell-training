data Chain = Chain String String String
instance Show Chain where
  show (Chain a b c)=a++b++c

defaultChain = Chain "" "" ""

getChar' (x:xs)
   |x== ' ' = "."
   |otherwise = [x]

start (s::string) =
   up defaultChain s

up (Chain a b c) (xs::String)
       |null xs = Chain a b c
       |otherwise = middleToDown (Chain (a++(getChar' xs)) (b++".") (c++".")) (tail xs)

middleToDown (Chain a b c) (xs::String)
       |null xs = Chain a b c
       |otherwise = down (Chain (a++".") (b++(getChar' xs)) (c++".")) (tail xs)

down (Chain a b c) (xs::String)
       |null xs = Chain a b c
       |otherwise = middleToUp (Chain (a++".") (b++".") (c++(getChar' xs))) (tail xs)

middleToUp (Chain a b c) (xs::String)
       |null xs = Chain a b c
       |otherwise = up (Chain (a++".") (b++(getChar' xs)) (c++".")) (tail xs)

loop = do
  line<-getLine
  print (start line)
  loop

main = do
    loop
