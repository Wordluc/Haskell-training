areDifference::Char ->Char->Int
areDifference s1 s2
        |s1==s2 = 0
        |otherwise = 1

hamming::Int->String->String->Int
hamming c [] []=c
hamming c (x:xs) (y:ys)
        |length xs/=length ys = -1
        |otherwise = let cnew= areDifference x y
                     in hamming (cnew+c) xs ys
loop=do
    print "prima linea"
    linea1<-getLine
    print "seconda linea"
    linea2<-getLine
    print (hamming 0 linea1 linea2)

main=loop
