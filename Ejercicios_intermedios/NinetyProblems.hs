
myLast x:[] = x
myLast x:xs = myLast xs

myButLast x1:[x2] = x1
myButLast x:xs = myLast xs

elementAt xs 0 = head xs
elementAt (x:xs) n = elementAt xs (n-1)

isPalindrome xs = xs == (reverse xs)