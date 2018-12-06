
myLast x:[] = x
myLast x:xs = myLast xs

myButLast x1:[x2] = x1
myButLast x:xs = myLast xs

elementAt xs 0 = head xs
elementAt (x:xs) n = elementAt xs (n-1)

isPalindrome xs = xs == (reverse xs)

data NestedList a = Elem a | List [NestedList a]

flatten (Elem num)    = [(Elem num)]
flatten (List (x:xs)) = (flatten x) ++ (flatten xs)
flatten (List [])     = []