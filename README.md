# Haskell and Fortran
Haskell and Fortran programs to solve the same problems

**Compute the primes below 50, using the Sieve of Eratosthenes and excluding even numbers above 2 and considering only factors up to sqrt(n).**

**Haskell**

```Haskell
primesBelowN :: Int -> [Int]
primesBelowN n = 2 : [i | i <- [3,5..n], isPrime i]

isPrime :: Int -> Bool
isPrime n = not $ hasFactor 2 (floor.sqrt.fromIntegral $ n) n

hasFactor :: Int -> Int -> Int -> Bool
hasFactor f t n
  | f > t = False
  | n `mod` f == 0 = True
  | otherwise = hasFactor (f+1) t n

main :: IO ()
main = print $ primesBelowN 50
```

output: 
`[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]`
