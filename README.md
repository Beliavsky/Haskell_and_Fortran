# Haskell and Fortran
Haskell and Fortran programs to solve the same problems The Glasgow Haskell Compiler and gfortran are used.

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

**Fortran**

```Fortran
module m
implicit none
contains
elemental logical function is_prime(n)
  integer, intent(in) :: n
  integer :: i
  is_prime = .false.
  do i = 2, int(sqrt(real(n)))
     if (mod(n, i) == 0) return
  end do
  is_prime = .true.
end function is_prime

function primes_below(n) result(pvec)
  integer, intent(in) :: n
  integer, allocatable :: pvec(:)
  integer :: i
  pvec = [2, (i, i=3,n,2)]
  pvec = pack(pvec, is_prime(pvec))
end function primes_below
end module m

program main
  use m
  implicit none
  print "(*(i0,:,','))",primes_below(50)
end
```

output: `2,3,5,7,11,13,17,19,23,29,31,37,41,43,47`

----

**Print a table of powers for integers from 1 to 10.**

**Haskell**

```Haskell
import Text.Printf

n = 10
powers = [1/2, 1/3, 2/3]

main :: IO ()
main = do
    printLabels powers
    mapM_ printPowers ([1..n] :: [Integer])
  where
    printLabels powers = do
      putStrLn $ printf "%12s" "i" ++ concat (map (printf "%12s" . (("pow(" ++) . (++ ")") . printf "%.3f")) powers)
    printPowers i = do
      putStrLn $ printf "%12d" i ++ concat (map (printf "%12.4f" . ((fromIntegral i :: Double) **)) powers)
```

output:
```
           i  pow(0.500)  pow(0.333)  pow(0.667)
           1      1.0000      1.0000      1.0000
           2      1.4142      1.2599      1.5874
           3      1.7321      1.4422      2.0801
           4      2.0000      1.5874      2.5198
           5      2.2361      1.7100      2.9240
           6      2.4495      1.8171      3.3019
           7      2.6458      1.9129      3.6593
           8      2.8284      2.0000      4.0000
           9      3.0000      2.0801      4.3267
          10      3.1623      2.1544      4.6416
```

**Fortran**

```Fortran
program main
implicit none
integer, parameter :: n = 10
double precision, parameter :: powers(*) = [1/2.0d0, 1/3.0d0, 2/3.0d0]
integer :: i
print "(a12,*(:,3x,'pow(',f5.3,')'))", "i", powers
do i=1,10
   print "(i12,*(f12.4))", i, i**powers
end do
end
```

output:
```
           i   pow(0.500)   pow(0.333)   pow(0.667)
           1      1.0000      1.0000      1.0000
           2      1.4142      1.2599      1.5874
           3      1.7321      1.4422      2.0801
           4      2.0000      1.5874      2.5198
           5      2.2361      1.7100      2.9240
           6      2.4495      1.8171      3.3019
           7      2.6458      1.9129      3.6593
           8      2.8284      2.0000      4.0000
           9      3.0000      2.0801      4.3267
          10      3.1623      2.1544      4.6416
```
