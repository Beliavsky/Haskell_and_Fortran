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
