# Haskell and Fortran
Here are Haskell and Fortran programs to solve the same problems, which I am writing to learn Haskell. The Glasgow Haskell Compiler and gfortran are used.

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
---
**Create functions to compute the mean and standard deviation of an array and compute the mean, standard deviation, minimum, and maximum of [10.0, 20.0, 30.0, 40.0]**

**Haskell**

```Haskell
stats :: [Double] -> (Double, Double, Double, Double)
stats xs = (mean xs, stddev xs, minimum xs, maximum xs)

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

stddev :: [Double] -> Double
stddev xs = sqrt $ sum [(x - m) ** 2 | x <- xs] / fromIntegral (length xs - 1)
  where m = mean xs

main :: IO ()
main = print $ stats [10.0, 20.0, 30.0, 40.0]
```

output: `(25.0,12.909944487358056,10.0,40.0)`

**Fortran**

```Fortran
module stats_mod
  implicit none
  integer, parameter :: dp = kind(1.0d0)
contains
  function stats(x)
    real(kind=dp), intent(in) :: x(:)
    real(kind=dp) :: stats(4)
    stats = [mean(x), stddev(x), minval(x), maxval(x)]
  end function stats
  
  real(kind=dp) function mean(x)
    real(kind=dp), intent(in) :: x(:)
    mean = sum(x)/size(x)
  end function mean

  real(kind=dp) function stddev(x)
    real(kind=dp), intent(in) :: x(:)
    stddev = sqrt(sum((x - mean(x))**2) / (size(x) - 1))
  end function stddev
end module stats_mod

program main
  use stats_mod
  implicit none
  print*,stats([10.0_dp, 20.0_dp, 30.0_dp, 40.0_dp])
end
```

output: `25.000000000000000        12.909944487358056        10.000000000000000        40.000000000000000     `

---

**Define functions to compute the mean and standard deviation in one source file and call those functions from a main program.**

**Haskell**

file `stats.hs`
```Haskell
module Stats (mean, stddev) where

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

stddev :: [Double] -> Double
stddev xs = sqrt $ sum [(x - m) ** 2 | x <- xs] / fromIntegral (length xs - 1)
  where m = mean xs
```

file `xstats.hs`
```Haskell
import Stats (mean, stddev)

main :: IO ()
main = do
    let xs = [10.0, 20.0, 30.0, 40.0]
    putStrLn $ "Mean: " ++ show (mean xs)
    putStrLn $ "Standard Deviation: " ++ show (stddev xs)
```

Compile with `ghc xstats.hs`.

output:
```
Mean: 25.0
Standard Deviation: 12.909944487358056
```

**Fortran**

Module `stats_mod` was defined above and is stored in `stats.f90`. The main program is

file `main.f90`
```Fortran
  use stats_mod
  implicit none
  real(kind=dp), parameter :: x(*) = [10.0_dp, 20.0_dp, 30.0_dp, 40.0_dp]
  print*,"Mean:", mean(x)
  print*,"Standard Deviation:", stddev(x)
end program
```

Compile with `gfortran stats.f90 main.f90`<br>
output:
```
 Mean:   25.000000000000000     
 Standard Deviation:   12.909944487358056
```

---

**Matrix operations**

**Haskell**
```Haskell
{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding ((<>))
import Numeric.LinearAlgebra

main :: IO ()
main = do
    -- Create a 3x3 matrix
    let a = (3><3) [1..9] :: Matrix R
    putStrLn "Matrix a:"
    print a

    putStrLn "\n Matrix a squared:"
    print $ a <> a

    putStrLn "\n Transpose:"
    print $ tr a

    putStrLn "\n Maximum:"
    print $ maxElement a

    putStrLn "\n Sum:"
    print $ sumElements a

    putStrLn "\n Sum of each column:"
    print $ fromList [sumElements v | v <- toColumns a]

 -- rows and columns are numbered from 0
    putStrLn "\n element in 2nd row and 3rd column:"
    print $ a `atIndex` (1,2)

    putStrLn "\n 2nd row:"
    print (a ? [1])

    putStrLn "\n 2nd column:"
    print (tr a ? [1])

    putStrLn "\n twice a:"
    print $ cmap (*2) a
```
output:
```
Matrix a:
(3><3)
 [ 1.0, 2.0, 3.0
 , 4.0, 5.0, 6.0
 , 7.0, 8.0, 9.0 ]

 Matrix a squared:
(3><3)
 [  30.0,  36.0,  42.0
 ,  66.0,  81.0,  96.0
 , 102.0, 126.0, 150.0 ]

 Transpose of matrix a:
(3><3)
 [ 1.0, 4.0, 7.0
 , 2.0, 5.0, 8.0
 , 3.0, 6.0, 9.0 ]

 Maximum of matrix a:
9.0

 Sum of elements of matrix a:
45.0

 Sum of each column in matrix a:
[12.0,15.0,18.0]

 element in 2nd row and 3rd column:
6.0

 2nd row:
(1><3)
 [ 4.0, 5.0, 6.0 ]

 2nd column:
(1><3)
 [ 2.0, 5.0, 8.0 ]

 twice a:
(3><3)
 [  2.0,  4.0,  6.0
 ,  8.0, 10.0, 12.0
 , 14.0, 16.0, 18.0 ]
```

**Fortran**
```Fortran
module m
  implicit none
contains
  subroutine print_mat(x)
    real, intent(in) :: x(:,:)
    integer          :: i
    do i=1,size(x,1)
       print "(*(f8.1))", x(i,:)
    end do
  end subroutine print_mat
end module m

program main
  use m
  implicit none
  integer, parameter :: n = 3
  real :: a(n,n)
  integer :: i
  character (len=*), parameter :: fmt_c = "(/,a)", fmt_r = "(*(f8.1))"
  a = transpose(reshape([(i, i=1,n**2)], [n,n]))
  print fmt_c, "Matrix a:"
  call print_mat(a)

  print fmt_c, "Matrix a squared:"
  call print_mat(matmul(a,a))

  print fmt_c, "Transpose:"
  call print_mat(transpose(a))

  print fmt_c, "Maximum:"
  print fmt_r, maxval(a)

  print fmt_c, "Sum:"
  print fmt_r, sum(a)

  print fmt_c, "Sum of each column:"
  print fmt_r, sum(a, dim=1)

  print fmt_c, "element in 2nd row and 3rd column"
  print fmt_r, a(2,3)

  print fmt_c, "2nd row"
  print fmt_r, a(2,:)

  print fmt_c, "2nd column"
  print fmt_r, a(:,2)

  print fmt_c, "twice a"
  call print_mat(2*a)
end
```

output:
```
Matrix a:
     1.0     2.0     3.0
     4.0     5.0     6.0
     7.0     8.0     9.0

Matrix a squared:
    30.0    36.0    42.0
    66.0    81.0    96.0
   102.0   126.0   150.0

Transpose:
     1.0     4.0     7.0
     2.0     5.0     8.0
     3.0     6.0     9.0

Maximum:
     9.0

Sum:
    45.0

Sum of each column:
    12.0    15.0    18.0

element in 2nd row and 3rd column
     6.0

2nd row
     4.0     5.0     6.0

2nd column
     2.0     5.0     8.0

twice a
     2.0     4.0     6.0
     8.0    10.0    12.0
    14.0    16.0    18.0
```

---
**Define a data type date(year, month), and write functions to print it and create a sequence of dates**

**Haskell**

file `dateutils.hs`

```Haskell
module DateUtils (Date(..), strDate, seqDate) where

import Text.Printf (printf)

data Date = Date{year :: Int, month :: Int} deriving (Show, Eq)

-- Convert a Date to a string in the format "yyyy-mm"
strDate :: Date -> String
strDate (Date y m) = printf "%04d-%02d" y m

-- Generate a sequence of dates, starting from a given date and for a given number of months
seqDate :: Date -> Int -> [Date]
seqDate startDate numDates = take numDates $ iterate nextMonth startDate
  where
    nextMonth (Date y m) 
        | m < 12 = Date y (m + 1)
        | otherwise = Date (y + 1) 1
```

main program
```Haskell
import DateUtils (Date(..), strDate, seqDate)

main :: IO ()
main = do
    mapM_ (putStrLn . strDate) $ seqDate Date{year = 2023, month = 11} 4
```

output:
```
2023-11
2023-12
2024-01
2024-02
```

**Fortran**

```Fortran
module DateUtils
  implicit none
  type :: Date
     integer :: year, month
  end type Date
contains
  elemental character (len=7) function strDate(xDate)
    ! Convert a Date to a string in the format "yyyy-mm"
    type(Date), intent(in) :: xDate
    write (strDate,"(i4.4,'-',i2.2)") xDate
  end function strDate

  function seqDate(xDate, numDates) result(Dates)
    ! Generate a sequence of dates, starting from a given date and for a given number of months
    type(Date), intent(in) :: xDate
    integer, intent(in) :: numDates
    type(Date) :: Dates(numDates)
    integer :: i
    if (numDates < 1) return
    Dates(1) = xDate
    do i=2,numDates
       if (Dates(i-1)%month == 12) then
          Dates(i) = Date(Dates(i-1)%year + 1, 1)
       else
          Dates(i) = Date(Dates(i-1)%year, Dates(i-1)%month + 1)
       end if
    end do
  end function seqDate
end module DateUtils

program main
  use DateUtils
  implicit none
  print "(a)", strDate(seqDate(Date(2023, 11), 4))
end program main
```

output:
```
2023-11
2023-12
2024-01
2024-02
```

---
**Compute the complex roots of a quadratic equation**

**Haskell**
```Haskell
import Data.Complex

-- The function roots takes three Complex numbers a, b, and c
-- (representing the coefficients of the quadratic equation ax^2 + bx + c = 0),
-- and returns a pair of Complex numbers (representing the two roots of the equation).
roots :: (RealFloat a) => Complex a -> Complex a -> Complex a -> (Complex a, Complex a)
roots a b c = ((-b + sqrt (b*b - 4*a*c)) / (2*a), (-b - sqrt (b*b - 4*a*c)) / (2*a))

main :: IO ()
main = do
-- Here ":+" is used to create a complex number. The number before ":+" is the real part,
-- and the number after ":+" is the imaginary part.
    let a = (1.0 :+ 0.0)
    let b = ((-2.0) :+ 3.0)
    let c = (0.0 :+ (-6.0))
    putStrLn $ "Coefficients of the equation:"
    putStrLn $ "a = " ++ show a
    putStrLn $ "b = " ++ show b
    putStrLn $ "c = " ++ show c
    let (root1, root2) = roots a b c
    putStrLn "Roots:"
    print root1
    print root2
```

output:
```
Coefficients of the equation:
a = 1.0 :+ 0.0
b = (-2.0) :+ 3.0
c = 0.0 :+ (-6.0)
Roots:
2.0 :+ 0.0
0.0 :+ (-3.0)
```

**Fortran** (default reals have been used in this code for simplicity)
```Fortran
module quadratic_mod
  implicit none
  contains

! The subroutine roots takes three Complex numbers a, b, and c
! (representing the coefficients of the quadratic equation ax^2 + bx + c = 0),
! and returns a pair of Complex numbers (representing the two roots of the equation).
    subroutine roots(a, b, c, root1, root2)
    complex, intent(in) :: a, b, c
    complex, intent(out) :: root1, root2
    root1 = ((-b + sqrt(b*b - 4.0*a*c)) / (2.0*a))
    root2 = ((-b - sqrt(b*b - 4.0*a*c)) / (2.0*a))
  end subroutine roots
end module quadratic_mod

program main
  use quadratic_mod
  implicit none
  complex :: a, b, c, root1, root2

  a = (1.0, 0.0)
  b = (-2.0, 3.0)
  c = (0.0, -6.0)

  print*, "Coefficients of the equation are"
  print*, "a = ", a
  print*, "b = ", b
  print*, "c = ", c

  call roots(a, b, c, root1, root2)

  print*, "Roots are:"
  print*, root1
  print*, root2
end program main
```

output:
```
 Coefficients of the equation are
 a =              (1.00000000,0.00000000)
 b =             (-2.00000000,3.00000000)
 c =             (0.00000000,-6.00000000)
 Roots are:
             (2.00000000,0.00000000)
            (0.00000000,-3.00000000)
```

---
**Read a matrix from a file, where the first line contains the dimensions of the matrix and following lines contain rows of the matrix.**

File `data.txt` contains
```
2 3
10.0 20.0 30.0
40.0 50.0 60.0
```

**Haskell**
```Haskell
import System.IO
import Data.List.Split
import Text.Read

-- Function to convert a string to a list of Doubles
strToDoubleList :: String -> [Double]
strToDoubleList s = map read (words s)

-- Main function
main :: IO ()
main = do
  -- Open the file
  handle <- openFile "data.txt" ReadMode
  -- Read the first line
  dims <- hGetLine handle
  let [rows, cols] = map read $ words dims
  -- Read the rest of the file
  contents <- hGetContents handle
  -- Split the contents into lines, then convert each line to a list of Doubles
  let matrix = map strToDoubleList (take rows (lines contents))
  -- Print the matrix
  mapM_ print matrix
  -- Close the file
  hClose handle
```

output:
```
[10.0,20.0,30.0]
[40.0,50.0,60.0]
```

**Fortran**

```Fortran
program read_matrix
    implicit none
    integer :: i, rows, cols, iu
    integer, parameter :: dp = kind(1.0d0)
    real(kind=dp), allocatable :: matrix(:,:)
    open(newunit=iu, file='data.txt', action="read")

    ! Read the number of rows and columns
    read(iu, *) rows, cols

    ! Allocate the matrix
    allocate(matrix(rows, cols))

    ! Read the matrix
    do i = 1, rows
        read(iu, *) matrix(i, 1:cols)
    end do

    ! Print the matrix
    do i = 1, rows
        print *, matrix(i, 1:cols)
    end do

    ! Close the file
    close(iu)
end program read_matrix
```

output:
```
   10.000000000000000        20.000000000000000        30.000000000000000     
   40.000000000000000        50.000000000000000        60.000000000000000
```

---
**Read an unknown number of integers from a file, one per line**

The file `integers.txt` has contents
```
10
20
30
```

**Haskell**
```Haskell
main = do
    content <- readFile "integers.txt"           -- Read the content of the file
    let linesOfFiles = lines content             -- Split the content by newlines
    let numbers = map read linesOfFiles :: [Int] -- Convert each line to an integer
    print numbers                                -- Print the list of numbers
```
output:
```
[10,20,30]
```
**Fortran**
```Fortran
program main
  implicit none
  integer, parameter :: max_read = 10**6
  integer, allocatable :: numbers(:)
  integer :: i,iu,ierr
  allocate (numbers(max_read))
  open (newunit=iu, file="integers.txt", action="read") ! open the file
  do i=1,max_read
     read (iu,*,iostat=ierr) numbers(i) ! try to read an integer
     if (ierr /= 0) then 
        numbers = numbers(:i-1) ! resize numbers and exit loop if integer could not be read
        exit
     end if
  end do
  print*,numbers
end program main
```
output:
```
          10          20          30
```

---

**Write a generic function that returns the positions of changed elements, including the first position, in a 1-D array.**

**Haskell**

```Haskell
-- The changePositions function takes a list of any type that supports equality testing as input.
-- If the input list is empty, it returns an empty list.
-- If the input list is not empty, it returns a list containing 0 followed by the positions of changes in the input list.
changePositions :: Eq a => [a] -> [Int]
changePositions [] = [] -- Base case: if the input list is empty, return an empty list.
changePositions xs = 0 : [i | (i, (x, y)) <- zip [1..] (zip xs (tail xs)), x /= y]
-- Recursive case: if the input list is not empty...
-- 1. Zip the input list with its tail, creating a list of pairs, where each pair consists of an element of xs and its successor.
-- 2. Zip this list of pairs with the list [1..], effectively assigning an index to each pair.
-- 3. Use a list comprehension to select the indices where a change occurred (i.e., where the two elements in a pair are not equal).
-- 4. Prepend 0 to the resulting list.

main :: IO ()
main = do -- test with lists of integer and strings
    print $ changePositions [3, 3, 6, 2, 2, 2, 1] -- Expected output: [0, 2, 3, 6]
    print $ changePositions ["a", "a", "b", "p", "p", "p", "o"] -- Expected output: [0, 2, 3, 6]
```

```
[0,2,3,6]
[0,2,3,6]
```

**Fortran** (works only for integers and character variables)

```Fortran
module list_mod
  implicit none
  interface change_positions
     module procedure :: change_positions_int, change_positions_char
  end interface change_positions
contains
  function change_positions_int(vec) result(pos)
    integer, intent(in)  :: vec(:)
    integer, allocatable :: pos(:)
    integer :: i,j,n
    n = size(vec)
    allocate (pos(n))
    if (n < 1) return
    pos = 0
    pos(1) = 1
    j = 1
    do i=2,n
       if (vec(i) /= vec(i-1)) then
          j = j+1
          pos(j) = i
       end if
    end do
    pos = pack(pos, pos > 0)
  end function change_positions_int

  function change_positions_char(vec) result(pos)
    character (len=*), intent(in)  :: vec(:)
    integer, allocatable :: pos(:)
    integer :: i,j,n
    n = size(vec)
    allocate (pos(n))
    if (n < 1) return
    pos = 0
    pos(1) = 1
    j = 1
    do i=2,n
       if (vec(i) /= vec(i-1)) then
          j = j+1
          pos(j) = i
       end if
    end do
    pos = pack(pos, pos > 0)
  end function change_positions_char
end module list_mod
!
program main
  use list_mod
  implicit none
  print*,change_positions([3, 3, 6, 2, 2, 2, 1])
    print*,change_positions(["a", "a", "b", "p", "p", "p", "o"])
end program main
```

output:
```
           1           3           4           7
           1           3           4           7
```
