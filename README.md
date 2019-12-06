# Fortran4Researchers

These are the code files and example sheets for Warwick RSEs "Fortran for Researchers"
one-day workshop.

The Example codes referred to in the slides are in the ExampleCode directory, and the slides are in the Slides directory.


For the second part of the afternoon, we provide a series of suggestions for simple programs, from
the very basic through to some real codes. A pdf describing the suggestions is in the main directory.
We also provide "model solutions" for all of the suggestions. Note that these are just ONE way of
solving the problem, not necessarily the only one or the optimal one. Use them for suggestions how
to approach the problem, or as "oracles" showing the correct results.



The Model solutions come with simple build scripts. Most of the programs use "./build <name of f90 file>
and this will build an executable with the same name as the input file (minus the .f90 part). Some of the
programs need extra support code - these use build\_w\_mods and also produce an executable named
after the input file. These are:

* GameOfLife.f90
* IsingSpin.f90
* LinkedList.f90


The basic examples, in the order they come in the pdf/exercises are:

## Basic examples
These follow the steps in the exercises closely:

* HelloWorld.f90
* BasicVariables.f90
* Fibonacci.f90
* StdDev\_and\_Average.f90

## Further suggestions

### Basic Calculations
These are all quite self explanatory:

* Circle.f90
* TempConverter.f90
* QuadraticEquation.f90
* RelyGamma.f90

### FizzBuzz
This is a real classic "can you program language x" problem. I give 3 approaches here.

* FizzBuzz\_v1\_mod.f90 - The obvious solution - MOD(x, 3) is the remainder when x is divided by 3
* FizzBuzz\_v1\_count.f90 - A solution without MOD using a counter
* FizzBuzz\_v2.f90 - A "more elegant" solution which aligns all the outputs and handles 15 without a special case

Note that all of these can be modified to change 3 and 5 to other co-prime numbers, although we would need to make them into parameters as well.

### Examples with big and small numbers
These start to include considerations about the size of variable
you need to store the results, and some considerations of comparison
of REAL numbers and the like.

Also, they are becoming interesting enough that there are multiple solutions
so we provide several approaches.

* Factorial
    * Factorial\_int.f90 - this uses an INTEGER, either default or longer. The answer is exact, but the range is restricitive (12-20 terms only)
    * Factorial\_re.f90 - this uses a REAL, again default or longer. The answer strays from the exact answer in the later digits, but the range is better
* Sin (x)
    * Sin.f90 - the Taylor expansion with a fixed number of iterations
    * Sin\_threshold.f90 - terminate at a given fractional change


### Examples with Arrays
We don't give examples with both compile-time and ALLOCATABLE arrays
as they are very similar, but both are quite suitable for these.

* Caesar.f90 - Fortran string handling is not very elegant, but it can do most things
* Sin\_array.f90 - Finding sign-changes of the SIN function
* Bubble\_sort.f90 - Bubble sorting data

### Real Programs
These are idealised versions of the sort of program we might write for a real problem, in
contrast to the above which are quite clearly "exercises". We give some variants of
the solutions, to demonstrate more bits of Fortran.

* Numerical Root - Newton-Raphson root finding
    * Numerical\_Root.f90 - the basic solution with a hard-coded test function
    * Numerical\_Root\_type.f90 - a more flexible solution which works for arbitrary polynomials using a TYPE
    * Numerical\_Root\_ptr.f90 - part of a general solution - the test function can be swapped to anything we like, but this still uses a simple hard-coded cubic
* GameOfLife - Conway's game of Life
    * GameOfLife.f90 - the basic solution using allocatable arrays and the helper code modules
    * ???? - 
* IsingSpin - the Ising model for Magnetism



