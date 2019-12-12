# Fortran4Researchers

These are the code files and example sheets for Warwick RSEs "Fortran for Researchers"
one-day workshop.

The code from the slides is all in the FromSlides directory, and the slides are in the Slides directory.
We suggest using these to recap syntax, and you might wish to borrow from them for the examples session.

The examples session is in the second part of the afternoon, and is a chance to actually use Fortran.
We provide a series of suggestions for simple programs, from
the very basic through to some real codes. A pdf describing the suggestions is in the main directory.
We also provide "model solutions" for all of the suggestions. Note that these are just ONE way of
solving the problem, not necessarily the only one or the optimal one. In some cases the model solution
is deliberately overbuilt to demonstrate some part of Fortran. Use the Models for suggestions how
to approach the problem, as "oracles" showing the correct results, or just as Fortran code to
look at. Reading, and understanding, code in a new language is a good way to get the hang of the
harder parts.

## Building the Examples

Most of the code examples build with a simple gfortran command, although we *strongly* recommend
using -Wall to enable all warnings, and -std=f2003 to force adherence to the Fortran 2003 standard.
A simple build script is provided to build using these. The command "./build <name of f90 file>"
will build an executable with the same name as the input file (minus the .f90 part).

A few of the examples have a more complicated build. In the FromSlides, the following need the F2008
standard, so use the "build2008" script instead. These are:
* 43-KindsF2008.f90

Some of the model programs need extra support code -
these use build\_w\_mods to include the helper modules supplied and described below. These are:

* GameOfLife.f90
* IsingSpin.f90
* IsingSpin\_OO.f90
* LinkedList.f90
* LinkedList\_OO.f90

The following sections describe the programs created in the exercises and available
as model codes.

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
* FizzBuzz\_v1\_case.f90 - A solution using a CASE statement which works well 3 and 5, but requires a lot of typing if the lowest common multiple is large
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
* IsingSpin - the Ising model for Magnetism (see instructions for details)
    * IsingSpin.f90 - the basic version of the Ising model
    * IsingSpin\_OO.f90 - an Object-Oriented version of the Ising model. *WARNING* - overbuilt for this problem.
* LinkedList - a toy physics problem using a Linked List data structure
    * Linked lists are handy when things are added and removed a lot
    * They're also handy when you want to be able to loop over things separately OR combined
    * See ExamplePointer.f90 for an illustration of how pointers work in Fortran
    * LinkedList.f90 - the basic linked list code
    * LinkedList\_OO.f90 - the same as the Linked List, but using Class member functions for more modularity

## Helper Code

Some of the "real" programs become a lot more interesting once we have the ability to do
random numbers, display the results, and control them at runtime. We include some helper
codes to make these things smoother. In particular we have:

* kinds.f90 - Supplies the basic numeric KINDs
* random\_mod.f90 - Wraps Fortran's built-in random number generator, but handles seeding for you
* sleep\_mod.f90 - Wraps the C/Gnu sleep functions to get millisecond resolution for nice display
* ascii\_display.f90 - Ascii-art display of arrays
* command\_line.f90 - Handle command line arguments. Also illustrates a useful overloaded-interface
    * command\_line\_snippet.f90 - Demo of using the command line code
