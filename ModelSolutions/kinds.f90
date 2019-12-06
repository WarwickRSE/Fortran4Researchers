  !> @brief Standard kinds for numeric types
  !>
  !> This module provides the numeric kinds we think
  !> you might need, using names that match those in
  !> F2008. This makes it smooth to change to std F2008
  !> by simply removing this module.
  !>
  !> Do remember that not every length of type has to
  !> exist on a particular system - for those which
  !> don't you will get AT LEAST the requested length
  !> if this does not exceed the longest the system offers.

MODULE kinds

  IMPLICIT NONE



  !> Very short integer (8 bit, -128 to 127)
  INTEGER, PARAMETER :: INT8 = SELECTED_INT_KIND(2)
  !> Short integer (16 bit, -32 768 to 32 767)
  INTEGER, PARAMETER :: INT16 = SELECTED_INT_KIND(4)
  !> "Normal" integer (32 bit == 4 byte, -2 147 483 648 to 2 147 483 647)
  INTEGER, PARAMETER :: INT32 = SELECTED_INT_KIND(9)
  !> Long integer (64 bit), -9 223 372 036 854 775 808 to
  !> 9 223 372 036 854 775 807)
  INTEGER, PARAMETER :: INT64 = SELECTED_INT_KIND(15)

  !> Normal "float" (32 bit = 4 bytes, approx -3.4e38 to 3.4e38 and
  !> covering values down to about 1e-38 magnitude)
  INTEGER, PARAMETER :: REAL32 = SELECTED_REAL_KIND(6, 37)
  !> Longer "double" (64 bit, approx -1.8e308 to 1.8e308 and
  !> covering values down to about 2e-308 magnitude)
  INTEGER, PARAMETER :: REAL64 = SELECTED_REAL_KIND(15, 307)
  !> Extra long real (128 bit = 16 bytes). Few systems have numbers
  !> this long at all!
  INTEGER, PARAMETER :: REAL128 = SELECTED_REAL_KIND(33, 4931)

END MODULE
