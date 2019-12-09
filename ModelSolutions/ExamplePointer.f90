PROGRAM target

  INTEGER, TARGET :: i, i2
  INTEGER, POINTER :: p_i, p_i2

  REAL, DIMENSION(:), ALLOCATABLE, TARGET :: rdata
  REAL, DIMENSION(:), POINTER :: pdata
  REAL, POINTER :: p_r


  PRINT *,'**** Simple variable ****'
  PRINT *,'Setting i to 10'
  i = 10
  p_i => i
  PRINT *, 'Pointer to i is now ', p_i
  PRINT *, 'Setting pointer to i to 20'
  p_i = 20
  PRINT *,'i is now ', i

  PRINT *, ''
  PRINT *, '**** Accidental equality ****'
  PRINT *, 'Setting i to 10'
  i = 10
  PRINT *, 'Setting i2 to 20'
  i2 = 20
  PRINT *, 'Pointing pointer to i'
  p_i => i
  PRINT *, 'Is pointer pointing to i :', ASSOCIATED(p_i, TARGET = i)
  PRINT *, 'Now using equality rather than points to operator'
  p_i = i2
  PRINT *, 'Is pointer still pointing to i :', ASSOCIATED(p_i, TARGET = i)
  PRINT *, 'p_i is ', p_i
  PRINT *, 'i is ', i
  PRINT *, 'i2 is ', i2
  PRINT *, 'p_i appears to correctly have the value 20 but by changing the &
      &value of i rather than pointing it to i2'
  PRINT *, 'Watch out for using = rather than => in pointer code'
  PRINT *, 'If you use = on a NULL pointer you will probably crash your code'
  PRINT *, 'Now setting i2 to 100'
  i2 = 100
  PRINT *, 'p_i is ', p_i
  PRINT *, 'i is ', i
  PRINT *, 'i2 is ', i2
  PRINT *, 'The value of p_i is not changed by changing i2'

  PRINT *,''
  PRINT *,'**** Arrays ****'
  PRINT *,'Allocating data to be -10:10'
  ALLOCATE(rdata(-10:10))
  pdata => rdata
  PRINT *, 'Bounds and sizes of data'
  PRINT *, LBOUND(rdata), UBOUND(rdata), SIZE(rdata)
  PRINT *, 'Bounds and sizes of pointer to data'
  PRINT *, LBOUND(pdata), UBOUND(pdata), SIZE(pdata)

  PRINT *, ''
  PRINT *, '**** Direct allocation ****'
  PRINT *,'Allocating p_i and setting to 100'
  ALLOCATE(p_i)
  p_i = 100 

  PRINT *,'i is ', i
  PRINT *,'p_i is ', p_i
  PRINT *,'p_i is now just a variable and is no longer pointing to i'
  DEALLOCATE(p_i, rdata)

  PRINT *,''
  PRINT *,'**** Pointer to element of array ****'
  PRINT *,'Allocating array with 5 elements and setting to 0'
  ALLOCATE(rdata(1:5))
  rdata = 0
  PRINT *,'Pointing single pointer to element 3 and setting to 1'
  p_r => rdata(3)
  p_r = 1.0
  PRINT *,'Array contents is now'
  PRINT *, rdata
  DEALLOCATE(rdata)

  PRINT *,''
  PRINT *,'**** Pointer to array subsection ****'
  PRINT *,'Allocating array with 10 elements and setting to 0'
  ALLOCATE(rdata(1:10))
  rdata = 0
  PRINT *,'Setting array pointer to elements 3 to 5'
  pdata => rdata(3:5)
  PRINT '(A,I2,A,I2)','Size of pdata is ', LBOUND(pdata), ':', UBOUND(pdata)
  PRINT *,'Setting elements to 1'
  pdata = 1.0
  PRINT *,'Array contents is now'
  PRINT *, rdata
  DEALLOCATE(rdata)

  PRINT *, ''
  PRINT *, '**** No pointer to pointer ****'
  PRINT *, 'Setting i to 10 and i2 to 20'
  i  = 10
  i2 = 20
  PRINT *, 'Pointing p_i to i and p_i2 to p_i'
  p_i => i
  p_i2 => p_i

  PRINT *, 'Values of p_i and p_i2'
  PRINT *, 'p_i :', p_i
  PRINT *, 'p_i2 :', p_i2
  PRINT *, 'Pointing p_i to p2'
  p_i => i2
  PRINT *, 'Values of p_i and p_i2'
  PRINT *, 'p_i :', p_i
  PRINT *, 'p_i2 :', p_i2
  PRINT *, 'Is p_i2 pointing to i :', ASSOCIATED(p_i2, TARGET = i)
  PRINT *, 'Even though p_i2 was pointed to p_i it is NOT a pointer to p_i.'
  PRINT *, 'It is still a pointer to i'
  PRINT *, 'There is no concept of a pointer to a pointer in Fortran'

END PROGRAM target
