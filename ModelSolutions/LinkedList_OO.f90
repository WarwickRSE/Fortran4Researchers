! The doubly-linked list, adapted to use OO fortran
! Basically, we swap the functions to be member functions
! Compare this to LinkedList.f90

MODULE doubly_linked_list

  USE kinds

  IMPLICIT NONE

  !> Type describing a particle (list node)
  TYPE particle
    !> The data - when node is created it will be set to zero by this
    !> v is velocity and runs 0 to c
    !> 'gamma' is a built-in function in F2008, but we can use the name anyway
    REAL(KIND=REAL64) :: v_x = 0.0_REAL64, gamma = 1.0_REAL64
    !> The links to prev and next nodes
    !> These MUST BE pointers else we have
    !> infinite recursion
    !> We nullify them each time a node is created
    TYPE(particle), POINTER :: prev=>NULL(), next=>NULL()

  END TYPE particle

  !> Type describing entire list
  TYPE dl_list
    !> head and tail are created and nullified
    !> These will be set any time the type is created
    TYPE(particle), POINTER :: head=>NULL(), tail=>NULL()
    CONTAINS
    PROCEDURE :: add_at_end
    PROCEDURE :: remove_item
  END TYPE dl_list

  CONTAINS

  ! I don't need any routine to initialise the list since I
  ! initialised head and tail in the def'n

  !> Add "item" to the tail of "list". "list" can be empty
  !> Item MUST NOT be part of a list already
  ! "This" is not special, I could call it anything, but
  ! I use this for clarity
  SUBROUTINE add_at_end(this, item)

    TYPE(particle), POINTER :: item
    ! 'this' must use CLASS not TYPE attribute
    CLASS(dl_list) :: this

    IF(ASSOCIATED(this%tail)) THEN
      !List tail is not null, i.e. there is at least one item

      ! Prev link of item is tail of list
      item%prev => this%tail
      ! Next link of old tail is item
      this%tail%next => item
      ! item becomes new tail
      this%tail => item
      ! Just in case item%next was not null
      NULLIFY(item%next)

    ELSE
      ! New item is both head and tail!
      this%tail => item
      this%head => item
      ! Just in case item pointers are not null
      NULLIFY(item%prev, item%next)
    END IF

  END SUBROUTINE add_at_end


  ! Remove "item" from "list". Item MUST BE in given list
  ! After the call, item is a "free" node and can be added to other lists etc
  SUBROUTINE remove_item(this, item)

    TYPE(particle), POINTER :: item
    CLASS(dl_list) :: this

     ! 4 cases - item is at head, tail or middle,
     ! or is the only item i.e. is head and tail

     IF(ASSOCIATED(item, TARGET=this%head)) THEN
       ! Item was the head. Check if head is also the tail

       IF(ASSOCIATED(this%head, TARGET=this%tail)) THEN
         ! If head is same as tail, there are no other nodes
         ! to consider so just null both ends of list
         NULLIFY(this%head, this%tail)
         !NOTE - this DOES NOT affect 'item' itself - we are only
         ! removing LINKS
       ELSE
         ! Swap head to next element in line
         this%head => item%next
         ! Nullify new heads 'prev' pointer
         NULLIFY(this%head%prev)
       END IF

     ELSE IF(ASSOCIATED(item, TARGET=this%tail)) THEN
       ! Item is ONLY tail - there must be an element before it
       ! Point list's 'tail' to this item
       this%tail => item%prev
       ! Nullify the 'next' pointer of this element
       ! We could also have done "NULLIFY(list%tail%next)"
       NULLIFY(item%prev%next)

     ELSE
       ! Item is neither head NOR tail
       ! Must have an element on either side. We simply link these
       ! to each other
       item%prev%next => item%next
       item%next%prev => item%prev

     END IF

     ! Now nullify item's links
     NULLIFY(item%prev, item%next)

  END SUBROUTINE remove_item

END MODULE doubly_linked_list

MODULE physics

!> Module to hold "physics" things

  USE kinds
  USE doubly_linked_list

  IMPLICIT NONE

  SAVE

  REAL(KIND=REAL64) :: c = 3.0e8

  CONTAINS

  FUNCTION rely_gamma(vel)

    REAL(KIND=REAL64) :: vel, rely_gamma, beta

    ! Adapted from RelyGamma program
    beta = vel/c

    ! Do the gamma calculation in 2 parts. First calculate 1/gamma. I use gamma
    ! as it's own temporary here. This is OK if you're careful
    rely_gamma = sqrt(1.0 - beta*beta)

    ! Check if 1/gamma is too small to represent.
    ! I use "TINY" to get the smallest non-zero number

    IF(rely_gamma < TINY(rely_gamma)) THEN
      PRINT*, "Gamma has overflowed! Try something slower!"
    END IF

    ! Soften the division to still give a useable number
    ! This is useful, but be careful that it doesn't "cover up"
    ! things having gone wrong

    rely_gamma = 1.0_REAL64/(rely_gamma + TINY(rely_gamma))

  END FUNCTION rely_gamma

END MODULE physics

PROGRAM MAIN

  USE kinds
  USE random_mod
  USE doubly_linked_list
  USE physics

  IMPLICIT NONE

  ! Create lists
  ! I am pretending these are particles which
  ! I want split into two lists for processing
  ! I am giving a mock example of this here
  TYPE(dl_list) :: electron, fast_electron
  ! Create a POINTER to a 'node' so we can e.g. loop
  TYPE(particle), POINTER :: current, next
  INTEGER :: i, n_part
  REAL(KIND=REAL64) :: inc, max_gamma_slow, min_gamma_fast
  REAL(KIND=REAL64) :: max_gamma, min_gamma


  n_part = 1000

  ! Create a list of 'electron' with random momentum
  DO i = 1, n_part

    ! Create a particle. A new one is created each time
    ! We add them to the list so we still have a pointer
    ! even once we re-use current
    ALLOCATE(current)

    ! Set a random velocity
    current%v_x = random() * c*0.999 ! Stay a bit below c
    CALL electron%add_at_end(current)

  END DO


  ! Basic loop - loop over and calculate gamma
  current => electron%head
  ! Loop while there is a next particle
  DO WHILE(ASSOCIATED(current%next))

    current%gamma = rely_gamma(current%v_x)

    ! Go on to next particle
    current => current%next

  END DO


  ! Loop over list, and if gamma is > 1+ inc move to "fast" list
  ! Suppose have to account for relativistic dynamics on these
  ! but on the slower ones we can do a faster calculation

  inc = 1e-4_REAL64

  !Start at the beginning again
  current => electron%head

  ! Loop while there is a next particle
  DO WHILE(ASSOCIATED(current%next))

    ! Keep ptr to next particle, because once we remove or add to list
    ! we can't use current%next anymore
    next => current%next

    IF (current%gamma - 1.0_REAL64 > inc) THEN
      ! Move to fast list
      CALL electron%remove_item(current)
      CALL fast_electron%add_at_end(current)
    END IF

    ! Go on to next item in the electron list
    current => next

  END DO

  ! Now we can do our physics. I am not going to, I am just going to
  ! walk both lists and show that we have partitioned on gamma

  ! Usuall min/max - start with something less or greater than expected respectively
  max_gamma_slow = 1.0_REAL64
  min_gamma_fast = 1.0_REAL64 + 2.0*inc

  ! Loop through the items one list at a time:

  ! Basic loops, one after another
  current => electron%head
  DO WHILE(ASSOCIATED(current%next))
    max_gamma_slow = MAX(max_gamma_slow, current%gamma)
    ! Go on to next particle
    current => current%next
  END DO

  ! No difference in loop once we point current to list start
  current => fast_electron%head
  DO WHILE(ASSOCIATED(current%next))
    min_gamma_fast = MIN(min_gamma_fast, current%gamma)
    ! Go on to next particle
    current => current%next
  END DO

  PRINT*, "Electron list max gamma: ",  max_gamma_slow, &
      "Fast_electron list min gamma: ", min_gamma_fast


  !Sometimes we just want to loop through ALL the items
  ! The following is super handy, but we MUST NOT add or
  ! remove items in general, because that could change the
  ! head or tail of the lists, and the usual way to find those
  ! elements (prev or next is NULL) won't work

  !Link from tail of list 'electron' to head of list 'fast_electron'
  electron%tail%next =>fast_electron%head
  fast_electron%head%prev => electron%tail

  min_gamma = HUGE(max_gamma)
  max_gamma = 1.0_REAL64

  ! This now goes through ALL elements
  current => electron%head
  DO WHILE(ASSOCIATED(current%next))
    max_gamma = MAX(max_gamma, current%gamma)
    min_gamma = MIN(min_gamma, current%gamma)
    ! Go on to next particle
    current => current%next
  END DO

  ! Undo the linkage

  NULLIFY(electron%tail%next)
  NULLIFY(fast_electron%head%prev)

  PRINT*, "Minimum gamma: ", min_gamma, "Maximum gamma: ", max_gamma

END PROGRAM
