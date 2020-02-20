!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!   Storage bin for cards taken to count points at end of hand
!   Andrew Gerlach
!   11/8/2016
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE card_bin

  implicit none
  private

  public :: bin_type

  type :: bin_type
    ! number of card stored in bin
    integer :: n
    ! maximum number of cards that can be stored in the bin
    integer :: nmax
    ! cards in bin
    character(len=3),allocatable :: b(:)
    ! card values in bin
    integer,allocatable :: b_val(:)

    CONTAINS
      procedure,pass :: init => init_bin
      procedure,pass :: clear => clear_bin
  endtype bin_type

  CONTAINS
!-------------------------------------------------------------------------------
! init() initializes the bin
!-------------------------------------------------------------------------------
    SUBROUTINE init_bin(me,spots)
      class(bin_type),intent(inout) :: me
      ! maximum number of spots in bin (typically cards in deck)
      integer,intent(in) :: spots

      me%nmax=spots
      me%n=0
      allocate(me%b(me%nmax))
      allocate(me%b_val(me%nmax))
      me%b='   '
      me%b_val=0

    ENDSUBROUTINE init_bin
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! clear() clears the bin
!-------------------------------------------------------------------------------
    SUBROUTINE clear_bin(me)
      class(bin_type),intent(inout) :: me

      me%nmax=0
      me%n=0
      deallocate(me%b)
      deallocate(me%b_val)

    ENDSUBROUTINE clear_bin
!-------------------------------------------------------------------------------

ENDMODULE card_bin
