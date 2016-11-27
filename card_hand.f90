!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!   Scopa Main Program
!   Andrew Gerlach
!   11/8/2016
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE card_hand

  implicit none
  private

  public :: hand_type

  type :: hand_type
    ! number of cards in hand
    integer :: n=0
    ! hand of cards
    character(len=3),allocatable :: h(:)
    ! hand IDs
    integer,allocatable :: h_id(:)
    ! hand values
    integer,allocatable :: h_val(:)

    CONTAINS
      procedure,pass :: init => init_hand
  endtype hand_type  

  CONTAINS
!-------------------------------------------------------------------------------
! init() initializes the hand
!-------------------------------------------------------------------------------
    SUBROUTINE init_hand(me,cards)
      class(hand_type),intent(inout) :: me
      ! number of cards in each hand
      integer :: cards

      me%n=cards
      allocate(me%h(me%n))
      allocate(me%h_id(me%n))
      allocate(me%h_val(me%n))

    ENDSUBROUTINE init_hand
!-------------------------------------------------------------------------------

ENDMODULE card_hand
