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
    ! hand values
    integer,allocatable :: h_val(:)

    CONTAINS
      procedure,pass :: init => init_hand
      procedure,pass :: pull => pull_hand
      procedure,pass :: clear => clear_hand
  endtype hand_type  

  CONTAINS
!-------------------------------------------------------------------------------
! init() initializes the hand
!-------------------------------------------------------------------------------
    SUBROUTINE init_hand(me,cards)
      class(hand_type),intent(inout) :: me
      ! number of cards in each hand
      integer,intent(in) :: cards

      me%n=cards
      allocate(me%h(me%n))
      allocate(me%h_val(me%n))
      me%h='   '
      me%h_val=0

    ENDSUBROUTINE init_hand
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! pull() pulls a card from the hand
!-------------------------------------------------------------------------------
    SUBROUTINE pull_hand(me,i)
      class(hand_type),intent(inout) :: me
      ! index of card to be pulled
      integer,intent(in) :: i
      character(len=3) :: tmp1
      integer :: tmp2

      if(i==1) then
        tmp1=me%h(2)
        tmp2=me%h_val(2)
        me%h(1)=tmp1
        me%h_val(1)=tmp2
      endif
      if(i<3) then
        tmp1=me%h(3)
        tmp2=me%h_val(3)
        me%h(2)=tmp1
        me%h_val(2)=tmp2
      endif
      me%h(3)=" --"
      me%h_val(3)=0

    ENDSUBROUTINE pull_hand
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! clear() clears the hand
!-------------------------------------------------------------------------------
    SUBROUTINE clear_hand(me)
      class(hand_type),intent(inout) :: me

      me%n=0
      deallocate(me%h)
      deallocate(me%h_val)

    ENDSUBROUTINE clear_hand
!-------------------------------------------------------------------------------
 
ENDMODULE card_hand
