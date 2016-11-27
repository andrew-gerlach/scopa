!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!   Scopa Main Program
!   Andrew Gerlach
!   11/8/2016
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE card_pot

  implicit none
  private

  public :: pot_type

  type :: pot_type
    ! number of cards in pot
    integer :: n=0
    ! pot of cards
    character(len=3),allocatable :: p(:)
    ! pot IDs
    integer,allocatable :: p_id(:)
    ! pot values
    integer,allocatable :: p_val(:)

    CONTAINS
      procedure,pass :: init => init_pot
      procedure,pass :: check => check_pot
  endtype pot_type  

  CONTAINS
!-------------------------------------------------------------------------------
! init() initializes the pot
!-------------------------------------------------------------------------------
    SUBROUTINE init_pot(me,cards)
      class(pot_type),intent(inout) :: me
      ! number of cards in the pot
      integer :: cards

      me%n=cards
      allocate(me%p(me%n))
      allocate(me%p_id(me%n))
      allocate(me%p_val(me%n))

    ENDSUBROUTINE init_pot
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! check() checks the pot to see what is available to be taken
!-------------------------------------------------------------------------------
    SUBROUTINE check_pot(me)
      class(pot_type),intent(inout) :: me
      integer :: tmp,tmp_sum,i,j,combos
      integer,allocatable :: tmp_pot(:)

      ! sort pot by value
      allocate(tmp_pot(me%n))
      tmp_pot=me%p_val
      do i=1,me%n-1
        do j=i+1,me%n
          if(tmp_pot(j)<tmp_pot(i)) then
            tmp=tmp_pot(j)
            tmp_pot(j)=tmp_pot(i)
            tmp_pot(i)=tmp
          endif
        enddo
      enddo
print*, "sorted pot:",me%p_val

    ENDSUBROUTINE check_pot
!-------------------------------------------------------------------------------

ENDMODULE card_pot
