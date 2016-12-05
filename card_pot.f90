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
    ! pot possibilities to be taken, first column is value, all others are
    ! indices within the pot
    integer,allocatable :: take_vals(:,:)
    ! counter
    integer :: c=0
    ! index
    integer :: j=0

    CONTAINS
      procedure,pass :: init => init_pot
      procedure,pass :: check => check_pot
      procedure,pass :: sum1
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
      integer :: s

      allocate(me%take_vals(me%n,10))
      me%take_vals=0
      me%c=me%n
      do i=1,me%n
        me%take_vals(i,1)=me%p_val(i)
        me%take_vals(i,2)=i
      enddo
      do i=1,me%n-1
        me%j=i+1
        call sum1(me,me%p_val(i))
      enddo
print*, "Possible take combinations are:"
      do i=1,me%c
        print*, me%take_vals(i,:)
      enddo
    ENDSUBROUTINE check_pot
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! 
!-------------------------------------------------------------------------------
    RECURSIVE SUBROUTINE sum1(me,s)
      class(pot_type),intent(inout) :: me
      integer,intent(inout) :: s
      integer,allocatable :: tmp_take_vals(:,:)
      integer :: i

!print*, "sum1, enter"
!print*, "sum1, input s=",s
      s=s+me%p_val(me%j)
!print*, "sum1, new s=",s
      if(s<10) then
!print*, "sum1, s<10, c=",me%c," j=",me%j
        me%c=me%c+1
        allocate(tmp_take_vals(me%c,10))
        tmp_take_vals(1:me%c-1,:)=me%take_vals(1:me%c-1,:)
        tmp_take_vals(me%c,:)=0
print*, "Possible take combinations are:"
do i=1,me%c
  print*, tmp_take_vals(i,:)
enddo
        deallocate(me%take_vals)
        allocate(me%take_vals(me%c,10))
        me%take_vals=tmp_take_vals
print*, "Possible take combinations are:"
do i=1,me%c
  print*, me%take_vals(i,:)
enddo
        me%j=me%j+1
!print*, "sum1, s<10, new c=",me%c," j=",me%j
        call sum1(me,s)
      else
!print*, "sum1, s>10"
        if(me%j<me%n) then
          s=s-me%p_val(me%j)
!print*, "sum1, j<n, new s=",s," j=",me%j
          me%j=me%j+1
          call sum1(me,s)
        else
!print*, "sum1, j!>n, so we're done"
        endif
      endif
!print*, "sum1, exit"
        
    ENDSUBROUTINE sum1
!-------------------------------------------------------------------------------

ENDMODULE card_pot
