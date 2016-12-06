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
    ! counter for pot possibilities to be taken
    integer :: c
    ! index for cards included in pot
    integer :: k
    ! temporary storage or card indices
    integer :: tmp_vals(8)

    CONTAINS
      procedure,pass :: init => init_pot
      procedure,pass :: check => check_pot
      procedure,pass :: sum_me
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
      integer :: i,s

      ! assign a temporary pot to debug with
      deallocate(me%p_val)
      me%n=5
      allocate(me%p_val(me%n))
      me%p_val=(/8,1,5,4,10/)
       
      me%c=0
      me%k=1
      me%tmp_vals=0
      ! cycle through pot to check for combos
      do i=1,me%n
        s=0
        call sum_me(me,i,s)
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
    RECURSIVE SUBROUTINE sum_me(me,i,s)
      class(pot_type),intent(inout) :: me
      integer, intent(in) :: i
      integer, intent(inout) :: s
      integer :: j
      integer,allocatable :: tmp_take_vals(:,:)
      
      ! add new card to sum
      s=s+me%p_val(i)
      ! check to see if sum is takeable (10 or less)
      if(s<=10) then
        ! increment counter of possible takes
        me%c=me%c+1
        ! add card position in pot
        me%tmp_vals(me%k)=i
        ! copy previous combinations and reallocate take_vals for new combo
        allocate(tmp_take_vals(me%c-1,10))
        tmp_take_vals=0
        if(me%c>1) then
          tmp_take_vals=me%take_vals
          deallocate(me%take_vals)
        endif
        allocate(me%take_vals(me%c,10))
        me%take_vals=0
        me%take_vals(1:me%c-1,:)=tmp_take_vals(1:me%c-1,:)
        ! add new combo to take_vals
        me%take_vals(me%c,1)=s
        me%take_vals(me%c,2:9)=me%tmp_vals
        me%take_vals(me%c,10)=me%k
        ! iterate further is sum is less than 10 and not on last card
        if(s<10.and.i<me%n) then
          ! increment number of cards in combo
          me%k=me%k+1
          do j=i+1,me%n
            call sum_me(me,j,s)
          enddo
          ! de-increment number of cards in combo
          me%k=me%k-1
          ! remove value from sum for next iteration
          s=s-me%p_val(i)
        else
          ! remove value from sum for next iteration
          s=s-me%p_val(i)
        endif
        ! remove card position to move on
        me%tmp_vals(me%k)=0
      else
        ! remove value from sum for next iteration
        s=s-me%p_val(i)
      endif

    ENDSUBROUTINE sum_me
!-------------------------------------------------------------------------------

ENDMODULE card_pot
