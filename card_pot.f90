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
    integer :: j,k=0
    ! temporary storage of card indices
    integer :: tmp(8)

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
      integer,allocatable :: tmp_take_vals(:,:)
      integer :: i,s

      ! assign a temporary pot to debug with
      deallocate(me%p_val)
      me%n=5
      allocate(me%p_val(me%n))
      me%p_val=(/8,1,5,4,10/)
       
    
      do i=1,me%n
        me%j=i
        s=0
        call sum_me(me,s)
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
    RECURSIVE SUBROUTINE sum_me(me,s)
      class(pot_type),intent(inout) :: me
      integer,intent(inout) :: s
      integer,allocatable :: tmp_take_vals(:,:)
      integer :: i

      s=s+me%p_val(me%j)
      ! check to see if sum is a valid take
      if(s<=10) then
        ! increment combination counter
        me%c=me%c+1
        ! temporarily allocate array to store me%take_vals while actual
        ! me%take_vals array is expanded to accommodate new combination
        allocate(tmp_take_vals(me%c,10))
        tmp_take_vals=0
        if(me%c>1) then
          tmp_take_vals(1:me%c-1,:)=me%take_vals(1:me%c-1,:)
          deallocate(me%take_vals)
        endif
        allocate(me%take_vals(me%c,10))
        me%take_vals=tmp_take_vals
        ! assign new data
        me%take_vals(me%c,1)=s
        if(s==me%p_val(me%j)) then
          ! single card only
          me%tmp=0
          me%k=1
        else
          ! multiple cards expands on previous entry
          me%k=me%k+1
        endif
        me%tmp(me%k)=me%j
        me%take_vals(me%c,2:9)=me%tmp

        ! debug print
        print*, "Possible take combinations are:"
        do i=1,me%c
          print*, me%take_vals(i,:)
        enddo

        ! iterate on if the sum is less than 10 and we're not on the last card
        if(s<10.and.me%j<me%n) then
          me%j=me%j+1
          call sum_me(me,s)
        endif
      else
        if(me%j<me%n) then
          s=s-me%p_val(me%j)
          me%j=me%j+1
          me%tmp(me%k)=0
          me%k=me%k-1
          call sum_me(me,s)
        endif
      endif
        
    ENDSUBROUTINE sum_me
!-------------------------------------------------------------------------------

ENDMODULE card_pot
