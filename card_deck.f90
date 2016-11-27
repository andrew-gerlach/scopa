!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!   Scopa Main Program
!   Andrew Gerlach
!   11/8/2016
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE card_deck
  use card_hand
  use card_pot

  implicit none
  private

  public :: deck_type

  ! number of suits in deck
  integer,parameter :: nsuits=4
  ! max cards per suit
  integer,parameter :: nums_max=13

  type :: deck_type
    ! type of card deck [rah,ral,ita,euc]
    character(len=3) :: deck_kind='nul'
    ! number of cards in deck
    integer :: ncards=0
    ! number of numbers per suit (=ncards/nsuits)
    integer :: nnums=0
    ! number of jokers in deck (not used currently)
    integer :: njs=0
    ! unique identifier for each card (redundant with deck?)
    integer,allocatable :: deck_id(:)
    ! numerical value of each card
    integer,allocatable :: deck_val(:)
    ! display value of cards
    character(len=3),allocatable :: deck(:)
    ! index of current top card in the deck
    integer :: top_card=0

    CONTAINS
      procedure,pass :: init => init_deck
      procedure,pass :: shuffle => shuffle_deck
      procedure,pass :: deal => deal_deck
  endtype deck_type  

  CONTAINS

!-------------------------------------------------------------------------------
! init() initializes the deck
!-------------------------------------------------------------------------------
    SUBROUTINE init_deck(me,deck_in)
      class(deck_type),intent(inout) :: me
      ! type of card deck
      character(len=3),intent(in) :: deck_in
      ! numerical value of cards in deck, 0 implies not used
      integer :: values(nums_max)=0
      ! numerical value of cards actually contined in the deck
      integer,allocatable :: card_vals(:)
      ! display number of cards in deck
      character(len=2),allocatable :: card_nums(:)
      ! display number of all possible cards
      character(len=2) :: numbers(nums_max)
      ! display suit
      character(len=1) :: suits(nsuits)
      ! indexing and temporary storage
      integer :: i, j
      character(len=2) :: tmp_number
      character(len=1) :: tmp_suit
      real :: tmp

      me%deck_kind=deck_in
      ! assign card names and suits
      suits=(/"S","C","H","D"/)
      numbers=(/" A"," 2"," 3"," 4"," 5"," 6"," 7"," 8"," 9","10"," J"," Q"," K"/)
      ! assign used card numbers based on deck type
      if(me%deck_kind=='rah') then
        values=(/14,2,3,4,5,6,7,8,9,10,11,12,13/)
      elseif(me%deck_kind=='ral') then
        values=(/1,2,3,4,5,6,7,8,9,10,11,12,13/)
      elseif(me%deck_kind=='ita') then
        values=(/1,2,3,4,5,6,7,0,0,0,9,8,10/)
      elseif(me%deck_kind=='euc') then
        values=(/14,0,0,0,0,0,0,0,9,10,11,12,13/)
      else
        print*, "Deck type is not recognized!"
        stop
      endif 
      ! determine the number of cards per suit
      me%nnums=0
      do i=1,nums_max
        if(values(i)>0) me%nnums=me%nnums+1
      enddo
      if(me%nnums<1 .or. me%nnums>nums_max) then
        print*, "Modifications are required to handle this deck type!"
        stop
      endif
      ! assign the card numbers
      allocate(card_vals(me%nnums))
      allocate(card_nums(me%nnums))
      j=1
      do i=1,nums_max
        if(values(i)>0) then
          card_vals(j)=values(i)
          card_nums(j)=numbers(i)
          j=j+1
        endif
      enddo

      ! establish the unshuffled deck with a unique identifier, numerical card
      ! value, and card display
      me%ncards=me%nnums*4
      allocate(me%deck_id(me%ncards))
      allocate(me%deck_val(me%ncards))
      allocate(me%deck(me%ncards))
      do i=1,me%ncards
        me%deck_id(i)=i
        tmp=real(i)/me%nnums
        j=ceiling(tmp)
        tmp_suit=suits(j)
        me%deck_val(i)=card_vals(i-me%nnums*(j-1))
        tmp_number=card_nums(i-me%nnums*(j-1))
        me%deck(i)=tmp_number//tmp_suit
      enddo

    ENDSUBROUTINE init_deck
!-------------------------------------------------------------------------------
! shuffle() shuffles the deck of cards into a random order
!-------------------------------------------------------------------------------
    SUBROUTINE shuffle_deck(me)
      class(deck_type),intent(inout) :: me
      real,allocatable :: order(:)
      integer :: i, j, spot
      integer,allocatable :: tmp_deck_id(:), tmp_deck_val(:)
      character(len=3),allocatable :: tmp_deck(:)
      logical,save :: first_shuffle=.true.

      ! intialize deck and allocate arrays if first shuffle
      if(first_shuffle) then
        allocate(order(me%ncards))
        allocate(tmp_deck(me%ncards))
        allocate(tmp_deck_id(me%ncards))
        allocate(tmp_deck_val(me%ncards))
        first_shuffle=.false.
      endif
      ! shuffle deck and assign to temporary deck
      call random_number(order)
      do i=1,me%ncards
        spot=1
        do j=1,me%ncards
          if(order(i)>order(j)) spot=spot+1
        enddo
        tmp_deck(spot)=me%deck(i)
        tmp_deck_id(spot)=me%deck_id(i)
        tmp_deck_val(spot)=me%deck_val(i)
      enddo
      ! reassign temporary deck to main deck
      me%deck=tmp_deck
      me%deck_id=tmp_deck_id
      me%deck_val=tmp_deck_val
      print*,me%deck

    ENDSUBROUTINE shuffle_deck
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! deal() deals out the appropriate number of cards to each player and flips up
!   the appropriate number of cards
!-------------------------------------------------------------------------------
    SUBROUTINE deal_deck(me,phand,chand,pot,spot,first_hand)
      class(deck_type),intent(inout) :: me
      ! player hand
      class(hand_type),intent(inout) :: phand
      ! computer hand
      class(hand_type),intent(inout) :: chand
      ! pot
      class(pot_type),intent(inout) :: pot
      ! spot in which player is dealt
      integer,intent(in) :: spot
      ! deal pot if first hand
      logical,intent(in) :: first_hand
      integer,save :: nhands, cards_hand, cards_up
      integer :: i

      cards_hand=phand%n
      nhands=2
      do i=1,cards_hand
        phand%h(i)=me%deck((i-1)*nhands+spot)
        phand%h_id(i)=me%deck_id((i-1)*nhands+spot)
        phand%h_val(i)=me%deck_val((i-1)*nhands+spot)
        chand%h(i)=me%deck(i*nhands-spot+1)
        chand%h_id(i)=me%deck_id(i*nhands-spot+1)
        chand%h_val(i)=me%deck_val(i*nhands-spot+1)
      enddo
      me%top_card=nhands*cards_hand+1
      if(first_hand) then
        cards_up=size(pot%p)
        pot%p(1:cards_up)=me%deck(me%top_card:me%top_card+cards_up-1)
        pot%p_id(1:cards_up)=me%deck_id(me%top_card:me%top_card+cards_up-1)
        pot%p_val(1:cards_up)=me%deck_val(me%top_card:me%top_card+cards_up-1)
        me%top_card=me%top_card+cards_up
      endif
      print*, " player  hand is",phand%h(:)
      print*, "computer hand is",chand%h(:)
      print*, "pot is: ",pot%p(:)
          
    ENDSUBROUTINE deal_deck

ENDMODULE card_deck
