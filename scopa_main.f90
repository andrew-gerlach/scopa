!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!   Scopa Main Program
!   Andrew Gerlach
!   11/8/2016
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE scopa_main
  use card_deck
  use card_hand
  use card_pot

  implicit none
  private

  public :: deal_scopa
  public :: play_scopa

  ! number of actual players
  integer :: nplayers
  ! number of computer players
  integer :: ncomps
  ! total number of players (hands)
  integer :: ntot
  ! cards per hand
  integer :: cards_hand=3
  ! player spot in order
  integer :: spot
  ! computer spot in order
  integer :: comp_spot
  ! number of cards left in the player's hand
  integer :: cards_in_hand
  ! pointer to the card deck (this needs to be a target, not a pointer so that
  ! it can be accessed by a pointer)
  type(deck_type),target :: deck
  ! pointer to the player's hand
  type(hand_type),target :: phand
  ! pointer to the computer's hand
  type(hand_type),target :: chand
  ! pointer to the pot
  type(pot_type),target :: pot

  CONTAINS
!-------------------------------------------------------------------------------
! deal_scopa() shuffles the deck of cards, deals out the appropriate number of
!   cards to each player, and turns up the appropriate number of cards
!
!   tot_players - number of players in the game (2,4)
!   real_players - number of real people in the game (1-4)
!-------------------------------------------------------------------------------
    SUBROUTINE deal_scopa()
      integer,parameter :: tot_players=2
      integer,parameter :: real_players=1
      character(len=3),parameter :: deck_kind='ita'
      integer,parameter :: cards_up=4
      real :: tmp

      call deck%init(deck_kind)
      call phand%init(cards_hand)
      call chand%init(cards_hand)
      call pot%init(cards_up)
      call deck%shuffle()
      nplayers=real_players
      ntot=tot_players
      ncomps=ntot-nplayers
      call random_number(tmp)
      if(tot_players==2) then
        if(tmp<0.5) then
          spot=1
          comp_spot=2
          print*, "Computer deals, you go first"
        else
          spot=2
          comp_spot=1
          print*, "You deal, computer goes first"
        endif
      else
        print*, "Modifications are required to establish the order for more",&
          " than 2 players"
      endif
      call deck%deal(phand,chand,pot,spot,.true.)
    ENDSUBROUTINE deal_scopa
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! play_scopa() drives the play of the game
!-------------------------------------------------------------------------------
    SUBROUTINE play_scopa()
      integer :: i,j

      ! hand loop
      do i=1,5
        cards_in_hand=3
        ! play loop
        do j=1,3
          if(spot==1) then
            call play_player
            call play_comp
          else
            call play_comp
            call play_player
          endif
          cards_in_hand=cards_in_hand-1
        enddo
      enddo
    ENDSUBROUTINE play_scopa
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! play_player() manages the player's turn
!-------------------------------------------------------------------------------
    SUBROUTINE play_player()
      integer :: i

      print*, "Which card would you like to play?"
      read(*,*) i
      if(i<1.or.i>cards_in_hand) then
        print*, "Invalid entry"
        stop
      else
        call play_card(phand%h_id(i),spot)
      endif
    ENDSUBROUTINE play_player
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! play_comp() manages the computer's turn
!-------------------------------------------------------------------------------
    SUBROUTINE play_comp()

      print*, "The computer is playing",chand%h(1)
      call play_card(chand%h_id(1),comp_spot)
    ENDSUBROUTINE play_comp
!-------------------------------------------------------------------------------
 
!-------------------------------------------------------------------------------
! play_card() applies the card played to the pot
!-------------------------------------------------------------------------------
    SUBROUTINE play_card(card_id,bin)
      integer,intent(in) :: card_id
      integer,intent(in) :: bin

      call pot%check
    ENDSUBROUTINE play_card
!-------------------------------------------------------------------------------
 
ENDMODULE scopa_main
