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
  ! bin to store taken cards in
  character(len=3) :: bin(2,40)
  ! bin to store value of take cards in
  integer :: bin_val(2,40)
  ! current bin indices
  integer :: bi(2)=1

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
      print*, " player  hand is",phand%h(:)
      print*, "computer hand is",chand%h(:)
      print*, "    pot is:     ",pot%p(:)
 
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
            call show
            call play_comp
            call show
          else
            call play_comp
            call show
            call play_player
            call show
          endif
          cards_in_hand=cards_in_hand-1
        enddo
        call deck%deal(phand,chand,pot,spot,.false.)
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
        call apply_card(i,spot)
      endif
    ENDSUBROUTINE play_player
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! play_comp() manages the computer's turn
!-------------------------------------------------------------------------------
    SUBROUTINE play_comp()

      print*, "The computer is playing",chand%h(1)
      call apply_card(1,comp_spot)
    ENDSUBROUTINE play_comp
!-------------------------------------------------------------------------------
 
!-------------------------------------------------------------------------------
! apply_card() applies the card played to the pot
!-------------------------------------------------------------------------------
    SUBROUTINE apply_card(i,b)
      integer,intent(in) :: i
      integer,intent(in) :: b
      ! prety sure this should be a type, but maybe should be class??
      type(hand_type),pointer :: hand
      integer :: j,k,opt
      ! first column is combo number (pot%take_vals row), second is # of cards
      integer,allocatable :: pot_opts(:,:),tmp_opts(:,:)
      ! number of single card options
      integer :: nso
      ! index of single card options
      integer,allocatable :: sco(:),tmp(:)

      ! assign hand pointer to computer or player
      if(b==spot) then
        hand=>phand
      else
        hand=>chand
      endif
      ! check the pot for possible values to take
      call pot%check
      k=0
      ! cycle through the take_vals and add any matches to pot_opts
      do j=1,pot%c
        if(hand%h_val(i)==pot%take_vals(j,1)) then
          k=k+1
          if(allocated(pot_opts)) then
            allocate(tmp_opts(k-1,2))
            tmp_opts=pot_opts
            deallocate(pot_opts)
            allocate(pot_opts(k,2))
            pot_opts=0
            pot_opts(1:k-1,:)=tmp_opts(1:k-1,:)
            deallocate(tmp_opts)
          else
            allocate(pot_opts(k,2))
            pot_opts=0
          endif
          pot_opts(k,1)=j
          pot_opts(k,2)=pot%take_vals(j,10)
        endif
      enddo
      ! determine options if there is a match
      if(k>0) then
        ! only one option available to be taken
        if(k==1) then
          opt=1
          do j=1,pot_opts(k,2)
            bin(b,bi(b))=pot%p(pot%take_vals(pot_opts(k,1),j+1))
            bin_val(b,bi(b))=pot%p_val(pot%take_vals(pot_opts(k,1),j+1))
            bi(b)=bi(b)+1
          enddo
        ! multiple options available to be taken
        else
          nso=0
          ! determine how many single card options are available
          do j=1,k
            if(pot_opts(j,2)==1) then
              nso=nso+1
              if(nso>1) then
                allocate(tmp(nso-1))
                tmp=sco
                deallocate(sco)
                allocate(sco(nso))
                sco=0
                sco(1:nso-1)=tmp(1:nso-1)
                deallocate(tmp)
              else
                allocate(sco(nso))
                sco=0
              endif
              sco(nso)=j
            endif
          enddo
          ! one single card option must be taken
          if(nso==1) then
            opt=nso
            bin(b,bi(b))=pot%p(pot%take_vals(pot_opts(sco(1),1),2))
            bin_val(b,bi(b))=pot%p_val(pot%take_vals(pot_opts(sco(1),1),2))
            bi(b)=bi(b)+1
          ! option must be chosen
          else
            ! player chooses
            if(b==spot) then
              print*, "Select which combination you would like to take"
              ! limit options to single cards if available
              if(nso>1) then
                do j=1,nso
!!! need to have this print cards rather than indices
                  print*, pot%take_vals(pot_opts(sco(j),1),2:1+pot_opts(sco(j),2))
                enddo
              ! no single card options exist, so player may choose any combo
              else
                do j=1,k
                  print*, pot%take_vals(pot_opts(j,1),2:1+pot_opts(j,2))
                enddo
              endif
              read(*,*) opt
            ! computer chooses
            else
              ! keeping it simple for now
              opt=1
            endif
            ! draw from limited set of options if single cards are available
            if(nso>1) then
              if(opt>0.and.opt<=nso) then
                do j=1,pot_opts(opt,2)
                  bin(b,bi(b))=pot%p(pot%take_vals(pot_opts(sco(opt),1),j+1))
                  bin_val(b,bi(b))=pot%p_val(pot%take_vals(pot_opts(sco(opt),1),j+1))
                  bi(b)=bi(b)+1     
                enddo
              else
                print*, "Invalid option!"
              endif
            ! draw from full set of options
            else
              if(opt>0.and.opt<=k) then
                do j=1,pot_opts(opt,2)
                  bin(b,bi(b))=pot%p(pot%take_vals(pot_opts(opt,1),j+1))
                  bin_val(b,bi(b))=pot%p_val(pot%take_vals(pot_opts(opt,1),j+1))
                  bi(b)=bi(b)+1
                enddo
              else
                print*, "Invalid option!"
                stop
              endif
            endif
          endif
        endif
        ! remove cards from pot
        call pot%pull(pot_opts(opt,1))
        ! add played card to pin
        bin(b,bi(b))=hand%h(i)
        bin_val(b,bi(b))=hand%h_val(i)
        bi(b)=bi(b)+1
      else
        ! add card to pot
        call pot%add(hand,i)
      endif
      ! remove card from hand
      call hand%pull(i) 

    ENDSUBROUTINE apply_card
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! show() - print relevant data to screen
!-------------------------------------------------------------------------------
    SUBROUTINE show()

      print*, " player  hand is",phand%h(:)
      print*, "computer hand is",chand%h(:)
      print*, "    pot is:     ",pot%p(:)
      print*, "bins are:"
      print*, bin(1,1:10)
      print*, bin(2,1:20) 

    ENDSUBROUTINE show
!-------------------------------------------------------------------------------
 
ENDMODULE scopa_main
