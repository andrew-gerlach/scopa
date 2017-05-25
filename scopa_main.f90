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

  public :: setup
  public :: next_deal
  public :: play_scopa

  ! number of actual players
  integer,save :: nplayers
  ! number of computer players
  integer,save :: ncomps
  ! total number of players (hands)
  integer,save :: ntot
  ! cards per hand
  integer :: cards_hand=3
  ! player spot in order
  integer,save :: spot
  ! computer spot in order
  integer,save :: comp_spot
  ! number of cards left in the player's hand
  integer :: cards_in_hand
  ! pointer to the card deck (this needs to be a target, not a pointer so that
  ! it can be accessed by a pointer)
  type(deck_type),target,save :: deck
  ! pointer to the player's hand
  type(hand_type),target,save :: phand
  ! pointer to the computer's hand
  type(hand_type),target,save :: chand
  ! pointer to the pot
  type(pot_type),target :: pot
  ! last player to take from pot
  integer :: last_take=0
  ! bin to store taken cards in
  character(len=3),allocatable :: bin(:,:)
  ! bin to store value of take cards in
  integer,allocatable :: bin_val(:,:)
  ! current bin indices
  integer,allocatable :: bi(:)
  ! score
  integer,save :: score(2)=0

  CONTAINS
!-------------------------------------------------------------------------------
! setup() initializes the game and shuffles the deck of cards
!
!   tot_players - number of players in the game (2,4)
!   real_players - number of real people in the game (1-4)
!-------------------------------------------------------------------------------
    SUBROUTINE setup()
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
      if(ntot==2) then
        if(spot==2) then
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
      allocate(bin(2,deck%ncards))
      allocate(bin_val(2,deck%ncards))
      allocate(bi(2))
      bin='   '
      bin_val=0
      bi=1
 
    ENDSUBROUTINE setup
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! reset() shuffles the deck and sets up for next round of play
!-------------------------------------------------------------------------------
    SUBROUTINE next_deal()
      integer,parameter :: cards_up=4

      call pot%init(cards_up)
      call deck%shuffle()
      if(ntot==2) then
        if(spot==2) then
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
      allocate(bin(2,deck%ncards))
      allocate(bin_val(2,deck%ncards))
      bin='   '
      bin_val=0
      bi=1 
 
    ENDSUBROUTINE next_deal
!-------------------------------------------------------------------------------
 
!-------------------------------------------------------------------------------
! play_scopa() drives the play of the game
!-------------------------------------------------------------------------------
    SUBROUTINE play_scopa(game_on)
      logical,intent(inout) :: game_on
      integer :: i,j

      ! hand loop
      do i=1,6
        if(i>1) then
          call deck%deal(phand,chand,pot,spot,.false.)
        else
          call deck%deal(phand,chand,pot,spot,.true.)
        endif
        call show
        cards_in_hand=3
        ! play loop
        do j=1,3
          if(spot==1) then
            call play_player
            call show
            call play_comp
          else
            call play_comp
            call show
            call play_player
          endif
          call show
          cards_in_hand=cards_in_hand-1
        enddo
      enddo
      ! pull remaining cards from the pot to whoever took last
      if(pot%n>0) then
        do i=1,pot%n
          bin(last_take,bi(last_take))=pot%p(i)
          bin_val(last_take,bi(last_take))=pot%p_val(i)
          bi(last_take)=bi(last_take)+1 
        enddo
        pot%n=0
        deallocate(pot%p)
        deallocate(pot%p_val)
      endif
      call show
      call tally_score
      print*, "The score is:",score

      ! check to see if game is over
      if(score(spot)>10 .and. score(spot)>score(comp_spot)) then
        print*, "Player wins!!!"
        game_on=.false.
      endif
      if(score(comp_spot)>10 .and. score(comp_spot)>score(spot)) then
        print*, "Computer wins!!!"
        game_on=.false.
      endif

      ! cleanup
      deallocate(bin)
      deallocate(bin_val)
    ENDSUBROUTINE play_scopa
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! play_player() manages the player's turn
!-------------------------------------------------------------------------------
    SUBROUTINE play_player()
      integer :: i,j
      logical :: card_played=.false.

      print*, "Which card would you like to play?"
      read(*,*) i
      do j=1,3
        if(i<1.or.i>cards_in_hand) then
          print*, "Invalid entry"
        else
          call apply_card(i,spot)
          card_played=.true.
          exit
        endif
      enddo
      if(.not.card_played) stop
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
      ! index of card in hand
      integer,intent(in) :: i
      ! bin position of player
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
            opt=sco(nso)
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
        ! check for scopa
        if(pot%n==0) then
          print*, "SCOPA!!!"
          score(b)=score(b)+1
        endif
        ! add played card to pin
        bin(b,bi(b))=hand%h(i)
        bin_val(b,bi(b))=hand%h_val(i)
        bi(b)=bi(b)+1
        last_take=b
      else
        ! add card to pot
        call pot%add(hand,i)
      endif
      ! remove card from hand
      call hand%pull(i) 

    ENDSUBROUTINE apply_card
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! tally_score() - checks players bins to determine score for the hand
!-------------------------------------------------------------------------------

    SUBROUTINE tally_score()
      integer :: sb(ntot)
      integer :: coins(ntot)
      integer :: prem(ntot)
      integer :: cards(ntot)
      integer :: i,j,k(4)
      character(len=1) :: tmp_ch
      ! point value for card in each suit
      integer,allocatable :: suit_points(:,:,:)
      ! point value conversion, first row is card value, second is point value
      integer :: point_value(0:10)=(/0,16,12,13,14,15,18,21,10,10,10/)

      allocate(suit_points(ntot,deck%nsuits,deck%nnums))
      sb=0
      coins=0
      prem=0
      cards=0
      suit_points=0
      do i=1,ntot
        k=1
        do j=1,deck%ncards
          if(bin_val(i,j)==0) then
            exit
          else
            cards(i)=cards(i)+1
            tmp_ch=bin(i,j)(3:3)
            if(tmp_ch=='D') then
              coins(i)=coins(i)+1
              tmp_ch=bin(i,j)(2:2)
              if(tmp_ch=='7') then
                sb(i)=sb(i)+1
              endif
              suit_points(i,1,k(1))=point_value(bin_val(i,j))
              k(1)=k(1)+1
            elseif(tmp_ch=='H') then
              suit_points(i,2,k(2))=point_value(bin_val(i,j))
              k(2)=k(2)+1
            elseif(tmp_ch=='C') then
              suit_points(i,3,k(3))=point_value(bin_val(i,j))
              k(3)=k(3)+1
            elseif(tmp_ch=='S') then
              suit_points(i,4,k(4))=point_value(bin_val(i,j))
              k(4)=k(4)+1
            else
              print*, "suit not recognized in tally_score!!!"
            endif
          endif
        enddo
        do j=1,4
          prem(i)=prem(i)+maxval(suit_points(i,j,:)) 
        enddo
      enddo
      print*, "Scoring Summary"
      if(spot==1) then
        print*, "                    you        comp"
      else
        print*, "                       comp        you"
      endif
      print*, "----------------------------------------"
      print*, "siete bella: ",sb
      print*, "   coins   : ",coins
      print*, "   cards   : ",cards
      print*, "  premier  : ",prem
      print*, " "
      score=score+sb
      if(coins(1)>coins(2)) then
        score(1)=score(1)+1
      elseif(coins(2)>coins(1)) then
        score(2)=score(2)+1
      endif
      if(cards(1)>cards(2)) then
        score(1)=score(1)+1
      elseif(cards(2)>cards(1)) then
        score(2)=score(2)+1
      endif
      if(prem(1)>prem(2)) then
        score(1)=score(1)+1
      elseif(prem(2)>prem(1)) then
        score(2)=score(2)+1
      endif

    ENDSUBROUTINE tally_score
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! show() - print relevant data to screen
!-------------------------------------------------------------------------------
    SUBROUTINE show()

      print*, " "
      print*, " player  hand is",phand%h(:)
      print*, "computer hand is",chand%h(:)
      print*, " "
      if(allocated(pot%p)) then
        print*, "    pot is:     ",pot%p(:)
      else
        print*, "    pot is:     "
      endif
      print*, " "
      print*, "bins are:"
      print*, bin(1,1:40)
      print*, bin(2,1:40) 
      print*, " "

    ENDSUBROUTINE show
!-------------------------------------------------------------------------------
 
ENDMODULE scopa_main
