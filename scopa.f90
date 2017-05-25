!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!   Scopa Executable
!   Andrew Gerlach
!   11/8/2016
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM scopa
  use scopa_main

  implicit none
  
  logical :: first_deal=.true.
  logical :: game_on=.true.
  
  do while(game_on)
    if(first_deal) then
      call setup()
      first_deal=.false.
    else
      call next_deal()
    endif
    call play_scopa(game_on)
  enddo

ENDPROGRAM scopa
