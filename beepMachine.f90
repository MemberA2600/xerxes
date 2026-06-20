MODULE beepMachine

    USE WINTERACTER
    USE RESID 
    USE WINAPIS
    USE debugWindow
    USE engineConstants  
    USE subs

    IMPLICIT NONE

    PRIVATE
    PUBLIC   :: beepInit, initChannel, loadSoundEffect, genBeep, &
              & generateTS, playTS, saveTS, fillChannel, playChannels

    Integer, parameter                     :: NUM_OF_CHANNELS = 2,                              &
                                            & MAX_LEN = 30, MIN_FREQ = 37, MAX_FREQ = 32767,    &
                                            & MIN_DUR = 1 , MAX_DUR  = 100,                     &
                                            & MAX_HEAR = 5000, MIN_HEAR = 75, MAX_MULTI = 1500, &
                                            & MIDDLE = 2500      

    Integer                                :: NumOfEffects    = 0, maxNumOfEffects = 0, current = 0

    TYPE SoundEffect
        character(20)                      :: name
        Integer                            :: length
        Integer, dimension(:), allocatable :: freq, dur
    END TYPE

    Type BeepChannel
        Logical                            :: playing = .FALSE., hasSound = .FALSE.
        Integer                            :: ind, maxInd
        Integer, dimension(:), allocatable :: freq, dur
        !TYPE(CounterTimer)                 :: timer

        contains
        procedure  :: initChannel          => initChannel
        procedure  :: fillChannel          => fillChannel 
        procedure  :: playChannel          => playChannel 

    End Type

    type(BeepChannel), dimension(NUM_OF_CHANNELS) :: beeper
    type(SoundEffect), dimension(:), allocatable  :: soundeffects
    type(SoundEffect)                             :: tempEffect

    contains

    subroutine BeepInit()
        integer(2)           :: ind
    
        do ind = 1, NUM_OF_CHANNELS, 1
           call beeper(ind)%initChannel()            
        end do

        current = 0

    end subroutine

    subroutine initChannel(this)
        class(BeepChannel), intent(inout) :: this
        integer(2)           :: rc

        this%playing   = .FALSE.
        this%hasSound  = .FALSE.    
        this%ind       = 0
        this%maxInd    = 0

        if (allocated(this%freq)) then
            deallocate(this%freq, stat = rc)
            if (rc /= 0) call displayDebug("Failed to deallocate frequency array!")

            deallocate(this%dur, stat = rc)
            if (rc /= 0) call displayDebug("Failed to deallocate duration array!")

        end if

    end subroutine

    subroutine fillChannel(this, se)
        class(BeepChannel), intent(inout) :: this
        type(SoundEffect)                 :: se
        integer(2)                        :: rc, ind
        !character(40)                     :: msgString

        this%maxInd    = se%length

        allocate(this%freq(this%maxInd), stat = rc)
        if (rc /= 0) call displayDebug("Failed to deallocate frequency array!")

        allocate(this%dur(this%maxInd), stat = rc)
        if (rc /= 0) call displayDebug("Failed to deallocate duration array!")

        do ind = 1, this%maxInd, 1

           this%freq(ind)   = se%freq (ind)
           this%dur (ind)   = se%dur  (ind)

           !write(msgString, '("Freq: ", I0, " | Dur: ", I0)')     &
           !                    this%freq(ind), this%dur (ind)  

           !call displayDebug(msgString)   

        end do

        this%hasSound = .TRUE.

    end subroutine

    function playChannel(this) result(rc)
        class(BeepChannel), intent(inout) :: this
        integer(2)                        :: rc
        !character(40)                     :: msgString

        rc = 0
    
        if (this%hasSound .EQV. .FALSE. .OR. beepPlaying .EQV. .TRUE.) return

        !if (this%ind > 0) then
            !if (this%playing .EQV. .TRUE.) then 
                !if (this%timer%timerEnded() .EQV. .FALSE.) then
                    !return 
                !end if
            !end if
        !end if

        this%playing = .FALSE.
        this%ind     = this%ind + 1
        if (this%ind > this%maxInd) then 
           call this%initChannel()
           return
        end if

        rc = 1

        !write(msgString, '(I0, " | ", I0, " | ", I0)')     &
        !                    this%ind, this%maxind, this%dur(this%ind) 

        !call displayDebug(msgString)  

        !call this%timer%timerStart(this%dur(this%ind))
        this%playing = .TRUE.

    end function 

    subroutine loadSoundEffect(name)
        type(SoundEffect), dimension(:), allocatable  :: soundeffectsTemp
        integer(2)                                    :: rc
        integer                                       :: origMax, ind                 
        character(*)                                  :: name

        NumOfEffects    = NumOfEffects + 1

        if (NumOfEffects .EQ. 0) then  
            maxNumOfEffects = 10
        
            allocate(soundeffects(maxNumOfEffects), stat = rc)
            if (rc /= 0) call displayDebug("Failed to first allocate SoundEffects!")
        else
            if (NumOfEffects > maxNumOfEffects) then
                origMax         = maxNumOfEffects 
                maxNumOfEffects = maxNumOfEffects * 2
                           
                allocate(soundeffectsTemp(maxNumOfEffects), stat = RC)
                if (rc /= 0) call displayDebug("Failed to allocate temp SoundEffects!")

                do ind = 1, origMax, 1
                   ! soundeffectsTemp(ind) = soundeffects(ind) 
                   call SoundEffectSwapper(soundeffectsTemp(ind), soundeffects(ind))
                end do

                deallocate(soundeffects, stat = RC)
                if (rc /= 0) call displayDebug("Failed to deallocate SoundEffects!")
                
                allocate(soundeffects(maxNumOfEffects), stat = RC)
                if (rc /= 0) call displayDebug("Failed to allocate SoundEffects!")

                do ind = 1, origMax, 1
                   ! soundeffects(ind) = soundeffectsTemp(ind)
                   call SoundEffectSwapper(soundeffects(ind), soundeffectsTemp(ind))
                end do

                deallocate(soundeffectsTemp, stat = RC)
                if (rc /= 0) call displayDebug("Failed to deallocate temp SoundEffects again!") 

            end if
        end if

    end subroutine

    subroutine SoundEffectSwapper(to, from)
        type(SoundEffect), intent(inout):: to, from
        integer(2)                      :: rc, ind

        to%name                 = from%name
        to%length               = from%length

        allocate(to%freq(to%length), stat = rc)
        if (rc /= 0) call displayDebug("Failed to allocate 'to' freq!") 

        allocate(to%dur(to%length), stat = rc)
        if (rc /= 0) call displayDebug("Failed to allocate 'to' dur!") 
        
        do ind = 1, to%length, 1
            to%freq (ind) = from%freq (ind)
            to%dur  (ind) = from%dur  (ind)
        end do

        from%name      = ""
        from%length    = 0

        deallocate(from%freq, stat = rc)
        if (rc /= 0) call displayDebug("Failed to deallocate 'from' freq!") 

        deallocate(from%dur, stat = rc)
        if (rc /= 0) call displayDebug("Failed to deallocate 'from' dur!") 

    end subroutine

    subroutine genBeep()
       INTEGER                        :: ITYPE
       TYPE(WIN_MESSAGE)              :: MESSAGE
       character(10)                  :: msgString

       CALL WDialogLoad(IDD_GENBEEP)

       do
          CALL WDialogSelect(IDD_GENBEEP)
          CALL WDialogShow(ITYPE=Modal)     

          if (WinfoDialog(CurrentDialog) == IDD_GENBEEP) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     EXIT
                  CASE(ID_GENBEEP_GEN)
                     call generateTS()                 
                  CASE(ID_GENBEEP_PLAY)
                     call playTS()
                  CASE(ID_GENBEEP_SAVE)
                     call saveTS()
                    
              END SELECT
          end if

       end do 

       CALL WDialogUnLoad()

    END SUBROUTINE

    subroutine generateTS()
       character(40)         :: msgString
       integer(2)            :: rc, ind, theshape

       tempEffect%name      = "No Name Needed"
       theshape             = randInt(0, 2)  
 
       tempEffect%length    = randInt(5, MAX_LEN)

       !if (theShape == 2) tempEffect%length = 1 

       if (allocated(tempEffect%freq)) then
            deallocate(tempEffect%freq, stat = rc)
            if (rc /= 0) call displayDebug("Failed to deallocate tempEffect frequency array!")

            deallocate(tempEffect%dur, stat = rc)
            if (rc /= 0) call displayDebug("Failed to deallocate tempEffect duration array!")
       end if 

       allocate(tempEffect%freq(tempEffect%length), stat = rc) 
       if (rc /= 0) call displayDebug("Failed to allocate tempEffect frequency array!")
 
       allocate(tempEffect%dur(tempEffect%length), stat = rc) 
       if (rc /= 0) call displayDebug("Failed to allocate tempEffect duration array!")

       CALL WDialogFieldState(ID_GENBEEP_PLAY, 1)

       SELECT CASE(theshape)
       CASE(0)     
           call triShape(randInt(MIN_HEAR, MAX_HEAR),  randInt(10, 100), randInt(20, 100), randInt(0, 1), &
                       & randInt(MIDDLE , MAX_HEAR),  randInt(MIN_HEAR, MIDDLE ) ) 

       CASE(1) 

           call linearShape(randInt(MIN_HEAR, MAX_HEAR),  randInt(10, 100), randInt(20, 100), randInt(0, 1), &
                       & randInt(MIDDLE , MAX_HEAR),  randInt(MIN_HEAR, MIDDLE ) ) 

       CASE(2)  
           call vibraShape(randInt(MIDDLE , MAX_HEAR),  randInt(MIN_HEAR, MIDDLE ), randInt(10, 50))
       END SELECT  

       !do ind = 1, tempEffect%length, 1 
       !   tempEffect%freq(ind) = randInt(MIN_FREQ, MAX_FREQ)  
       !   tempEffect%dur (ind) = randInt(MIN_DUR , MAX_DUR ) 
       !end do 
         
    END SUBROUTINE 

    subroutine vibraShape(top, bottom, duration)
         integer(2)    ::  top, bottom, duration
         integer(2)    ::  dir, counter, dur, ind

         counter = tempEffect%length
         ind     = 1
         dur     = duration
         dir     = 0

         if (dur * counter > MAX_MULTI) dur = MAX_MULTI / counter 

         do while (counter > 0)  
           if (dir == 0) then 
              tempEffect%freq(ind) = top
           else
              tempEffect%freq(ind) = bottom
           end if

           tempEffect%dur (ind) = dur

           dir = 1 - dir
           ind     = ind + 1 
           counter = counter - 1  

         end do

    end subroutine

    subroutine linearShape( startf, step, duration, startD, top, bottom)
        integer(2)    :: startf, step, duration, startD, top, bottom
        integer       :: counter, ind, freq, dir, dur
        character(40) :: msgString

        freq = startf
        dir  = startd

        counter = tempEffect%length
        ind     = 1
        dur     = duration

        if (dur * counter > MAX_MULTI) dur = MAX_MULTI / counter 

        do while (counter > 0)  
            
           tempEffect%freq(ind) = freq
           tempEffect%dur (ind) = dur

           if (dir == 0 .AND. (freq + step) > top)    freq = bottom            
           if (dir == 1 .AND. (freq - step) < bottom) freq = top            

           if (dir == 0) then
               freq = freq + step
           else
               freq = freq - step
           end if  

           ind     = ind + 1 
           counter = counter - 1 

        end do

    end subroutine


    subroutine triShape( startf, step, duration, startD, top, bottom)
        integer(2)    :: startf, step, duration, startD, top, bottom
        integer       :: counter, ind, freq, dir, dur
        character(40) :: msgString

        !write(msgString, '("tempEffect%length: ", I0)') tempEffect%length
        !call displayDebug(msgString)

        freq = startf
        dir  = startd

        counter = tempEffect%length
        ind     = 1
        dur     = duration

        if (dur * counter > MAX_MULTI) dur = MAX_MULTI / counter 

        do while (counter > 0)  
            
           tempEffect%freq(ind) = freq
           tempEffect%dur (ind) = dur

           if (dir == 0 .AND. (freq + step) > top)    dir = 1             
           if (dir == 1 .AND. (freq - step) < bottom) dir = 0             

           if (dir == 0) then
               freq = freq + step
           else
               freq = freq - step
           end if  

           ind     = ind + 1 
           counter = counter - 1 

        end do

    end subroutine


    !subroutine testTS() 
    !   integer(2)               :: ind 
    !   character(40)            :: msgString

    !   do ind = 1, tempEffect%length, 1 
    !      write(msgString, '("Y - Freq: ", I0, " | Dur: ", I0)') tempEffect%freq(ind), tempEffect%dur(ind)
    !      call displayDebug(msgString)  
    !   end do 

    !END SUBROUTINE

    subroutine playTS()
       integer(2)               :: ind 
       character(40)            :: msgString

       if (beeper(1)%playing .EQV. .TRUE.) return

      ! do ind = 1, tempEffect%length, 1 
      !    write(msgString, '("Y - Freq: ", I0, " | Dur: ", I0)') tempEffect%freq(ind), tempEffect%dur(ind)
      !    call displayDebug(msgString)  
      ! end do 
  
       CALL addToChannel(tempEffect, 1) 
      
       do  
          CALL playChannels() 
          if (beeper(1)%playing .EQV. .FALSE.) exit
       end do 

       CALL WDialogFieldState(ID_GENBEEP_SAVE, 1)

    END SUBROUTINE

    subroutine saveTS()

    END SUBROUTINE

    subroutine addToChannel(se, c)
        type(SoundEffect), intent(in)       :: se
        integer(2),        intent(in)       :: c
        integer(2)                          :: channel, ind
        logical                             :: allplaying      
        integer, dimension(NUM_OF_CHANNELS) :: left
        !character(40)                       :: msgString

        allplaying = .TRUE.       

        if (c /= 0) then
            channel = c    
        else        
            do ind = 1, NUM_OF_CHANNELS, 1
               if (beeper(ind)%playing .EQV. .FALSE.) then
                   allplaying = .FALSE.
                   channel = ind  
               end if 
               left(ind) = beeper(ind)%maxInd - beeper(ind)%ind
            end do

            if (allplaying .EQV. .TRUE.) then
                channel = minloc(left, dim = 1)
            end if

        end if

        !do ind = 1, se%length, 1

           !write(msgString, '("Freq: ", I0, " | Dur: ", I0)')     &
           !                    se%freq(ind), se%dur (ind)  

           !call displayDebug(msgString)  
        !end do

        call beeper(channel)%initChannel()
        call beeper(channel)%fillChannel(se)

    END SUBROUTINE

    subroutine playChannels()
        USE KERNEL32
        USE WINMM

        integer(2)               :: ind, rc, counter = NUM_OF_CHANNELS
        !character(40)            :: msgString

        counter = NUM_OF_CHANNELS

        !call WindowOutStatusBar(1, 'Hörcsögfarm!') 

        ind = current

        do while (counter > 0)

           ind = ind + 1
           if (ind > NUM_OF_CHANNELS) ind = 1 
           
           rc  = beeper(ind)%playChannel()

           if (rc /= 0) then

               !write(msgString, '("Freq: ", I0, " | Dur: ", I0)')     &
               !                    beeper(ind)%freq(beeper(ind)%ind), &
               !                    beeper(ind)%dur(beeper(ind)%ind)      

               !call displayDebug(msgString)   

               call asyncBeep(                           &
                    beeper(ind)%freq(beeper(ind)%ind),   &
                    beeper(ind)%dur( beeper(ind)%ind)) 
               
               !rc = Beep(beeper(ind)%freq(beeper(ind)%ind), beeper(ind)%dur( beeper(ind)%ind))
               current = ind
               exit  
           end if    

           counter = counter - 1 

        end do

        !call WindowOutStatusBar(1, "Nyúlüreg!") 


    END SUBROUTINE

END MODULE beepMachine
