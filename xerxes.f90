!
!     Xerxes Retro Video
!     Game Engine
!
!
      PROGRAM XERXES
!
! Use of the WINTERACTER module is compulsory
!
      USE WINTERACTER
      USE RESID
      USE debugWindow
      USE subs  
      USE colors
      USE engineConstants  
      USE screen
      USE winAPIs
      USE BEEPMACHINE
      USE DATALOADER
      USE KERNEL32
      USE WINMM
      USE wavePlayer  
      USE TIA

      IMPLICIT NONE
!
! Declare variables to be returned by WMessage
!
      INTEGER                        :: ITYPE
      TYPE(WIN_MESSAGE)              :: MESSAGE
      INTEGER(KIND=2), DIMENSION (2) :: scr
      LOGICAL, PARAMETER             :: editMode = .TRUE.
      CHARACTER(20)                  :: msgString
      INTEGER                        :: intDummy, beepF, stat

      !INTEGER(2), dimension(:), allocatable :: tiaTestData  
      !TYPE(TIASfx)                          :: tester  
!
! Initialise Winteracter
!
      CALL WInitialise()
      CALL IGrColourModel(24,ColModelDef)
      CALL WBitmapAlloc(1)
!
! Open the root window with a status bar and menu
!
      CALL WindowOpen(FLAGS =SysMenuOn+MinButton+StatusBar+FixedSizeWin, &
                      MENUID=IDM_MAIN,                                   &
                      TITLE ='Xerxes',                                   &
                      ncol256=128 )

      CALL generateColors() 
      scr  = getScreenSize()
      call autoSizeScreen()  
      call initScreenBuff(1)  

      !testMe = 0
      !testMe = testC(2,1)
          
      !write(msgString, '("Test: ", I0)') testMe       
      !call displayDebug(msgString)     

      call WMEssageEnable(BorderSelect, Enabled)

      !call WindowOutStatusBar(1, 'Hörcsögfarm!')  

      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrAreaClear() 
      CALL IGrPlotMode(' ')  
      call setSpeed(1)
      CALL WMessageTimer(1000/MFPS,IREPEAT=Enabled)  
      call WindowClear(RGB=RGB_BLACK)

      !call getFolder("beeps", "xxb")  
      call random_seed() 
      if (editMode .EQV. .FALSE.) call WMenuSetState(ID_DEV, ItemEnabled, 0)  

      do beepF = 400, 1000, 200  
         intDummy = Beep(beepF, 110)
      end do

      call initWavChannels()
      !call TIA_test(7, 12, 7, 20000)
  
      !allocate(tiaTestData(28), stat = stat)  

      !tiaTestData = (/ &
      !               7, 6, 6, 3, &    
      !               7, 6, 5, 4, &  
      !               7, 6, 4, 5, &  
      !               7, 6, 3, 4, &  
      !               7, 6, 7, 3, &  
      !               7, 6, 8, 2, &  
      !               7, 6, 9, 4  &  
      !               /)      

      !call tester%createTIASfx("Putty", tiaTestData)   
      !call tester%playTIASfx(1)        

      !call TIA_test(6, 6, 4, 5000)
      !call TIA_test(6, 6, 3, 5000)
!
! Main message loop
!
      DO                                 ! Loop until user terminates

        CALL WMessagePeek(ITYPE,MESSAGE)   

        SELECT CASE (ITYPE)
          CASE (TimerExpired) 
            if (timer < 1) then
                CALL setResolutionMenu() 
                CALL buffer2Real() 
                timer = speed
            else
                timer = timer - 1
            end if 

          CASE (BorderSelect,Expose,Resize)
            call buffer2Real()
            timer = speed

          CASE (MenuSelect)              ! Menu item selected
            SELECT CASE (MESSAGE%VALUE1)
              CASE (ID_AUTO)             ! Exit program (menu option)
                    call autoSizeScreen()  
              CASE (ID_320x240:ID_2048x1536)  
                    call setScreenSize(MESSAGE%VALUE1) 
              CASE (ID_SPEED1:ID_SPEED5)  
                    call setSpeed(MESSAGE%VALUE1 - ID_SPEED) 
              CASE (ID_BEEP)              
                    call genBeep()     

            END SELECT 


          CASE (CloseRequest)            ! Close window (e.g. Alt/F4)
            EXIT   

        END SELECT
        !call playChannels()

      END DO
      CALL WindowClose()                 ! Remove program window
      !CALL playsoundClose()

      STOP
      END PROGRAM XERXES
