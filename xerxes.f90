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
      USE DATALOADER
      USE KERNEL32
      USE WINMM
      USE wavePlayer  
      USE TIA
      USE folderParser
      USE vgm  
      USE adlib  
      USE threadMaster
      use IFWIN

      IMPLICIT NONE
!
! Declare variables to be returned by WMessage
!
      INTEGER                        :: ITYPE
      TYPE(WIN_MESSAGE)              :: MESSAGE
      INTEGER(KIND=2), DIMENSION (2) :: scr
      LOGICAL                        :: editMode
      CHARACTER(20)                  :: msgString
      INTEGER                        :: intDummy, beepF, stat
        
      !CHARACTER(255)                 :: fname  

      !INTEGER(2), dimension(:), allocatable :: tiaTestData  
      !TYPE(TIASfx)                          :: tester  
!
! Initialise Winteracter
!

      inquire(file="xerxes.f90", exist=editMode)
      if (editMode .EQV. .FALSE.) call WMenuSetState(ID_DEV, ItemEnabled, 0)  

      CALL setCWD()  

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

      call WMEssageEnable(BorderSelect, Enabled)

      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrAreaClear() 
      CALL IGrPlotMode(' ')  
      call setSpeed(1)
      CALL WMessageTimer(1000/MFPS,IREPEAT=Enabled)  
      call WindowClear(RGB=RGB_BLACK)

      call random_seed() 
      if (editMode .EQV. .FALSE.) call WMenuSetState(ID_DEV, ItemEnabled, 0)  

      call initWavChannels()
      call getFolder("tia", "xxt")
!
!   Start threads
!
      call initThreadList()
      call addThread("soundChannelLoop", soundChannelLoopT)   
      call addThread("playAdlib"       , playAdlibT       )   
      call addThread("playMusic"       , playMusicT       )   
      call addThread("checkDialogs"    , checkDialogsT    )   

      do beepF = 400, 1000, 200  
         intDummy = Beep(beepF, 110)
      end do

!
!    Put tests here!  
!
      !call playTIAbyName("Putty", 0)  
      !call openVGM()  
      !call displayDebug(trim(CWD()) // "!!")  

      !write(msgString, '(I0)') shortWaitMask2Code(Z'0D') - Z'70' + 1
      !call displayDebug(msgString) 

!
!   Main message loop
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
              CASE (ID_TIA_Noiser)              
                    call tiaMaker()     
              CASE (ID_VGM2XXA)              
                    call vgmConverter() 

            END SELECT 

          CASE (CloseRequest)            ! Close window (e.g. Alt/F4)
            EXIT   

        END SELECT
        if (editMode .EQV. .FALSE.) then 
            call WMenuSetState(ID_DEV, ItemEnabled, 0)  
            !call runGameLogic()
        end if
        !CALL soundChannelLoop()
        !call playAdlib()

      END DO
      CALL WindowClose()                 ! Remove program window

      call closeAllThreads()

      STOP

      CONTAINS  

      function soundChannelLoopT(lpParameter) result(rc)
          use IFWIN

          integer(LPVOID), value   :: lpParameter
          integer                  :: rc
          character(40), parameter :: name = "soundChannelLoop" 

          !DEC$ ATTRIBUTES STDCALL :: soundChannelLoopT

           do while (isThreadRunning(name) .EQV. .TRUE.)
              call threadThings(name)  
           end do

           rc = 0
        end function

      function playAdlibT(lpParameter) result(rc)
          use IFWIN

          integer(LPVOID), value   :: lpParameter
          integer                  :: rc
          character(40), parameter :: name = "playAdlib" 

          !DEC$ ATTRIBUTES STDCALL :: playAdlibT

           do while (isThreadRunning(name) .EQV. .TRUE.)
              call threadThings(name)  
           end do

           rc = 0
        end function

      function playMusicT(lpParameter) result(rc)
          use IFWIN

          integer(LPVOID), value   :: lpParameter
          integer                  :: rc
          character(40), parameter :: name = "playMusic" 

          !DEC$ ATTRIBUTES STDCALL :: playMusicT

           do while (isThreadRunning(name) .EQV. .TRUE.)
              call threadThings(name)  
           end do

           rc = 0
        end function

      function checkDialogsT(lpParameter) result(rc)
          use IFWIN

          integer(LPVOID), value   :: lpParameter
          integer                  :: rc
          character(40), parameter :: name = "checkDialogs" 

          !DEC$ ATTRIBUTES STDCALL :: checkDialogsT

           do while (isThreadRunning(name) .EQV. .TRUE.)
              call threadThings(name)  
           end do

           rc = 0
        end function

        subroutine threadThings(name)
          character(*)              :: name 

          if (getThreadCommand(name) == PAUSE_COMMAND) then
              call pauseThread(name, .TRUE.)
          end if

          do while(isThreadPaused(name) .EQV. .TRUE.)
             call sleep(1)
             if (getThreadCommand(name) == UNPAUSE_COMMAND) then
                 call pauseThread(name, .FALSE.)
             end if   
             if (getThreadCommand(name) == FORCE_EXIT) return
          end do   

          if (getThreadCommand(name) /= FORCE_EXIT .AND. &
              getThreadCommand(name) /= PAUSE_COMMAND) then   
              select case(name)
              case("soundChannelLoop")
                    call soundChannelLoop()
              case("playAdlib")  
                    call playAdlib()
              case("playMusic")  
                    call playMusic()
              case("checkDialogs")  
                    call dialogChecker()
              end select  
           end if
        end subroutine

        subroutine dialogChecker()
            integer             :: dialogID

            dialogID = WInfoDialog(CurrentDialog)

            select case(dialogID)
            case(IDD_TIA)    
                call TIAchangeSavePlay()
            case(IDD_VGM2XXA)
                call setConverterFields()
            end select

        end subroutine

      END PROGRAM XERXES
