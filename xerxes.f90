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
      USE wavePlayerWindow  
      USE inpout  
      USE inputReader
      USE config  
      USE imagefactory  

      IMPLICIT NONE
!
! Declare variables to be returned by WMessage
!
      INTEGER                        :: ITYPE
      TYPE(WIN_MESSAGE)              :: MESSAGE
      INTEGER(KIND=2), DIMENSION (2) :: scr
      LOGICAL                        :: editMode, firstTime = .TRUE.
      CHARACTER(20)                  :: msgString
      INTEGER                        :: intDummy, beepF
        
      !CHARACTER(255)                 :: fname  

      !INTEGER(2), dimension(:), allocatable :: tiaTestData  
      !TYPE(TIASfx)                          :: tester  
!
! Initialise Winteracter
!

      inquire(file="xerxes.f90", exist=editMode)
      if (editMode .EQV. .FALSE.) call WMenuSetState(ID_DEV, ItemEnabled, 0)  

      CALL setCWD() 
      CALL setdbgBin(CWD()) 
      CALL start256Timer()  
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

      CALL getCPUInfo()  
      CALL generateColors() 
      scr  = getScreenSize()
      call autoSizeScreen()  
      call initScreenBuff(1)  

      call WMEssageEnable(BorderSelect, Enabled)
      !CALL WMessageEnable(KeyDown,Enabled)

      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrAreaClear() 
      CALL IGrPlotMode(' ')  
      call setSpeed(1)
      CALL WMessageTimer(1000/MFPS,IREPEAT=Enabled)  
      call WindowClear(RGB=RGB_BLACK)

      call random_seed() 

      call openAllOuts()    
      call initWavChannels()
      call loadFolders() 

      call openIODLL() 
      call openJoyDLL()   
!
!     Call config inits  
!
      call resetSoundSettings()  
      call sendTheValues()  
      call restoreKeyButtons()
      call restoreJoyButtons()

!
!   Start threads
!
      call initThreadList()
      call addThread("playAdlib"       , playAdlibT       )   
      call addThread("playMusic"       , playMusicT       )    
      call addThread("allOthers"       , allOthersT)   

!
!    Put tests here!  
!
      !call openVGM()  
      !call displayDebug(trim(CWD()) // "!!")  

      !write(msgString, '(I0)') shortWaitMask2Code(Z'0D') - Z'70' + 1
      !call displayDebug(msgString) 
      !call playAdlibbyName("ultima")   

       !if (inpOutTest() .EQV. .TRUE.) then
       !    call displayDebug("It works!") 
       !else
       !    call displayDebug("It tells you to f*** off!") 
       !end if  

       !call loadBMP() 
!
!   Load the config!
!
     call loadConfig()
     call playTIAbyName("StartUp", 0)  

!
!   Main message loop
!
      DO                                 ! Loop until user terminates
        if (firstTime) then
           if (allOpened()) then 
               if (editMode .EQV. .TRUE.) call WMenuSetState(ID_DEV, ItemEnabled, 1)  
               call WMenuSetState(ID_SCREENSIZE, ItemEnabled, 1)  
               call WMenuSetState(ID_SPEED     , ItemEnabled, 1)  
               call WMenuSetState(ID_SoundInput, ItemEnabled, 1)  
           end if
        END IF  

        CALL WMessagePeek(ITYPE,MESSAGE)   
        CALL setUpTo256()

        SELECT CASE (ITYPE)
          CASE (TimerExpired) 
            if (timer < 1) then
                CALL setResolutionMenu() 
                CALL buffer2Real() 
                timer = speed
            else
                timer = timer - 1
            end if 

          !CASE (KeyDown)
          !      call setLastKey(MESSAGE%VALUE1)  

          CASE (BorderSelect,Expose,Resize)
            call buffer2Real()
            timer = speed

          CASE (MenuSelect)              ! Menu item selected
            SELECT CASE (MESSAGE%VALUE1)
              CASE (ID_AUTO)             ! Exit program (menu option)
                    call autoSizeScreen()  
              CASE (ID_320x240:ID_2048x1536)  
                    call setScreenSize(MESSAGE%VALUE1) 
                    call saveConfig()
              CASE (ID_SPEED1:ID_SPEED5)  
                    call setSpeed(MESSAGE%VALUE1 - ID_SPEED) 
                    call saveConfig()                
              CASE (ID_TIA_Noiser)              
                    call tiaMaker()
                    call dropTIAList() 
                    call getFolder("tia"  , "xxt") 
              CASE (ID_VGM2XXA)              
                    call vgmConverter() 
                    call dropAdlibList()     
                    call getFolder("adlib", "xxa")
              CASE (ID_SoundSettings)              
                    call soundSettings()
                    call saveConfig() 
              CASE (ID_InputSettings)
                    call inputWindow()  
                    call saveConfig()
              CASE (ID_BMP2XXP)
                    call bitMapWindow()  
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

      call closeIODLL()  
      !call closeJoyDLL()  
      call closeAllThreads()
      call closeAllOuts()  

      STOP

      CONTAINS  

      subroutine loadFolders()
           call getFolder("tia"  , "xxt")
           call getFolder("adlib", "xxa")
      end subroutine  

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

      function allOthersT(lpParameter) result(rc)
          use IFWIN

          integer(LPVOID), value   :: lpParameter
          integer                  :: rc
          character(40), parameter :: name = "allOthers" 

          !DEC$ ATTRIBUTES STDCALL :: allOthersT

           do while (isThreadRunning(name) .EQV. .TRUE.)
              call threadThings(name)  
           end do

           rc = 0
        end function

        subroutine threadThings(name)
          use KERNEL32, only: WinSleep => Sleep

          character(*)              :: name 

          if (getThreadCommand(name) == PAUSE_COMMAND) then
              call pauseThread(name, .TRUE.)
          end if

          do while(isThreadPaused(name) .EQV. .TRUE.)
             call WinSleep(100)

             if (getThreadCommand(name) == UNPAUSE_COMMAND) then
                 call pauseThread(name, .FALSE.)
             end if   
             if (getThreadCommand(name) == FORCE_EXIT) return
          end do   

          if (getThreadCommand(name) /= FORCE_EXIT .AND. &
              getThreadCommand(name) /= PAUSE_COMMAND) then   
              select case(name)
              case("playAdlib")  
                    call playAdlib()
              case("playMusic")  
                    call playMusic()
              case("allOthers")  
                    call soundChannelLoop()
                    call readInput()
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
            case(IDD_SoundSettings)
                call checkForSoundSettingUpdates()
            case(IDD_InputSetter)
                call checkOnInputSettings()
            case(IDD_BMP2XXP)
                call checkImageWindowFields()
            end select

        end subroutine

      END PROGRAM XERXES
