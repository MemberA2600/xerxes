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
      USE sprite7up
      USE dict  

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

      call initScreenBuff(layerNum)  
      call initBlockMaps (layerNum, wOfScreenBuffer * 2, hOfScreenBuffer * 2)  

      call WMEssageEnable(BorderSelect, Enabled)
      !CALL WMessageEnable(KeyDown,Enabled)

      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrAreaClear() 
      CALL IGrPlotMode(' ')  
      call setSpeed(5)
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

    call setWeather(WEATHER_DAY_RAIN)

    call createSpriteObjSky(       'Bird',  'Bird', 222, 65, TYPE_EMPTY, .TRUE., NO_FILTER, 60)
    call createSpriteObjSky(       'Bird',  'Bird', 444, 401, TYPE_EMPTY, .TRUE., NO_FILTER, 90)

    call createSpriteObjPlayGround('Tree',  'Tree', 425, -60, TYPE_EMPTY, .TRUE., NO_FILTER)
    call createSpriteObjPlayGround('Tree',  'Tree', 575, 190, TYPE_EMPTY, .TRUE., NO_FILTER)
    call createSpriteObjPlayGround('Tree',  'Tree', 201, 10, TYPE_EMPTY, .TRUE., NO_FILTER)
    call createSpriteObjPlayGround('Tree',  'Tree', 289, 320, TYPE_EMPTY, .TRUE., NO_FILTER)

    call createSpriteObjPlayGround('Suika', 'Suika', 250, 100, TYPE_EMPTY, .TRUE., NO_FILTER)
    call createSpriteObjBackground('Grass', 'Grass', NO_FILTER)
    call createSpriteObjPlayGround('Tree',  'Tree', 300, 140, TYPE_EMPTY, .TRUE., NO_FILTER)
    call createSpriteObjPlayGround('Tree',  'Tree', 22, -70, TYPE_EMPTY, .TRUE., NO_FILTER)
    call createSpriteObjPlayGround('Tree',  'Tree', 380, 20, TYPE_EMPTY, .TRUE., NO_FILTER)
    call createSpriteObjPlayGround('Tree',  'Tree', 35, 325, TYPE_EMPTY, .TRUE., NO_FILTER)
    call createSpriteObjSky(       'Bird',  'Bird', 47, 150, TYPE_EMPTY, .TRUE., NO_FILTER, 80)
    call createSpriteObjPlayGround('Tree',  'Tree', 680, 10, TYPE_EMPTY, .TRUE., NO_FILTER)
    call createSpriteObjPlayGround('Tree',  'Tree', 720, 455, TYPE_EMPTY, .TRUE., NO_FILTER)
    call createSpriteObjSky(       'Bird',  'Bird', 688, 322, TYPE_EMPTY, .TRUE., NO_FILTER, 120)
    call createSpriteObjPlayGround('Tree',  'Tree', 666, 510, TYPE_EMPTY, .TRUE., NO_FILTER)
    call createSpriteObjSky(       'Bird',  'Bird', 625, 425, TYPE_EMPTY, .TRUE., NO_FILTER, 120)

!
!   Load the config!
!
     call loadConfig()
     call setMenuLabels()

     call playTIAbyName("StartUp", 0)  
!
!   Main message loop
!
      DO                                 ! Loop until user terminates
        if (firstTime) then
           if (allOpened()) then 
               if (editMode .EQV. .TRUE.) call WMenuSetState(ID_DEV, ItemEnabled, 1)  
               call WMenuSetState(ID_SoundInput, ItemEnabled, 1)  
           end if
        END IF  

        CALL WMessagePeek(ITYPE,MESSAGE)   
        CALL setUpTo256()

        SELECT CASE (ITYPE)
          CASE (TimerExpired) 
            CALL setResolutionMenu() 
            CALL buffer2Real() 

          CASE (BorderSelect,Expose,Resize)
            call buffer2Real()

          CASE (MenuSelect)              ! Menu item selected
            SELECT CASE (MESSAGE%VALUE1)
              CASE (ID_AUTO)             ! Exit program (menu option)
                    call autoSizeScreen()  
              CASE (ID_320x240:ID_2048x1536)  
                    call setScreenSize(MESSAGE%VALUE1) 
                    call saveConfig()  
              CASE (ID_SPEED)  
                    call setSpeedScreen(editMode) 
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
                    call dropImageList()     
                    call getFolder("img", "xxp")
              CASE (ID_DisplayPalette)
                    call displayPalette()  
              CASE (ID_STARTGAME)
                    editMode = .FALSE.
              CASE (ID_ENGLISH:ID_DEUTSCH)
                    call setLang(MESSAGE%VALUE1 - ID_ENGLISH) 
                    call saveConfig() 
                    call setMenuLabels()
          END SELECT 

          CASE (CloseRequest)            ! Close window (e.g. Alt/F4)
            EXIT   

        END SELECT
        if (editMode .EQV. .FALSE.) then 
            call WMenuSetState(ID_DEV, ItemEnabled, 0)  

            intDummy = intDummy + 1

            if (intDummy > 256) intdummy = 1 

            if ((intDummy / 16) > 7) call addToOffset(-1,-1)
            if ((intDummy / 16) < 8) call addToOffset( 1, 1)

            if ( intDummy == 1) call &
                 addTempFiltertoAllByName(LAYER_PLAYGROUND, "Bird", FILTER_YELLOW, FILTER_TIME_2)

            call putSpritesOnBuffer()
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

      subroutine setMenuLabels()
            call WMenuSetString(ID_SoundInput       , trim(getWordInCurrentLang( "settings"           )))
            call WMenuSetString(ID_SCREENSIZE       , trim(getWordInCurrentLang( "resolution"         )))
            call WMenuSetString(ID_SPEED            , trim(getWordInCurrentLang( "gameSpeed"          )))
            call WMenuSetString(ID_SoundSettings    , trim(getWordInCurrentLang( "sfxMusic"           )))
            call WMenuSetString(ID_InputSettings    , trim(getWordInCurrentLang( "keyboardController" )))
            call WMenuSetString(ID_DEV              , trim(getWordInCurrentLang( "development"        )))
            call WMenuSetString(ID_TIA_Noiser       , trim(getWordInCurrentLang( "tiaNoiseMaker"      )))
            call WMenuSetString(ID_VGM2XXA          , trim(getWordInCurrentLang( "vgmToXXA"           )))
            call WMenuSetString(ID_BMP2XXP          , trim(getWordInCurrentLang( "bmpToXXP"           )))
            call WMenuSetString(ID_DisplayPalette   , trim(getWordInCurrentLang( "displayPalette"     )))
            call WMenuSetString(ID_LANG             , trim(getWordInCurrentLang( "language"           )))
            call WMenuSetString(ID_STARTGAME        , trim(getWordInCurrentLang( "startGame"          )))
            call WMenuSetString(ID_auto             , trim(getWordInCurrentLang( "auto"               )))

      end subroutine  

      subroutine loadFolders()
           call getFolder("tia"  , "xxt")
           call getFolder("adlib", "xxa")
           call getFolder("img"  , "xxp")
           call getFolder("dict" , "xxd")
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
            case(IDD_SpeedSetter)
                call testSpeedLoop(editMode)
                if (editMode .EQV. .FALSE.) call putSpritesOnBuffer()

            end select

        end subroutine

      END PROGRAM XERXES

