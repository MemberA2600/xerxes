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

      IMPLICIT NONE
!
! Declare variables to be returned by WMessage
!
      INTEGER                        :: ITYPE
      TYPE(WIN_MESSAGE)              :: MESSAGE
      INTEGER(KIND=2), DIMENSION (2) :: scr
      LOGICAL, PARAMETER             :: editMode = .TRUE.
      CHARACTER(20)                  :: msgString

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

      !write(msgString, '(I0, "|", I0)') scr(1), scr(2)       
      !call displayDebug(msgString)     

      call WMEssageEnable(BorderSelect, Enabled)

      !call WindowOutStatusBar(1, 'Hörcsögfarm!')  

      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrAreaClear() 
      CALL IGrPlotMode(' ')  
      call setSpeed(1)
      CALL WMessageTimer(timer,IREPEAT=Enabled)  
      call WindowClear(RGB=RGB_BLACK)
!
! Main message loop
!
      DO                                 ! Loop until user terminates

        CALL WMessagePeek(ITYPE,MESSAGE)   

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
              CASE (ID_SPEED1:ID_SPEED5)  
                    call setSpeed(MESSAGE%VALUE1 - ID_SPEED) 
                   
            END SELECT
          CASE (CloseRequest)            ! Close window (e.g. Alt/F4)
            EXIT     

        END SELECT
      END DO
      CALL WindowClose()                 ! Remove program window
      STOP
      END PROGRAM XERXES
