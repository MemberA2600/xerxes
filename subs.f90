MODULE subs

      USE WINTERACTER
      USE RESID
      USE debugWindow  
      USE engineConstants  
      USE screen  

      IMPLICIT NONE

      PRIVATE
      PUBLIC            :: getScreenSize, autoSizeScreen, setResolutionMenu, &
                           getWindowDim, setScreenSize, timer, speed, setSpeed

      CHARACTER(20)     :: msgString
      INTEGER(KIND = 1) :: speed, timer

      CONTAINS  

      SUBROUTINE setSpeed(s)
           INTEGER(KIND = 1) :: s
           speed = s
           timer = s 

           !write(msgString, '("Speed: ",I0)') timer
           !call displayDebug(msgString)  

      END SUBROUTINE setSpeed

      FUNCTION getScreenSize() result(scr)
          INTEGER(KIND=2), DIMENSION(2) :: scr
          INTEGER(KIND=1)               :: currMon

          currMon       = WInfoScreen(ScreenMonitor)
          scr(1)        = WInfoMonitor(MonitorWidth ,currMon )
          scr(2)        = WInfoMonitor(MonitorHeight,currMon )

      END FUNCTION  

      SUBROUTINE autoSizeScreen()
          INTEGER(KIND=2), DIMENSION(2) :: scr
          INTEGER(KIND=1)               :: num, lastOK 

      ! write(msgString, '(I0, "|", I0)') standards(1,1), standards(1,2)       
      ! call displayDebug(msgString)   

          scr    = getScreenSize()  

          do num = 1, maxNumberOfScreenSizes, 1
             if (standards(num, 1) >= scr(1) .OR. standards(num, 2) >= scr(2)) exit
             lastOK = num
          end do   

          !call WindowSizePos(width  = standards(lastOK, 1), &
          !                   height = standards(lastOK, 2))  
 
          
          !write(msgString, '(I0)') lastOK + screenSizeStarter
          !call displayDebug(msgString)  
          call setScreenSize(lastOK + ID_AUTO)


      END SUBROUTINE 

      SUBROUTINE setScreenSize(id)
          integer          :: id     

          call WindowSizePos(width  = standards(id - ID_AUTO, 1), &
                             height = standards(id - ID_AUTO, 2))  
 
          call initRealScreen(standards(id - ID_AUTO, 1), &
                              standards(id - ID_AUTO, 2)) 

      END SUBROUTINE 

      FUNCTION getWindowDim() result(win)
          INTEGER(KIND=2), DIMENSION(4) :: win

          win(1) = WinfoWindow(WindowWidth)  
          win(2) = WinfoWindow(WindowHeight)  
          win(3) = WinfoWindow(WindowXPos)
          win(4) = WinfoWindow(WindowYPos)  

          !write(msgString, '(I0, "|", I0, "|", I0, "|", I0)') scr(1), scr(2), win(3), win(4)   
          !call displayDebug(msgString)  

      END FUNCTION 
  
      SUBROUTINE setResolutionMenu()
          INTEGER(KIND=2), DIMENSION(2) :: scr
          INTEGER(KIND=1)               :: num   

          scr    = getScreenSize()

          do num = 1, maxNumberOfScreenSizes, 1
             call WMenuSetState(num + ID_AUTO, iprop=ItemEnabled, ivalue=0)

             ! write(msgString, '(I0, "|", I0, "|", I0, "|", I0)') standards(num,1), standards(num,2), scr(1), scr(2)       
             ! call displayDebug(msgString)   

             if (standards(num, 1) > scr(1) .OR. standards(num, 2) > scr(2)) cycle
 
             call WMenuSetState(num + ID_AUTO, iprop=ItemEnabled, ivalue=1)

          end do    

      END SUBROUTINE   

END MODULE subs
