MODULE subs

      USE, INTRINSIC :: ISO_C_BINDING
      USE WINTERACTER
      USE RESID
      USE debugWindow  
      USE engineConstants  
      USE screen  
      USE IFWIN
      USE IFWINTY

      IMPLICIT NONE

      PRIVATE
      PUBLIC            :: getScreenSize, autoSizeScreen, setResolutionMenu, &
                           getWindowDim, setScreenSize, timer, speed, setSpeed, &
                           randInt, getTime, FileDialog, countCharInString, &
                           getNextPoz

      CHARACTER(20)     :: msgString
      INTEGER(KIND = 1) :: speed, timer

      CONTAINS  

      function FileDialog(dir, sav, typ) result(fname)
            character(*)                            :: dir
            character(255)                          :: fname  
            logical                                 :: sav
            integer                                 :: iflags, ind             
            character(25), dimension(1,3)           :: typeList         
            character(4)                            :: typ
            character(40)                           :: title, ftyp 

            typeList(1,1) = "wave"
            typelist(1,2) = "Wave Files (*wav)|*.wav|"
            typelist(1,3) = "Windows Wave File"

            iflags = 8 + 32

            title = ""

            if (sav    .EQV. .TRUE.) then 
                iflags = iflags + 1
                title = "Save"
            else
                title = "Open"
            end if
            
            do ind = 1, size(typeList, 1), 1
            
               if (typeList(ind,1) == typ) then 
                   title = trim(title) // " " // typelist(ind,3)
                   ftyp  = typelist(ind,2)
                   exit 
               end if
            end do

            fname = dir    

            call WSelectFile(trim(ftyp), iflags, fname, trim(title))

            if (WinFoDialog(4) /= CommonOK) fname = ""  

      end function    

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

      function getTime() result(ret)
 
        !class(CounterTimer), intent(inout) :: this    
        real(8)                            :: ret
        type(T_LARGE_INTEGER)              :: t, freq
        integer(BOOL)                      :: rc   
        !character(40)                      :: msgString

        rc = QueryPerformanceFrequency(freq)
        if (rc == 0) then
            call displayDebug("Failed to load Freq!") 
        end if

        rc = QueryPerformanceCounter(t)
        if (rc == 0) then 
            call displayDebug("Failed to load Counter!") 
        end if    

        ret = real(largeToInt64(t), 8) * 1.0d6 / real(largeToInt64(freq), 8)
        !write(msgString, '("ret: ", G0)') ret      
        !call displayDebug(msgString)  

      end function    

     function largeToInt64(li) result(v)
        use ifwin
        type(T_LARGE_INTEGER), intent(in) :: li
        integer(8)                        :: v

        v = int(li%HighPart, 8) * 4294967296_8 + int(li%LowPart, 8)
     end function

     function randInt(low, high) result(r)
          integer, intent(in) :: low, high
          integer             :: r
          real(8)             :: x
          character(60)       :: msgString          
          integer(8)          :: xx  

          call random_number(x)
          xx = x * 368974687435677964 
          r  = mod(abs(xx), high - low + 1) + low  

     end function

    function countCharInString(text, ch) result(c)
        character(*)             :: text
        character                :: ch
        integer                  :: ind, c

        c = 0

        do ind = 1, len_trim(text), 1
           if (text(ind:ind) == ch) c = c + 1 
        end do

     end function

    function getNextPoz(text, ch, startPoz) result(newPoz)
        character(*)             :: text
        character                :: ch
        integer                  :: startPoz
        integer                  :: newPoz
        integer                  :: ind        

        newPoz = -1

        do ind = startPoz, len_trim(text), 1
           if (text(ind:ind) == ch) then        
               newPoz = ind 
               return
           end if                  
        end do

    end function 

END MODULE subs
