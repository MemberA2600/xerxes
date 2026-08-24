MODULE subs

      USE, INTRINSIC :: ISO_C_BINDING
      USE WINTERACTER
      USE RESID
      USE debugWindow  
      USE engineConstants  
      USE screen  
      USE IFWIN
      USE IFWINTY
      use IFPORT

      IMPLICIT NONE

      PRIVATE
      PUBLIC            :: getScreenSize, autoSizeScreen, setResolutionMenu, &
                           getWindowDim, setScreenSize, timer, speed, setSpeed, &
                           randInt, getTime, FileDialog, countCharInString, &
                           getNextPoz, dFile, f2bitsTo1Bit, getNullTermString, &
                           CWD, setCWD, getScreenSizeId, getSpeed

      CHARACTER(20)     :: msgString
      INTEGER(KIND = 1) :: speed, timer, screenSize
      CHARACTER(MAX_PATH_LEN) :: CWDReal  

      CONTAINS  

      function CWD() result(r)
           character(MAX_PATH_LEN) :: r
           integer                 :: l    
        
           r = CWDReal 

      end function 
               
      subroutine setCWD()
           character(MAX_PATH_LEN) :: r
           integer                 :: l    
        
           l = GetCurrentDirectory(260, CWDReal)
           CWDReal(l + 1:MAX_PATH_LEN) = ""

      end subroutine 

      function FileDialog(dir, sav, typ) result(fname)
            character(*)                            :: dir
            character(MAX_PATH_LEN)                 :: fname  
            logical                                 :: sav
            integer                                 :: iflags, ind             
            character(25), dimension(5,3)           :: typeList         
            character(4)                            :: typ
            character(40)                           :: title, ftyp 

            typeList(1,1) = 'wave'
            typelist(1,2) = 'Wave Files|*.wav|'
            typelist(1,3) = 'Windows Wave File'

            typeList(2,1) = 'xxt '
            typelist(2,2) = 'TIA Files|*.xxt|'
            typelist(2,3) = 'Xerxes TIA File'

            typeList(3,1) = 'vgm '
            typelist(3,2) = 'VGM Files|*.vgm;*.vgz|'
            typelist(3,3) = 'Video Game Music'

            typeList(4,1) = 'xxa '
            typelist(4,2) = 'Adlib Files|*.xxa|'
            typelist(4,3) = 'Xerxes Adlib File'

            typeList(5,1) = 'bmp '
            typelist(5,2) = 'Bitmap Files|*.bmp|'
            typelist(5,3) = 'Windows Bitmap'

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

      subroutine dFile(fname)
            character(MAX_PATH_LEN)                 :: fname  
            integer                                 :: ios, unit

            unit = 19

            open(unit=unit, file=fname  , &
                 status='old', action='readwrite', iostat=ios)

            if (ios == 0) then
                close(unit, status='delete')
            end if
      end subroutine 

      SUBROUTINE setSpeed(s)
           INTEGER(KIND = 1) :: s
           speed = s
           timer = s 

           !write(msgString, '("Speed: ",I0)') timer
           !call displayDebug(msgString)  

      END SUBROUTINE setSpeed

      function getSpeed() result(r)
           integer(1)           :: r 

           r = speed 

      end function    

      FUNCTION getScreenSize() result(scr)
          INTEGER(KIND=2), DIMENSION(2) :: scr
          INTEGER(KIND=1)               :: currMon

          currMon       = WInfoScreen(ScreenMonitor)
          scr(1)        = WInfoMonitor(MonitorWidth ,currMon )
          scr(2)        = WInfoMonitor(MonitorHeight,currMon )

      END FUNCTION  

      SUBROUTINE autoSizeScreen()
          call setScreenSize(getLastOK() + ID_AUTO)
      END SUBROUTINE 

      FUNCTION getLastOK() result(lastOK)
          INTEGER(KIND=1)               :: num, lastOK 
          INTEGER(KIND=2), DIMENSION(2) :: scr

          scr    = getScreenSize()  

          lastOK = 0

          do num = 1, maxNumberOfScreenSizes, 1
             if (standards(num, 1) >= scr(1) .OR. standards(num, 2) >= scr(2)) exit
             lastOK = num
          end do   

      end function  

      function getScreenSizeId() result(r)
          integer(1)                     :: r  

          r = screenSize - ID_AUTO  

      end function   

      SUBROUTINE setScreenSize(id)
          integer                       :: id     
          INTEGER(KIND=1)               :: lastOK 
          !character(40)                 :: ttt

          lastOK = getLastOK()  
            
          !write(ttt, "(I0)") lastOK  
          !call displayDebug(ttt)  

          if (id > lastOK + ID_AUTO) id = lastOK + ID_AUTO

          screenSize = id  

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

             if (standards(num, 1) >= scr(1) .OR. standards(num, 2) >= scr(2)) cycle
 
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
          xx = x * getTime() 
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

    function f2bitsTo1Bit(v2bits) result (v1bit)
        
        INTEGER(2) :: v2bits
        INTEGER(1) :: v1bit

        if (v2bits >= 128) then
            v1bit = v2bits  - 256
        else
            v1bit = v2bits 
        end if

    end function

    subroutine getNullTermString(s, d, offset, siz)

        integer(2), dimension(:), allocatable    :: d
        integer(8), intent(inout)                :: offset
        integer(8)                               :: siz, ind, num
        character(:), allocatable, intent(inout) :: s
        character(40)                            :: test

        !write(test, "(Z0, ' | ', Z0)") offset, siz

        !call displayDebug(test)       

        do ind = offset, siz, 2
           offset = offset + 2 
           if (d(ind) == 0 .AND. d(ind + 1) == 0) then
               exit 
           end if 
        
           num = d(ind) + d(ind + 1) * 256 
           
           s = s // char(num) 
        end do
    end subroutine


END MODULE subs
