MODULE debugWindow
   USE WINTERACTER
   USE RESID 
   IMPLICIT NONE

   PRIVATE 
   PUBLIC        :: displayDebug, appendDebug, setdbgBin, writeOutDebug, &
                    displayDebugNum, displayDebugNumTxt

   TYPE(WIN_MESSAGE) :: MESSAGE
   INTEGER           :: ITYPE 

   character(255) :: dbgBin, dbgTxt 

   CONTAINS 

   subroutine setdbgBin(cwd)
         character(*)        :: cwd

         dbgbin = trim(CWD) // '\debug.bin'
         dbgTxt = trim(CWD) // '\debug.txt'

   end subroutine

   SUBROUTINE displayDebugNumTxt(txt, i)
         integer(8)                :: i  
         character(40)             :: t 
         CHARACTER(LEN = *), intent(in)   :: txt

         write(t, "(I0)") i
         call displayDebug(txt // ' ' // t)
   END SUBROUTINE 

   SUBROUTINE displayDebugNum(i)
         integer(8)                :: i  
         character(40)             :: t 
        
         write(t, "(I0)") i
         call displayDebug(t)
   END SUBROUTINE 

   SUBROUTINE displayDebug(txt)
      CHARACTER(LEN = *), intent(in)   :: txt
      
      if (WInfoDialog(CurrentDialog) == 0) then
          CALL WDialogLoad(IDD_DEBUGMSG)
          CALL WDialogSelect(IDD_DEBUGMSG)
        
          CALL WDialogPutString(IDF_DEBUGTXT, txt) 
          CALL WDialogShow(ITYPE=Modal)  
          CALL WDialogUnLoad(IDD_DEBUGMSG)
      else  
          CALL writeOutDebug(txt)        
      end if  
   END SUBROUTINE 

   SUBROUTINE appendDebug(b)
        integer(1)          :: b
        integer(1)          :: rc
        
        !call displayDebug(trim(CWD) // '\debug.bin')

        open(93, FILE = dbgbin, iostat = rc, access='stream', form='unformatted', &
             status='old', position='append')

        if (rc /= 0) open(93, FILE = dbgbin, iostat = rc, access='stream', form='unformatted', &
                          status='replace', position='append')
        if (rc /= 0) call displayDebug("Failed to append a byte to debug!")
        
        write(93) b

        close(93, iostat = rc)
        if (rc /= 0) call displayDebug("Failed to close debug!")

   end subroutine

   SUBROUTINE writeOutDebug(txt)
        CHARACTER(LEN = *), intent(in)   :: txt
        integer(1)          :: rc
        
        !call displayDebug(trim(CWD) // '\debug.bin')

        open(94, FILE = dbgtxt, iostat = rc, status='old', position='append')

        if (rc /= 0) open(94, FILE = dbgtxt, iostat = rc, &
                          status='replace', position='append')
        if (rc /= 0) call displayDebug("Failed to append a byte to debug!")
        
        write(94, "(A)") txt

        close(94, iostat = rc)
        if (rc /= 0) call displayDebug("Failed to close debug!")

   end subroutine

END MODULE debugWindow
