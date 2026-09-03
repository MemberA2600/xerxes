MODULE debugWindow
   USE WINTERACTER
   USE RESID 
   IMPLICIT NONE

   PRIVATE 
   PUBLIC        :: displayDebug, appendDebug, setdbgBin, writeOutDebug

   TYPE(WIN_MESSAGE) :: MESSAGE
   INTEGER           :: ITYPE 

   character(255) :: dbgBin, dbgTxt 

   CONTAINS 

   subroutine setdbgBin(cwd)
         character(*)        :: cwd

         dbgbin = trim(CWD) // '\debug.bin'
         dbgTxt = trim(CWD) // '\debug.txt'

   end subroutine

   SUBROUTINE displayDebug(txt)
      CHARACTER(LEN = *), intent(in)   :: txt
      
      CALL WDialogLoad(IDD_DEBUGMSG)
      CALL WDialogSelect(IDD_DEBUGMSG)

      CALL WDialogPutString(IDF_DEBUGTXT, txt) 
      CALL WDialogShow(ITYPE=Modal)  
      CALL WDialogUnLoad()

   END SUBROUTINE 

   SUBROUTINE appendDebug(b)
        integer(1)          :: b
        integer(1)          :: rc
        
        !call displayDebug(trim(CWD) // '\debug.bin')

        open(34, FILE = dbgbin, iostat = rc, access='stream', form='unformatted', &
             status='old', position='append')

        if (rc /= 0) open(34, FILE = dbgbin, iostat = rc, access='stream', form='unformatted', &
                          status='replace', position='append')
        if (rc /= 0) call displayDebug("Failed to append a byte to debug!")
        
        write(34) b

        close(34, iostat = rc)
        if (rc /= 0) call displayDebug("Failed to close debug!")

   end subroutine

   SUBROUTINE writeOutDebug(txt)
        CHARACTER(LEN = *), intent(in)   :: txt
        integer(1)          :: rc
        
        !call displayDebug(trim(CWD) // '\debug.bin')

        open(36, FILE = dbgtxt, iostat = rc, status='old', position='append')

        if (rc /= 0) open(36, FILE = dbgtxt, iostat = rc, &
                          status='replace', position='append')
        if (rc /= 0) call displayDebug("Failed to append a byte to debug!")
        
        write(36, "(A)") txt

        close(36, iostat = rc)
        if (rc /= 0) call displayDebug("Failed to close debug!")

   end subroutine

END MODULE debugWindow
