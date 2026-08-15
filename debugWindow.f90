MODULE debugWindow
   USE WINTERACTER
   USE RESID 
   IMPLICIT NONE

   PRIVATE 
   PUBLIC        :: displayDebug, appendDebug

   TYPE(WIN_MESSAGE) :: MESSAGE
   INTEGER           :: ITYPE 

   CONTAINS 

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
        
        open(34, FILE = 'debug.bin', iostat = rc, access='stream', form='unformatted', &
             status='old', position='append')

        if (rc /= 0) open(34, FILE = 'debug.bin', iostat = rc, access='stream', form='unformatted', &
                          status='replace', position='append')
        if (rc /= 0) call displayDebug("Failed to append a byte to debug!")
        
        write(34) b

        close(34, iostat = rc)
        if (rc /= 0) call displayDebug("Failed to close debug!")


   end subroutine

END MODULE debugWindow
