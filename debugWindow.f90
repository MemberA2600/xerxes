MODULE debugWindow
   USE WINTERACTER
   USE RESID 
   IMPLICIT NONE

   PRIVATE 
   PUBLIC        :: displayDebug

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


END MODULE debugWindow
