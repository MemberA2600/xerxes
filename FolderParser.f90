MODULE FolderParser

      USE WINTERACTER
      USE RESID
      USE debugWindow
      USE winAPIs
      use IFPORT
      use engineConstants  
      use TIA  

      IMPLICIT NONE  

      PRIVATE
      PUBLIC :: getFolder

      CONTAINS
      
      subroutine getFolder(folder, extension)
          character(*)                  :: folder, extension  
          TYPE(FILE$INFO)               :: DIR_INFO
          INTEGER(KIND=INT_PTR_KIND( )) :: hndl
          INTEGER                       :: NN,n,i
          character(MAX_PATH_LEN)       :: dirName
            
          call WindowOutStatusBar(1, "Loading data: " // trim(folder) // '\*.' // (trim(extension)))       

          N = 0
          hndl = FILE$FIRST
          DO 
             NN = GETFILEINFOQQ(trim(folder) // '\*.' // (trim(extension)),DIR_INFO, hndl)
	        IF(hndl.eq.FILE$LAST.or.hndl.eq.FILE$ERROR.or.NN.eq.0)exit
             N = N + 1
          END DO

          select case(extension)
          case("xxt")
              call initTiaList(N)  
          end select

          hndl = FILE$FIRST
    
          N = 0  
          DO 
             NN = GETFILEINFOQQ(trim(folder) // '\*.' // (trim(extension)),DIR_INFO, hndl)
	        IF(hndl.eq.FILE$LAST.or.hndl.eq.FILE$ERROR.or.NN.eq.0)exit
             N = N + 1

             select case(extension)
             case("xxt")
                  call loadTIAFile(N, dir_info%name)
             end select

          END DO

          call WindowOutStatusBar(1, "")       

      end subroutine       

END MODULE FolderParser
