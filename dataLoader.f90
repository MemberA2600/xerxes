MODULE dataLoader

      USE WINTERACTER
      USE RESID
      USE debugWindow
      USE winAPIs
      use IFPORT
      use engineConstants  

      PRIVATE
      PUBLIC :: getFolder, loadbinary, read4CharFromBin, readIntFromBin, copyBytes, copyBytesHalf, &
                writeChars2Bin, writeBytes2Bin, writeBin2File, bin2Char 

      CONTAINS
      
      subroutine getFolder(folder, extension)
          character(*)                  :: folder, extension  
          TYPE(FILE$INFO)               :: DIR_INFO
          INTEGER(KIND=INT_PTR_KIND( )) :: hndl
          INTEGER                       :: NN,n,i
          character(MAX_PATH_LEN)       :: dirName
            
          !call displayDebug("Search data: " // trim(folder) // '\*.' // (trim(extension)))       

          N = 0
          hndl = FILE$FIRST
          DO 
             NN = GETFILEINFOQQ(trim(folder) // '\*.' // (trim(extension)),DIR_INFO, hndl)
	        IF(hndl.eq.FILE$LAST.or.hndl.eq.FILE$ERROR.or.NN.eq.0)exit
             N = N + 1
		   
             test = ""
             write(dirName,'(I2,2x,A15,2x,I6)')N,dir_info%name,dir_info%length
             call displayDebug(dirName)
          END DO

      end subroutine       

      subroutine writeBin2File(filename, d2, dealloc)
         character(*), intent(in)             :: filename
         integer(1), allocatable              :: d (:)
         integer(2), allocatable              :: d2(:)
         logical                              :: dealloc  
         integer(2)                           :: stat, ind, unit, ios    
         character(40)                        :: test   

         unit = 16

         allocate(d(size(d2)), stat = stat)
         if (stat /= 0) call displayDebug("Failed to allocat 1-BYTE array for file write!") 

         do ind = 1, size(d2), 1
            !write(test, "(I0, ' | ', Z0, ' | ', A)") ind, d2(ind), achar(d2(ind))
            !call displayDebug(test) 
            d(ind) = d2(ind)
         end do

         open(unit=unit, &
              file=filename, &
              access='stream', &
              form='unformatted', &
              status='UNKNOWN', &
              action='write', &
              iostat=ios)

        if (ios /= 0) then
            call displayDebug("Failed to open binary file for write!") 
            s = 0
            return
        end if

        write(unit, iostat = ios) d
        if (ios /= 0) call displayDebug("Failed to write binary file!")      
    
        close(unit, iostat =ios)
        if (ios /= 0) call displayDebug("Failed to close binary file for write!")   

        deallocate(d, stat = stat)
        if (stat /= 0) call displayDebug("Failed to deallocate binary size = 1!")  

        if (dealloc .EQV. .TRUE.) then
            deallocate(d2, stat = stat)
            if (stat /= 0) call displayDebug("Failed to deallocate binary size = 2!")  
        end if

      end subroutine          

      subroutine loadBinary(filename, d2, s)

        character(*), intent(in)             :: filename
        integer(1), allocatable              :: d (:)
        integer(2), allocatable, intent(out) :: d2(:)

        integer(4), intent(out)              :: s
    
        integer :: unit, ios, stat, ind
    
        unit = 17

        inquire(file=filename, size=s)
    
        allocate(d(s), stat = stat)
        if (stat /= 0) call displayDebug("Failed to allocate binary size = 1!")      

        open(newunit=unit, &
             file=filename, &
             access='stream', &
             form='unformatted', &
             status='old', &
             action='read', &
             iostat=ios)
    
        if (ios /= 0) then
            call displayDebug("Failed to open binary file for read!") 
            s = 0
            return
        end if
    
        read(unit, iostat = ios) d
        if (ios /= 0) call displayDebug("Failed to read binary file!")      
    
        close(unit, iostat =ios)
        if (ios /= 0) call displayDebug("Failed to close binary file for read!")      

        allocate(d2(s), stat = stat)
        if (stat /= 0) call displayDebug("Failed to allocate binary size = 2!")      

        do ind = 1, s, 1
           d2(ind) = d(ind) 
        end do 

        deallocate(d, stat = stat)
        if (stat /= 0) call displayDebug("Failed to deallocate binary size = 1!")      

      end subroutine

      subroutine read4CharFromBin(d, s, offset, res)
          integer(2), allocatable              :: d (:)
          integer                              :: s
          integer                              :: ind, charInd
          integer     , intent(inout)          :: offset       
          character(4), intent(out)            :: res   
                      
          res     = ""
          charInd = 0  

          do ind = offset, offset + 3, 1
             charInd   = charInd + 1 
             write(res(charInd:charInd), "(A)") char(d(ind))
          end do  

          offset = offset + 4  

      end subroutine

      subroutine readIntFromBin(d, s, offset, res, L)
          integer(2), allocatable              :: d (:)
          integer                              :: s, L
          integer                              :: ind, locInd
          integer, intent(inout)               :: offset       
          integer, intent(out)                 :: res   
          integer(1), dimension(4)             :: temp                       
          !character(16)                        :: test   

          res     = 0
          locInd  = 0  
          temp    = 0     

          do ind = offset, offset + L - 1, 1
             locInd       = locInd + 1 
             temp(locInd) = d(ind)  
             !write(test, "(Z0, ' | ', I0)") d(ind), ind
             !call  displayDebug("Test: " // trim(test))

          end do  

          offset = offset + L
          
          res = transfer(temp, 4)  

      end subroutine

      subroutine copyBytes(fromD, toD, fromI, toI, limit)
         integer(2), allocatable              :: fromD(:)
         integer(2), allocatable, intent(out) :: toD  (:)
         integer                              :: fromI, toI, limit
         integer                              :: stat, ind, ind2
         !character(16)                        :: test   

         if (allocated(toD) .EQV. .FALSE.) then
             if (limit == 0) then   
                 allocate(toD(toI - fromI + 1), stat = stat)
             else
                 allocate(toD(limit), stat = stat)
             end if   

             if (stat /= 0) call displayDebug("Failed to allocate array for copy!")
         end if        

         ind2 = 0
         do ind = fromI, toI, 1
            ind2      = ind2 + 1
            toD(ind2) = fromD(ind)

            !write(test, "(Z0, ' | ', I0)") toD(ind2), ind
            !call  displayDebug("Test: " // trim(test))

            if (ind2 == limit) exit

         end do

      end subroutine

      subroutine copyBytesHalf(fromD, toD)
         integer(2), allocatable              :: fromD(:)
         integer(2), allocatable, intent(out) :: toD  (:)
         integer                              :: stat, ind, ind2
         integer(2)                           :: buffer1, buffer2, buffer
         !character(16)                        :: test   
         logical                              :: buffered

         if (allocated(toD) .EQV. .FALSE.) then
             allocate(toD(size(fromD) / 2), stat = stat)
             if (stat /= 0) call displayDebug("Failed to allocate array for half copy!")
         end if        

         ind2     = 0
         buffered = .FALSE.   
   
         do ind = 1, size(fromD), 1
            if (buffered .EQV. .FALSE.) then
                buffer1 = iand(fromD(ind), int(z'00FF', kind=2))    
                        
            else
                ind2    = ind2 + 1
                buffer2 = iand(fromD(ind), int(z'00FF', kind=2))               

                buffer = ior( ishft(iand(buffer2, int(z'00FF',kind=2)), 8), &
                                    iand(buffer1, int(z'00FF',kind=2)) )


                toD(ind2) = buffer
            end if

            buffered = .NOT. buffered

         end do

      end subroutine   

      subroutine writeChars2Bin(d, word, offset, L)
        integer(2), allocatable, intent(inout) :: d (:) 
        character(*)                           :: word
        integer                                :: offset, L
        integer                                :: ind, ind2

        if (L == 0) L = len_trim(word) 
        ind2 = 0
            
        do ind  = offset, offset + L - 1, 1
           ind2 = ind2 + 1 
           d(ind) = iachar(word(ind2:ind2))
        end do

      end subroutine  

      subroutine writeBytes2Bin(from, to, offset)  
          integer(2), allocatable, intent(in)    :: from (:) 
          integer(2), allocatable, intent(inout) :: to   (:) 
          integer                                :: offset  
          integer                                :: ind, ind2
          
          ind2 = 0    
          do ind = offset, offset + size(from) - 1, 1  
             ind2    = ind2 + 1
             to(ind) = from(ind2)   
          end do  

      end subroutine 

      subroutine bin2Char(c, d, limit, dealloc)  
         logical                                 :: dealloc
         character(NAME_MAX_LEN), intent(inout)  :: c 
         integer(2), allocatable                 :: d (:)
         integer(2)                              :: ind, stat

         c = ""

         do ind = 1, size(d), 1
            if (limit /= 0 .AND. ind > limit) exit 

            c(ind:ind) = achar(d(ind))
         end do   


         if (dealloc .EQV. .TRUE.) then  
           deallocate(d, stat = stat)
           if (stat /= 0) call displayDebug("Failed to deallocate temp for bin2Char!")
         end if

      end subroutine  
END MODULE dataLoader
