MODULE dataLoader

      USE WINTERACTER
      USE RESID
      USE debugWindow
      USE winAPIs
      use IFPORT
      use engineConstants  
      use gzip_mod  

      IMPLICIT NONE  

      PRIVATE
      PUBLIC :: loadbinary, read4CharFromBin, readIntFromBin, copyBytes, copyBytesHalf, &
                writeChars2Bin, writeBytes2Bin, writeBin2File, bin2Char, WriteInt8ToData, &
                ReadInt8FromData

      CONTAINS
      
      subroutine writeBin2File(filename, d2, dealloc, compress)
         character(*), intent(in)             :: filename
         integer(1), allocatable              :: d (:), dc(:)
         integer(2), allocatable              :: d2(:)
         logical                              :: dealloc, compress  
         integer(2)                           :: stat, unit, ios  
         integer(8)                           :: ind  
         character(40)                        :: test   
         integer                              :: status

         unit = 16

         allocate(d(size(d2)), stat = stat)
         if (stat /= 0) call displayDebug("Failed to allocat 1-BYTE array for file write!") 

         do ind = 1, size(d2), 1
            !write(test, "(I0, ' | ', Z0, ' | ', A)") ind, d2(ind), achar(d2(ind))
            !call displayDebug(test) 
            d(ind) = iand(d2(ind), Z'00FF')
         end do

         open(unit=unit, &
              file=filename, &
              access='stream', &
              form='unformatted', &
              status='REPLACE', &
              action='write', &
              iostat=ios)


        if (ios /= 0) then
            call displayDebug("Failed to open binary file for write!") 
            return
        end if


        if (compress  .EQV. .FALSE.) then
            write(unit, iostat = ios) d

        else
            call gzip_compress(d, dc, status)
            write(unit, iostat = ios) dc
            
            deallocate(dc, stat = stat)
            if (stat /= 0) call displayDebug("Failed to deallocate compressed!")

        end if

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

      subroutine loadBinary(filename, d2, s, uncompress)

        character(*), intent(in)             :: filename
        integer(1), allocatable              :: d (:), d3(:)
        integer(2), allocatable, intent(out) :: d2(:)
        logical                              :: uncompress
        integer(8), intent(out)              :: s
    
        integer :: unit, ios, stat, ind
        character(40)                        :: test    

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
            return
        end if
    
        read(unit, iostat = ios) d
        if (ios /= 0) call displayDebug("Failed to read binary file!")      
    
        close(unit, iostat =ios)
        if (ios /= 0) call displayDebug("Failed to close binary file for read!")      

        if (uncompress .EQV. .TRUE.) then
            call gzip_uncompress(d, d3, stat)

            !write(test, '(I0, " | ",I0)') stat, size(d3)  
            !call displaydebug(test)

            if (stat /= 0) call displayDebug("Failed to decompress binary!")

            s = size(d3)      
        end if

        allocate(d2(s), stat = stat)
        if (stat /= 0) call displayDebug("Failed to allocate binary size = 2!")      

        do ind = 1, s, 1
           if (allocated(d3)) then
               if (d3(ind) >= 0) then 
                   d2(ind) = d3(ind) 
               else
                   d2(ind) = d3(ind) + 256
               end if  
           else
               if (d(ind) >= 0) then 
                   d2(ind) = d(ind) 
               else
                   d2(ind) = d(ind) + 256
               end if  
           end if 
        end do 

        deallocate(d, stat = stat)
        if (stat /= 0) call displayDebug("Failed to deallocate binary size = 1!")      

        if (allocated(d3)) then
            deallocate(d3, stat = stat)
            if (stat /= 0) call displayDebug("Failed to deallocate uncompressed binary size = 1!")     
        end if

      end subroutine

      subroutine read4CharFromBin(d, s, offset, res)
          integer(2), allocatable              :: d (:)
          integer(8)                           :: s
          integer(8)                           :: ind, charInd
          integer(8)  , intent(inout)          :: offset       
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
          integer(8)                           :: s, L
          integer(8)                           :: ind, locInd
          integer(8), intent(inout)            :: offset       
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
         integer(8)                           :: fromI, toI, limit
         integer                              :: stat
         integer(8)                           :: ind, ind2
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
         integer(8)                           :: stat, ind, ind2
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
        integer(8)                             :: offset, L
        integer(8)                             :: ind, ind2

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
          integer(8)                             :: offset  
          integer(8)                             :: ind, ind2
          
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
         integer(2)                              :: limit

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

      subroutine WriteInt8ToData(d, offset, v)
        
          implicit none
        
          integer(2), intent(inout) :: d(:)
          integer(8), intent(inout) :: offset
          integer(8), intent(in)    :: v
        
          integer(1)                :: bytes(8)
          integer                   :: ind
       
          bytes = transfer(v, bytes)
        
          do ind = 1, 8
             d(offset + ind - 1) = iand(Z'00FF', bytes(ind))
          end do
        
          offset = offset + 8   

      end subroutine

      function ReadInt8FromData(d, offset) result(v)
        
          implicit none
      
          integer(2), intent(in)    :: d(:)
          integer(8), intent(inout) :: offset
        
          integer(8) :: v
          integer(1) :: bytes(8)
          integer    :: ind
        
          do ind = 1, 8
              bytes(ind) = int(d(offset + ind-1), kind=1)
          end do
        
          v = transfer(bytes, v)
          offset = offset + 8   

      end function

END MODULE dataLoader
