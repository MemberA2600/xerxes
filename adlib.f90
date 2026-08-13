MODULE adlib
    USE, INTRINSIC :: ISO_C_BINDING
    USE debugWindow
    USE dataLoader
    USE waveplayer
    USE WINTERACTER
    USE RESID
    USE subs
    USE engineConstants
    USE winapis
    USE ifport
    USE opl3_mod
    USE threadMaster
    USE inpout

    implicit none

    private
    public                                     :: initAdlibData, fillAdlibData, playAdlib, &
                                                  getNumOfAdlibBytes, continueToPlayA, saveAdlibData, &
                                                  getAdlibName, shortWaitCode2Mask, shortWaitMask2Code, &
                                                  initAdlibList, loadAdlibHeader, playAdlibbyName, &
                                                  dropAdlibList, playRandomAdlib, changeAdlibVolumeChanger, &
                                                  chipStart 
                                                  
                                                  
    real                                       :: adlibVolumeChanger = 1.0    

    character(40)                              :: test 
    logical                                    :: testDebug = .FALSE.
    integer, parameter                         :: RATE = 44100
    type(CounterTimer)                         :: chipTimer 

    type adlibData
         character(4)                          :: header
         integer(1)                            :: nameLen
         character(NAME_MAX_LEN)               :: name 
         integer(8)                            :: numOfBytes, loopByte
         integer(2), dimension(:), allocatable :: songBytes
         integer(8)                            :: ind  
         integer(2), dimension(:), allocatable :: outBuffer
    end type 

    type(adlibData)                             :: adlibD 
    type(CounterTimer)                          :: counter

    type adlibItem  
         character(MAX_PATH_LEN)               :: fileName 
         character(4)                          :: header
         integer(1)                            :: nameLen
         character(NAME_MAX_LEN)               :: name 
         integer(8)                            :: numOfBytes, loopByte
         integer(8)                            :: firstDataByte, dataLen
    end type

    type(adlibItem), dimension(:), allocatable  :: adlibList  


    integer(2), dimension(:), allocatable       :: outBufferFull
    integer(8)                                  :: bufferIndex, bufferSize, waitMe, last

    integer(2), parameter :: shortWaits(2,16) = reshape((/ &
                                                int(Z'70', kind=2), int(Z'19', kind=2), &
                                                int(Z'71', kind=2), int(Z'1A', kind=2), &
                                                int(Z'72', kind=2), int(Z'1B', kind=2), &
                                                int(Z'73', kind=2), int(Z'1C', kind=2), &
                                                int(Z'74', kind=2), int(Z'1D', kind=2), &
                                                int(Z'75', kind=2), int(Z'1E', kind=2), &
                                                int(Z'76', kind=2), int(Z'1F', kind=2), &
                                                int(Z'77', kind=2), int(Z'D9', kind=2), &
                                                int(Z'78', kind=2), int(Z'DA', kind=2), &
                                                int(Z'79', kind=2), int(Z'DB', kind=2), &
                                                int(Z'7A', kind=2), int(Z'DC', kind=2), &
                                                int(Z'7B', kind=2), int(Z'DD', kind=2), &
                                                int(Z'7C', kind=2), int(Z'DE', kind=2), &
                                                int(Z'7D', kind=2), int(Z'DF', kind=2), &
                                                int(Z'7E', kind=2), int(Z'0C', kind=2), &
                                                int(Z'7F', kind=2), int(Z'0D', kind=2)  &
                                            /), (/2,16/))

    contains
 
    subroutine volChange(buf, last)
        integer(8)                                           :: ind
        integer(8)                                           :: last
        integer(2), dimension(:), allocatable, intent(inout) :: buf

        do ind = 1, last, 1
           buf(ind) = buf(ind) * (adlibVolumeChanger ** 2)
           if (buf(ind) >  32767) buf(ind) =  32767  
           if (buf(ind) < -32768) buf(ind) = -32768 
        end do

    end subroutine

    subroutine changeAdlibVolumeChanger(r)
        real        :: r
        adlibVolumeChanger = r

    end subroutine 

    subroutine playRandomAdlib()

        call playAdlibbyName(adlibList(randInt(1, size(adlibList)))%name)

    end subroutine 

    subroutine dropAdlibList()    
        integer(1)              :: rc
   
        if (allocated(adlibList  ) .EQV. .TRUE.) then

            deallocate(adlibList  , stat = RC)
    
            if (rc /= 0) call displayDebug("Failed to dealloc AdlibList!") 
        end if
    end subroutine

    subroutine initAdlibList(num)
        integer(1)              :: rc
        integer(2)              :: num

        if (allocated(adlibList)) then
            deallocate(adlibList, stat = RC)

            if (rc /= 0) call displayDebug("Failed to deallocate adliblist!")
        end if
    
        allocate(adlibList(num), stat = RC)
        if (rc /= 0) call displayDebug("Failed to allocate adliblist!")

    end subroutine

    subroutine playAdlibbyName(name)
        character(*)                           :: name
        integer(1)                             :: rc
        integer(2)                             :: ind, ind2, offset
        integer(2), dimension(:), allocatable  :: d
        integer(8)                             :: siz

        ind2 = 0

        do ind = 1, size(adlibList), 1
           if (adlibList(ind)%name == name) then 
               ind2 = ind
               exit 
           end if
        end do

        if (ind2 == 0) return
        
        call loadBinary("adlib\" // adlibList(ind)%filename, d, adlibList(ind)%dataLen, .TRUE.)

        offset = adlibList(ind)%firstDataByte

        call initAdlibData()

        adlibD%header       = adlibList(ind2)%header 
        adlibD%name         = adlibList(ind2)%name
        adlibD%nameLen      = adlibList(ind2)%nameLen
        adlibD%numOfBytes   = adlibList(ind2)%numOfBytes   
        adlibD%loopByte     = adlibList(ind2)%loopByte     
               
        allocate(adlibD%songBytes(adlibD%numOfBytes), stat = RC)
        if (RC /= 0) call displayDebug("Failed to allocated Adlib bytes!")
        
        adlibD%songBytes = d(offset:size(d))

        deallocate(d, stat = RC)
        if (RC /= 0) call displayDebug("Failed to deallocate the loaded XXA!")  

        if (getLptMode() .EQV. .FALSE.) then
            call continueToPlayA()
        else
            call chipStart()
        end if

    end subroutine

    subroutine chipStart()
            adlibD%ind = 0
            last       = 0

            call chipTimer%timerInit()
            call initChip(.FALSE.)
    end subroutine

    subroutine loadAdlibHeader(num, fname)
        integer(1)                             :: rc
        integer(2)                             :: num
        character(*)                           :: fname
        integer(8)                             :: siz
        integer(2)                             :: stat
        integer(2), dimension(:), allocatable  :: d, temp
        integer(8)                             :: offset, dataLen

        call loadBinary("adlib\" // fname, d, siz, .TRUE.)

        adlibList(num)%dataLen = size(d)
        
        offset = 1
        call read4CharFromBin(d, siz, offset, adlibList(num)%header)  
        if (adlibList(num)%header /= OPL2_FILE_TYPE) then
            call displayDebug("This is not a valid OPL2 file!")
            return
        end if    
    
        adlibList(num)%nameLen = d(offset)
        offset                  = offset + 1

        call copyBytes(d, temp, offset, &
                       offset + adlibList(num)%nameLen - 1, &
                       adlibList(num)%nameLen) 

        offset = offset + adlibList(num)%nameLen

        call bin2Char(adlibList(num)%name, temp, adlibList(num)%nameLen, .TRUE.) 

        adlibList(num)%numOfBytes = ReadInt8FromData(d, offset) 
        adlibList(num)%loopByte   = ReadInt8FromData(d, offset) 
        adlibList(num)%fileName   = fname    

        adlibList(num)%firstDataByte = offset

        deallocate(d, stat = stat)
        if (stat /= 0) call displayDebug("Failed to deallocate the loaded XXA!")

    end subroutine

    function shortWaitCode2Mask(inp) result(out)
        integer(2)             :: inp
        integer(2)             :: out
        integer(1)             :: ind
        !character(40)          :: test

        out = 0
        do ind = 1, 16, 1
           if (shortWaits(1, ind) == inp) then
               out = shortWaits(2, ind) 
               exit 
           end if 
        end do

        !write(test, "(Z2.2, ' ' Z2.2)") inp, shortWaits(2, ind)
        !call displayDebug(test) 
    end function

    function shortWaitMask2Code(inp) result(out)
        integer(2)             :: inp
        integer(2)             :: out
        integer(1)             :: ind

        out = 0
        do ind = 1, 16, 1
           if (shortWaits(2, ind) == inp) then
               out = shortWaits(1, ind) 
               exit  
           end if 
        end do
    end function

    subroutine saveAdlibData(name, fname)
        integer(2), dimension(:), allocatable :: fullD, compressed
        integer(2)                            :: stat
        character(NAME_MAX_LEN)               :: name
        character(MAX_PATH_LEN)               :: fname
        integer(8)                            :: siz, ind                                   
        logical, parameter                    :: testW = .FALSE.
        !character(40)                         :: ttt

        ! File Type ('OPL2')     : 4 bytes
        ! Name Length (Max: 25)  : 1 byte
        ! Actual Len of Name     
        ! NumberOfReads          : 8 bytes
        ! NumberOfBytes          : 8 bytes
        ! LoopByte               : 8 bytes
        ! Actual Len of Data    

        siz = 4 + 1 + len_trim(name) + 16 + adlibD%numOfBytes

        allocate(fullD(siz), stat = stat)
        if (stat /= 0) call displayDebug("Failed to allocate output bytes for saving XXA!")

        fullD = 0
        call writeChars2Bin(fullD, adlibD%header, 1, 4)
        fullD(5) = len_trim(name) 
        call writeChars2Bin(fullD, trim(name), 6, len_trim(name))

        ind = 6 + len_trim(name)
        call WriteInt8ToData(fullD, ind, adlibd%numOfBytes) 
        call WriteInt8ToData(fullD, ind, adlibd%loopByte) 

        call writeBytes2Bin(adlibd%songBytes, fullD, ind)
        call writeBin2File(fname, fullD, .TRUE., .TRUE.)

        if (testW .EQV. .TRUE.) then
            open( 49, file = "logAdlibH.txt", status="replace", action="write")
            write(49, "(A)")     adlibD%header
            write(49, "(I0)")    len_trim(name)
            write(49, "(A)")     trim(name)
            write(49, "(I0)")    adlibd%numOfBytes
            write(49, "(I0)")    adlibd%loopByte
            close(49)
        end if

    end subroutine

    function getNumOfAdlibBytes() result(r)
         integer(8)     :: r   
         
         r = adlibD%numOfBytes   

    end function    

    subroutine pauseAdlib(s)
         logical    :: s   
         
         call pauseThread("playAdlib", s)
         call pauseThread("playMusic", s)

         if (s .EQV. .TRUE.) then
             do while (isThreadPaused("playAdlib") .EQV. .FALSE.)
                call sleep(1)    
             end do
         end if   

    end subroutine

    subroutine initAdlibData()
         integer(2)           :: stat   

         call pauseAdlib(.TRUE.)

         adlibD%header        = OPL2_FILE_TYPE
         adlibD%nameLen       =  0
         adlibD%name          =  ""   
         adlibD%numOfBytes    =  0          
         adlibD%loopByte      =  0
         adlibD%ind           =  0  

         if (allocated( adlibD%songBytes)) then
             deallocate(adlibD%songBytes, stat = stat)
             if (stat /= 0) call displayDebug("Failed to deallocate Adlib data bytes!")   
         end if   

         !adlibMemory = 0
         call OPL3_Reset(ym3812, RATE)
         bufferIndex = 0 
         bufferSize  = 0 
         waitMe      = 0
         last        = 0

    end subroutine

    function getAdlibName() result(name)
         character(NAME_MAX_LEN)   :: name       

         name = adlibD%name

    end function

    subroutine fillAdlibData(adlibName, songBytes, reads, byteNum, loopByte) 
         character(NAME_MAX_LEN)               :: adlibName       
         integer(2), dimension(:), allocatable :: songBytes
         integer(8)                            :: reads, byteNum, loopByte  
         integer(8)                            :: id         

         integer(2)                            :: stat   
         integer(8)                            :: ind

         if (len_trim(adlibName) > NAME_MAX_LEN) adlibName = adlibName(1:NAME_MAX_LEN)

         adlibD%name          =  adlibName 
         adlibD%nameLen       =  len_trim(adlibName)        
    
         adlibD%numOfBytes    =  byteNum          
         adlibD%loopByte      =  loopByte

         if (allocated( adlibD%songBytes)) then
             deallocate(adlibD%songBytes, stat = stat)
             if (stat /= 0) call displayDebug("Failed to deallocate Adlib data bytes!")   
         end if 

         allocate(adlibD%songBytes(byteNum), stat = stat)        
         if (stat /= 0) then 
             call displayDebug("Failed to allocate Adlib data bytes!")   
         else 
             do ind = 1, byteNum, 1
                adlibD%songBytes(ind) = songBytes(ind)
             end do
         end if   

         call continueToPlayA()
      end subroutine

      subroutine continueToPlayA()  
         integer(2)                            :: stat   

         call stopMusic() 

         last          = 0
         bufferSize    = 0
         bufferIndex   = 1
         waitMe        = 0
         adlibD%ind    = 0  

         if (allocated(outBufferFull)) then
             deallocate(outBufferFull, stat = stat)
             if (stat /= 0) call displayDebug("Failed to deallocate full Adlib buffer!")
         end if

         allocate(outBufferFull(chunkSize), stat = stat)
         if (stat /= 0) call displayDebug("Failed to allocate full Adlib buffer!")

         outBufferFull = 0
         bufferSize    = chunkSize
         last          = 0
         call playMusicInit(outBufferFull, bufferSize, .TRUE.)

         call pauseAdlib(.FALSE.)

    end subroutine

    subroutine playAdlib()
        integer(1)      :: rc

        if (adlibD%numOfBytes > 0) then
            if (last <= adlibD%ind) then
               if (getLptMode() .EQV. .FALSE.) then 
                   call generateAdlib() 
               else 
                   call generateAdlibLPT() 
               end if 
            else
               adlibD%ind = adlibD%loopByte
            end if 
            last = adlibD%ind
        else   
            if (allocated(outBufferFull)) then
                deallocate(outBufferFull, stat = rc)
                if (rc /= 0) call displayDebug("Failed to deallocate full Adlib buffer! #3")
            end if
        end if
    end subroutine


    subroutine buffer2Buffer()
        integer(8)                  :: lastIndex
        character(40)               :: test        

        !call AppendSamples("argh.raw", adlibD%outBuffer)  

        lastIndex = bufferIndex + size(adlibD%outBuffer) - 1
         
        outBufferFull(bufferIndex:lastIndex) = adlibD%outBuffer(1:size(adlibD%outBuffer))
        bufferIndex = bufferIndex + size(adlibD%outBuffer)
   
        if (lastIndex == bufferSize) then
            call volChange(outBufferFull, lastIndex)
            call wavFeedBuffer(outBufferFull, lastIndex)
            bufferIndex = 1
            !call musicLoop()
        end if

    end subroutine         

    subroutine generateAdlib()
        implicit none
        integer(8)          :: waitTime, numOfFrames, maxAllowed
        integer(1)          :: RC
        integer(2)          :: smallWait
        !character(40)       :: tt

        maxAllowed = bufferSize - bufferIndex + 1  

        if (waitMe == 0) then
            if (adlibD%ind  < adlibD%numOfBytes) then
                adlibD%ind  = adlibD%ind + 1
                waitTime    = 0

                test        = ""

                smallWait   = shortWaitMask2Code(adlibD%songBytes(adlibD%ind))
                if (smallWait > 0) then
                    waitTime = smallWait - Z'70' + 1  
                else
    
                    select case(adlibD%songBytes(adlibD%ind))
                    case(Z'05')
                          waitTime   =  adlibD%songBytes(adlibD%ind + 1) + &
                                       (adlibD%songBytes(adlibD%ind + 2) * 256)  
    
                          if (testDebug .EQV. .TRUE.) &  
                          write(test, "(I8.8, '# wait for ', I8.8 ' frames!' )") adlibD%ind, &
                                         adlibD%songBytes(adlibD%ind + 1) + &
                                       (adlibD%songBytes(adlibD%ind + 2) * 256)           
    
                          adlibD%ind = adlibD%ind + 2
                    case(Z'06')
                          waitTime   =  adlibD%songBytes(adlibD%ind + 1)
    
                          if (testDebug .EQV. .TRUE.) &  
                          write(test, "(I8.8, '# wait for ', I8.8 ' frames!' )") adlibD%ind, &
                                         adlibD%songBytes(adlibD%ind + 1)            
    
                          adlibD%ind = adlibD%ind + 1 
                    case(Z'0A') 
                          waitTime = 735               
                    case(Z'0B')
                          waitTime = 882               
                    case(Z'10':Z'1F')
                          call OPL3_WriteRegBuffered(ym3812, adlibD%songBytes(adlibD%ind) + Z'90', Z'00')    
                          !adlibD%ind = adlibD%ind + 1 
    
                    case(Z'D0':Z'DF')
                          !adlibD%ind = adlibD%ind + 1 
                          call OPL3_WriteRegBuffered(ym3812, adlibD%songBytes(adlibD%ind) - Z'20', Z'00')   
                    case default
                          call OPL3_WriteRegBuffered(ym3812, adlibD%songBytes(adlibD%ind),   & 
                                                adlibD%songBytes(adlibD%ind + 1))   
                          adlibD%ind = adlibD%ind + 1 
    
                    end select
                end if                

                if (testDebug .EQV. .TRUE.) then 
                    IF (test /= "") call displayDebug(test)
                end if

                if (allocated(adlibD%outBuffer)) then
                   deallocate(adlibD%outBuffer, stat = rc)  
                   if (rc /= 0) call displayDebug("Failed to deallocate outBuffer of Adlib data! #2") 
                end if

                if (waitTime > 0) then
                    !call counter%timerStart(waitTime)
                    
                    if (waittime > maxAllowed) then
                        waitMe   = waittime - maxAllowed
                        waitTime = maxAllowed
                    end if 

                    !write(test, "('Num of Frames: ', I0)") waitTime 
                    !call displayDebug(test)

                    allocate(adlibD%outBuffer(waitTime ), stat = rc)  
                    if (rc /= 0) call displayDebug("Failed to allocate outBuffer of Adlib data!")    
                    
                    call OPL3_GenerateStreamMono(ym3812, adlibD%outBuffer, waitTime )
                    call buffer2Buffer() 

                end if
            else
                adlibD%ind = adlibD%loopByte
            end if

        else  

            if (allocated(adlibD%outBuffer)) then
                deallocate(adlibD%outBuffer, stat = rc)  
                if (rc /= 0) call displayDebug("Failed to deallocate outBuffer of Adlib data! #3") 
            end if

            waitTime = waitMe
            waitMe   = 0

            if (waittime > maxAllowed) then
                waitMe   = waittime - maxAllowed
                waitTime = maxAllowed
            end if 

            !write(test, "('Num of Frames: ', I0)") waitTime 
            !call displayDebug(test)

            allocate(adlibD%outBuffer(waitTime ), stat = rc)  
            if (rc /= 0) call displayDebug("Failed to allocate outBuffer of Adlib data! #2")    
                    
            call OPL3_GenerateStreamMono(ym3812, adlibD%outBuffer, waitTime )
            call buffer2Buffer() 

        end if 

    end subroutine   
    
    subroutine generateAdlibLPT()
        implicit none
        integer(8)          :: waitTime, numOfFrames, maxAllowed, trueWait
        integer(2)          :: smallWait
        real, parameter     :: oneSample = 1487.0 / 65535.0
        character(40)       :: t

        if (chipTimer%TimerEnded() .EQV. .FALSE.) return

        if (adlibD%ind  < adlibD%numOfBytes) then
            adlibD%ind  = adlibD%ind + 1
            waitTime    = 0

            smallWait   = shortWaitMask2Code(adlibD%songBytes(adlibD%ind))
            if (smallWait > 0) then
                waitTime = smallWait - Z'70' + 1  
            else
  
                select case(adlibD%songBytes(adlibD%ind))
                case(Z'05')
                      waitTime   =  adlibD%songBytes(adlibD%ind + 1) + &
                                   (adlibD%songBytes(adlibD%ind + 2) * 256)  

                      adlibD%ind = adlibD%ind + 2
                case(Z'06')
                      waitTime   =  adlibD%songBytes(adlibD%ind + 1)
    
                      adlibD%ind = adlibD%ind + 1 
                case(Z'0A') 
                      waitTime = 735               
                case(Z'0B')
                      waitTime = 882               
                case(Z'10':Z'1F')
                      call writeReg(adlibD%songBytes(adlibD%ind) + Z'90', Z'00')    
                case(Z'D0':Z'DF')
                      call writeReg(adlibD%songBytes(adlibD%ind) - Z'20', Z'00')   
                case default
                      call writeReg(adlibD%songBytes(adlibD%ind), & 
                                    adlibD%songBytes(adlibD%ind + 1))   
                      adlibD%ind = adlibD%ind + 1 
    
                end select
            end if           

            trueWait = int(1000 * oneSample * waitTime, 8)

            if (trueWait > 0) then 
                !write(t, "(I0, ' | ', I0)") waitTime, trueWait 
                !call displayDebug(t)

                call chipTimer%timerStart(trueWait)
            end if
        else
            adlibD%ind = adlibD%loopByte
        end if

    end subroutine   


END MODULE adlib
