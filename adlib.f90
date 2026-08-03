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

    implicit none

    private
    public                                     :: initAdlibData, fillAdlibData, playAdlib
    character(40)                              :: test 
    logical                                    :: testDebug = .FALSE.
    integer(1), parameter                      :: minWait = 0 ! 27 on real hw
    integer, parameter                         :: RATE = 44100

    logical                                    :: loopMe
    type adlibData
         character(4)                          :: header
         integer(2)                            :: nameLen
         character(255)                        :: name 
         integer(8)                            :: numOfReads, numOfBytes, loopByte
         integer(2), dimension(:), allocatable :: songBytes
         integer(8)                            :: ind  
         integer(2), dimension(:), allocatable :: outBuffer
    end type 

    type(adlibData)                             :: adlibD 
    type(CounterTimer)                          :: counter

    integer(2), dimension(:), allocatable       :: outBufferFull
    integer(8)                                  :: bufferIndex, bufferSize, waitMe, last

    contains

    subroutine pauseAdlib(s)
         logical    :: s   
         
         call pauseThread("playAdlib", s)
         if (s .EQV. .TRUE.) then
             do while (isThreadPaused("playAdlib") .EQV. .FALSE.)
                call sleep(1)    
             end do
         end if   

    end subroutine

    subroutine initAdlibData()
         integer(2)           :: stat   

         call pauseAdlib(.TRUE.)

         adlibD%header        = 'xxa '
         adlibD%nameLen       =  0
         adlibD%name          =  ""   
         adlibD%numOfReads    =  0
         adlibD%numOfBytes    =  0          
         adlibD%loopByte      =  0
         loopMe               = .FALSE.

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

    subroutine fillAdlibData(adlibName, songBytes, reads, byteNum, loopByte) 
         character(255)                        :: adlibName       
         integer(2), dimension(:), allocatable :: songBytes
         integer(8)                            :: reads, byteNum, loopByte  
         integer(8)                            :: id         

         integer(2)                            :: stat   
         integer(8)                            :: ind

         adlibD%name          =  adlibName 
         adlibD%nameLen       =  len_trim(adlibName)        
    
         adlibD%numOfReads    =  reads
         adlibD%numOfBytes    =  byteNum          
         adlibD%loopByte      =  loopByte

         if (loopByte > 0) then
             loopMe           = .TRUE.
         else   
             loopMe           = .FALSE.
         end if

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

         last          = 0
         bufferSize    = 0
         bufferIndex   = 1
         waitMe        = 0

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
               call generateAdlib() 
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
            call wavFeedBuffer(outBufferFull, lastIndex)
            bufferIndex = 1
            call musicLoop()
        end if

    end subroutine         

    subroutine generateAdlib()
        implicit none
        integer(8)          :: waitTime, numOfFrames, maxAllowed
        integer(1)          :: RC

        maxAllowed = bufferSize - bufferIndex + 1

        !write(test, "('Left: ', I0)") maxAllowed
        !call displayDebug(test)    

        if (waitMe == 0) then
            if (adlibD%ind <= adlibD%numOfBytes) then
                adlibD%ind  = adlibD%ind + 1
                waitTime    = 0

                test        = ""

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
                case(Z'10':Z'1F')
                      waitTime = minWait
                      call OPL3_WriteRegBuffered(ym3812, adlibD%songBytes(adlibD%ind) + Z'90', Z'00')    
                      !adlibD%ind = adlibD%ind + 1 

                case(Z'D0':Z'DF')
                      waitTime = minWait
                      !adlibD%ind = adlibD%ind + 1 
                      call OPL3_WriteRegBuffered(ym3812, adlibD%songBytes(adlibD%ind) - Z'20', Z'00')   
                case default

                      waitTime = minWait
                      call OPL3_WriteRegBuffered(ym3812, adlibD%songBytes(adlibD%ind),   & 
                                            adlibD%songBytes(adlibD%ind + 1))   
                      adlibD%ind = adlibD%ind + 1 

                end select

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

    
    
END MODULE adlib
