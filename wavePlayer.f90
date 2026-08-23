MODULE wavePlayer

    USE, INTRINSIC :: ISO_C_BINDING
    USE WINTERACTER
    USE RESID
    USE IFWIN
    USE IFWINTY
    USE WINAPIS
    USE debugWindow
    USE engineConstants  
    USE subs
    USE KERNEL32
    use WINMM
    use dataloader 
    use threadMaster
    use inpout

    implicit none

    PRIVATE
    PUBLIC :: initWavChannels, stopChannel, TIA2Wav, soundChannelLoop, loadWaveFile, &
              stopMusic, playMusicInit, wavFeedBuffer, playMusic, anySFXPlaying, isMusicPlaying, &
              openAllOuts, closeAllOuts, allOpened  

    integer, parameter                      :: RATE = 44100
    integer, parameter                      :: NUMBER_OF_EFFECTS = 4
    logical                                 :: manualMode 

    integer(1), parameter                   :: numOfBuffs = 20
    logical, dimension(numOfBuffs)          :: bufferFed
    integer(2), dimension(:,:), allocatable :: buff
    integer, dimension(numOfBuffs)          :: buffLen     

    integer(1)                              :: fillIndex, playIndex

    logical                                 :: musicChunkWaiting, musicLastChunk, &
                                               musicFirstChunk, letFirstPlay
    TYPE WaveChannel
        integer                               :: L     
        integer(2), dimension(:), allocatable :: buffer
    
        type(T_WAVEFORMATEX) :: fmt
        type(T_WAVEHDR)      :: hdr 
        integer(HANDLE)      :: hEvent
        integer(HANDLE)      :: hWave
        logical              :: playing, headerSet, toneWaiting, waveOpened
        type(CounterTimer)   :: timer
        
        contains 

        procedure   :: initChannel   => initChannel 
        procedure   :: testSineWave  => testSineWave
        procedure   :: playwav       => playwav
        procedure   :: stopPlaying   => stopPlaying  
        procedure   :: canPlayNext   => canPlayNext
        procedure   :: addWave       => addWave       
        procedure   :: destroyHeader => destroyHeader
        procedure   :: playWavPrep   => playWavPrep
        procedure   :: openOut       => openOut 
        procedure   :: closeOut      => closeOut    
    END TYPE

    Type(WaveChannel)                               :: Music
    Type(WaveChannel), dimension(NUMBER_OF_EFFECTS) :: Effects

    TYPE WaveFile
         character(4) :: riff, wave, fmt, dat
         integer(4)   :: fileSize, fmtChunkSize, sRate, bRate, datSize
         integer(4)   :: form, channels, blockA, bPs   
         integer(2), dimension(:), allocatable :: bytes

    END TYPE

    CONTAINS

    !
    !  Main Player Stuff
    ! 

    function allOpened() result(r)
        logical            :: r
        integer(1)         :: ind

        r = .TRUE.

        do ind = 1, NUMBER_OF_EFFECTS, 1   
            if (effects(ind)%waveOpened .EQV. .FALSE.) then
                r = .FALSE.
                return
            end if
        end do  

        if (music%waveOpened .EQV. .FALSE.) r = .FALSE. 

    end function

    subroutine openAllOuts()
        integer(1)         :: ind

        do ind = 1, NUMBER_OF_EFFECTS, 1   
           call effects(ind)%openOut(.FALSE.)  
        end do  
        call music%openOut(.TRUE.) 

    end subroutine

    subroutine closeAllOuts()
        integer(1)         :: ind, rc

        do ind = 1, NUMBER_OF_EFFECTS, 1   
           call effects(ind)%closeOut() 
        end do  

        rc = waveOutUnprepareHeader( &
             music%hWave, music%hdr, sizeof(music%hdr))
       
        if (rc /= MMSYSERR_NOERROR) call displayDebug("Failed to unprepare music header!")  

        rc = CloseHandle(music%hEvent)
        if (rc /= 1) call displayDebug("Failed to close event!") 

        call music%closeOut() 

    end subroutine



    function isMusicPlaying() result(r)
        logical         :: r

        r = (musicChunkWaiting .EQV. .TRUE. .OR. musicLastChunk .EQV. .TRUE. .OR. &
             musicFirstChunk   .EQV. .TRUE.) .AND. (isChipPlaying()  .EQV. .FALSE.)
 
    end function

    function anySFXPlaying() result(r)
        logical         :: r
        integer         :: ind

        r = .FALSE.

        do ind = 1, NUMBER_OF_EFFECTS, 1
           if (effects(ind)%playing .EQV. .TRUE.) then
               r = .TRUE.
               return 
           end if 

           if (effects(ind)%toneWaiting .EQV. .TRUE.) then
               r = .TRUE.
               return 
           end if 

           if (effects(ind)%canPlayNext("9") .EQV. .FALSE.) then
               r = .TRUE.
               return 
           end if 

        end do 

    end function

    subroutine soundChannelLoop()
        integer         :: ind

        do ind = 1, NUMBER_OF_EFFECTS, 1
           if (effects(ind)%playing       .EQV. .FALSE.) then
               if (effects(ind)%toneWaiting   .EQV. .TRUE.) then     
                   call effects(ind)%playWav() 
                   exit 
               end if 
           end if 
        end do    

    end subroutine

    subroutine addWaveToChannel(d)
        integer(2), dimension(:), allocatable :: d
        integer         :: ind

        do ind = 1, NUMBER_OF_EFFECTS, 1
           if (effects(ind)%canPlayNext("1") .EQV. .TRUE.) then
               if (effects(ind)%toneWaiting   .EQV. .FALSE.) then            
                   call effects(ind)%addWave(d) 
                   exit 
               end if 
           end if 
        end do    

    end subroutine

    subroutine initWavChannels()
        integer           :: ind
            
        do ind = 1, NUMBER_OF_EFFECTS, 1
           call effects(ind)%initChannel(0) 
        end do

        call music%initChannel(0)  
        musicChunkWaiting = .FALSE.
        musicLastChunk    = .FALSE.
        musicFirstChunk   = .FALSE.

    end subroutine

    function musicState() result(state)
        integer(1)        :: state

        state = 0
        if (musicChunkWaiting .EQV. .TRUE.) state = 1
        if (musicLastChunk    .EQV. .TRUE.) state = 2

    end function

    subroutine stopChannel(n)
        integer :: n
        call effects(n)%stopPlaying()
    end subroutine

    subroutine stopMusic()
        if (isMusicPlaying() .EQV. .TRUE.)  then
            if (getLptMode() .EQV. .FALSE.) then
                call pauseWavePlayer(.TRUE.)
                call music%stopPlaying()
                    
                musicChunkWaiting = .FALSE.
                musicLastChunk    = .FALSE.
               !musicInd          = 0
                musicFirstChunk   = .FALSE.
                bufferFed         = .FALSE.
                music%L           = 0

                !if (musicstate() /= 0) call playMusicEnd()
            else
                call initChip(.TRUE.)
            end if
        end if
    end subroutine

    subroutine testSine()
        call effects(1)%testSineWave()
        call effects(1)%playWav()
    end subroutine

    !
    !   WaveChannel stuff
    !

    function canPlayNext(this, c) result(rc)
        class(WaveChannel), intent(inout) :: this
        character                         :: c             
        logical                           :: rc

        !call displayDebug(c)
        rc = this%timer%TimerEnded()
 
    end function

    subroutine loadWaveFile(cNum)
        character(MAX_PATH_LEN) :: fname
        TYPE(WaveFile)          :: wfile
        integer(2), dimension(:), allocatable :: d
        integer(8)            :: s, ind, offset
        integer(8)            :: stat
        !character(40)         :: test
        integer               :: cNum

        fname = FileDialog("", .FALSE., "wave") 
        if (fname /= "") then  
            ! call  displayDebug("Opened file: " // trim(fname) // "!")
            call loadBinary(fname, d, s, .FALSE.)

            if (s > 0) then
                offset = 1
                call read4CharFromBin(d, s, offset, wfile%riff)  
                call readIntFromBin(d, s, offset, wfile%fileSize, 4)  
                call read4CharFromBin(d, s, offset, wfile%wave)  
                call read4CharFromBin(d, s, offset, wfile%fmt)  
                call readIntFromBin(d, s, offset, wfile%fmtChunkSize, 4)  
                call readIntFromBin(d, s, offset, wfile%form, 2)  
                call readIntFromBin(d, s, offset, wfile%channels, 2)  
                call readIntFromBin(d, s, offset, wfile%sRate, 4)  
                call readIntFromBin(d, s, offset, wfile%bRate, 4)  
                call readIntFromBin(d, s, offset, wfile%blockA, 2)  
                call readIntFromBin(d, s, offset, wfile%bps, 2)  

                if (wfile%fmtChunkSize > 16) offset = offset + wfile%fmtChunkSize - 16

                call read4CharFromBin(d, s, offset, wfile%dat)  
                !call  displayDebug("Test: " // trim(wfile%dat))
                call readIntFromBin(d, s, offset, wfile%datSize, 4)  

                if (wfile%riff /= 'RIFF' .OR. wfile%wave /= 'WAVE' .OR. &
                    wfile%fmt  /= 'fmt ' .OR. wfile%dat  /= 'data') call displayDebug("Corrupted Wave File!")

                if (wfile%form /= 1 .OR. wfile%channels /= 1 .OR. wfile%channels /= 1 .OR. &          
                    wfile%sRate /= 44100 .OR. wfile%blockA /= 2 .OR. wfile%bps) &
                    call displayDebug("Requires 44100 Mono 16bit samples!")

                if (allocated(wfile%bytes) .EQV. .TRUE.) then
                    deallocate(wfile%bytes, stat = stat)
                    if (stat /= 0) call  displayDebug("Failed to deallocate wave array!")
                end if

                call copyBytes(d, wfile%bytes, offset, s, wfile%datSize)

                deallocate(d, stat = stat)
                if (stat /= 0) call  displayDebug("Failed to deallocate original array!")

                call copyBytesHalf(wfile%bytes, d)

                if (cNum > 0) then 
                    call effects(cNum)%addWave(d)
                    call effects(cNum)%playWav()
                else
                    call addWaveToChannel(d)
                end if

                deallocate(d, stat = stat)
                if (stat /= 0) call  displayDebug("Failed to deallocate half array!")

            end if 

        end if    

    end subroutine

    subroutine wavFeedBuffer(d, s)
        integer(2), dimension(:), allocatable :: d            
        integer(8)                            :: s
        integer(4)                            :: ind
        !character(40)                         :: t

        fillIndex = fillIndex + 1
        if (fillIndex > numOfBuffs) fillIndex = 1

        do while (bufferFed(fillIndex) .EQV. .TRUE.)
           call sleep(1) 
        end do

        !write(t, "('FillIndex: ', I0)") fillIndex
        !call displayDebug(t)

        do ind = 1, chunkSize, 1
           buff(ind, fillIndex) = d(ind)
        end do

        !do ind = 1, chunkSize, 1
        !   call AppendOne2("geci.raw", d(ind)) 
        !end do
        
        buffLen( fillIndex)      = s
        bufferFed(fillIndex)     = .TRUE. 

        if (letFirstPlay .EQV. .FALSE.) then
           if (fillIndex == numOfBuffs / 2) letFirstPlay = .TRUE.
        end if

    end subroutine

    subroutine playMusic()
        integer(4)                           :: rc
        !character(40)                        :: t

        if (letFirstPlay .EQV. .FALSE.) return

        playIndex = playIndex + 1
        if (playIndex > numOfBuffs) playIndex = 1

        do while(bufferFed(playIndex) .EQV. .FALSE.)
           call sleep(1) 
        end do

        music%hdr%dwBufferLength = buffLen(playIndex) * 2
        musicLastChunk           = .TRUE. 
        music%hdr%lpData         = loc(buff(1, playIndex))  

        !write(t, "('PlayIndex: ', I0)") playIndex
        !call displayDebug(t)

        musicFirstChunk   = .FALSE.

33      rc = waveOutWrite( &
             music%hWave, music%hdr, sizeof(music%hdr))

        if (rc /= MMSYSERR_NOERROR) then
            if (rc == 33) then 
               go to 33  
            else 
               call displayDebug("Failed to write out wave buffer!")   
            end if 
        end if 

        rc = WaitForSingleObject(music%hEvent, INFINITE) 
        if (rc /= 0) call displayDebug("WaitForSingleObject failed!")   

        do 
           if (iand(music%hdr%dwFlags, WHDR_DONE) == 1 .AND. &
               iand(music%hdr%dwFlags, WHDR_INQUEUE) == 0) exit       
        end do

        bufferFed(playIndex) = .FALSE.

    end subroutine

    subroutine playMusicInit(d, s, m)
        integer(2), dimension(:), allocatable :: d            
        integer(4)                            :: rc
        character(40)                         :: t
        integer(8)                            :: s
        logical                               :: m

        call pauseWavePlayer(.TRUE.)
        call music%initChannel(0) 
        !call music%openOut(.TRUE.)

        manualMode = m
        bufferFed  = .FALSE.

        if (s == 0) then
            music%L = size(d)
        else    
            music%L = s
        end if    

        allocate(music%buffer(music%L), stat = rc) 
        if (rc /= 0 ) call displaydebug("Failed to allocate music buffer!")

        if (manualMode .EQV. .FALSE.) music%buffer(1:music%L) = d(1:music%L)

        if (rc /= 0 ) call displaydebug("Failed to allocate music buffer!")
        call music%playWavPrep()

        if (manualMode .EQV. .FALSE.) music%hdr%lpData = loc(music%buffer)

        !call displayDebug("Prep#1")

        call prepWavHeader(music)

        rc = ResetEvent(music%hEvent)
        if (rc /= 1) call displayDebug("ResetEvent failed!")  

        if (allocated(buff)) then
            deallocate(buff, stat = rc) 
            if (rc /= 0 ) call displaydebug("Failed to deallocate BUFF!")
        end if

        allocate(buff(chunkSize, numOfBuffs), stat = rc) 
        if (rc /= 0 ) call displaydebug("Failed to allocate BUFF!")

        musicChunkWaiting = .TRUE.
        musicLastChunk    = .FALSE. 
        !musicInd          = 1 
        musicFirstChunk   = .TRUE.
        letFirstPlay      = .FALSE.
        fillIndex         = 0
        playIndex         = 0

    end subroutine

    subroutine prepWavHeader(this)
        type(WaveChannel), intent(inout)      :: this 
        integer(4)                            :: rc
        character(40)                         :: t

        rc = waveOutPrepareHeader( &
        this%hWave, this%hdr, sizeof(this%hdr))

        write(t, "(I0)") rc

        if (rc /= MMSYSERR_NOERROR) then
            call displayDebug("Failed to prepare wave header! " // trim(t))    
        end if

    end subroutine

!    subroutine playMusicEnd()
!        integer(4)                            :: rc

        !call displayDebug("UnPrep#1")
!        rc = waveOutUnprepareHeader( &
!             music%hWave, music%hdr, sizeof(music%hdr))
       
!        if (rc /= MMSYSERR_NOERROR) call displayDebug("Failed to unprepare header!")   

        !rc = waveOutClose(music%hWave)

        !if (rc /= MMSYSERR_NOERROR) call displayDebug("Failed to close wave out!")   

        !rc = CloseHandle(music%hEvent)
        !if (rc /= 1) call displayDebug("Failed to close event!")   

        !music%hEvent = 0
        !musicInd          = 0 

!        deallocate(buff, stat = rc) 
!        if (rc /= 0 ) call displaydebug("Failed to deallocate BUFF!")

!        if (allocated(music%buffer)) then
!            deallocate(music%buffer, stat = rc) 
!            if (rc /= 0 ) call displaydebug("Failed to deallocate music buffer!")
!        end if 

!    end subroutine

    subroutine TIA2Wav(d, cNum)
        integer(2), dimension(:), allocatable :: d            
        integer                               :: cNum

        if (cNum /= 0) then 
            call effects(cNum)%addWave(d)
            call effects(cNum)%playWav()
        else
            call addWaveToChannel(d)
        end if
    end subroutine

    subroutine addWave(this, d)
        class(WaveChannel), intent(inout)     :: this 
        integer(2), dimension(:), allocatable :: d            
        integer                               :: ind

        call this%initChannel(size(d))

        do ind = 1, size(d), 1
           this%buffer(ind) = d(ind)
        end do

        this%toneWaiting         = .TRUE.

    end subroutine

    subroutine initChannel(this, s)
        class(WaveChannel), intent(inout) :: this             
        integer                           :: s, rc

        if (this%headerSet .EQV. .TRUE.) call this%destroyHeader()

        this%playing             = .FALSE.
        this%L                   = s
        this%toneWaiting         = .FALSE.

        if (allocated(this%buffer)) then 
            deallocate(this%buffer, stat = rc)
            if (rc /= 0) call displayDebug("Failed to deallocate buffer!")
        end if

        if (this%L > 0) then
            allocate(this%buffer(this%L), stat = rc)
            if (rc /= 0) call displayDebug("Failed to allocate buffer!")
        end if

    end subroutine

    subroutine destroyHeader(this)
        class(WaveChannel), intent(inout) :: this             
        integer(2)                        :: rc
        character(25)                     :: test
        
        do 
           if (this%playing .EQV. .FALSE. .AND. this%canPlayNext("2") .EQV. .TRUE. &
              .AND. iand(this%hdr%dwFlags, WHDR_DONE)    == 1  &
              .AND. iand(this%hdr%dwFlags, WHDR_INQUEUE) == 0  &
           ) exit  
        end do

        !call displayDebug("UnPrep#2")
        rc = waveOutUnprepareHeader( &
                this%hWave, this%hdr, sizeof(this%hdr))

        write(test, "(I0)") rc
    
        if (rc /= MMSYSERR_NOERROR) call displayDebug("Failed to unprepare wave header! " // trim(test))   

        this%toneWaiting         = .FALSE.
        this%playing             = .FALSE.
        this%headerSet           = .FALSE.
    end subroutine

    subroutine closeOut(this)    
        class(WaveChannel), intent(inout) :: this             
        integer(2)                        :: rc

        rc = waveOutClose(this%hWave)

        if (rc /= MMSYSERR_NOERROR) call displayDebug("Failed to close wave out!")   

        this%waveOpened = .FALSE.

    end subroutine

    subroutine stopPlaying(this)
        class(WaveChannel), intent(inout) :: this             
        integer(2)                        :: rc

        this%playing             = .FALSE.
        rc = waveOutReset(this%hWave)
        if (rc /= 0 .AND. rc /= 5) call displayDebug("Failed to reset sound!")

        call this%initChannel(0)

    end subroutine

    subroutine testSineWave(this)
        class(WaveChannel), intent(inout) :: this             
        real(8)     :: pi
        integer     :: i
        real(8)     :: t

        if (this%canPlayNext("3") .EQV. .FALSE.) return

        call this%initChannel(RATE)

        pi = 4.0d0 * atan(1.0d0)
    
        ! Generate 1 second of 440 Hz sine
    
        do i = 1, this%L
            t = dble(i-1) / RATE
            this%buffer(i) = int(3000.0d0 * sin(2.0d0*pi*440.0d0*t))
        end do
    
    end subroutine

    subroutine playWavPrep(this)
        class(WaveChannel), intent(inout) :: this             
        integer              :: rc, micro
        ! Format
        character(40)        :: test
        integer              :: ind

        if (this%headerSet .EQV. .TRUE.) call this%destroyHeader()

        this%toneWaiting         = .FALSE.
        this%playing             = .TRUE.
    
        ! Header
    
        this%hdr%dwBytesRecorded = 0
        this%hdr%dwUser          = 0
        this%hdr%dwFlags         = 0
        this%hdr%dwLoops         = 0
        this%hdr%lpNext          = 0
        this%hdr%reserved        = 0

    end subroutine

    subroutine openOut(this, handler)
        class(WaveChannel), intent(inout) :: this             
        logical                           :: handler
        integer(1)                        :: rc 

        this%fmt%wFormatTag      = WAVE_FORMAT_PCM
        this%fmt%nChannels       = 1
        this%fmt%nSamplesPerSec  = RATE
        this%fmt%wBitsPerSample  = 16
        this%fmt%nBlockAlign     = 2
        this%fmt%nAvgBytesPerSec = RATE * 2
        this%fmt%cbSize          = 0
    
        ! Open device
        if (handler .EQV. .TRUE.) then      
            this%hEvent = CreateEvent(NULL, .FALSE., .FALSE., NULL)
    
            rc = waveOutOpen( &
                    this%hWave, &
                    WAVE_MAPPER, &
                    this%fmt, &
                    this%hEvent, 0, CALLBACK_EVENT)    
        else  
            rc = waveOutOpen( &
                    this%hWave, &
                    WAVE_MAPPER, &
                    this%fmt, &
                    0, 0, 0)
        end if    

        if (rc /= MMSYSERR_NOERROR) call displayDebug("Failed to open wave output!")

        this%waveOpened = .TRUE.

    end subroutine

    subroutine playWav(this)
        class(WaveChannel), intent(inout) :: this             
        integer              :: rc, micro
        ! Format
        character(40)        :: test
        integer              :: ind

        call this%playWavPrep()

        this%hdr%lpData          = loc(this%buffer)
        this%hdr%dwBufferLength  = this%L * 2

        !test = ""
        !write(test, "(I0, ' | ', I0)") size(this%buffer), this%hdr%dwBufferLength
        !call displayDebug(test)  

        micro = this%hdr%dwBufferLength * 1000000_8 / RATE
        call this%timer%timerStart(micro)

        !call displayDebug("Prep#2")
        rc = waveOutPrepareHeader( &
                this%hWave, this%hdr, sizeof(this%hdr))

        if (rc /= MMSYSERR_NOERROR) call displayDebug("Failed to prepare wave header!")   

        call prepWavHeader(this)
        this%headerSet = .TRUE.

34      rc = waveOutWrite( &
                this%hWave, this%hdr, sizeof(this%hdr))
       
        if (rc /= MMSYSERR_NOERROR) then
            if (RC == 33) then 
                goto 34
            else 
                call displayDebug("Failed to write out wave buffer!")   
            end if    
        end if

        this%playing             = .FALSE.

    end subroutine

    subroutine pauseWavePlayer(s)
         logical    :: s   
         
         call pauseThread("playMusic", s)
         call pauseThread("playAdlib", s)

         if (s .EQV. .TRUE.) then
             do while (isThreadPaused("playMusic") .EQV. .FALSE.)
                call sleep(1)    
             end do
         end if   

    end subroutine


END MODULE wavePlayer