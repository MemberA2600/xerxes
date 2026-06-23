MODULE winAPIs

    USE, INTRINSIC :: ISO_C_BINDING
    USE WINTERACTER
    USE RESID
    USE IFWIN
    USE IFWINTY
    USE debugWindow
    USE KERNEL32
    USE WINMM
    USE subs

    PRIVATE
    PUBLIC :: CounterTimer, TimerEnded, TimerRestart, asyncBeep, beepPlaying

    !
    ! Internal Timer Type
    ! 

    TYPE CounterTimer
       
        integer(8) :: freq
        integer(8) :: started, diffCheck

        contains
        procedure  :: timerStart   => timerStart
        procedure  :: timerEnded   => timerEnded
        procedure  :: TimerRestart => TimerRestart

    END TYPE

    !
    ! Beeper Stuff
    ! 

    type T_BEEP_PARAM
        integer(DWORD) :: freq
        integer(DWORD) :: duration
    end type

    LOGICAL            :: beepPlaying = .FALSE.

    CONTAINS

    !
    ! Timer functions
    ! 

     subRoutine timerStart(this, diffCheck)
        class(CounterTimer), intent(inout) :: this    
        integer(8)                         :: diffCheck

        this%diffCheck = diffCheck 
        call this%timerRestart()

     end subRoutine     

     subRoutine timerRestart(this)
        class(CounterTimer), intent(inout) :: this    

        this%started = getTime()

     end subRoutine   

     function TimerEnded(this) result(ended)
        class(CounterTimer), intent(inout) :: this    
        LOGICAL                            :: ended 
        integer(8)                         :: now
        character(100)                     :: text 
        
        now = getTime()

        !write(text, "(A, I0, A, I0, A, I0)") "Start: ", this%started, " | Now: ", now, &
        !                                     " | Diff: ", this%diffCheck 
        !call displayDebug(text) 
        
        ended = (now - this%started) > this%diffCheck

     end function   

    !
    ! Beep Functions
    ! 

   integer(DWORD) function beep_thread(lpParam)
        !DEC$ ATTRIBUTES STDCALL :: beep_thread
        integer(LPVOID), value :: lpParam

        type(T_BEEP_PARAM), pointer :: p
        type(c_ptr) :: cp
        integer(BOOL) :: rc

        cp = transfer(lpParam, cp)
        call c_f_pointer(cp, p)

        rc = Beep(p%freq, p%duration)

        if (rc == 0) then 
            call displayDebug("Failed to Beep!") 
        end if   

        deallocate(p)

        beep_thread = 0_DWORD
        beepPlaying = .FALSE.

    end function beep_thread


    subroutine asyncBeep(freq, duration)
        integer(DWORD), intent(in) :: freq
        integer(DWORD), intent(in) :: duration

        type(T_BEEP_PARAM), pointer :: p
        integer(HANDLE) :: hThread
        integer(DWORD)  :: threadId
        integer(BOOL) :: rc

        beepPlaying = .TRUE.
        allocate(p)

        p%freq     = freq
        p%duration = duration

        hThread = CreateThread( &
            NULL, &
            0_DWORD, &
            LOC(beep_thread), &
            LOC(p), &
            0_DWORD, &
            LOC(threadId) )

        if (hThread /= NULL) then
            rc = CloseHandle(hThread)
            if (rc == 0) then 
                call displayDebug("Failed to Close Beep Thread!") 
            end if   
        end if
    end subroutine asyncBeep

END MODULE winAPIs
