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
    PUBLIC :: CounterTimer, TimerEnded, TimerRestart, &
              AppendSamples, AppendOne, AppendOne2, AppendOne4

    integer, parameter :: i32 = selected_int_kind(9)
    integer, parameter :: i64 = selected_int_kind(18)

    !
    ! Internal Timer Type
    ! 

    TYPE CounterTimer
       
        integer(8) :: freq
        integer(8) :: started, diffCheck

        contains
        procedure  :: timerInit    => timerInit
        procedure  :: timerStart   => timerStart
        procedure  :: timerEnded   => timerEnded
        procedure  :: TimerRestart => TimerRestart

    END TYPE

    CONTAINS

    !
    ! Timer functions
    ! 

     subRoutine timerInit(this)
        class(CounterTimer), intent(inout) :: this    

        this%diffCheck = 0 
        this%started   = 0 

     end subRoutine   

     subRoutine timerStart(this, diffCheck)
        class(CounterTimer), intent(inout) :: this    
        integer(8)                         :: diffCheck

        call this%timerRestart()
        this%diffCheck = diffCheck 

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

        if (this%diffCheck > 0) then         
            now = getTime()

        !write(text, "('Start: ', I0, ' |Now: ', I0, ' |Diff: ', I0, ' |Wait: ', I0, ' | OK:', L)") &
        !              this%started, now, this%diffCheck, this%diffCheck - (now - this%started), &
        !              (now - this%started) > this%diffCheck 
        !call displayDebug(text) 

            ended = (now - this%started) > this%diffCheck
        else
            ended = .TRUE.
        end if    

     end function   

    subroutine AppendSamples(filename, data)
    
        implicit none
    
        character(*), intent(in) :: filename
        integer(2), intent(in)   :: data(:)
    
        integer :: unit
    
        unit = 49
    
        open( &
            unit=unit, &
            file=filename, &
            access='stream', &
            form='unformatted', &
            status='REPLACE', &
            position='append')
    
        write(unit) data
    
        close(unit)
    
    end subroutine

    subroutine AppendOne(filename, data)
    
        implicit none
    
        character(*), intent(in) :: filename
        integer(1), intent(in)   :: data
    
        integer :: unit
    
        unit = 49
    
        open( &
            unit=unit, &
            file=filename, &
            access='stream', &
            form='unformatted', &
            status='REPLACE', &
            position='append')
    
        write(unit) data
    
        close(unit)
    
    end subroutine

    subroutine AppendOne2(filename, data)
    
        implicit none
    
        character(*), intent(in) :: filename
        integer(2), intent(in)   :: data
    
        integer :: unit
    
        unit = 49
    
        open( &
            unit=unit, &
            file=filename, &
            access='stream', &
            form='unformatted', &
            status='REPLACE', &
            position='append')
    
        write(unit) data
    
        close(unit)
    
    end subroutine

    subroutine AppendOne4(filename, data)
    
        implicit none
    
        character(*), intent(in) :: filename
        integer(i32), intent(in)   :: data
    
        integer :: unit
    
        unit = 49
    
        open( &
            unit=unit, &
            file=filename, &
            access='stream', &
            form='unformatted', &
            status='REPLACE', &
            position='append')
    
        write(unit) data
    
        close(unit)
    
    end subroutine


END MODULE winAPIs
