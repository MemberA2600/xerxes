MODULE realTimer

    IMPLICIT NONE

    PRIVATE
    PUBLIC :: ended

    LOGICAL     :: ended = .FALSE.
    
    CONTAINS

    function getMicroseconds() result(us)
        integer(8) :: us
        integer(8) :: rawtime, rate

    
        call system_clock(rawtime, rate)
    
        us = rawtime * 1000000_8 / rate
    end function

END MODULE realTimer
