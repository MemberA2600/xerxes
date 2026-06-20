MODULE playSound
    USE, INTRINSIC :: ISO_C_BINDING
    USE WINTERACTER
    USE RESID
    USE debugWindow
  ! use ifwinty
  ! use kernel32

    IMPLICIT NONE

    PRIVATE
    PUBLIC :: playsoundInit, playSoundClose, playsoundInited, currentMS

    LOGICAL :: playsoundInited = .FALSE., playsoundError = .FALSE.
    

    type(c_ptr)    :: waveOut
    type(c_funptr) :: waveOutOpen, waveOutPrepareHeader,                  &
                    & waveOutWrite, waveOutUnprepareHeader, waveOutClose

    type(c_funptr) :: fp_waveOutOpen, fp_waveOutPrepareHeader, fp_waveOutWrite, &
                    & fp_waveOutUnprepareHeader, fp_waveOutClose

    procedure(waveOutOpen_IF), pointer              :: waveOutOpen
    procedure(waveOutPrepareHeader_IF), pointer     :: waveOutPrepareHeader
    procedure(waveOutWrite_IF), pointer             :: waveOutWrite
    procedure(waveOutUnprepareHeader_IF), pointer   :: waveOutUnprepareHeader
    procedure(waveOutClose_IF), pointer             :: waveOutClose

    !DEC$ OBJCOMMENT LIB:"winmm.lib"

    integer, parameter :: CALLBACK_NULL = 0
    integer, parameter :: WAVE_MAPPER   = -1
    integer, parameter :: WAVE_FORMAT_PCM = 1
    integer, parameter :: WHDR_DONE = int(Z'00000001')

    type, bind(C) :: WAVEFORMATEX
        integer(c_short) :: wFormatTag
        integer(c_short) :: nChannels
        integer(c_int)   :: nSamplesPerSec
        integer(c_int)   :: nAvgBytesPerSec
        integer(c_short) :: nBlockAlign
        integer(c_short) :: wBitsPerSample
        integer(c_short) :: cbSize
    end type

    type, bind(C) :: WAVEHDR
        type(c_ptr)      :: lpData
        integer(c_int)   :: dwBufferLength
        integer(c_int)   :: dwBytesRecorded
        type(c_ptr)      :: dwUser
        integer(c_int)   :: dwFlags
        integer(c_int)   :: dwLoops
        type(c_ptr)      :: lpNext
        type(c_ptr)      :: reserved
    end type


    INTERFACE
        function LoadLibraryA(name) bind(C, name="LoadLibraryA")
            import :: c_ptr, c_char
            !DEC$ ATTRIBUTES STDCALL :: LoadLibraryA

            type(c_ptr) :: LoadLibraryA
            character(kind=c_char), dimension(*) :: name
        end function

        function GetProcAddress(hModule, name) bind(C, name="GetProcAddress")
            import :: c_ptr, c_funptr, c_char
            !DEC$ ATTRIBUTES STDCALL :: GetProcAddress

            type(c_ptr), value :: hModule
            character(kind=c_char), dimension(*) :: name
            type(c_funptr) :: GetProcAddress
        end function

        function FreeLibrary(hModule) bind(C, name="FreeLibrary")
            import :: c_ptr, c_int
            !DEC$ ATTRIBUTES STDCALL :: FreeLibrary

            type(c_ptr), value :: hModule
            integer(c_int) :: FreeLibrary
        end function

    END INTERFACE

    abstract interface
    
        function waveOutOpen_IF(phwo, uDeviceID, pwfx, dwCallback, dwInstance, fdwOpen) bind(C)
            import :: c_int, c_ptr
            !DEC$ ATTRIBUTES STDCALL :: waveOutOpen_IF
            integer(c_int) :: waveOutOpen_IF
            type(c_ptr), value :: phwo
            integer(c_int), value :: uDeviceID
            type(c_ptr), value :: pwfx
            type(c_ptr), value :: dwCallback
            type(c_ptr), value :: dwInstance
            integer(c_int), value :: fdwOpen
        end function
    
        function waveOutPrepareHeader_IF(hwo, pwh, cbwh) bind(C)
            import :: c_int, c_ptr
            !DEC$ ATTRIBUTES STDCALL :: waveOutPrepareHeader_IF
            integer(c_int) :: waveOutPrepareHeader_IF
            type(c_ptr), value :: hwo
            type(c_ptr), value :: pwh
            integer(c_int), value :: cbwh
        end function
    
        function waveOutWrite_IF(hwo, pwh, cbwh) bind(C)
            import :: c_int, c_ptr
            !DEC$ ATTRIBUTES STDCALL :: waveOutWrite_IF
            integer(c_int) :: waveOutWrite_IF
            type(c_ptr), value :: hwo
            type(c_ptr), value :: pwh
            integer(c_int), value :: cbwh
        end function
    
        function waveOutUnprepareHeader_IF(hwo, pwh, cbwh) bind(C)
            import :: c_int, c_ptr
            !DEC$ ATTRIBUTES STDCALL :: waveOutUnprepareHeader_IF
            integer(c_int) :: waveOutUnprepareHeader_IF
            type(c_ptr), value :: hwo
            type(c_ptr), value :: pwh
            integer(c_int), value :: cbwh
        end function
    
        function waveOutClose_IF(hwo) bind(C)
            import :: c_int, c_ptr
            !DEC$ ATTRIBUTES STDCALL :: waveOutClose_IF
            integer(c_int) :: waveOutClose_IF
            type(c_ptr), value :: hwo
        end function
    
    end interface


    CONTAINS

    subroutine playsoundInit()
        playsoundError = .FALSE.
        waveOut = LoadLibraryA("winmm.dll" // c_null_char)

        if (.not. c_associated(waveOut )) then
            call displayDebug("Failed to load winmm.dll!") 
            playsoundError = .TRUE.
        end if

        call makeProcessPointer(fp_waveOutOpen           , "waveOutOpen"           )
        call makeProcessPointer(fp_waveOutPrepareHeader  , "waveOutPrepareHeader"  )
        call makeProcessPointer(fp_waveOutWrite          , "waveOutWrite"          )
        call makeProcessPointer(fp_waveOutUnprepareHeader, "waveOutUnprepareHeader")
        call makeProcessPointer(fp_waveOutClose          , "waveOutClose"          )

        call c_f_procpointer(fp_waveOutOpen              , waveOutOpen)
        call c_f_procpointer(fp_waveOutPrepareHeader     , waveOutPrepareHeader)
        call c_f_procpointer(fp_waveOutWrite             , waveOutWrite)
        call c_f_procpointer(fp_waveOutUnprepareHeader   , waveOutUnprepareHeader)
        call c_f_procpointer(fp_waveOutClose             , waveOutClose)

        if (playsoundError .EQV..FALSE.) playsoundInited = .TRUE.

    end subroutine   
  
    subroutine makeProcessPointer(p, n)
        type(c_funptr) :: p
        character(*)   :: n

        p = GetProcAddress(waveOut, trim(n) // c_null_char)
        if (.not. c_associated(p)) then
            call displayDebug("Could not create pointer for " // n // "!")
            playsoundError = .TRUE.
        end if

    end subroutine

    subroutine playsoundClose()
        integer(c_int) :: rc

        rc = FreeLibrary(waveOut)
        playsoundInited = .FALSE.

    end subroutine

    function currentMS result(ms)







    end function

END MODULE playSound
