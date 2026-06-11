MODULE playSound
    USE, INTRINSIC :: ISO_C_BINDING
    USE WINTERACTER
    USE RESID
    USE debugWindow
    use ifwinty
  ! use kernel32

    IMPLICIT NONE

    PRIVATE
    PUBLIC :: playsoundInit

    type(c_ptr)    :: waveOut
    type(c_funptr) :: waveOutOpen, waveOutPrepareHeader,                  &
                    & waveOutWrite, waveOutUnprepareHeader, waveOutClose

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

    CONTAINS

    subroutine playsoundInit()
        waveOut = LoadLibraryA("winmm.dll" // c_null_char)

        if (.not. c_associated(waveOut )) then
            call displayDebug("Failed to load winmm.dll!") 
        end if

        call makeProcessPointer(waveOutOpen           , "waveOutOpen"           )
        call makeProcessPointer(waveOutPrepareHeader  , "waveOutPrepareHeader"  )
        call makeProcessPointer(waveOutWrite          , "waveOutWrite"          )
        call makeProcessPointer(waveOutUnprepareHeader, "waveOutUnprepareHeader")
        call makeProcessPointer(waveOutClose          , "waveOutClose"          )

    end subroutine   
  
    subroutine makeProcessPointer(p, n)
        type(c_funptr) :: p
        character(*)   :: n

        p = GetProcAddress(waveOut, trim(n) // c_null_char)
        if (.not. c_associated(p)) then
            call displayDebug("Could not create pointer for " // n // "!")
        end if

    end subroutine

END MODULE playSound
