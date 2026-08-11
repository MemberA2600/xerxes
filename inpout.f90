module inpout

    USE KERNEL32
    use, intrinsic :: iso_c_binding
    use debugWindow
    implicit none

    PRIVATE
    PUBLIC      :: setLPTAddress, openDLL, closeDLL

    integer(2)      :: lptAddress = Z'0378'

    integer(HANDLE) :: hLib
    integer(LPVOID) :: pOut32 , pInp32 , pIsOpen
    type(C_FUNPTR)  :: cpOut32, cpInp32, cpIsOpen

    abstract interface
    
        !-----------------------------------------
        ! void __stdcall Out32(short, short)
        !-----------------------------------------
        subroutine Out32_int(port, value)
            integer(2), value :: port
            integer(2), value :: value
    
            !DEC$ ATTRIBUTES STDCALL :: Out32_int
        end subroutine Out32_int
    
    
        !-----------------------------------------
        ! short __stdcall Inp32(short)
        !-----------------------------------------
        integer(2) function Inp32_int(port)
            integer(2), value :: port
    
            !DEC$ ATTRIBUTES STDCALL :: Inp32_int
        end function Inp32_int
    
    
        !-----------------------------------------
        ! BOOL __stdcall IsInpOutDriverOpen()
        !-----------------------------------------
        integer(4) function IsInpOutDriverOpen_int()
            !DEC$ ATTRIBUTES STDCALL :: IsInpOutDriverOpen_int
        end function IsInpOutDriverOpen_int
    
    end interface

    procedure(Out32_int),              pointer :: Out32
    procedure(Inp32_int),              pointer :: Inp32
    procedure(IsInpOutDriverOpen_int), pointer :: IsInpOutDriverOpen

    contains

    subroutine setLPTAddress(addr)
        integer(2)  :: addr

        LPTAddress = addr

    end subroutine

    subroutine openDLL()
        integer(1)      :: rc

        hLib = LoadLibrary("inpout32.dll" // c_null_char)

        if (hLib == 0) then
            call displayDebug("Failed to load Inpout32!")
        endif

        pOut32 = GetProcAddress(hLib, 'Out32' // char(0))
        if (pOut32 == 0) then
            call displayDebug("Failed to load function Out32!")
            call closeDLL
            return
        endif

        pInp32 = GetProcAddress(hLib, 'Inp32' // char(0))

        if (pInp32 == 0) then
            call displayDebug("Failed to load function Inp32!")
            call closeDLL
            return
        endif

        pIsOpen = GetProcAddress(hLib, 'IsInpOutDriverOpen' // char(0))
        if (pIsOpen == 0) then
            call displayDebug("Failed to load function IsOpen!")
            call closeDLL
            return
        endif

        cpOut32 = transfer(pOut32, cpOut32)
        cpInp32 = transfer(pInp32, cpInp32)
        cpIsOpen = transfer(pIsOpen, cpIsOpen)
        
        call C_F_PROCPOINTER(cpOut32 , Out32)
        call C_F_PROCPOINTER(cpInp32 , Inp32)
        call C_F_PROCPOINTER(cpIsOpen, IsInpOutDriverOpen)

        if (.not. associated(Out32)) then
            call displayDebug("Out32 association failed!")
        endif

        if (.not. associated(Inp32)) then
            call displayDebug("Inp32 association failed!")
        endif

        if (.not. associated(IsInpOutDriverOpen)) then
            call displayDebug("IsInpOutDriverOpen association failed!")
        endif

        rc = IsInpOutDriverOpen()
        if (rc == 0)  call displayDebug("Driver not opened!")

        call Out32(lptAddress, 0_2)

    end subroutine

    subroutine closeDLL()
        integer(1)      :: rc

        rc = FreeLibrary(hLib)
        if (rc == 0) call displayDebug("Failed to unload Inpout32!")

    end subroutine

end module

