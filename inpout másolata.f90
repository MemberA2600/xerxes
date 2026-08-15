module inpout

    USE KERNEL32
    use, intrinsic :: iso_c_binding
    use debugWindow
    use threadMaster

    implicit none

    PRIVATE
    PUBLIC      :: setLPTAddress, openIODLL, closeIODLL, getLptMode, initChip, &
                   writeReg, lightTest, isChipPlaying         

    integer(2)      :: lptAddress = Z'0378'
    logical         :: OPL2LPT, chipPlay = .FALSE.

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

    integer(2), parameter :: OPL2_REGS(123) = (/ &
        Z'001', Z'002', Z'003', Z'004', Z'008', Z'0BD', &
    
        Z'020', Z'021', Z'022', Z'023', Z'024', Z'025', &
        Z'028', Z'029', Z'02A', Z'02B', Z'02C', Z'02D', &
        Z'030', Z'031', Z'032', Z'033', Z'034', Z'035', &
    
        Z'040', Z'041', Z'042', Z'043', Z'044', Z'045', &
        Z'048', Z'049', Z'04A', Z'04B', Z'04C', Z'04D', &
        Z'050', Z'051', Z'052', Z'053', Z'054', Z'055', &
    
        Z'060', Z'061', Z'062', Z'063', Z'064', Z'065', &
        Z'068', Z'069', Z'06A', Z'06B', Z'06C', Z'06D', &
        Z'070', Z'071', Z'072', Z'073', Z'074', Z'075', &
    
        Z'080', Z'081', Z'082', Z'083', Z'084', Z'085', &
        Z'088', Z'089', Z'08A', Z'08B', Z'08C', Z'08D', &
        Z'090', Z'091', Z'092', Z'093', Z'094', Z'095', &
    
        Z'0A0', Z'0A1', Z'0A2', Z'0A3', Z'0A4', &
        Z'0A5', Z'0A6', Z'0A7', Z'0A8', &
    
        Z'0B0', Z'0B1', Z'0B2', Z'0B3', Z'0B4', &
        Z'0B5', Z'0B6', Z'0B7', Z'0B8', &
    
        Z'0C0', Z'0C1', Z'0C2', Z'0C3', Z'0C4', &
        Z'0C5', Z'0C6', Z'0C7', Z'0C8', &
    
        Z'0E0', Z'0E1', Z'0E2', Z'0E3', Z'0E4', Z'0E5', &
        Z'0E8', Z'0E9', Z'0EA', Z'0EB', Z'0EC', Z'0ED', &
        Z'0F0', Z'0F1', Z'0F2', Z'0F3', Z'0F4', Z'0F5' /)

    contains

    function isChipPlaying result(r)
        logical :: r
        
        r = chipPlay

    end function

    function getLptMode() result(r)
        logical :: r

        r = OPL2LPT    

    end function

    subroutine setLPTAddress(addr, enabled)
        integer(2)  :: addr
        logical     :: enabled

        LPTAddress = addr
        OPL2LPT    = enabled

    end subroutine

    subroutine openIODLL()
        integer(1)      :: rc
        logical         :: firstOne

        firstOne        = .TRUE.
    
        hLib = LoadLibrary("inpout32.dll" // c_null_char)

444     if (hLib == 0) then
            call displayDebug("Failed to load InpOut32!")
        endif

        if (firstOne .EQV. .TRUE.) then
            pOut32 = GetProcAddress(hLib, 'Out32' // char(0))
            if (pOut32 == 0) then
                call displayDebug("Failed to load function Out32!")
                call closeIODLL()
                return
            endif
    
            pInp32 = GetProcAddress(hLib, 'Inp32' // char(0))
            if (pInp32 == 0) then
                call displayDebug("Failed to load function Inp32!")
                call closeIODLL()
                return
            endif
    
            pIsOpen = GetProcAddress(hLib, 'IsInpOutDriverOpen' // char(0))
            if (pIsOpen == 0) then
                call displayDebug("Failed to load function IsOpen!")
                call closeIODLL()
                return
            endif
        else 
            pOut32 = GetProcAddress(hLib, '?Inp32@@YAFF@Z' // char(0))
            if (pOut32 == 0) then
                call displayDebug("Failed to load function Out32!")
                call closeIODLL()
                return
            endif
    
            pInp32 = GetProcAddress(hLib, '?Out32@@YAXFF@Z' // char(0))
            if (pInp32 == 0) then
                call displayDebug("Failed to load function Inp32!")
                call closeIODLL()
                return
            endif
    
            pIsOpen = GetProcAddress(hLib, '?IsInpOutDriverOpen@@YAHXZ' // char(0))
            if (pIsOpen == 0) then
                call displayDebug("Failed to load function IsOpen!")
                call closeIODLL()
                return
            endif        
        end if

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
        if (rc == 0) then 
            if (firstOne .EQV. .TRUE.) then
                call closeIODLL()
                firstOne = .FALSE.
                hLib = LoadLibrary("inpoutx64.dll" // c_null_char)
            
                goto 444
            else
                 call displayDebug("InpOut32 Driver not opened!")
                 return
            end if
        end if
    end subroutine

    subroutine closeIODLL()
        integer(1)      :: rc

        rc = FreeLibrary(hLib)
        if (rc == 0) call displayDebug("Failed to unload Inpout32!")

    end subroutine

    subroutine pauseThem(s)
         logical    :: s   
         
         call pauseThread("playAdlib", s)
         call pauseThread("playMusic", .TRUE.)

         if (s .EQV. .TRUE.) then
             do while (isThreadPaused("playAdlib") .EQV. .FALSE.)
                call sleep(1)    
             end do
         end if   

    end subroutine

    subroutine initChip(ending)
        integer(2)          ::  ind
        logical             ::  ending        

        call pauseThem(.TRUE.)

        do ind = 1, size(OPL2_REGS), 1
           call writeReg(OPL2_REGS(ind), 0) 
        end do

        call pauseThem(ending)
        chipPlay = (ending .NEQV. .TRUE.)

    end subroutine

    subroutine writeReg(reg, dat)
        integer(1)                          :: reg, dat
        integer(1), dimension(3), parameter :: statics = (/ 12, 8, 12 /) 
        integer(2)                          :: lptCtrlPort, ind, val, test
        character(40)                       :: t            
        
        !write(t, '(Z4.4, " | ", Z2.2, " | ", Z2.2)') lptAddress, reg, dat

        !call displayDebug(t)
        lptCtrlPort = lptAddress + 2        

        call Out32(lptAddress, reg)
        
        do ind = 1, 3, 1
           call Out32(lptCtrlPort , statics(ind) + 1)
        end do

        do ind = 1, 6, 1 
           test =  Inp32(lptCtrlPort)
        end do

        call Out32(lptAddress, dat)

        do ind = 1, 3, 1
           call Out32(lptCtrlPort, statics(ind))
        end do

        do ind = 1, 35, 1 
           test =  Inp32(lptCtrlPort)
        end do

    end subroutine

    subroutine lightTest()
        integer(2)          :: ind, num

        call Out32(Z'0378', 0)

        num = 1

        do num = 1, 1000, 1
           call Out32(Z'0378', 255)
           call sleep(1000) 
           call Out32(Z'0378', 0)
           call sleep(1000) 
        end do

    end subroutine


end module

