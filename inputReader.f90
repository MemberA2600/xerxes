MODULE inputReader

    USE, INTRINSIC :: ISO_C_BINDING
    USE debugWindow
    USE dataLoader
    USE waveplayer
    USE WINTERACTER
    USE RESID
    USE subs
    USE engineConstants
    USE winapis
    use IFPORT
    use USER32
    use IFWINTY
    USE KERNEL32, WinSleep => Sleep
    USE WINMM

    implicit none

    private
    public              :: inputWindow, checkOnInputSettings, readInput, openJoyDLL, closeJoyDLL

    logical             :: canKill
    integer(2)          :: lastPressedKey

    integer, parameter :: n_special = 30
    integer, parameter :: vk_code(n_special) = (/ &
        8, 9, 13, 16, 17, 18, 19, 20, 27, 32, &
        33, 34, 35, 36, 37, 38, 39, 40, 45, 46, &
        144, 145, 106, 107, 109, 110, 111, 1, 2, 4 /)
    character(len=12), parameter :: vk_name(n_special) = (/ &
        "BACKSPACE   ", "TAB         ", "ENTER       ", "SHIFT       ", &
        "CTRL        ", "ALT         ", "PAUSE       ", "CAPSLOCK    ", &
        "ESC         ", "SPACE       ", "PAGE UP     ", "PAGE DOWN   ", &
        "END         ", "HOME        ", "LEFT ARROW  ", "UP ARROW    ", &
        "RIGHT ARROW ", "DOWN ARROW  ", "INSERT      ", "DELETE      ", &
        "NUM LOCK    ", "SCROLL LOCK ", "*"           , "+"           , &
        "-"           , "NUM , ."     , "/"           , "MOUSE LEFT"  , &
        "MOUSE RIGHT" , "MOUSE MIDDLE"   /)

    ! Button masks
    integer(2), parameter :: XINPUT_GAMEPAD_DPAD_UP        = Z'0001'
    integer(2), parameter :: XINPUT_GAMEPAD_DPAD_DOWN      = Z'0002'
    integer(2), parameter :: XINPUT_GAMEPAD_DPAD_LEFT      = Z'0004'
    integer(2), parameter :: XINPUT_GAMEPAD_DPAD_RIGHT     = Z'0008'
    integer(2), parameter :: XINPUT_GAMEPAD_START          = Z'0010'
    integer(2), parameter :: XINPUT_GAMEPAD_BACK           = Z'0020'
    integer(2), parameter :: XINPUT_GAMEPAD_LEFT_THUMB     = Z'0040'
    integer(2), parameter :: XINPUT_GAMEPAD_RIGHT_THUMB    = Z'0080'
    integer(2), parameter :: XINPUT_GAMEPAD_LEFT_SHOULDER  = Z'0100'
    integer(2), parameter :: XINPUT_GAMEPAD_RIGHT_SHOULDER = Z'0200'
    integer(2), parameter :: XINPUT_GAMEPAD_A              = Z'1000'
    integer(2), parameter :: XINPUT_GAMEPAD_B              = Z'2000'
    integer(2), parameter :: XINPUT_GAMEPAD_X              = Z'4000'
    integer(2), parameter :: XINPUT_GAMEPAD_Y              = Z'8000'

    ! --------------------------------------------------------
    ! 6-button controller mapping
    ! --------------------------------------------------------

    integer(2), parameter :: JOY_BUTTON_1   = XINPUT_GAMEPAD_A
    integer(2), parameter :: JOY_BUTTON_2   = XINPUT_GAMEPAD_B
    integer(2), parameter :: JOY_BUTTON_3   = XINPUT_GAMEPAD_X
    integer(2), parameter :: JOY_BUTTON_4   = XINPUT_GAMEPAD_Y
    integer(2), parameter :: JOY_BUTTON_5   = XINPUT_GAMEPAD_LEFT_SHOULDER
    integer(2), parameter :: JOY_BUTTON_6   = XINPUT_GAMEPAD_RIGHT_SHOULDER

    type, bind(C) :: XINPUT_GAMEPAD
        integer(c_int16_t) :: wButtons
        integer(c_int8_t)  :: bLeftTrigger
        integer(c_int8_t)  :: bRightTrigger
        integer(c_int16_t) :: sThumbLX
        integer(c_int16_t) :: sThumbLY
        integer(c_int16_t) :: sThumbRX
        integer(c_int16_t) :: sThumbRY
    end type XINPUT_GAMEPAD
    
    type, bind(C) :: XINPUT_STATE
        integer(c_int32_t)   :: dwPacketNumber
        type(XINPUT_GAMEPAD) :: Gamepad
    end type XINPUT_STATE

    type(T_JOYINFOEX) :: ji

    integer(4), parameter    :: joycenter = 32767
    integer(4)               :: joydiff   = 15, joyDiffSaved = 15

    logical :: joyUp
    logical :: joyDown
    logical :: joyLeft
    logical :: joyRight

    logical :: joyButton(6)
    
    integer(HANDLE) :: hLib
    integer(LPVOID) :: pXInput
    type(C_FUNPTR)  :: cpXInput

    ! --------------------------------------------------------
    ! XInputGetState interface
    ! --------------------------------------------------------

    abstract interface
    
        integer(4) function XInputGetState_int(dwUserIndex, pState)
    
            import             :: XINPUT_STATE
    
            integer(4)         :: dwUserIndex
            type(XINPUT_STATE) :: pState

            !DEC$ ATTRIBUTES STDCALL :: XInputGetState_int
            !DEC$ ATTRIBUTES VALUE :: dwUserIndex
            !DEC$ ATTRIBUTES REFERENCE :: pState
    
        end function XInputGetState_int
    
    end interface
    
    procedure(XInputGetState_int), pointer :: XInput 

    contains  

    subroutine openJoyDLL()
        !character(40)           :: ttt

        hLib = LoadLibrary("xinput1_3.dll" // c_null_char)

        !write(ttt, '(Z8.8)') hLib 
        !call displayDebug(ttt)

        if (hLib == 0) then
            hLib = LoadLibrary("XInput1_4.dll" // c_null_char)
            if (hLib == 0) then
                call displayDebug("Failed to load XInput!")
            end if
        endif

        pXInput = GetProcAddress(hLib, 'XInputGetState' // c_null_char)
        if (pXInput == 0) then
            call displayDebug("Failed to load function XInput!")
            !call closeJoyDLL()
            return
        endif
        
        cpXInput = transfer(pXInput, cpXInput)
        call C_F_PROCPOINTER(cpXInput, XInput)

        if (.not. associated(XInput)) then
            call displayDebug("XInput association failed!")
        endif

    end subroutine

    subroutine closeJoyDLL()
        integer(BOOL)      :: rc
        !character(40)           :: ttt

        !nullify(XInput)

        !write(ttt, '(Z8.8)') hLib 
        !call displayDebug(ttt)

        rc = FreeLibrary(hLib)
        if (rc == 0) call displayDebug("Failed to unload XInput!")

    end subroutine

    subroutine inputWindow()
       INTEGER                                 :: ITYPE
       TYPE(WIN_MESSAGE)                       :: MESSAGE
       integer                                 :: c 

       canKill = .FALSE. 

       do
         if (WInfoDialog(CurrentDialog) == 0) exit
         call sleep(1)
       end do 

       CALL WDialogLoad(IDD_InputSetter)

       call WDialogPutInteger( IDF_JoySenseVal, joyDiffSaved)
       call WDialogPutTrackbar(IDF_JoySenseTrk, joyDiffSaved)

       do
          CALL WDialogSelect(IDD_InputSetter)
          CALL WDialogShow(ITYPE=Modal)     
    
          if (WinfoDialog(CurrentDialog) == IDD_InputSetter) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     EXIT
                  CASE(ID_InputCancel) 
                     EXIT
                  CASE(ID_InputOK) 
                     joyDiffSaved = joyDiff
                     EXIT
                  CASE(ID_InputRestore) 
                     call restoreAll()
                  END SELECT

              end if
       end do 

       canKill = .TRUE. 

    end subroutine

    subroutine restoreAll()
        joyDiff     = 15
    
        call WDialogPutInteger( IDF_JoySenseVal, joyDiff)
        call WDialogPutTrackbar(IDF_JoySenseTrk, joyDiff)

    end subroutine

    subroutine checkOnInputSettings()
        character(len=12) :: keyname
        integer           :: i
        logical           :: found
        integer(4)        :: tempTrk, tempVal  

        select case(lastPressedKey) 
        case(0)  
            keyname = "" 
        case(48:57) 
            write(keyname, "(I0)") lastPressedKey - 48  
        case(65:90) 
            keyname = char(lastPressedKey)
        case(96:105) 
            write(keyname, "('NUM ', I0)") lastPressedKey - 96
        case(112:123) 
            write(keyname, "('F', I0)") lastPressedKey - 111
        case default   
            found = .FALSE.
            do i = 1, n_special
                if (lastPressedKey == vk_code(i)) then
                    keyname = trim(vk_name(i))
                    found = .TRUE.
                    exit
                end if
            end do

            if (found .EQV. .FALSE.) then
                write(keyname, '(A, " (", I0, ")")') char(lastPressedKey), lastPressedKey 
            end if
        end select

        CALL WDialogPutString(IDF_Pressed, keyname)  
        
        if (joyUp) then
            CALL WDialogPutString(IDF_JoyUp, "X")  
        else
            CALL WDialogPutString(IDF_JoyUp, " ")  
        end if

        if (joyDown) then
            CALL WDialogPutString(IDF_JoyDown, "X")  
        else
            CALL WDialogPutString(IDF_JoyDown, " ")  
        end if

        if (joyLeft) then
            CALL WDialogPutString(IDF_JoyLeft, "X")  
        else
            CALL WDialogPutString(IDF_JoyLeft, " ")  
        end if

        if (joyRight) then
            CALL WDialogPutString(IDF_JoyRight, "X")  
        else
            CALL WDialogPutString(IDF_JoyRight, " ")  
        end if

        if (joyButton(1)) then
            CALL WDialogPutString(IDF_Joy1, "X")  
        else
            CALL WDialogPutString(IDF_Joy1, " ")  
        end if

        if (joyButton(2)) then
            CALL WDialogPutString(IDF_Joy2, "X")  
        else
            CALL WDialogPutString(IDF_Joy2, " ")  
        end if

        if (joyButton(3)) then
            CALL WDialogPutString(IDF_Joy3, "X")  
        else
            CALL WDialogPutString(IDF_Joy3, " ")  
        end if

        if (joyButton(4)) then
            CALL WDialogPutString(IDF_Joy4, "X")  
        else
            CALL WDialogPutString(IDF_Joy4, " ")  
        end if

        if (joyButton(5)) then
            CALL WDialogPutString(IDF_Joy5, "X")  
        else
            CALL WDialogPutString(IDF_Joy5, " ")  
        end if

        if (joyButton(6)) then
            CALL WDialogPutString(IDF_Joy6, "X")  
        else
            CALL WDialogPutString(IDF_Joy6, " ")  
        end if

        call WDialogGetInteger( IDF_JoySenseVal, tempVal)
        call WDialogGetTrackbar(IDF_JoySenseTrk, tempTrk)

        if (tempVal /= joyDiff) then
            joyDiff = tempVal
        else
            if (tempTrk /= joyDiff) then
                joyDiff = tempTrk
            end if
        end if

        call WDialogPutInteger( IDF_JoySenseVal, joyDiff)
        call WDialogPutTrackbar(IDF_JoySenseTrk, joyDiff)

        if (canKill .EQV. .TRUE.) then 
            CALL WDialogUnLoad()
            canKill = .FALSE.
        end if

    end subroutine

    subroutine readInput()
        integer             :: vk
        integer(c_short)    :: state
        logical             :: found
        integer(4)          :: rc, diff1024
        type(XINPUT_STATE)  :: jstate
        integer(1)          :: ind

        found = .false.
    
        do vk = 1, 254
            state = GetAsyncKeyState(vk)
            if (state < 0_c_short) then
                found = .true.
                exit
            end if
        end do
    
        if (found)  then
            lastPressedKey = vk
        else
            lastPressedKey = 0
        end if
    
        rc = XInput(0, jstate)
        
        if (rc == 0) then
        
            joyUp = &
                iand(jstate%Gamepad%wButtons, XINPUT_GAMEPAD_DPAD_UP) /= 0
        
            joyDown = &
                iand(jstate%Gamepad%wButtons, XINPUT_GAMEPAD_DPAD_DOWN) /= 0
        
            joyLeft = &
                iand(jstate%Gamepad%wButtons, XINPUT_GAMEPAD_DPAD_LEFT) /= 0
        
            joyRight = &
                iand(jstate%Gamepad%wButtons, XINPUT_GAMEPAD_DPAD_RIGHT) /= 0
        
        
            joyButton(1) = &
                iand(jstate%Gamepad%wButtons, JOY_BUTTON_1) /= 0
        
            joyButton(2) = &
                iand(jstate%Gamepad%wButtons, JOY_BUTTON_2) /= 0
        
            joyButton(3) = &
                iand(jstate%Gamepad%wButtons, JOY_BUTTON_3) /= 0
        
            joyButton(4) = &
                iand(jstate%Gamepad%wButtons, JOY_BUTTON_4) /= 0
        
            joyButton(5) = &
                iand(jstate%Gamepad%wButtons, JOY_BUTTON_5) /= 0
        
            joyButton(6) = &
                iand(jstate%Gamepad%wButtons, JOY_BUTTON_6) /= 0 
        else
            ji%dwSize  = sizeof(ji)
            ji%dwFlags = JOY_RETURNALL
    
            rc = joyGetPosEx(JOYSTICKID1, ji)
    
            if (rc == JOYERR_NOERROR) then
                
                diff1024 = JoyDiff * 1024

                do ind = 1, 6, 1
                   joyButton(ind) = btest(ji%dwButtons, ind-1)
                end do

                if (ji%dwXpos < (joyCenter - diff1024)) then     
                    joyLeft  = .TRUE.
                    joyRight = .FALSE.
                else
                    if (ji%dwXpos > (joyCenter + diff1024)) then     
                        joyLeft  = .FALSE.
                        joyRight = .TRUE.
                    else
                        joyLeft  = .FALSE.
                        joyRight = .FALSE.    
                    end if 
                end if 
                       
                if (ji%dwYpos < (joyCenter - diff1024)) then     
                    joyUp   = .TRUE.
                    joyDown = .FALSE.
                else
                    if (ji%dwYpos > (joyCenter + diff1024)) then     
                        joyUp   = .FALSE.
                        joyDown = .TRUE.
                    else
                        joyUp   = .FALSE.
                        joyDown = .FALSE.    
                    end if 
                end if 
            end if

        end if

    end subroutine

END MODULE inputReader
