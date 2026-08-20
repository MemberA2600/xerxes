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
    public              :: inputWindow, checkOnInputSettings, readInput, openJoyDLL, closeJoyDLL, &
                           restoreKeyButtons, restoreJoyButtons, getControllerSettings, &
                           setControllerSettings

    logical             :: canKill, justACancel
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

    integer(2), dimension(20) :: buttons
    integer(2), dimension(20) :: buttonsOld

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

    subroutine getControllerSettings(bytes, ind)
        integer(2), dimension(:), allocatable  :: bytes
        integer(2)                             :: ind, ind2
 
        bytes(ind)      = int(joyDiffSaved, 1) 
        
        do ind2 = 1, size(buttons), 1
           bytes(ind + ind2) = int(buttons(ind2), 1) 
        end do

        ind = ind + 22

    end subroutine

    subroutine setControllerSettings(bytes, ind, version)
        integer(2), dimension(:), allocatable  :: bytes
        integer(2)                             :: ind, ind2
        integer(1)                             :: version 

        joyDiffSaved    = bytes(ind) 
      
        do ind2 = 1, size(buttons), 1
           buttons(ind2) = bytes(ind + ind2) 
        end do

        ind = ind + 22

    end subroutine

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

       buttonsOld  = buttons 
       justACancel = .FALSE. 

       call WDialogPutInteger( IDF_JoySenseVal, joyDiffSaved)
       call WDialogPutTrackbar(IDF_JoySenseTrk, joyDiffSaved)
       call addTextToKeyButtons() 
       call addTextToJoyButtons() 

321    do
          CALL WDialogSelect(IDD_InputSetter)
          CALL WDialogShow(ITYPE=Modal)     
    
          if (WinfoDialog(CurrentDialog) == IDD_InputSetter) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     buttons = buttonsOld 
                     EXIT
                  CASE(ID_InputCancel)
                     buttons = buttonsOld 
                     EXIT
                  CASE(ID_InputOK) 
                     joyDiffSaved = joyDiff
                     EXIT
                  CASE(ID_InputRestore) 
                     call restoreAll()
                !
                ! They must be in a continous range or must be  
                ! set one by one! :(
                !
                  CASE(ID_AssignLeftKey:ID_AssignMenuKey)
                     call assignKey(WinfoDialog(ExitButton))   
                  CASE(ID_AssignLeftJoy:ID_AssignMenuJoy)
                     call assignJoy(WinfoDialog(ExitButton))  
                  END SELECT

              end if
       end do 

       if (justACancel .EQV. .TRUE.) goto 321  

       canKill = .TRUE. 

    end subroutine

    subroutine assignKey(buttonID)
        integer(4)             :: buttonID
        integer(4)             :: buttonArrayID, ind 
        logical                :: pressed

        buttonArrayID          = buttonID - ID_AssignLeftKey + 1
        call WindowOutStatusBar(1, "Press a Button (or ESC to Cancel)!")       

        pressed = .FALSE.

        DO
            call WinSleep(10)
            if (lastPressedKey /= 0) then
                If (getKeyName(lastPressedKey) /= "ESC") then 
                    do ind = 1, 10, 1
                       if (buttons(ind) == lastPressedKey) then 
                           buttons(ind) = buttons(buttonArrayID)
                           CALL WDialogPutString(ind + ID_AssignLeftKey - 1, getKeyName(buttons(ind)))  
                           exit 
                       end if 
                    end do 

                    buttons(buttonArrayID) = lastPressedKey
                    pressed = .TRUE.
                else 
                    justACancel = .TRUE.
                end if

                exit
            end if
        END DO

        if (pressed .EQV. .TRUE.) CALL WDialogPutString(buttonID, getKeyName(buttons(buttonArrayID)))  
        call WindowOutStatusBar(1, "")       

    end subroutine

    subroutine assignJoy(buttonID)
        integer(4)             :: buttonID
        integer(4)             :: buttonArrayID, ind  
        logical                :: pressed
        integer(1)             :: dominantJoy
        !character(40)          :: tt

        buttonArrayID          = buttonID - ID_AssignLeftKey + 1
        call WindowOutStatusBar(1, "Use the Joystick (or ESC to Cancel)!")       
        pressed = .FALSE.

        !write(tt, "(I0)") buttonArrayID
        !call displayDebug(tt)

        DO
            call WinSleep(10)
            if (lastPressedKey /= 0) then
                If (getKeyName(lastPressedKey) == "ESC") then 
                    justACancel = .TRUE.
                    exit
                end if 
            end if

            dominantJoy = getDominantJoy()

            if (dominantJoy /= 0) then
                do ind = 10, 20, 1
                    if (buttons(ind) == dominantJoy) then 
                        buttons(ind) = buttons(buttonArrayID)
                        CALL WDialogPutString(ind + ID_AssignLeftKey - 1, getJoyName(buttons(ind)))  
                        exit 
                    end if 
                end do 

                pressed = .TRUE.
                buttons(buttonArrayID) = dominantJoy 
                exit
            end if

        END DO

        if (pressed .EQV. .TRUE.) CALL WDialogPutString(buttonID, getJoyName(buttons(buttonArrayID)))  
        call WindowOutStatusBar(1, "")       

    end subroutine

    function getDominantJoy() result(joyVal)
        integer(1)          :: joyVal, ind

        joyVal = 0


        do ind = 1, size(joyButton), 1
        
           if (joyButton(ind) .EQV. .TRUE.) then
               joyVal = ind + IND_JOY_BUTTON1 - 1 
               exit 
           end if 
        end do 

        if (joyVal == 0) then
            if (joyUp)    joyVal = IND_JOY_UP
            if (joyDown)  joyVal = IND_JOY_DOWN
            if (joyLeft)  joyVal = IND_JOY_LEFT
            if (joyRight) joyVal = IND_JOY_RIGHT
        end if

    end function


    subroutine restoreAll()
        joyDiff     = 15
    
        call WDialogPutInteger( IDF_JoySenseVal, joyDiff)
        call WDialogPutTrackbar(IDF_JoySenseTrk, joyDiff)

        call restoreKeyButtons()
        call restoreJoyButtons()

        call addTextToKeyButtons()
        call addTextToJoyButtons()

    end subroutine

    subroutine restoreKeyButtons()

        buttons(1)  = getValueOfName("LEFT ARROW")
        buttons(2)  = getValueOfName("RIGHT ARROW")
        buttons(3)  = getValueOfName("UP ARROW")
        buttons(4)  = getValueOfName("DOWN ARROW")
        buttons(5)  = Z'58'                          ! X
        buttons(6)  = getValueOfName("SHIFT")
        buttons(7)  = Z'61'                          ! NUM 1
        buttons(8)  = Z'62'                          ! NUM 2
        buttons(9)  = Z'63'                          ! NUM 3
        buttons(10) = getValueOfName("ENTER")        ! ENTER

    end subroutine

    subroutine restoreJoyButtons()

        buttons(11)  = getValueOfJoyName("LEFT")
        buttons(12)  = getValueOfJoyName("RIGHT")
        buttons(13)  = getValueOfJoyName("UP")
        buttons(14)  = getValueOfJoyName("DOWN")
        buttons(15)  = getValueOfJoyName("BUTTON1")                         
        buttons(16)  = getValueOfJoyName("BUTTON2")
        buttons(17)  = getValueOfJoyName("BUTTON3")
        buttons(18)  = getValueOfJoyName("BUTTON4")
        buttons(19)  = getValueOfJoyName("BUTTON5")
        buttons(20)  = getValueOfJoyName("BUTTON6")
    end subroutine

    function getValueOfJoyName(n) result(v)
        character(*)            :: n
        integer(1)              :: v        

        select case(n)
        case("LEFT")
            v = IND_JOY_LEFT
        case("RIGHT")
            v = IND_JOY_RIGHT
        case("UP")
            v = IND_JOY_UP
        case("DOWN")
            v = IND_JOY_DOWN
        case("BUTTON1")
            v = IND_JOY_BUTTON1                         
        case("BUTTON2")
            v = IND_JOY_BUTTON2
        case("BUTTON3")
            v = IND_JOY_BUTTON3
        case("BUTTON4")
            v = IND_JOY_BUTTON4
        case("BUTTON5")
            v = IND_JOY_BUTTON5
        case("BUTTON6")
            v = IND_JOY_BUTTON6
        case default
            v = 0
        end select

    end function

    subroutine addTextToKeyButtons()
        integer(2)               :: ind

        do ind = 0, 9, 1
            CALL WDialogPutString(ind + ID_AssignLeftKey, getKeyName(buttons(ind + 1)))  
        end do

    end subroutine

    subroutine addTextToJoyButtons()
        integer(2)               :: ind

        do ind = 10, 19, 1
            CALL WDialogPutString(ind + ID_AssignLeftKey, getJoyName(buttons(ind + 1)))  
        end do

    end subroutine

    function getValueOfName(n) result(v)
        character(*)             :: n
        integer(2)               :: ind, v

        v = 0
        do ind = 1, n_special, 1
           if (vk_name(ind) == n) then 
               v = vk_code(ind)
               return
           end if  
        end do

    end function

    function getKeyName(val) result(keyname)
        character(len=12) :: keyname
        logical           :: found
        integer           :: i
        integer(2)        :: val

        select case(val) 
        case(0)  
            keyname = "" 
        case(48:57) 
            write(keyname, "(I0)") val - 48  
        case(65:90) 
            keyname = char(val)
        case(96:105) 
            write(keyname, "('NUM ', I0)") val - 96
        case(112:123) 
            write(keyname, "('F', I0)") val - 111
        case default   
            found = .FALSE.
            do i = 1, n_special
                if (val == vk_code(i)) then
                    keyname = trim(vk_name(i))
                    found = .TRUE.
                    exit
                end if
            end do

            if (found .EQV. .FALSE.) then
                write(keyname, '(A, " (", I0, ")")') char(val), val 
            end if
        end select

    end function

    function getJoyName(val) result(joyname)
        integer(2)        :: val
        character(len=12) :: joyName

        select case(val)
        case(IND_JOY_LEFT)
            joyName = "LEFT"
        case(IND_JOY_RIGHT)
            joyName = "RIGHT"
        case(IND_JOY_UP)
            joyName = "UP"
        case(IND_JOY_DOWN)
            joyName = "DOWN"
        case(IND_JOY_BUTTON1)
            joyName = "BUTTON1"                         
        case(IND_JOY_BUTTON2)
            joyName = "BUTTON2"
        case(IND_JOY_BUTTON3)
            joyName = "BUTTON3"
        case(IND_JOY_BUTTON4)
            joyName = "BUTTON4"
        case(IND_JOY_BUTTON5)
            joyName = "BUTTON5"
        case(IND_JOY_BUTTON6)
            joyName = "BUTTON6"
        case default
            joyName = ""
        end select

    end function

    subroutine checkOnInputSettings()
        integer(4)        :: tempTrk, tempVal  

        CALL WDialogPutString(IDF_Pressed, getKeyName(lastPressedKey))  
        
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
