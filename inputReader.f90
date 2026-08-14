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

    implicit none

    private
    public              :: inputWindow, checkOnInputSettings, readInput

    logical             :: canKill
    integer(2)          :: lastPressedKey

    integer, parameter :: n_special = 27
    integer, parameter :: vk_code(n_special) = (/ &
        8, 9, 13, 16, 17, 18, 19, 20, 27, 32, &
        33, 34, 35, 36, 37, 38, 39, 40, 45, 46, &
        144, 145, 106, 107, 109, 110, 111 /)
    character(len=12), parameter :: vk_name(n_special) = (/ &
        "BACKSPACE   ", "TAB         ", "ENTER       ", "SHIFT       ", &
        "CTRL        ", "ALT         ", "PAUSE       ", "CAPSLOCK    ", &
        "ESC         ", "SPACE       ", "PAGE UP     ", "PAGE DOWN   ", &
        "END         ", "HOME        ", "LEFT ARROW  ", "UP ARROW    ", &
        "RIGHT ARROW ", "DOWN ARROW  ", "INSERT      ", "DELETE      ", &
        "NUM LOCK    ", "SCROLL LOCK ", "*"           , "+"           , &
        "-"           , "NUM , ."     , "/"   /)

    contains  

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

       do
          CALL WDialogSelect(IDD_TIA)
          CALL WDialogShow(ITYPE=Modal)     
    
          if (WinfoDialog(CurrentDialog) == IDD_InputSetter) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     EXIT
                  END SELECT

              end if
       end do 

       canKill = .TRUE. 

    end subroutine

    subroutine checkOnInputSettings()
        character(len=12) :: keyname
        integer           :: i
        logical           :: found

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

        if (canKill .EQV. .TRUE.) then 
            CALL WDialogUnLoad()
            canKill = .FALSE.
        end if

    end subroutine


    subroutine readInput()
    integer :: vk
    integer(c_short) :: state
    logical :: found

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

    end subroutine

END MODULE inputReader
