MODULE config

    USE WINTERACTER
    USE RESID
    use debugWindow
    USE engineConstants  
    USE wavePlayerWindow
    USE inputReader
    USE subs
    USE dataLoader

    implicit none

    PRIVATE
    PUBLIC          :: loadConfig, saveConfig

    !
    !   Config File Layout
    !   01-04th BYTES: 'CONF'
    !   05th    BYTE : Version (current: 0) 
    !   06th    BYTE : Screen Size 
    !   07th    BYTE : Speed
    !   08th    BYTE : Sound Volume
    !   09th    BYTE : Music Volume
    !   10th    BYTE : OPL2LPT Mode
    !   11-12th BYTES: LPT Hex
    !   13th    BYTE : Analog Stick Sensitivity
    !   14-23th BYTE : Keys
    !   24-33th BYTE : Joystick Settings
    !   

    contains

    subroutine loadConfig()
        logical                                :: ex
        integer(2), dimension(:), allocatable  :: bytes
        integer(2)                             :: rc
        integer(8)                             :: s, ind
        integer(1)                             :: version
        character(4)                           :: typ

        INQUIRE(FILE=trim(CWD()) // "\" // configName, exist = ex, size = s)

        !call displayDebug(trim(CWD()) // "\" // configName)    

        if (ex == .TRUE.) then
            call loadBinary(trim(CWD()) // "\" // configName, bytes, S, .FALSE.)

            ind = 1     
            call read4CharFromBin(bytes, s, ind, typ)
            if (typ /= CONF_FILE_TYPE) call displayDebug("Invalid Config File!")

            version = bytes(5)
            call setScreenSize(bytes(6) + ID_AUTO)
            call setSpeed(bytes(7))

            ind      = 8

            call setSoundSettings(bytes, ind, version)
            call setControllerSettings(bytes, ind, version)

            deallocate(bytes, stat = rc)
            if (rc /= 0) call displayDebug("Failed to deallocate Config File bytes!")
        end if

    end subroutine

    subroutine saveConfig()
        integer(2), dimension(:), allocatable  :: bytes
        integer(2)                             :: ind, rc, s
        
        allocate(bytes(33), stat = rc)

        if (rc /= 0) call displayDebug("Failed to allocate Config File bytes!")

        call writeChars2Bin(bytes, CONF_FILE_TYPE, 1, 4)
        bytes(5) = configVersion       
        bytes(6) = getScreenSizeId()
        bytes(7) = getSpeed()

        ind      = 8

        call getSoundSettings(bytes, ind)
        call getControllerSettings(bytes, ind)

        call writeBin2File(trim(CWD()) // "\" // configName, bytes, .TRUE., .FALSE.)

    end subroutine

END MODULE config
