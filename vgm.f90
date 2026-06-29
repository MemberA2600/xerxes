MODULE vgm

    USE, INTRINSIC :: ISO_C_BINDING
    USE debugWindow
    USE dataLoader
    USE waveplayer
    USE WINTERACTER
    USE RESID
    USE subs
    USE engineConstants

    implicit none

    private
    public :: openVGM

    type vgmHeader
         character(4) :: filetyp
         integer(2)   :: version    , SN76489sfW      , SN76489flags
         integer(4)   :: SN76489FB  , volMod          , loopBase    , loopMod
         integer(8)   :: eofOffset  , SN76489         , YM2413      , GD3Offset  , totalWaits , &
                         loopOffset , totalLoopSamples, rate        , YM2612     , YM2151     , &
                         dataOffset , PCMClock        , PCMiReg     , RF5C68     , YM2203     , &
                         YM2608     , YM2610          , YM3812      , YM3526     , Y8950      , &
                         YMF262     , YMF278B         , YMF271      , YMZ280B    , RF5C164    , &    
                         PWM        , AY8910          , AY8910Flags 
    !    
    ! We need only the GD3 and data offsets, and the three OPL chips (YM3526, YM3812, YMF262)
    !
    end type

    type(vgmHeader), allocatable :: vhead

    contains

    subroutine buildVGMHeader(d, s, error)
        integer(2)                            :: stat, ind
        integer(4)                            :: s, offset
        integer(2), dimension(:), allocatable :: d
        logical, intent(out)                  :: error
        integer(2), dimension(:), allocatable :: v
        character(8)                          :: v2                           
        character(2)                          :: vChar
        integer                               :: temp
 
        error  = .FALSE.
        offset = 1
        call read4CharFromBin(d, s, offset, vhead%filetyp)  

        if (vhead%filetyp /= "vgm " .AND. vhead%filetyp /= "Vgm " .AND. vhead%filetyp /= "VGM ") then 
            call displayDebug("Corrupted VGM File!") 
            error = .TRUE.
            return
        end if    

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%eofOffset = temp
  
        call copyBytes(d, v, offset, offset + 4, 4)

        do ind = 4, 1, -1 
           write(vChar, "(Z2)") v(ind)
           if (vChar(1:1) == " ") vChar(1:1) = "0" 
  
           v2(9 - (ind * 2) : 10 - (ind * 2)) = vChar  
        end do

        read(v2, "(I8)") temp   
        vhead%version = temp

        deallocate(v, stat = stat)
        if (stat /= 0) call displayDebug("Failed to not fail! - 1")

        if (vhead%version < 151) then
            call displayDebug("Incompatible VGM version! Must be at least 1.51!") 
            error = .TRUE.
            return
        end if  

        offset = offset + 4

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%SN76489 = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%YM2413  = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%GD3Offset = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%totalWaits = temp
        
        call readIntFromBin(d, s, offset, temp, 4)
        vhead%loopOffset = temp
        
        call readIntFromBin(d, s, offset, temp, 4)
        vhead%totalLoopSamples = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%rate = temp

        call readIntFromBin(d, s, offset, temp, 2)
        vhead%SN76489FB = temp

        call readIntFromBin(d, s, offset, temp, 1)
        vhead%SN76489sfW = temp

        call readIntFromBin(d, s, offset, temp, 1)
        vhead%SN76489flags = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%YM2612 = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%YM2151 = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%dataOffset = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%PCMClock = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%PCMIReg = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%RF5C68  = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%YM2203  = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%YM2608  = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%YM2610  = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%YM3812  = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%YM3526  = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%Y8950   = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%YMF262  = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%YMF278B = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%YMF271  = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%YMZ280B = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%RF5C164 = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%PWM     = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%AY8910  = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%AY8910Flags = temp

        call readIntFromBin(d, s, offset, temp, 1)
        vhead%volMod = temp

        call readIntFromBin(d, s, offset, temp, 1)
        ! skip one

        call readIntFromBin(d, s, offset, temp, 1)
        vhead%loopBase = temp

        call readIntFromBin(d, s, offset, temp, 1)
        vhead%loopMod = temp

    end subroutine

    subroutine openVGM()
        character(MAX_PATH_LEN)               :: fname, CMDMSG
        integer(2)                            :: lt, ind, ind2, RC, stat
        integer(2), dimension(:), allocatable :: d
        integer(4)                            :: s, volMod, numOfLoops, loopMod
        logical                               :: del, error
        integer(8)                            :: stopByte, GD3Index, loopIndex, dataIndex
        character(40)                         :: test
        integer(1)                            :: temp1

        fname   = FileDialog("", .FALSE., "vgm ") 
        lT = len_trim(fname)

        del = .FALSE.

        if (fname(lT-2:lt) == "vgz") then
            ind = 0
            do ind = (lT - 2), 1, -1
               if (fname(ind:ind) == "\") then
                   ind2 = ind  
                   exit 
               end if
            end do
            ind = ind + 1

            call execute_command_line( &
                 "exe\gzip.exe -d -c -f " // trim(fname) // " > temp\" //  fname(ind:lT - 4) // ".vgm", &
                  wait = .TRUE., exitstat = RC, CMDMSG = CMDMSG)

            if (RC /= 0) then 
                call displayDebug("Failed to decompress VGZ! " // CMDMSG)
            else
                fname = "temp\" //  fname(ind:lT - 4) // ".vgm"

            end if

            del = .TRUE.

        end if    

        call loadBinary(fname, d, s)

        allocate(vhead, stat = stat)
        if (stat /= 0) call displayDebug("Failed to allocate VGM header!") 

        call buildVGMHeader(d, s, error)

        if (error .EQV. .FALSE.) then
            stopByte = vhead%eofOffset + 4
    
            if (vhead%GD3Offset > 0) then
                GD3Index = vhead%GD3Offset + 20
            else
                GD3Index = 0  
            end if
    
            if (vhead%loopOffset > 0) then
                loopIndex = vhead%loopOffset + 28
            else
                loopIndex = 0  
            end if
    
            dataIndex= vhead%dataOffset + 52
    
            temp1 = f2bitsTo1Bit(vhead%volMod)
            if (temp1 == -63) temp1 = -64 

            volMod = 2 ** (temp1 / 32)            

            !test = ""
            !write(test, "(I0)") volMod 
            !call displayDebug(test)

        end if

        ! NEXT STEP: Get the song's name from VGM

        deallocate(vhead, stat = stat)
        if (stat /= 0) call displayDebug("Failed to deallocate VGM header!") 

        deallocate(d, stat = stat)
        if (stat /= 0) call  displayDebug("Failed to deallocate bytes of VGM!")

        !if (del .EQV. .TRUE.) call dFile(fname)

    end subroutine

END MODULE vgm
