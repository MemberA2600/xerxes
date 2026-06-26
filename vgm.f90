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
         integer(2)   :: version    , SN76489sfW      , SN76489flags, AY8910Typ  , AY8910flags, &
                         YM2203Flags, YM2608Flags     , volMod      , reserv1    , loopBase   , &
                         loopMod    , MSM6258Flags    , K054539Flags, C140Typ    , reserv2    , &
                         ES5503amout, ES5505amount    , C352ClockDiv  
         integer(4)   :: SN76489FB    
         integer(8)   :: eofOffset  , SN76489         , YM2413      , GD3Offset  , totalWaits , &
                         loopOffset , totalLoopSamples, rate        , YM2612     , YM2151     , &
                         dataOffset , PCMClock        , PCMiReg     , RF5C68     , YM2203     , &
                         YM2608     , YM2610                        , YM3812     , YM3526     , & 
                         Y8950      , YMF262          , YMF278B     , YMF271     , YMZ280B    , &
                         RF5C164    , PWM             , AY8910      , DGM        , APU        , &
                         MultiPCM   , uPD7759         , MSM6258     , MSM6295    , K051649    , &
                         K054539    , HuC6280         , C140        , K053260    , Pokey      , &
                         QSound     , SCSP            , extraHead   , WonderSwan , VSU        , &
                         SAA1099    , ES5503          , ES5505      , X1_010     , C352       , &
                         GA20       , Mikey  
    end type

    type(vgmHeader)      :: vhead

    contains

    subroutine buildVGMHeader(d, s, error)
        integer(2)                            :: stat, ind
        integer(4)                            :: s, offset
        integer(2), dimension(:), allocatable :: d
        logical, intent(out)                  :: error
        integer(8)                            :: stopByte, GD3Index
        integer(2), dimension(:), allocatable :: v
        character(8)                          :: v2                           
        character(2)                          :: vChar
        integer                               :: temp
        character(40)                         :: test

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
  
        stopByte = vhead%eofOffset + 4

        call copyBytes(d, v, offset, offset + 4, 4)

        do ind = 4, 1, -1 
           write(vChar, "(Z2)") v(ind)
           if (vChar(1:1) == " ") vChar(1:1) = "0" 
  
           v2(9 - (ind * 2) : 10 - (ind * 2)) = vChar  
        end do

        read(v2, "(I8)") temp   
        vhead%version = temp

        !test = ""
        !write(test, "(I4)") vhead%version  
        !call displayDebug("version: " // test)

        offset = offset + 4

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%SN76489 = temp

        call readIntFromBin(d, s, offset, temp, 4)
        vhead%YM2413  = temp

        call readIntFromBin(d, s, offset, temp, 4)

        vhead%GD3Offset = temp

        if (vhead%GD3Offset) > 0 then
            GD3Index = vhead%GD3Offset + 20
        else
            GD3Index = 0  
        end if

        !test = ""
        !write(test, "(Z0)") GD3Index
        !call displayDebug("Offset: " // test)

        deallocate(v, stat = stat)
        if (stat /= 0) call displayDebug("Failed to not fail! - 1")

    end subroutine

    subroutine openVGM()
        character(MAX_PATH_LEN)               :: fname, CMDMSG
        integer(2)                            :: lt, ind, ind2, RC, stat
        integer(2), dimension(:), allocatable :: d
        integer(4)                            :: s
        logical                               :: del, error

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

        call buildVGMHeader(d, s, error)

        deallocate(d, stat = stat)
        if (stat /= 0) call  displayDebug("Failed to deallocate bytes of VGM!")

        !if (del .EQV. .TRUE.) call dFile(fname)

    end subroutine

END MODULE vgm
