MODULE vgm

    USE, INTRINSIC :: ISO_C_BINDING
    USE debugWindow
    USE dataLoader
    USE WINTERACTER
    USE RESID
    USE subs
    USE engineConstants
    USE adlib

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

    type gd3Tags

         character(:), allocatable :: title, game, system, author
                                        
    end type

    type(gd3Tags) :: gd3

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

        if (vhead%YM3812 == 0 .AND. vhead%YM3526 == 0 .AND. vhead%YMF262 == 0) then
            call displayDebug("VGM has no OPL, OPL2 or OPL3!")
            error = .TRUE.
        end if

    end subroutine

    subroutine openVGM()
        character(MAX_PATH_LEN)               :: fname, CMDMSG
        integer(2)                            :: lt, ind, ind2, RC, stat
        integer(2), dimension(:), allocatable :: d, songBytes
        integer(4)                            :: s, volMod, numOfLoops, loopMod
        logical                               :: del, error
        integer(8)                            :: stopByte, GD3Index, loopIndex, dataIndex
        !character(40)                         :: test
        integer(1)                            :: temp1
        character(MAX_PATH_LEN)               :: nameFinal
        character(255)                        :: adlibName, inBrackets        

        gd3%title    = "" 
        gd3%game     = ""
        gd3%system   = "" 
        gd3%author   = ""

        fname   = FileDialog("", .FALSE., "vgm ") 
        if (fname == "") return

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
            !write(test, "(Z0)") GD3Index 
            !call displayDebug(test)
            call fillGD3(d, GD3Index, error, s)

        end if

        if (error .EQV. .FALSE.) then
            adlibName = ""
    
            if (gd3%author /= "") adlibName = gd3%author   
               
            if (gd3%title /= "") then
                if (adlibName /= "") then 
                    adlibName = trim(adlibName) // ": " // gd3%title 
                else
                    adlibName = gd3%title     
                end if
            end if
    
            inBrackets = ""
            if (gd3%game /= "") inBrackets = gd3%game
    
            if (gd3%system /= "") then
                if (inBrackets /= "") then 
                    inBrackets = trim(inBrackets) // " | " // gd3%system
                else
                    inBrackets = gd3%system     
                end if
            end if
    
            if (adlibName == "") then
                adlibname = inBrackets
            else
                if (inBrackets /= "") then
                    adlibName = trim(adlibName) // " (" // trim(inBrackets) // ")"
                end if
            end if

            call vgmBytesToAdlibBytes(d, songBytes, dataIndex, GD3Index)

        end if

        if (allocated(vhead) .EQV. .TRUE.) then
            deallocate(vhead, stat = stat)
            if (stat /= 0) call displayDebug("Failed to deallocate VGM header!") 
        end if

        if (allocated(d) .EQV. .TRUE.) then
            deallocate(d, stat = stat)
            if (stat /= 0) call  displayDebug("Failed to deallocate bytes of VGM!")
        end if
        
        if (allocated(songBytes) .EQV. .TRUE.) then
            deallocate(songBytes, stat = stat)
            if (stat /= 0) call  displayDebug("Failed to deallocate songBytes of VGM!")
        end if

        if (allocated(gd3%title) .EQV. .TRUE.) then
            deallocate(gd3%title , stat = stat)
            deallocate(gd3%game  , stat = stat)
            deallocate(gd3%system, stat = stat)
            deallocate(gd3%author, stat = stat)
        end if

        !if (del .EQV. .TRUE.) call dFile(fname)

    end subroutine

    subroutine fillGD3(d, GD3Index, error, s)
        integer(2), dimension(:), allocatable :: d
        integer(8)                            :: GD3Index 
        integer(4)                            :: offset
        character(4)                          :: gd3HeaderName
        logical, intent(inout)                :: error
        integer(2), dimension(:), allocatable :: v
        character(8)                          :: v2                           
        character(2)                          :: vChar
        integer                               :: temp
        integer(2)                            :: ind, version, stat 
        integer(4)                            :: s
        !character(40)                         :: test
        character(:), allocatable             :: waste

        offset = GD3Index + 1
        waste  = ""

        call read4CharFromBin(d, s, offset, gd3HeaderName)  

        if (gd3HeaderName /= "Gd3 ") then 
            error = .TRUE.
            call displayDebug("Corrupted GD3 Header! It MUST be Gd3!")   
            return
        end if        
 
        call copyBytes(d, v, offset, offset + 4, 4)

        do ind = 4, 1, -1 
           write(vChar, "(Z2)") v(ind)
           if (vChar(1:1) == " ") vChar(1:1) = "0" 
  
           v2(9 - (ind * 2) : 10 - (ind * 2)) = vChar  
        end do

        read(v2, "(I8)") temp   
        version = temp

        offset  = offset + 4

        if (version /= 100) then 
            error = .TRUE.
            call displayDebug("Corrupted GD3 Version! It MUST be 1.00!")   
            return
        end if      

        call readIntFromBin(d, s, offset, temp, 4)
        
        !write(test, "(I0)") temp
        !call displayDebug(test) 

        call getNullTermString(gd3%title , d, offset, s)
        call getNullTermString(waste     , d, offset, s)
        call getNullTermString(gd3%game  , d, offset, s)
        call getNullTermString(waste     , d, offset, s)
        call getNullTermString(gd3%system, d, offset, s)
        call getNullTermString(waste     , d, offset, s)
        call getNullTermString(gd3%author, d, offset, s)

        deallocate(waste, stat = stat)

    end subroutine 

    subroutine vgmBytesToAdlibBytes(d, songBytes, dataIndex, GD3Index)
        integer(8)                                           :: GD3Index, dataIndex
        integer(2), dimension(:), allocatable                :: d
        integer(2), dimension(:), allocatable, intent(inout) :: songBytes
        integer(8)                                           :: ind, counter, waitTime
        integer(1)                                           :: ind2, stat

        integer(2), dimension(7)                             :: command_codes = &
        (/ Z'61', Z'62', Z'63', Z'66', Z'5A', Z'5B', Z'5E' /)       
        integer(1), dimension(7)                             :: command_indexAdd = &
        (/ 3, 1, 1, 0, 3, 3, 3 /)       
        
        logical                                              :: found = .FALSE.
        character(40)                                        :: test
        character(2)                                         :: command
         
        !write(test, "(Z0, ' | ', Z0)" ) dataIndex, GD3Index
        !call displayDebug(test)
        ind     = dataIndex + 1
        counter = 0    

        do while (ind <= GD3Index .AND. d(ind) /= Z'66')
           !write(test, "(Z0, ' | ', I0)") ind, d(ind)
           !call displayDebug(test)

           do ind2 = 1, size(command_codes), 1 
              found = .FALSE.  
              !write(test, "(Z0, ' | ', Z0)") d(ind), command_codes(ind2)
              !call displayDebug(test)

              if (command_codes(ind2) == d(ind)) then                 
                  found = .TRUE.  

                  select case(command_codes(ind2))  
                  case(Z'66')  
                       counter = counter + 0
                  case(Z'62')                      
                       counter = counter + 3  
                  case(Z'63')                      
                       counter = counter + 3                    
                  case(Z'61') 
                       waitTime = d(ind + 1) + (d(ind + 2) * 256)  

                       if (waitTime > 255) then
                           counter = counter + 3                    
                       else
                           counter = counter + 2                    
                       end if 

                  case default
                       counter = counter + 2 
                  end select

                  exit

              end if

           end do

           if (found .EQV. .FALSE.) then 
               write(command,  "(Z2)") d(ind)
               if (command(1:1) == " ") command(1:1) = "0"  

               call displayDebug("Invalid Command (" // command // ") parsed in VGM data!") 
               return
           else  
               ind = ind + command_indexAdd(ind2)
           end if
        end do

        allocate(songBytes(counter), stat = stat)
        if (stat /= 0) call displayDebug("Failed to allocate songBytes!")

        ind     = dataIndex + 1
        counter = 1

        ! 
        ! Normally, we just write the chip commands and values, but there are specials:
        ! $F6 : Wait 255+  samples
        ! $F7 : Wait 1-255 samples
        !

        do while (ind <= GD3Index .AND. d(ind) /= Z'66')
           do ind2 = 1, size(command_codes), 1 

              if (command_codes(ind2) == d(ind)) then                 
                  select case(command_codes(ind2))  
                  case(Z'66')  
                       counter = counter + 0
                  case(Z'62')                      
                       songBytes(counter    ) = Z'F6'
                       songBytes(counter + 1) = Z'DF'
                       songBytes(counter + 2) = Z'02'

                       counter = counter + 3  
                  case(Z'63')          
                       songBytes(counter    ) = Z'F6'
                       songBytes(counter + 1) = Z'72'
                       songBytes(counter + 2) = Z'03'
            
                       counter = counter + 3                    
                  case(Z'61') 
                       waitTime = d(ind + 1) + (d(ind + 2) * 256)  

                       if (waitTime > 255) then
                           counter = counter + 3  
                           songBytes(counter    ) = Z'F6'
                           songBytes(counter + 1) = d(ind + 1)
                           songBytes(counter + 2) = d(ind + 2)

                       else
                           if (waitTime > 0) then
                               counter = counter + 2     
                               songBytes(counter    ) = Z'F7'
                               songBytes(counter + 1) = d(ind + 1)  
                           end if
                       end if 

                  case default
                       songBytes(counter    ) = d(ind + 1) 
                       songBytes(counter + 1) = d(ind + 2) 
                       counter = counter + 2 
                  end select
                  ind = ind + command_indexAdd(ind2)
                  exit
              end if
           end do
        end do

    end subroutine 

END MODULE vgm
