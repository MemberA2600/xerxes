MODULE vgm

    USE, INTRINSIC :: ISO_C_BINDING
    USE debugWindow
    USE dataLoader
    USE WINTERACTER
    USE RESID
    USE subs
    USE engineConstants
    USE adlib
    USE waveplayer 
    USE inpout

    implicit none

    private
    public :: vgmConverter, setConverterFields

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

    type(gd3Tags)                :: gd3
    logical                      :: isPlaying

    contains

    !
    !  VGM Converter Window
    !

    subroutine VGMConverter()
       INTEGER                                 :: ITYPE
       TYPE(WIN_MESSAGE)                       :: MESSAGE
       !character(10)                  :: msgString
       integer                                 :: c 

       isPlaying = .FALSE. 

       CALL WDialogLoad(IDD_VGM2XXA)

       do
          CALL WDialogSelect(IDD_VGM2XXA)
          CALL WDialogShow(ITYPE=Modal)     
    
          if (WinfoDialog(CurrentDialog) == IDD_VGM2XXA) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     call stopPlayback()  
                     isPlaying = .FALSE. 
                     EXIT
                  CASE(ID_VGMLoad)
                     call openVGM()
                     isPlaying = .TRUE. 
                     call WDialogPutString(ID_XXANAME, getAdlibName())
                  CASE(ID_XXASave)
                     call saveXXA()
                  CASE(ID_XXAPlay)
                     call playM()
                     isPlaying = .TRUE. 
                  CASE(ID_XXAStop)
                     call stopPlayback()  
                     isPlaying = .FALSE.  
                  END SELECT
              end if
       end do 

       if (isPlaying .EQV. .TRUE.) then
            call stopPlayback()
       end if

       CALL WDialogUnLoad()

    END SUBROUTINE

    subroutine saveXXA()
        integer(2), dimension(:), allocatable :: d, fullD
        integer(2)                            :: stat, siz
        character(NAME_MAX_LEN)               :: name
        character(MAX_PATH_LEN)               :: fname

        fname = FileDialog("adlib\", .TRUE., "xxa ")  
        call WDialogGetString(ID_XXAName,  name)
        if (fname /= "") call saveAdlibData(name, fname)

    end subroutine

    subroutine playM()
        if (getLptMode() .EQV. .FALSE.) then
            call continueToPlayA()
        else
            call chipStart()
        end if
        isPlaying  = .TRUE.
    end subroutine

    subroutine stopPlayback()
        call stopMusic()
        isPlaying  = .FALSE.
    end subroutine

    subroutine setConverterFields()
       character(NAME_MAX_LEN)   :: name

       call WDialogGetString(ID_XXAName,  name)

        if (getNumOfAdlibBytes() == 0) then
            CALL WDialogFieldState(ID_VGMLoad, ENABLED) 
            CALL WDialogFieldState(ID_XXAPlay, DISABLED) 
            CALL WDialogFieldState(ID_XXASave, DISABLED) 
            CALL WDialogFieldState(ID_XXAStop, DISABLED) 
        else
            if (isPlaying .EQV. .TRUE.) then
                 CALL WDialogFieldState(ID_VGMLoad, DISABLED) 
                 CALL WDialogFieldState(ID_XXAPlay, DISABLED) 
                 CALL WDialogFieldState(ID_XXASave, DISABLED) 
                 CALL WDialogFieldState(ID_XXAStop, ENABLED) 
            else
                if (len_trim(name) == 0) then
                    CALL WDialogFieldState(ID_XXASave, DISABLED) 
                else
                    CALL WDialogFieldState(ID_XXASave, ENABLED) 
                end if

                 CALL WDialogFieldState(ID_VGMLoad, ENABLED) 
                 CALL WDialogFieldState(ID_XXAPlay, ENABLED) 
                 CALL WDialogFieldState(ID_XXAStop, DISABLED) 

            end if
        end if

    end subroutine

    subroutine buildVGMHeader(d, s, error)
        integer(2)                            :: stat, ind
        integer(8)                            :: s, offset
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

        if (vhead%YM3812 == 0 .AND. vhead%YM3526 == 0) then
            call displayDebug("VGM has no OPL or OPL2!")
            error = .TRUE.
        end if

    end subroutine

    subroutine openVGM()
        character(MAX_PATH_LEN)               :: fname, CMDMSG, out, tempPath
        integer(2)                            :: lt, ind, ind2, RC, stat
        integer(2), dimension(:), allocatable :: d, songBytes
        integer(8)                            :: s, volMod, numOfLoops, loopMod
        logical                               :: error, unc
        integer(8)                            :: stopByte, GD3Index, loopIndex, dataIndex
        !character(40)                         :: test
        integer(1)                            :: temp1
        character(MAX_PATH_LEN)               :: nameFinal
        character(255)                        :: adlibName, inBrackets        
        integer(8)                            :: reads, byteNum, loopByte

        gd3%title    = "" 
        gd3%game     = ""
        gd3%system   = "" 
        gd3%author   = ""

        fname   = FileDialog("", .FALSE., "vgm ") 
        if (fname == "") return

        lT = len_trim(fname)

        unc = (fname(lT-2:lt) == "vgz")          
        call loadBinary(fname, d, s, unc)

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

            
            call vgmBytesToAdlibBytes(d, songBytes, dataIndex, GD3Index, &
                                      reads, byteNum, loopIndex, loopByte)

            call initAdlibData()    

            if (len_trim(adlibName) > NAME_MAX_LEN) then
                adlibName = ""
                adlibName = gd3%title 

                if (gd3%game /= "") then
                    adlibName = trim(adlibName) // " (" // gd3%game // ")"
                end if

            end if

            call fillAdlibData(adlibName, songBytes, reads, byteNum, loopByte) 
            !call testPlay()

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
        integer(8)                            :: offset
        character(4)                          :: gd3HeaderName
        logical, intent(inout)                :: error
        integer(2), dimension(:), allocatable :: v
        character(8)                          :: v2                           
        character(2)                          :: vChar
        integer                               :: temp
        integer(2)                            :: ind, version, stat 
        integer(8)                            :: s
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

    subroutine vgmBytesToAdlibBytes(d, songBytes, dataIndex, GD3Index, &
                                    reads, byteNum, loopIndex, loopByte)
        integer(8)                                           :: GD3Index, dataIndex, loopIndex
        integer(2), dimension(:), allocatable                :: d
        integer(2), dimension(:), allocatable, intent(inout) :: songBytes
        integer(8)                                           :: ind, counter, waitTime, loopInd
        integer(1)                                           :: ind2, stat
        integer(8), intent(out)                              :: reads, byteNum, loopByte      

        integer(2), dimension(22)                            :: command_codes = &
        (/ Z'61', Z'62', Z'63', Z'66', Z'5A', Z'5B', &
           Z'70', Z'71', Z'72', Z'73', Z'74', Z'75', Z'76', Z'77', &
           Z'78', Z'79', Z'7A', Z'7B', Z'7C', Z'7D', Z'7E', Z'7F'  &
        /)       
        integer(1), dimension(6)                             :: command_indexAdd = &
        (/ 3, 1, 1, 0, 3, 3 /)
      
        
        logical                                              :: found = .FALSE., minus
        character(40)                                        :: test
        character(2)                                         :: command
        logical, parameter                                   :: debug = .TRUE.         

        if (debug .EQV. .TRUE.) open(unit = 21       , file = 'vmg_adlib_info.txt', &
                                     status='replace', action='write')

        !write(test, "(Z0, ' | ', Z0)" ) dataIndex, GD3Index
        !call displayDebug(test)
        ind     = dataIndex + 1
        If (loopIndex > 0) then
            loopInd = loopIndex - dataIndex      
        else
            loopInd = 0
        end if    

        counter  = 0    
        loopByte = 0     

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
                       counter = counter + 1  
                  case(Z'63')                      
                       counter = counter + 1                    
                  case(Z'61') 
                       waitTime = d(ind + 1) + (d(ind + 2) * 256)  

                       if (waitTime > 255) then
                           counter = counter + 3                    
                       else
                           counter = counter + 2                    
                       end if 
                  case(Z'70':Z'7F') 
                       counter = counter + 1 

                  case default
                       counter = counter + 2 
                      
                       select case(d(ind + 1)) 
                       case(Z'A0':Z'B8')
                            if (d(ind + 2) == 0) counter = counter - 1
                       end select  

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
               if (ind2 < 7) then   
                   ind = ind + command_indexAdd(ind2)
               else
                   ind = ind + 1  
               end if 
           end if
        end do

        allocate(songBytes(counter), stat = stat)
        if (stat /= 0) call displayDebug("Failed to allocate songBytes!")

        ind       = dataIndex + 1
        byteNum   = counter 
        songBytes = 0
        counter   = 1

        ! 
        ! Normally, we just write the chip commands and values, but there are specials:
        ! $05 : Wait 255+  samples
        ! $06 : Wait 1-255 samples
        ! $0A : Wait 735 samples
        ! $0B : wait 882 samples


        ! $A0 : If the value to write is 0, change "register" to $10 and write no 0.
        ! $B0 : If the value to write is 0, change "register" to $D0 and write no 0.
        !

        reads   = 0
        do while (ind <= GD3Index .AND. d(ind) /= Z'66')
           do ind2 = 1, size(command_codes), 1 

              if (command_codes(ind2) == d(ind)) then 
                  reads = reads + 1  
                  if (ind >= loopInd .AND. loopInd /= 0 .AND. loopByte == 0) loopByte = counter  - 1
                
                  minus = .FALSE.

                  select case(command_codes(ind2))  
                  case(Z'66')  
                       if (debug .EQV. .TRUE.) &
                           write(21, '(A)') "EndByte (0x66) found!!" 
                       counter = counter + 0
                  case(Z'62')                      
                       songBytes(counter    ) = Z'0A'
                       !songBytes(counter + 1) = Z'DF'
                       !songBytes(counter + 2) = Z'02'

                       if (debug .EQV. .TRUE.) &
                           write(21, '(A)') "62 >> 0A" 

                       counter = counter + 1  
                  case(Z'63')          
                       songBytes(counter    ) = Z'0B'
                       !songBytes(counter + 1) = Z'72'
                       !songBytes(counter + 2) = Z'03'
            
                       if (debug .EQV. .TRUE.) &
                           write(21, '(A)') "63 >> 0B" 

                       counter = counter + 1                    
                  case(Z'61') 
                       waitTime = d(ind + 1) + (d(ind + 2) * 256)  

                       if (waitTime > 255) then
                           songBytes(counter    ) = Z'05'
                           songBytes(counter + 1) = d(ind + 1)
                           songBytes(counter + 2) = d(ind + 2)
    
                           if (debug .EQV. .TRUE.) &
                               write(21, '("61 ", Z2.2, " ", Z2.2, " >> 05 ", Z2.2, " ", Z2.2)') &
                                     d(ind + 1), d(ind + 2), &
                                     d(ind + 1), d(ind + 2)

                           counter = counter + 3  

                       else
                           if (waitTime > 0) then
                               songBytes(counter    ) = Z'06'
                               songBytes(counter + 1) = d(ind + 1)  

                           if (debug .EQV. .TRUE.) &
                               write(21, '("61 ", Z2.2, " ", Z2.2,  " >> 06 ", Z2.2)') &
                                            d(ind + 1), d(ind + 2), &
                                            d(ind + 1)

                               counter = counter + 2     
                           end if
                       end if 
                  case(Z'70':Z'7F') 
                       !songBytes(counter    ) = Z'06' 
                       !songBytes(counter + 1) = d(ind) - Z'69' 

                       !if (debug .EQV. .TRUE.) &
                       !        write(21, "(Z2.2, ' >> 06 ', Z2.2)") &
                       !              command_codes(ind2), songBytes(counter + 1)

                       songbytes(counter) = shortWaitCode2Mask(d(ind)) 
                       if (debug .EQV. .TRUE.) &
                               write(21, "(Z2.2, ' >> ', Z2.2)") &
                                     d(ind), songBytes(counter)

                  case default
                       select case(d(ind+1))   
                       case(Z'A0':Z'B8')
                            if (d(ind + 2) == 0) minus = .TRUE.
                       !case default     
                       !     write(test, '("SZAR: ", Z2.2)') command_codes(ind2)
                       !     call displayDebug(test)
                       end select  

                       if (minus .EQV. .FALSE.) then 
                           songBytes(counter    ) = d(ind + 1) 
                           songBytes(counter + 1) = d(ind + 2) 

                           if (debug .EQV. .TRUE.) &
                               write(21, "(Z2.2, ' ', Z2.2, ' ', Z2.2, ' >> ', Z2.2, ' ', Z2.2)") &
                                     command_codes(ind2), d(ind + 1), d(ind + 2), &  
                                                          d(ind + 1), d(ind + 2)

                           counter = counter + 2 
                       else 
                           !
                           !  AX 00 >> 1X, BX 00 >> DX 
                           !      

                           select case(d(ind + 1))   
                           case(Z'A0':Z'A8')
                               songBytes(counter) = d(ind + 1) - Z'90'
                           case(Z'B0':Z'B8')                        
                               songBytes(counter) = d(ind + 1) + Z'20'    
                           !case default     
                           !     write(test, '("SZAR: ", Z2.2)') command_codes(ind2)
                           !     call displayDebug(test)    
                           end select  

                           if (debug .EQV. .TRUE.) &
                               write(21, "(Z2.2, ' ', Z2.2, ' ', Z2.2, ' >> ', Z2.2)") &
                                     command_codes(ind2), d(ind + 1), d(ind + 2), &  
                                                          songBytes(counter)

                           counter = counter + 1 
                       end if
                  end select

                  if (ind2 < 7) then  
                      ind = ind + command_indexAdd(ind2)
                  else  
                      ind = ind + 1
                  end if     

                  if (debug .EQV. .TRUE.) write(21, "('Counter: ', I0)") counter 

                  exit
              end if
           end do
        end do

        if (debug .EQV. .TRUE.) then
            write(21, '("Reads: ", I0, " Bytes: ", I0)') reads, byteNum
            close(unit = 21)
        end if    

    end subroutine 

END MODULE vgm
