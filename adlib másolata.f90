MODULE adlib
    USE, INTRINSIC :: ISO_C_BINDING
    USE debugWindow
    USE dataLoader
    USE waveplayer
    USE WINTERACTER
    USE RESID
    USE subs
    USE engineConstants
    USE winapis

    implicit none

    private
    public                      :: initAdlibData, fillAdlibData

    type adlibData
         character(4)                          :: header
         integer(2)                            :: nameLen
         character(255)                        :: name 
         integer(8)                            :: numOfReads, numOfBytes, loopByte
         integer(2), dimension(:), allocatable :: songBytes
    end type 

    type(adlibData)                             :: adlibD 

    integer(2), parameter                       :: memLen = 124     

    integer(2), dimension(memLen ), parameter   :: oplAddresses = &

    (/ Z'01', Z'02', Z'03', Z'04', Z'05', Z'08', &     ! Global 
                                                       ! Test register. Bit 5 enables waveform selection.
                                                       ! Timer1, Timer2, Timer control, IRQ reset,
                                                       ! Keyboard split / Note Select  
      
       Z'20', Z'21', Z'22', Z'23', Z'24', Z'25', &     ! Operators
       Z'28', Z'29', Z'2A', Z'2B', Z'2C', Z'2D', &     ! Tremolo, Vibrato, Sustain, 
       Z'30', Z'31', Z'32', Z'33', Z'34', Z'35', &     ! KSR, Frequency Multiplier 

       Z'40', Z'41', Z'42', Z'43', Z'44', Z'45', &     ! Operators
       Z'48', Z'49', Z'4A', Z'4B', Z'4C', Z'4D', &     ! Key Scale Level (KSL), 
       Z'50', Z'51', Z'52', Z'53', Z'54', Z'55', &     ! Total Level (volume)

       Z'60', Z'61', Z'62', Z'63', Z'64', Z'65', &     ! Operators
       Z'68', Z'69', Z'6A', Z'6B', Z'6C', Z'6D', &     ! Attack Rate, 
       Z'70', Z'71', Z'72', Z'73', Z'74', Z'75', &     ! Decay Rate 

       Z'80', Z'81', Z'82', Z'83', Z'84', Z'85', &     ! Operators
       Z'88', Z'89', Z'8A', Z'8B', Z'8C', Z'8D', &     ! Sustain Level, 
       Z'90', Z'91', Z'92', Z'93', Z'94', Z'95', &     ! Release Rate 

       Z'A0', Z'A1', Z'A2', Z'A3', Z'A4', Z'A5', &     ! Channels
       Z'A6', Z'A7', Z'A8',                      &     ! F-Number (LOW)

       Z'B0', Z'B1', Z'B2', Z'B3', Z'B4', Z'B5', &     ! Channels
       Z'B6', Z'B7', Z'B8',                      &     ! Key-On, Block (octave), F-Number (HI2)

       Z'BD',                                    &     ! Global
                                                       ! Tremolo depth, Vibrato depth, 
                                                       ! Rhythm mode, Drum triggers                                                    

       Z'C0', Z'C1', Z'C2', Z'C3', Z'C4', Z'C5', &     ! Channels
       Z'C6', Z'C7', Z'C8',                      &     ! F-Number (LOW)

       Z'E0', Z'E1', Z'E2', Z'E3', Z'E4', Z'E5', &     ! Operators
       Z'E8', Z'E9', Z'EA', Z'EB', Z'EC', Z'ED', &     ! Waveform Select
       Z'F0', Z'F1', Z'F2', Z'F3', Z'F4', Z'F5'  &     !  

       /)

    integer(2), dimension(memLen )        :: adlibMemory         

    Integer(4), dimension(256), parameter :: sinROM = (/ &
        Z'0859', Z'06c3', Z'0607', Z'058b', Z'052e', Z'04e4', Z'04a6', Z'0471',&
        Z'0443', Z'041a', Z'03f5', Z'03d3', Z'03b5', Z'0398', Z'037e', Z'0365',&
        Z'034e', Z'0339', Z'0324', Z'0311', Z'02ff', Z'02ed', Z'02dc', Z'02cd',&
        Z'02bd', Z'02af', Z'02a0', Z'0293', Z'0286', Z'0279', Z'026d', Z'0261',&
        Z'0256', Z'024b', Z'0240', Z'0236', Z'022c', Z'0222', Z'0218', Z'020f',&
        Z'0206', Z'01fd', Z'01f5', Z'01ec', Z'01e4', Z'01dc', Z'01d4', Z'01cd',&
        Z'01c5', Z'01be', Z'01b7', Z'01b0', Z'01a9', Z'01a2', Z'019b', Z'0195',&
        Z'018f', Z'0188', Z'0182', Z'017c', Z'0177', Z'0171', Z'016b', Z'0166',&
        Z'0160', Z'015b', Z'0155', Z'0150', Z'014b', Z'0146', Z'0141', Z'013c',&
        Z'0137', Z'0133', Z'012e', Z'0129', Z'0125', Z'0121', Z'011c', Z'0118',&
        Z'0114', Z'010f', Z'010b', Z'0107', Z'0103', Z'00ff', Z'00fb', Z'00f8',&
        Z'00f4', Z'00f0', Z'00ec', Z'00e9', Z'00e5', Z'00e2', Z'00de', Z'00db',&
        Z'00d7', Z'00d4', Z'00d1', Z'00cd', Z'00ca', Z'00c7', Z'00c4', Z'00c1',&
        Z'00be', Z'00bb', Z'00b8', Z'00b5', Z'00b2', Z'00af', Z'00ac', Z'00a9',&
        Z'00a7', Z'00a4', Z'00a1', Z'009f', Z'009c', Z'0099', Z'0097', Z'0094',&
        Z'0092', Z'008f', Z'008d', Z'008a', Z'0088', Z'0086', Z'0083', Z'0081',&
        Z'007f', Z'007d', Z'007a', Z'0078', Z'0076', Z'0074', Z'0072', Z'0070',&
        Z'006e', Z'006c', Z'006a', Z'0068', Z'0066', Z'0064', Z'0062', Z'0060',&
        Z'005e', Z'005c', Z'005b', Z'0059', Z'0057', Z'0055', Z'0053', Z'0052',&
        Z'0050', Z'004e', Z'004d', Z'004b', Z'004a', Z'0048', Z'0046', Z'0045',&
        Z'0043', Z'0042', Z'0040', Z'003f', Z'003e', Z'003c', Z'003b', Z'0039',&
        Z'0038', Z'0037', Z'0035', Z'0034', Z'0033', Z'0031', Z'0030', Z'002f',&
        Z'002e', Z'002d', Z'002b', Z'002a', Z'0029', Z'0028', Z'0027', Z'0026',&
        Z'0025', Z'0024', Z'0023', Z'0022', Z'0021', Z'0020', Z'001f', Z'001e',&
        Z'001d', Z'001c', Z'001b', Z'001a', Z'0019', Z'0018', Z'0017', Z'0017',&
        Z'0016', Z'0015', Z'0014', Z'0014', Z'0013', Z'0012', Z'0011', Z'0011',&
        Z'0010', Z'000f', Z'000f', Z'000e', Z'000d', Z'000d', Z'000c', Z'000c',&
        Z'000b', Z'000a', Z'000a', Z'0009', Z'0009', Z'0008', Z'0008', Z'0007',&
        Z'0007', Z'0007', Z'0006', Z'0006', Z'0005', Z'0005', Z'0005', Z'0004',&
        Z'0004', Z'0004', Z'0003', Z'0003', Z'0003', Z'0002', Z'0002', Z'0002',&
        Z'0002', Z'0001', Z'0001', Z'0001', Z'0001', Z'0001', Z'0001', Z'0001',&
        Z'0000', Z'0000', Z'0000', Z'0000', Z'0000', Z'0000', Z'0000', Z'0000' &
    /)

    integer(4), dimension(256), parameter :: exprom = (/ &
        Z'07fa', Z'07f5', Z'07ef', Z'07ea', Z'07e4', Z'07df', Z'07da', Z'07d4',&
        Z'07cf', Z'07c9', Z'07c4', Z'07bf', Z'07b9', Z'07b4', Z'07ae', Z'07a9',&
        Z'07a4', Z'079f', Z'0799', Z'0794', Z'078f', Z'078a', Z'0784', Z'077f',&
        Z'077a', Z'0775', Z'0770', Z'076a', Z'0765', Z'0760', Z'075b', Z'0756',&
        Z'0751', Z'074c', Z'0747', Z'0742', Z'073d', Z'0738', Z'0733', Z'072e',&
        Z'0729', Z'0724', Z'071f', Z'071a', Z'0715', Z'0710', Z'070b', Z'0706',&
        Z'0702', Z'06fd', Z'06f8', Z'06f3', Z'06ee', Z'06e9', Z'06e5', Z'06e0',&
        Z'06db', Z'06d6', Z'06d2', Z'06cd', Z'06c8', Z'06c4', Z'06bf', Z'06ba',&
        Z'06b5', Z'06b1', Z'06ac', Z'06a8', Z'06a3', Z'069e', Z'069a', Z'0695',&
        Z'0691', Z'068c', Z'0688', Z'0683', Z'067f', Z'067a', Z'0676', Z'0671',&
        Z'066d', Z'0668', Z'0664', Z'065f', Z'065b', Z'0657', Z'0652', Z'064e',&
        Z'0649', Z'0645', Z'0641', Z'063c', Z'0638', Z'0634', Z'0630', Z'062b',&
        Z'0627', Z'0623', Z'061e', Z'061a', Z'0616', Z'0612', Z'060e', Z'0609',&
        Z'0605', Z'0601', Z'05fd', Z'05f9', Z'05f5', Z'05f0', Z'05ec', Z'05e8',&
        Z'05e4', Z'05e0', Z'05dc', Z'05d8', Z'05d4', Z'05d0', Z'05cc', Z'05c8',&
        Z'05c4', Z'05c0', Z'05bc', Z'05b8', Z'05b4', Z'05b0', Z'05ac', Z'05a8',&
        Z'05a4', Z'05a0', Z'059c', Z'0599', Z'0595', Z'0591', Z'058d', Z'0589',&
        Z'0585', Z'0581', Z'057e', Z'057a', Z'0576', Z'0572', Z'056f', Z'056b',&
        Z'0567', Z'0563', Z'0560', Z'055c', Z'0558', Z'0554', Z'0551', Z'054d',&
        Z'0549', Z'0546', Z'0542', Z'053e', Z'053b', Z'0537', Z'0534', Z'0530',&
        Z'052c', Z'0529', Z'0525', Z'0522', Z'051e', Z'051b', Z'0517', Z'0514',&
        Z'0510', Z'050c', Z'0509', Z'0506', Z'0502', Z'04ff', Z'04fb', Z'04f8',&
        Z'04f4', Z'04f1', Z'04ed', Z'04ea', Z'04e7', Z'04e3', Z'04e0', Z'04dc',&
        Z'04d9', Z'04d6', Z'04d2', Z'04cf', Z'04cc', Z'04c8', Z'04c5', Z'04c2',&
        Z'04be', Z'04bb', Z'04b8', Z'04b5', Z'04b1', Z'04ae', Z'04ab', Z'04a8',&
        Z'04a4', Z'04a1', Z'049e', Z'049b', Z'0498', Z'0494', Z'0491', Z'048e',&
        Z'048b', Z'0488', Z'0485', Z'0482', Z'047e', Z'047b', Z'0478', Z'0475',&
        Z'0472', Z'046f', Z'046c', Z'0469', Z'0466', Z'0463', Z'0460', Z'045d',&
        Z'045a', Z'0457', Z'0454', Z'0451', Z'044e', Z'044b', Z'0448', Z'0445',&
        Z'0442', Z'043f', Z'043c', Z'0439', Z'0436', Z'0433', Z'0430', Z'042d',&
        Z'042a', Z'0428', Z'0425', Z'0422', Z'041f', Z'041c', Z'0419', Z'0416',&
        Z'0414', Z'0411', Z'040e', Z'040b', Z'0408', Z'0406', Z'0403', Z'0400' &
    /)

    real(4), dimension(16), parameter :: freqMulti = (/ &
        0.5, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 10.0, 12.0, 12.0, 15.0, 15.0 /)

    integer(1), dimension(16), parameter :: kslrom = (/ &
        0, 32, 40, 45, 48, 51, 53, 55, 56, 58, 59, 60, 61, 62, 63, 64 /)

    integer(1), dimension(4), parameter :: kslShift = (/ 8, 1, 2, 0 /)

    integer(1), dimension(4,4), parameter :: incStep = reshape((/ &
        0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0 /), (/ 4,4 /))


    real, dimension(4)                        :: outDecrementByOctave = (/ 0, 1.5, 3, 6 /)

    integer(2), dimension(124)                :: oplValues

    !
    ! Operator Addresses:
    ! ---------------------------
    ! Modulator addresses for channels:
    ! 1. 0x00          
    ! 2. 0x01   
    ! 3. 0x02    
    ! 4. 0x08   
    ! 5. 0x09  
    ! 6. 0x0A  
    ! 7. 0x10   
    ! 8. 0x11
    ! 9. 0x12
    ! 
    ! The Carrier is always Mod + 0x03.       
    !

    integer(2), dimension(21,2), parameter  :: channelSlotLookUp = &
    reshape((/ &
        1, 1, 2, 1, 3, 1, 1, 2, 2, 2, 3, 2, &
        0, 0, 0 ,0,                         &
        4, 1, 5, 1, 6, 1, 4, 2, 5, 2, 6, 2, &
        0, 0, 0 ,0,                         &
        7, 1, 8, 1, 9, 1, 7, 2, 8, 2, 9, 2, &
        0, 0, 0 ,0                          &
        /), (/ 21,2 /))  

    !
    ! Address › value:          0.00335 ms
    ! Value › next address:     0.0235  ms
    !
    type(CounterTimer)                    :: adlibTimer

    type YM3812s
         logical                          :: AM, VIB, sustainGate, KSR
         integer(1)                       :: outDecr: wave
         real(8)                          :: attack, decay, sustain, release

    end type

    type YM3812C
        integer(2)                        :: octave, freqLO, freqHI
        integer(2)                        :: frequency
        real(8)                           :: feedback
        logical                           :: doubleOp, keyON
        type(YM3812s), dimension(2)       :: slots

    end type

    type YM3812
         logical                          :: waveFormSelect, noteSelect, rythmEnable, &
                                             rHH, rTC, rTM, rSD, rBD 
         integer(2)                       :: vibratioDepth   
         real                             :: AMDepth  

         type(YM3812C), dimension(9)      :: channels

    end type

    type(YM3812)                          :: Ychip


    integer(2), dimension(2), parameter   :: vibratioDepthValues = (/ 7  , 14  /)
    real      , dimension(2), parameter   :: AMDepthValues       = (/ 1.0, 4.8 /)      
    real(8)   , dimension(8), parameter   :: feedbackList        = (/ 0.0 * pi,   &
                                                                      pi / 16.0,  &
                                                                      pi / 8.0,   &
                                                                      pi / 4.0,   &
                                                                      pi / 2.0,   &
                                                                      pi,         &
                                                                      pi * 2.0,   &
                                                                      pi * 4.0    /)

    

    contains

    function getMemoryIndex(address) result(ind)
         integer(2)            :: address
         integer(2)            :: ind

         do ind = 1, memLen, 1
            if (oplAddresses(ind) == address) return 
         end do

         ind = 0

    end function
    

    subroutine initChip()
          Ychip%waveFormSelect = .FALSE.
          Ychip%noteSelect     = .FALSE.
          Ychip%rythmEnable    = .FALSE.
          Ychip%rHH            = .FALSE.
          Ychip%rTC            = .FALSE.
          Ychip%rTM            = .FALSE.
          Ychip%rSD            = .FALSE. 
          Ychip%rBD            = .FALSE. 

          Ychip%vibratioDepth  = vibratioDepthValues(1) 
          Ychip%AMDepth        = AMDepthValues      (1)  !

    end subroutine

    subroutine initAdlibData()
         integer(2)             :: stat   

         adlibD%header        = 'xxa '
         adlibD%nameLen       =  0
         adlibD%name          =  ""   
         adlibD%numOfReads    =  0
         adlibD%numOfBytes    =  0          
         adlibD%loopByte      =  0

         if (allocated( adlibD%songBytes)) then
             deallocate(adlibD%songBytes, stat = stat)
             if (stat /= 0) call displayDebug("Failed to deallocate Adlib data bytes!")   
         end if   

         oplValues = 0
         call initChip()
    end subroutine

    subroutine fillAdlibData(adlibName, songBytes, reads, byteNum, loopByte) 
         character(255)                        :: adlibName       
         integer(2), dimension(:), allocatable :: songBytes
         integer(8)                            :: reads, byteNum, loopByte  
         integer(8)                            :: id         

         integer(2)                            :: stat   
         integer(8)                            :: ind

         adlibD%name          =  adlibName 
         adlibD%nameLen       =  len_trim(adlibName)        
    
         adlibD%numOfReads    =  reads
         adlibD%numOfBytes    =  byteNum          
         adlibD%loopByte      =  loopByte

         if (allocated( adlibD%songBytes)) then
             deallocate(adlibD%songBytes, stat = stat)
             if (stat /= 0) call displayDebug("Failed to deallocate Adlib data bytes!")   
         end if 

         allocate(adlibD%songBytes(byteNum), stat = stat)        
         if (stat /= 0) then 
             call displayDebug("Failed to allocate Adlib data bytes!")   
         else 
             do ind = 1, byteNum, 1
                adlibD%songBytes(ind) = songBytes(ind)
             end do
         end if   

         call initChip() 
        




    end subroutine

END MODULE adlib
