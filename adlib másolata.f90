!
!   Based on Nuked-OPL
!   https://github.com/nukeykt/Nuked-OPL3
!

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
    USE ifport

    implicit none

    private
    public                      :: initAdlibData, fillAdlibData, testPlay

    type adlibData
         character(4)                          :: header
         integer(2)                            :: nameLen
         character(255)                        :: name 
         integer(8)                            :: numOfReads, numOfBytes, loopByte
         integer(2), dimension(:), allocatable :: songBytes
         integer(8)                            :: ind  
         integer(2), dimension(:), allocatable :: outBuffer
    end type 

    type(adlibData)                             :: adlibD 

    integer(2), dimension(:), allocatable       :: outBufferFull
    integer(8)                                  :: bufferIndex    
    integer(8)                                  :: bufferSize

    integer(2), parameter                       :: memLen = 124     
    logical                                     :: loopMe, adlibFirst = .TRUE.
    logical, parameter                          :: ch_norm = .FALSE., ch_drum = .TRUE., testDebug = .FALSE.
    character(40)                               :: test

    integer, parameter                          :: RATE = 44100
    integer(1), parameter                       :: minWait = 0 ! 27 on real hw
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

    !integer(2), dimension(memLen )        :: adlibMemory         
    integer(2), parameter                 :: OPL_WRITEBUF_SIZE  =  1024, &
                                             OPL_WRITEBUF_DELAY =  2


    type YM3812S
         type(YM3812C), pointer              :: channel

         integer(4)                          :: out, fbMod, prout
         integer(4), pointer                 :: mod
         integer(4), pointer                 :: trem 

         integer(4)                          :: eg_rout, eg_out
         integer(2)                          :: eg_inc , eg_gen  , eg_rate, eg_ksl, &
                                                reg_vib , reg_type, reg_ksr, reg_mult, reg_ksl, reg_tl, &
                                                reg_ar  , reg_dr  , reg_sl , reg_rr  , reg_wf
         integer(1)                          :: key, slot_num

         integer(8)                          :: pg_reset, pg_phase
         integer(4)                          :: pg_phaseOut
        
         contains
         procedure                           :: initSlot          => initSlot
         procedure                           :: envelopeUpdateKSL => envelopeUpdateKSL
         procedure                           :: envelopeCalc      => envelopeCalc
         procedure                           :: envelopeKeyOn     => envelopeKeyOn
         procedure                           :: envelopeKeyOff    => envelopeKeyOff
         procedure                           :: phaseGenerate     => phaseGenerate
         procedure                           :: slotGenerate      => slotGenerate
         procedure                           :: slotCalcFB        => slotCalcFB
         procedure                           :: processSlot       => processSlot

    end type

    type YM3812C
        type(YM3812S), dimension(2)          :: slotz
        integer(2)   , dimension(2)          :: out
        integer(1)                           :: channel_num

        integer(2)                           :: block, fb, con, alg, ksv
        integer(4)                           :: fnum, ch0, ch1
        logical                              :: chtype

        contains
        procedure                            :: initChannel => initChannel
        
    end type

    type OPLWritebuf
         integer(8) :: time
         integer(4) :: reg
         integer(2) :: data

    end type

    type YM3812
        type(YM3812C), dimension(9)          :: channels
        type(CounterTimer)                   :: counter
    !
    ! Address › value:          0.00335 ms
    ! Value › next address:     0.0235  ms
    !
        integer(8)                           :: timer, eg_timer, noise, writebuf_samplecnt, &
                                                writebuf_cur, writebuf_last, writebuf_lasttime     
        integer(2)                           :: eg_timerrem, eg_state, eg_add, eg_timer_lo, &
                                                newm, nts, rhy, vibpos, vibshift,  &
                                                tremolopos, tremoloshift 
        integer(4)                           :: rateratio, samplecnt                    
        integer(8)                           :: mixBuff, oldsample, sample
        integer(2)                           :: rm_hh_bit2, rm_hh_bit3, rm_hh_bit7, rm_hh_bit8, &
                                                rm_tc_bit3, rm_tc_bit5
        integer(4)                           :: tremolo, zeromod

        type(OPLWritebuf), dimension(OPL_WRITEBUF_SIZE) :: writeBuf      

    end type

    type(YM3812), target                     :: chip

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

    integer(1), dimension(16), parameter :: freqMulti = (/ &
        1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 20, 24, 24, 30, 30 /)

    integer(1), dimension(16), parameter :: kslrom = (/ &
        0, 32, 40, 45, 48, 51, 53, 55, 56, 58, 59, 60, 61, 62, 63, 64 /)

    integer(1), dimension(4), parameter :: kslShift = (/ 8, 1, 2, 0 /)

    integer(1), dimension(4,4), parameter :: incStep = reshape((/ &
        0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0 /), (/ 4,4 /), order=(/2,1/))

    integer(1), parameter                 :: RSM_FRAC                  = 10,     &
                                             egk_norm                  = Z'01',  & 
                                             egk_drum                  = Z'02',  &
                                             envelope_gen_num_attack   = 0,      &
                                             envelope_gen_num_decay    = 1,      &
                                             envelope_gen_num_sustain  = 2,      & 
                                             envelope_gen_num_release  = 3       

    integer(1), parameter, dimension(9) :: slots = (/ Z'00', Z'01', Z'02', &
                                                      Z'08', Z'09', Z'0A', &  
                                                      Z'10', Z'11', Z'12' /)
    
    integer(1), parameter               :: slot1Add = 3
    integer(1), parameter               :: slotLookUp(9,2) = reshape((/ &
                                            int(z'00',1), int(z'03',1), &
                                            int(z'01',1), int(z'04',1), &
                                            int(z'02',1), int(z'05',1), &
                                            int(z'08',1), int(z'0B',1), &
                                            int(z'09',1), int(z'0C',1), &
                                            int(z'0A',1), int(z'0D',1), &
                                            int(z'10',1), int(z'13',1), &
                                            int(z'11',1), int(z'14',1), &
                                            int(z'12',1), int(z'15',1)  &
                                        /), (/ 9, 2 /), order=(/ 2, 1 /))



    contains

    !
    !   Slot Related
    !

    subroutine initSlot(this, full, num, c)
        class(YM3812S), intent(inout) :: this
        logical                       :: full    
        integer(1)                    :: num
        type(YM3812C), target         :: c

        if (full .EQV. .TRUE.) then
            this%channel  => c
            this%mod      => null()
            this%trem     => null()   
            this%slot_num = num
        end if

        this%out         = 0
        this%fbMod       = 0
        this%prout       = 0

        this%eg_rout     = 0                
        this%eg_out      = 0
                      
        this%eg_inc      = 0
        this%eg_gen      = 0
        this%eg_rate     = 0
        this%eg_ksl      = 0
        this%reg_vib     = 0    
        this%reg_type    = 0
        this%reg_ksr     = 0
        this%reg_mult    = 0
        this%reg_ksl     = 0
        this%reg_tl      = 0
        this%reg_ar      = 0
        this%reg_dr      = 0
        this%reg_sl      = 0
        this%reg_rr      = 0
        this%reg_wf      = 0
        this%key         = 0    

        this%pg_reset    = 0
        this%pg_phase    = 0
        this%pg_phaseOut = 0     

    
        this%eg_rout = int(Z'1FF', kind=2)
        this%eg_out  = int(Z'1FF', kind=2)
        this%eg_gen = envelope_gen_num_release
        this%trem => chip%zeromod
    

    end subroutine

    subroutine envelopeUpdateKSL(this)
    
        class(YM3812S), intent(inout) :: this
    
        integer(2) :: ksl
    
        ksl = ishft(kslrom(ishft(this%channel%fnum, -6) + 1), 2) - &
              ishft(8 - this%channel%block, 5)
    
        if (ksl < 0) then
            ksl = 0
        end if
    
        this%eg_ksl = ksl
    
    end subroutine

    subroutine envelopeCalc(this)
        implicit none
    
        class(YM3812S), intent(inout) :: this

        integer(2) :: nonzero, rate, rate_hi, rate_lo, reg_rate, &
                      ks, eg_shift, shift, eg_off, reset
        integer(4) :: eg_rout, eg_inc
    
        reg_rate = 0
        reset    = 0
    
        this%eg_out = this%eg_rout                         + &
                      ishft(int(this%reg_tl, 4), 2)        + &
                      shiftr(int(this%eg_ksl, 4),           &
                             kslshift(int(this%reg_ksl, 4) + 1)) + &
                      int(this%trem, 4)
    
        if (this%key /= 0 .and.                              &
            this%eg_gen == envelope_gen_num_release) then
    
            reset    = 1
            reg_rate = this%reg_ar
    
        else
    
            select case (this%eg_gen)
    
            case (envelope_gen_num_attack)
                reg_rate = this%reg_ar
    
            case (envelope_gen_num_decay)
                reg_rate = this%reg_dr
    
            case (envelope_gen_num_sustain)
                if (this%reg_type == 0) then
                    reg_rate = this%reg_rr
                end if
    
            case (envelope_gen_num_release)
                reg_rate = this%reg_rr
    
            end select
    
        end if
    
        this%pg_reset = reset
    
        ks = shiftr(                                                   &
            int(this%channel%ksv, 4),                                  &
            ishft(ieor(int(this%reg_ksr, 4), 1), 1)                    &
        )
    
        if (reg_rate /= 0) then
            nonzero = 1
        else
            nonzero = 0
        end if
    
        rate    = ks + ishft(reg_rate, 2)
        rate_hi = shiftr(rate, 2)
        rate_lo = iand(rate, int(z'03', 4))
    
        if (iand(rate_hi, int(z'10', 4)) /= 0) then
            rate_hi = int(z'0F', 4)
        end if
    
        eg_shift = rate_hi + int(chip%eg_add, 4)
        shift    = 0
    
        if (nonzero /= 0) then
    
            if (rate_hi < 12) then
    
                if (chip%eg_state /= 0) then
    
                    select case (eg_shift)
    
                    case (12)
                        shift = 1
    
                    case (13)
                        shift = iand(shiftr(rate_lo, 1), 1)
    
                    case (14)
                        shift = iand(rate_lo, 1)
    
                    case default
                        ! No change.
    
                    end select
    
                end if
    
            else
    
                shift = iand(rate_hi, int(z'03', 4)) +             &
                        incStep(rate_lo + 1,                       &
                                int(chip%eg_timer_lo, 4) + 1)
    
                if (iand(shift, int(z'04', 4)) /= 0) then
                    shift = int(z'03', 4)
                end if
    
                if (shift == 0) then
                    shift = chip%eg_state
                end if
    
            end if
    
        end if
    
        eg_rout = this%eg_rout
        eg_inc  = 0
        eg_off  = 0
    
        ! Instant attack
        if (reset /= 0 .and. rate_hi == int(z'0F', 4)) then
            eg_rout = 0
        end if
    
        ! Envelope off
        if (iand(int(this%eg_rout, 4), int(z'1F8', 4)) == &
            int(z'1F8', 4)) then
            eg_off = 1
        end if
    
        if (this%eg_gen /= envelope_gen_num_attack .and. &
            reset == 0 .and. eg_off /= 0) then
    
            eg_rout = int(z'1FF', 4)
    
        end if
    
        select case (this%eg_gen)
    
        case (envelope_gen_num_attack)
    
            if (this%eg_rout == 0) then
    
                this%eg_gen = envelope_gen_num_decay
    
            else if (this%key /= 0 .and. shift > 0 .and. &
                     rate_hi /= int(z'0F', 4)) then
    
                eg_inc = shifta(                                     &
                    not(int(this%eg_rout, 4)), 4 - shift             )
    
            end if
    
        case (envelope_gen_num_decay)
    
            if (shiftr(int(this%eg_rout, 4), 4) == &
                int(this%reg_sl, 4)) then
    
                this%eg_gen = envelope_gen_num_sustain
    
            else if (eg_off == 0 .and. reset == 0 .and. shift > 0) then
    
                eg_inc = ishft(1, shift - 1)
    
            end if
    
        case (envelope_gen_num_sustain, envelope_gen_num_release)
    
            if (eg_off == 0 .and. reset == 0 .and. shift > 0) then
                eg_inc = ishft(1, shift - 1)
            end if
    
        end select
    
        this%eg_rout = iand(eg_rout + eg_inc, int(z'1FF', 4))
    
        ! Key on/reset
        if (reset /= 0) then
            this%eg_gen = envelope_gen_num_attack
        end if
    
        ! Key off
        if (this%key == 0) then
            this%eg_gen = envelope_gen_num_release
        end if
    
    end subroutine

    subroutine envelopeKeyOn(this, type)
    
        class(YM3812S), intent(inout) :: this
        integer(1), intent(in)        :: type
    
        this%key = ior(this%key, type)
    
    end subroutine 
    
    subroutine envelopeKeyOff(this, type)
    
        class(YM3812S), intent(inout) :: this
        integer(1), intent(in)        :: type
    
        this%key = iand(this%key, not(type))
    
    end subroutine 

    subroutine phaseGenerate(this)
        implicit none
    
        class(YM3812S), intent(inout) :: this
    
        integer(4) :: fnum, phase
        integer(8) :: basefreq, noise
        integer(2) :: rm_xor, n_bit, range, vibpos
       
        fnum = int(this%channel%fnum, 4)
    
        if (this%reg_vib /= 0) then
    
            range  = iand(shiftr(fnum, 7), 7)
            vibpos = int(chip%vibpos, 4)
    
            if (iand(vibpos, 3) == 0) then
                range = 0
    
            else if (iand(vibpos, 1) /= 0) then
                range = shiftr(range, 1)
            end if
    
            range = shiftr(range, int(chip%vibshift, 4))
    
            if (iand(vibpos, 4) /= 0) then
                range = -range
            end if
    
            fnum = fnum + range
    
        end if
    
        basefreq = shiftr( &
            ishft(fnum, int(this%channel%block, 4)), 1)
    
        phase = iand(shiftr(this%pg_phase, 9), int(z'FFFF', 4))
    
        if (this%pg_reset /= 0) then
            this%pg_phase = 0
        end if
    
        this%pg_phase = this%pg_phase + shiftr( &
            basefreq * int(freqMulti(int(this%reg_mult, 4) + 1), 4), 1)
    
        ! Rhythm mode
        noise = chip%noise
    
        this%pg_phaseOut = phase
    
        if (this%slot_num == 13) then       ! Hi-hat
            chip%rm_hh_bit2 = iand(shiftr(phase, 2), 1)
            chip%rm_hh_bit3 = iand(shiftr(phase, 3), 1)
            chip%rm_hh_bit7 = iand(shiftr(phase, 7), 1)
            chip%rm_hh_bit8 = iand(shiftr(phase, 8), 1)
        end if
    
        if (this%slot_num == 17 .and. &
            iand(int(chip%rhy, 4), int(z'20', 4)) /= 0) then
    
            ! Top cymbal
            chip%rm_tc_bit3 = iand(shiftr(phase, 3), 1)
            chip%rm_tc_bit5 = iand(shiftr(phase, 5), 1)
    
        end if
    
        if (iand(int(chip%rhy, 4), int(z'20', 4)) /= 0) then
    
            rm_xor = ior( &
                ior( &
                    ieor(int(chip%rm_hh_bit2, 4), &
                         int(chip%rm_hh_bit7, 4)), &
                    ieor(int(chip%rm_hh_bit3, 4), &
                         int(chip%rm_tc_bit5, 4))  &
                ), &
                ieor(int(chip%rm_tc_bit3, 4), &
                     int(chip%rm_tc_bit5, 4))      &
            )
    
            select case (this%slot_num)
    
            case (13)                         ! Hi-hat
    
                this%pg_phaseOut = ishft(rm_xor, 9)
    
                if (ieor(rm_xor, iand(noise, 1)) /= 0) then
                    this%pg_phaseOut = ior( &
                        this%pg_phaseOut, int(z'D0', 4))
                else
                    this%pg_phaseOut = ior( &
                        this%pg_phaseOut, int(z'34', 4))
                end if
    
            case (16)                         ! Snare drum
    
                this%pg_phaseOut = ior( &
                    ishft(int(chip%rm_hh_bit8, 4), 9), &
                    ishft( &
                        ieor(int(chip%rm_hh_bit8, 4), &
                             iand(noise, 1)), &
                        8) &
                )
    
            case (17)                         ! Top cymbal
    
                this%pg_phaseOut = ior( &
                    ishft(rm_xor, 9), int(z'80', 4))
    
            case default
                ! No rhythm-specific phase adjustment.
    
            end select
    
        end if
    
        n_bit = iand(ieor(shiftr(noise, 14), noise), 1)
    
        chip%noise = ior( &
            shiftr(noise, 1), &
            ishft(n_bit, 22))
    
    end subroutine 

    subroutine slotGenerate(this)
        class(YM3812S), intent(inout)      :: this

        this%out = envelopeCalcSin(this%pg_phaseOut + this%mod, this%eg_out, this%reg_wf)

    end subroutine

    subroutine slotCalcFB(this)
        implicit none

        class(YM3812S), intent(inout)      :: this
    
        if (this%channel%fb /= 0) then
    
            this%fbmod = shiftr( &
                this%prout + this%out, &
                int(z'09',4) - int(this%channel%fb,4))
    
        else
    
            this%fbmod = 0
    
        end if
    
        this%prout = this%out
    
    end subroutine

    subroutine processSlot(this)
        implicit none
    
        class(YM3812S), intent(inout) :: this
    
        call this%slotCalcFB()
        call this%envelopeCalc()
        call this%phaseGenerate()
        call this%slotGenerate()
    
    end subroutine

    !
    !   Channel Related
    !

    subroutine initChannel(this, full, num)
        class(YM3812C), intent(inout) :: this
        logical                       :: full
        integer(1)                    :: RC          
        integer(1)                    :: num          

        if (full .EQV. .TRUE.) this%channel_num = num

        this%block  = 0
        this%fb     = 0
        this%con    = 0
        this%alg    = 0
        this%ksv    = 0
        this%fnum   = 0
        this%ch0    = 0
        this%ch1    = 0
        this%chtype = ch_norm

        call this%slotz(1)%initSlot(full, slots(num)           , chip%channels(num))
        call this%slotz(2)%initSlot(full, slots(num) + slot1Add, chip%channels(num))

        this%out(1) = chip%zeromod
        this%out(2) = chip%zeromod
    
        call channelSetupAlg(this)

    end subroutine

    !
    !   Sound Generation (from Nuked-OPL)
    !

    function getMemoryIndex(address) result(ind)
         integer(2)            :: address
         integer(2)            :: ind

         do ind = 1, memLen, 1
            if (oplAddresses(ind) == address) return 
         end do

         ind = 0

    end function

    integer(4) function opl_sin(x)
        integer(2), intent(in) :: x

        opl_sin = int(sin(real(x,8) * pi / 512.0d0) * 65536.0d0)
    end function

    subroutine initChip()
        integer(2)              :: ind, rc    
        
        do ind = 1, 9, 1
           call chip%channels(ind)%initChannel(adlibFirst, ind) 
        end do
       
        chip%timer              = 0
        chip%eg_timer           = 0
        chip%noise              = 0
        chip%writebuf_samplecnt = 0
        chip%writebuf_cur       = 1
        chip%writebuf_last      = 0
        chip%writebuf_lasttime  = 0
        chip%eg_timerrem        = 0
        chip%eg_state           = 0
        chip%eg_add             = 0
        chip%eg_timer_lo        = 0
        chip%newm               = 0
        chip%nts                = 0
        chip%rhy                = 0
        chip%vibpos             = 0
        chip%vibshift           = 0
        chip%tremolo            = 0
        chip%tremolopos         = 0
        chip%tremoloshift       = 0
        chip%zeromod            = 0
        chip%rateratio          = 0
        chip%samplecnt          = 0
        chip%mixBuff            = 0
        chip%oldsample          = 0
        chip%sample             = 0
        chip%rm_hh_bit2         = 0
        chip%rm_hh_bit3         = 0
        chip%rm_hh_bit7         = 0
        chip%rm_hh_bit8         = 0
        chip%rm_tc_bit3         = 0         
        chip%rm_tc_bit5         = 0

        adlibFirst              = .FALSE.
        call chip%counter%timerInit()    
        adlibD%ind              = 0

        if (allocated(adlibD%outBuffer)) then
           deallocate(adlibD%outBuffer, stat = rc)  
           if (rc /= 0) call displayDebug("Failed to deallocate outBuffer of Adlib data!") 
        end if

    end subroutine

    subroutine initAdlibData()
         integer(2)           :: stat   

         adlibD%header        = 'xxa '
         adlibD%nameLen       =  0
         adlibD%name          =  ""   
         adlibD%numOfReads    =  0
         adlibD%numOfBytes    =  0          
         adlibD%loopByte      =  0
         loopMe               = .FALSE.

         if (allocated( adlibD%songBytes)) then
             deallocate(adlibD%songBytes, stat = stat)
             if (stat /= 0) call displayDebug("Failed to deallocate Adlib data bytes!")   
         end if   

         !adlibMemory = 0
         call reset(RATE)

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

         if (loopByte > 0) then
             loopMe           = .TRUE.
         else   
             loopMe           = .FALSE.
         end if

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

         !adlibMemory = 0
         !call initChip(.FALSE.)

    end subroutine




    function envelopeCalcExp(level) result(r)
        implicit none
    
        integer(4), intent(in) :: level
        integer(4)             :: l
        integer(4)             :: r     

        l = level
        if (l > int(z'1FFF')) then
            l = int(z'1FFF')
        end if
    
        r = ishft(ishft(exprom(iand(l, int(z'FF'))) + 1, 1), - (ishft(l, -8)) )
    
    end function


    function envelopeCalcSin0(phase, envelope) result(r)
        integer(4), intent(in) :: phase, envelope 
        integer(4)             :: p, out, neg
        integer(4)             :: r 

        p   = phase
        out = 0
        neg = 0

        p   = iand(phase, int(z'03FF'))

        if (iand(p, int(z'0200')) /= 0) then
            neg = Z'FFFF'
        end if

        if (iand(p, int(z'0100')) /= 0) then
            out = sinRom(ieor(iand(p, int(z'00FF')), int(z'00FF')) + 1) 
        else
            out = sinRom(iand(p, int(z'00FF')) + 1)
        end if

        r = ieor(envelopeCalcExp( out + ishft(iand(envelope, int(z'FFFF')), 3) ), neg)

    end function


    function envelopeCalcSin1(phase, envelope) result(r)
        integer(4), intent(in) :: phase, envelope 
        integer(4)             :: p, out
        integer(4)             :: r 

        p   = phase
        out = 0

        p   = iand(phase, int(z'03FF'))

        if (iand(p, int(z'0200')) /= 0) then
            out = Z'1000'
        else if (iand(p, int(z'0100')) /= 0) then
            out = sinRom(ieor(iand(p, int(z'00FF')), int(z'00FF')) + 1) 
        else
            out = sinRom(iand(p, int(z'00FF')) + 1)
        end if


        r = envelopeCalcExp(out + ishft(iand(envelope, int(z'FFFF')), 3))

    end function

    function envelopeCalcSin2(phase, envelope) result(r)
        integer(4), intent(in) :: phase, envelope 
        integer(4)             :: p, out
        integer(4)             :: r 

        p   = phase
        out = 0

        p   = iand(phase, int(z'03FF'))

        if (iand(p, int(z'0100')) /= 0) then
            out = sinRom(ieor(iand(p, int(z'00FF')), int(z'00FF')) + 1) 
        else
            out = sinRom(iand(p, int(z'00FF')) + 1)
        end if

        r = envelopeCalcExp(out + ishft(iand(envelope, int(z'FFFF')), 3))

    end function


    function envelopeCalcSin3(phase, envelope) result(r)
        integer(4), intent(in) :: phase, envelope 
        integer(4)             :: p, out
        integer(4)             :: r 

        p   = phase
        out = 0

        p   = iand(phase, int(z'03FF'))

        if (iand(p, int(z'0100')) /= 0) then
            out = Z'1000'
        else
            out = sinRom(iand(p, int(z'00FF')) + 1)
        end if

        r = envelopeCalcExp(out + ishft(iand(envelope, int(z'FFFF')), 3))

    end function

    function envelopeCalcSin(phase, envelope, waveform) result(r)
        integer(4), intent(in) :: phase, envelope
        integer(4)             :: r 
        integer(1), intent(in) :: waveform

        select case(waveform)
        case(Z'00')
             r = envelopeCalcSin0(phase, envelope)
        case(Z'01')
             r = envelopeCalcSin1(phase, envelope)
        case(Z'02')
             r = envelopeCalcSin2(phase, envelope)
        case(Z'03')
             r = envelopeCalcSin3(phase, envelope)
        end select

    end function

    function getChannelSlot(addr) result(r)
        integer(1), dimension(2)  :: r    
        integer(1)                :: ind, ind2, addrLo
        integer(2)                :: addr

        r = -1
        addrLo = iand(addr, Z'0F')

        do ind = 1, 9, 1
           do ind2 = 1, 2, 1
              if (addrLo == slotLookUp(ind,ind2)) then
                  r = (/ ind, ind2 /)
                  return
              end if
           end do 
        end do
    end function

    subroutine SlotWrite20(addrLOW, data)
    
        implicit none
    
        integer(2), intent(in)                  :: addrLOW, data
        integer(1), dimension(2)                :: channelSlot    
        type(YM3812S), pointer                  :: slot

        channelSlot =  getChannelSlot(addrLOW)   
        slot        => chip%channels(channelSlot(1))%slotz(channelSlot(2))

        if (iand(shiftr(int(data,4), 7), 1) /= 0) then
            slot%trem => chip%tremolo
        else
            slot%trem => chip%zeromod
        end if
    
        slot%reg_vib  = iand(shiftr(int(data,4), 6), 1)
        slot%reg_type = iand(shiftr(int(data,4), 5), 1)
        slot%reg_ksr  = iand(shiftr(int(data,4), 4), 1)
        slot%reg_mult = iand(int(data,4), int(z'0F',4))
    
    end subroutine


    subroutine SlotWrite40(addrLOW, data)
        integer(2), intent(in)                  :: addrLOW, data
        integer(1), dimension(2)                :: channelSlot    
        type(YM3812S), pointer                  :: slot       

        channelSlot =  getChannelSlot(addrLOW)   
        slot        => chip%channels(channelSlot(1))%slotz(channelSlot(2))

        slot%reg_ksl = iand(shiftr(int(data,4), 6), Z'03')
        slot%reg_tl  = iand(data, Z'3F')
        call slot%envelopeUpdateKSL()

    end subroutine


    subroutine SlotWrite60(addrLOW, data)

        integer(2), intent(in)                  :: addrLOW, data
        integer(1), dimension(2)                :: channelSlot    
        type(YM3812S), pointer                  :: slot       

        channelSlot =  getChannelSlot(addrLOW)   
        slot        => chip%channels(channelSlot(1))%slotz(channelSlot(2))

        slot%reg_ar = iand(shiftr(int(data,4), 4), Z'0F')
        slot%reg_dr = iand(data, Z'0F')

    end subroutine

    subroutine SlotWrite80(addrLOW, data)

        integer(2), intent(in)                  :: addrLOW, data
        integer(1), dimension(2)                :: channelSlot    
        type(YM3812S), pointer                  :: slot    

        channelSlot =  getChannelSlot(addrLOW)   
        slot        => chip%channels(channelSlot(1))%slotz(channelSlot(2))

        slot%reg_sl = iand(shiftr(int(data,4), 4), int(z'0F',4))
    
        if (slot%reg_sl == int(z'0F',4)) then
            slot%reg_sl = int(z'1F',4)
        end if
    
        slot%reg_rr = iand(int(data,4), int(z'0F',4))

    end subroutine

    subroutine SlotWriteE0(addrLOW, data)
        integer(2), intent(in)                  :: addrLOW, data
        integer(1), dimension(2)                :: channelSlot    
        type(YM3812S), pointer                  :: slot    

        channelSlot =  getChannelSlot(addrLOW)   
        slot        => chip%channels(channelSlot(1))%slotz(channelSlot(2))

        slot%reg_wf = iand(int(data,4), int(z'07',4))
    
        if (chip%newm == 0) then
            slot%reg_wf = iand(slot%reg_wf, int(z'03',4))
        end if

    end subroutine

    subroutine channelUpdateRhythm(data)
    
        implicit none
    
        integer(1), intent(in)                  :: data   
        integer(4) :: chnum
    
        chip%rhy = iand(int(data,4), int(z'3F',4))
    
        if (iand(int(chip%rhy,4), int(z'20',4)) /= 0) then
       
            chip%channels(7)%out(1) = chip%channels(7)%slotz(2)%out
            chip%channels(7)%out(2) = chip%channels(7)%slotz(2)%out
    
            chip%channels(8)%out(1) = chip%channels(8)%slotz(1)%out
            chip%channels(8)%out(2) = chip%channels(8)%slotz(1)%out
    
            chip%channels(9)%out(1) = chip%channels(9)%slotz(1)%out
            chip%channels(9)%out(2) = chip%channels(9)%slotz(1)%out
    
            do chnum = 7, 9, 1
                chip%channels(chnum)%chtype = ch_drum
            end do
    
            call channelSetupAlg(chip%channels(7))
            call channelSetupAlg(chip%channels(8))
            call channelSetupAlg(chip%channels(9))
    
            ! Hi-hat
            if (iand(int(chip%rhy,4), int(z'01',4)) /= 0) then
                call envelopeKeyOn(chip%channels(8)%slotz(1), egk_drum)
            else
                call envelopeKeyOff(chip%channels(8)%slotz(1), egk_drum)
            end if
    
            ! Top cymbal
            if (iand(int(chip%rhy,4), int(z'02',4)) /= 0) then
                call envelopeKeyOn(chip%channels(9)%slotz(2), egk_drum)
            else
                call envelopeKeyOff(chip%channels(9)%slotz(2), egk_drum)
            end if
    
            ! Tom-tom
            if (iand(int(chip%rhy,4), int(z'04',4)) /= 0) then
                call envelopeKeyOn(chip%channels(9)%slotz(1), egk_drum)
            else
                call envelopeKeyOff(chip%channels(9)%slotz(1), egk_drum)
            end if
    
            ! Snare drum
            if (iand(int(chip%rhy,4), int(z'08',4)) /= 0) then
                call envelopeKeyOn(chip%channels(8)%slotz(2), egk_drum)
            else
                call envelopeKeyOff(chip%channels(8)%slotz(2), egk_drum)
            end if
    
            ! Bass drum
            if (iand(int(chip%rhy,4), int(z'10',4)) /= 0) then
                call envelopeKeyOn(chip%channels(7)%slotz(1), egk_drum)
                call envelopeKeyOn(chip%channels(7)%slotz(2), egk_drum)
            else
                call envelopeKeyOff(chip%channels(7)%slotz(1), egk_drum)
                call envelopeKeyOff(chip%channels(7)%slotz(2), egk_drum)
            end if
    
        else
    
            do chnum = 7, 9
     
                chip%channels(chnum)%chtype = ch_norm

                call channelSetupAlg(chip%channels(chnum))
    
                call envelopeKeyOff( &
                    chip%channels(chnum)%slotz(1), egk_drum)
    
                call envelopeKeyOff( &
                    chip%channels(chnum)%slotz(2), egk_drum)
    
            end do
    
        end if
    
    end subroutine 

    subroutine channelWriteA0(addrLOW, data)
    
        implicit none
    
        type(YM3812C)               :: channel
        integer(2), intent(in)      :: data, addrLOW

        channel = chip%channels(iand(addrLOW, Z'000F') + 1)
       
        channel%fNum = ior( &
            iand(int(channel%fNum,4), int(z'300',4)), &
            iand(int(data,4), int(z'FF',4)) )
    
        channel%ksv = ior( &
            shiftl(int(channel%block,4), 1), &
            iand( &
                shiftr( &
                    int(channel%fNum,4), &
                    int(z'09',4) - int(chip%nts,4) ), &
                1 ) )
    
        call envelopeUpdateKSL(channel%slotz(1))
        call envelopeUpdateKSL(channel%slotz(2))
        
    end subroutine

    subroutine channelWriteB0(addrLOW, data)
    
        implicit none
    
        integer(4)                  :: data_u8
        type(YM3812C)               :: channel
        integer(2), intent(in)      :: data, addrLOW

        channel = chip%channels(iand(addrLOW, Z'000F') + 1)
    
        data_u8 = iand(int(data,4), int(z'FF',4))
    
        channel%fNum = ior( &
            iand(int(channel%fNum,4), int(z'00FF',4)), &
            shiftl(iand(data_u8, int(z'03',4)), 8) )
    
        channel%block = iand(shiftr(data_u8, 2), int(z'07',4))
    
        channel%ksv = ior( &
            shiftl(int(channel%block,4), 1), &
            iand( &
                shiftr( &
                    int(channel%fNum,4), &
                    int(z'09',4) - int(chip%nts,4) ), &
                1 ) )
    
        call envelopeUpdateKSL(channel%slotz(1))
        call envelopeUpdateKSL(channel%slotz(2))
    
    end subroutine 

    subroutine channelSetupAlg(channel)
    
        implicit none
    
        type(YM3812C), intent(inout), target :: channel
    
        integer(4) :: alg
    
        alg = int(channel%alg, 4)
    
        ! Rhythm/percussion channel
        if (channel%chtype .EQV. ch_drum) then
    
            ! Channels 7 and 8 in the C numbering:
            ! hi-hat/snare and tom/cymbal channels
            if (channel%channel_num == 8 .or. channel%channel_num == 9) then
    
                channel%slotz(1)%mod => chip%zeromod
                channel%slotz(2)%mod => chip%zeromod
    
                return
    
            end if
    
            select case (iand(alg, int(z'01',4)))
    
            case (int(z'00',4))
    
                channel%slotz(1)%mod => channel%slotz(1)%fbmod
                channel%slotz(2)%mod => channel%slotz(1)%out
    
            case (int(z'01',4))
    
                channel%slotz(1)%mod => channel%slotz(1)%fbmod
                channel%slotz(2)%mod => chip%zeromod
    
            end select
    
            return
    
        end if
    
        if (iand(alg, int(z'08',4)) /= 0) return
    
        select case (iand(alg, int(z'01',4)))
    
        case (int(z'00',4))
    
                channel%slotz(1)%mod => channel%slotz(1)%fbmod
                channel%slotz(2)%mod => channel%slotz(1)%out
    
                channel%out(1) = channel%slotz(2)%out
                channel%out(2) = chip%zeromod
    
        case (int(z'01',4))
    
                channel%slotz(1)%mod => channel%slotz(1)%fbmod
                channel%slotz(2)%mod => chip%zeromod
    
                channel%out(1) = channel%slotz(1)%out
                channel%out(2) = channel%slotz(2)%out
    
        end select
    
    end subroutine 

    subroutine channelUpdateAlg(channel)
    
        implicit none
    
        type(YM3812C), intent(inout) :: channel
    
        channel%alg = channel%con
        call channelSetupAlg(channel)
    
    end subroutine 

    subroutine channelWriteC0(addrLOW, data)
    
        implicit none
    
        type(YM3812C)               :: channel
        integer(2), intent(in)      :: data, addrLOW
        integer(4)                  :: data_u8

        channel = chip%channels(iand(addrLOW, Z'000F') + 1)
        
        data_u8 = iand(int(data,4), int(z'FF',4))
    
        channel%fb = iand( &
            shiftr(data_u8, 1), &
            int(z'07',4) )
    
        channel%con = iand(data_u8, int(z'01',4))
    
        call channelUpdateAlg(channel)
    
        if (chip%newm /= 0) then
    
            if (iand(shiftr(data_u8,4), 1) /= 0) then
                channel%ch0 = -1
            else
                channel%ch0 = 0
            end if
    
            if (iand(shiftr(data_u8,5), 1) /= 0) then
                channel%ch1 = -1
            else
                channel%ch1 = 0
            end if
       
        else
    
            channel%ch0 = -1
            channel%ch1 = -1
       
        end if
    
    end subroutine 

    subroutine channelKeyOn(addrLOW)
        implicit none
    
        type(YM3812C)               :: channel
        integer(2), intent(in)      :: addrLOW
    
        channel = chip%channels(iand(addrLOW, Z'000F') + 1)

        if (chip%newm /= 0) then
            call envelopeKeyOn(channel%slotz(0), egk_norm)
            call envelopeKeyOn(channel%slotz(1), egk_norm)
        else
    
            call envelopeKeyOn(channel%slotz(0), egk_norm)
            call envelopeKeyOn(channel%slotz(1), egk_norm)
    
        end if
    
    end subroutine

    subroutine channelKeyOff(addrLOW)
        implicit none
    
        type(YM3812C)               :: channel
        integer(2), intent(in)      :: addrLOW
    
        channel = chip%channels(iand(addrLOW, Z'000F') + 1)
    
        if (chip%newm /= 0) then
    
            call envelopeKeyOff(channel%slotz(0), egk_norm)
            call envelopeKeyOff(channel%slotz(1), egk_norm)
    
        else
    
            call envelopeKeyOff(channel%slotz(0), egk_norm)
            call envelopeKeyOff(channel%slotz(1), egk_norm)
    
        end if
    
    end subroutine 

    integer(2) function clipSample(sample)
        implicit none
    
        integer(4), intent(in) :: sample
            

        if (sample > 32767) then
            clipSample = 32767_2
        else if (sample < -32768) then
            clipSample = -32768_2
        else
            clipSample = int(sample, kind=2)
        end if
    
    end function 

    subroutine generate(buffer)
        implicit none
    
        integer(8), intent(out)    :: buffer
    
        type(YM3812C)    , pointer :: channel
        type(OPLWritebuf), pointer :: writebuf
    
        integer(4) :: mix
        integer(4) :: accm
        integer(2) :: ii
        integer(4) :: shift
    
        ! ------------------------------------------------------------
        ! Generate all 18 OPL2 operators.
        ! ------------------------------------------------------------
    
        do ii = 1, 9, 1
           call chip%channels(ii)%slotz(1)%processSlot()
           call chip%channels(ii)%slotz(2)%processSlot()
        end do
    
        ! ------------------------------------------------------------
        ! Mix the nine OPL2 channels.
        ! ------------------------------------------------------------
    
        mix = 0
    
        do ii = 1, 9
    
            channel => chip%channels(ii)
    
            accm = channel%out(1) + channel%out(2) 
    
            mix = mix + accm
    
        end do
    
        chip%mixbuff= mix
        buffer = clipSample(chip%mixbuff)
    
        ! ------------------------------------------------------------
        ! Tremolo generator.
        ! ------------------------------------------------------------
    
        if (iand(chip%timer, int(Z'3F', kind=4)) == int(Z'3F', kind=4)) then
            chip%tremolopos = mod(chip%tremolopos + 1, 210)
        end if
    
        if (chip%tremolopos < 105) then
            chip%tremolo = ishft(chip%tremolopos, -chip%tremoloshift)
        else
            chip%tremolo = ishft(210 - chip%tremolopos, &
                                 -chip%tremoloshift)
        end if
    
        ! ------------------------------------------------------------
        ! Vibrato generator.
        ! ------------------------------------------------------------
    
        if (iand(chip%timer, int(Z'3FF', kind=4)) == &
            int(Z'3FF', kind=4)) then
    
            chip%vibpos = iand(chip%vibpos + 1, 7)
    
        end if
    
        chip%timer = chip%timer + 1
    
        ! ------------------------------------------------------------
        ! Envelope generator timer.
        ! ------------------------------------------------------------
    
        shift = 0
    
        if (chip%eg_state /= 0) then
    
            do while (shift < 13 .and. &
                      .not. btest(chip%eg_timer, shift))
    
                shift = shift + 1
    
            end do
    
            if (shift > 12) then
                chip%eg_add = 0
            else
                chip%eg_add = shift + 1
            end if
    
            chip%eg_timer_lo = int(iand(chip%eg_timer, 3_8), kind=1)
    
        end if
    
        if (chip%eg_timerrem /= 0 .or. chip%eg_state /= 0) then
    
            if (chip%eg_timer == int(Z'FFFFFFFFF', kind=8)) then
    
                chip%eg_timer = 0_8
                chip%eg_timerrem = 1
    
            else
    
                chip%eg_timer = chip%eg_timer + 1_8
                chip%eg_timerrem = 0
    
            end if
    
        end if
    
        chip%eg_state = ieor(chip%eg_state, 1)
    
        ! ------------------------------------------------------------
        ! Process buffered register writes.
        ! ------------------------------------------------------------
    
        do
    
            writebuf => chip%writebuf(chip%writebuf_cur)
    
            if (writebuf%time > chip%writebuf_samplecnt) exit
    
            if (.not. btest(writebuf%reg, 9)) exit
    
            writebuf%reg = iand(writebuf%reg, int(Z'1FF', kind=2))
    
            call writeReg(writebuf%reg, writebuf%data)
    
            chip%writebuf_cur = chip%writebuf_cur + 1
            if (chip%writebuf_cur > OPL_WRITEBUF_SIZE) chip%writebuf_cur = 1                                 
    
        end do
    
        chip%writebuf_samplecnt = chip%writebuf_samplecnt + 1
    
    end subroutine 

    subroutine generateResampledOutput(sample_out)
        implicit none
    
        integer(2), intent(out) :: sample_out
    
        integer(8) :: interpolated
        integer(8) :: old_weight
        integer(8) :: new_weight
    
        do while (chip%samplecnt >= chip%rateratio)
    
            chip%oldsample = chip%sample
    
            !test = ""
            !write(test, "(I0, ' | ' , I0)") chip%samplecnt, chip%rateratio
            !call displayDebug(test)

            call generate(chip%sample)
   
            chip%samplecnt = chip%samplecnt - chip%rateratio
    
        end do
        !call displayDebug("Y!")

        old_weight = chip%rateratio - chip%samplecnt
        new_weight = chip%samplecnt
    
        interpolated = int(chip%oldsample, kind=8) * old_weight + &
                       int(chip%sample,    kind=8) * new_weight
    
        sample_out = int(interpolated / chip%rateratio, kind=2)
    
        chip%samplecnt = chip%samplecnt + ishft(1_8, RSM_FRAC)
    
    end subroutine 


    subroutine reset(samplerate)
        implicit none
    
        integer(4), intent(in) :: samplerate
    
        type(YM3812S), pointer :: slot
        type(YM3812C), pointer :: channel
    
        integer :: slotnum
        integer :: channum
        integer :: local_ch_slot
    
        ! ------------------------------------------------------------
        ! Replace the C memset(chip, 0, sizeof(opl3_chip)).
        ! ------------------------------------------------------------
    
        call initChip()
    
        ! ------------------------------------------------------------
        ! Global chip state.
        ! ------------------------------------------------------------
    
        chip%noise = 1
    
        chip%rateratio = &
            ishft(int(samplerate, kind=8), RSM_FRAC) / 49716_8
    
        chip%tremoloshift = 4
        chip%vibshift = 1
    
    end subroutine 

    subroutine writeReg(reg, v)
        implicit none
       
        integer(2), intent(in) :: reg
        integer(2), intent(in) :: v
    
        integer(4) :: regm
        integer(4) :: reg_group
        integer(4) :: low_nibble
        integer(4) :: slot_index
        integer(4) :: channel_index
        integer(4) :: value_u8
    
        ! Convert the signed Fortran INTEGER(1) into an unsigned 0..255 value.
        value_u8 = iand(int(v, kind=4), int(Z'FF', kind=4))
    
        ! OPL2 uses only the low 8 bits of the register address.
        regm = iand(int(reg, kind=4), int(Z'FF', kind=4))
    
        reg_group = iand(regm, int(Z'F0', kind=4))
        low_nibble = iand(regm, int(Z'0F', kind=4))
    
        select case (reg_group)
    
        ! ------------------------------------------------------------
        ! Global registers: $00-$0F
        ! ------------------------------------------------------------
    
        case (int(Z'00', kind=4))
    
            select case (low_nibble)
    
            case (8)
    
                ! C:
                ! chip->nts = (v >> 6) & 0x01;
    
                if (btest(value_u8, 6)) then
                    chip%nts = 1
                else
                    chip%nts = 0
                end if
    
            end select
    
        ! ------------------------------------------------------------
        ! Slot registers: $20-$3F
        ! ------------------------------------------------------------
    
        case (int(Z'20', kind=4), int(Z'30', kind=4))
      
              call slotWrite20(low_nibble, v)

    
        ! ------------------------------------------------------------
        ! Slot registers: $40-$5F
        ! ------------------------------------------------------------
    
        case (int(Z'40', kind=4), int(Z'50', kind=4))
    
              call slotWrite40(low_nibble, v)
    
        ! ------------------------------------------------------------
        ! Slot registers: $60-$7F
        ! ------------------------------------------------------------
    
        case (int(Z'60', kind=4), int(Z'70', kind=4))
    
              call slotWrite60(low_nibble, v)
    
        ! ------------------------------------------------------------
        ! Slot registers: $80-$9F
        ! ------------------------------------------------------------
    
        case (int(Z'80', kind=4), int(Z'90', kind=4))
    
              call slotWrite80(low_nibble, v)
    
        ! ------------------------------------------------------------
        ! Channel frequency low byte: $A0-$A8
        ! ------------------------------------------------------------
    
        case (int(Z'A0', kind=4))
    
              call channelWriteA0(low_nibble, v)
    
        ! ------------------------------------------------------------
        ! Channel frequency high, block and key-on: $B0-$B8
        ! Rhythm control: $BD
        ! ------------------------------------------------------------
    
        case (int(Z'B0', kind=4))
    
            if (regm == int(Z'BD', kind=4)) then
    
                ! C:
                ! tremoloshift = (((v >> 7) ^ 1) << 1) + 2;
    
                if (btest(value_u8, 7)) then
                    chip%tremoloshift = 2
                else
                    chip%tremoloshift = 4
                end if
    
                ! C:
                ! vibshift = ((v >> 6) & 1) ^ 1;
    
                if (btest(value_u8, 6)) then
                    chip%vibshift = 0
                else
                    chip%vibshift = 1
                end if
    
                call channelUpdateRhythm(v)
    
            else if (low_nibble < 9) then
    
                call channelWriteB0(low_nibble, v)
    
                if (btest(value_u8, 5)) then
                    call channelKeyOn( low_nibble)
                else
                    call channelKeyOff(low_nibble)
                end if
    
            end if
    
        ! ------------------------------------------------------------
        ! Channel feedback and connection: $C0-$C8
        ! ------------------------------------------------------------
    
        case (int(Z'C0', kind=4))
    
            if (low_nibble < 9) then
    
                call channelWriteC0(low_nibble, v)
    
            end if
    
        ! ------------------------------------------------------------
        ! Slot waveform select: $E0-$FF
        ! ------------------------------------------------------------
    
        case (int(Z'E0', kind=4), int(Z'F0', kind=4))
    
                call slotWriteE0(low_nibble, v)
    
        end select
    
    end subroutine 

    subroutine writeRegBuffered(reg, v)
        implicit none
     
        integer(2), intent(in) :: reg
        integer(2), intent(in) :: v
    
        type(OPLWritebuf), pointer :: writebuf
    
        integer(8) :: time1
        integer(8) :: time2
        integer(4) :: writebuf_last
        integer(4) :: reg_u16
    
        if (testDebug .EQV. .TRUE.) &  
            write(test, "(I8.8, '# set register ', Z2.2 ' for ', Z2.2, '!' )") adlibD%ind, &
                          reg, v


        reg_u16 = iand(int(reg, kind=4), int(Z'FFFF', kind=4))
    
        writebuf_last = chip%writebuf_last
        writebuf => chip%writebuf(writebuf_last)
    
        ! If this buffer position still contains a pending write,
        ! perform that write immediately before overwriting it.
        if (btest(int(writebuf%reg, kind=4), 9)) then
    
            call writeReg(int(iand(int(writebuf%reg, kind=4), &
                          int(Z'1FF', kind=4)), kind=2),      &
                          writebuf%data)
    
            chip%writebuf_cur = writebuf_last + 1
    
            if (chip%writebuf_cur > OPL_WRITEBUF_SIZE) then
                chip%writebuf_cur = 1
            end if
    
            chip%writebuf_samplecnt = writebuf%time
    
        end if

        ! Bit 9 marks this buffer entry as occupied/pending.
        writebuf%reg = int(ior(reg_u16, int(Z'200', kind=4)), kind=2)
        writebuf%data = v
    
        time1 = chip%writebuf_lasttime + OPL_WRITEBUF_DELAY
        time2 = chip%writebuf_samplecnt
    
        if (time1 < time2) then
            time1 = time2
        end if
    
        writebuf%time = time1
        chip%writebuf_lasttime = time1
    
        chip%writebuf_last = writebuf_last + 1
    
        if (chip%writebuf_last > OPL_WRITEBUF_SIZE) then
            chip%writebuf_last = 1
        end if
    
    end subroutine 

    subroutine generateStream(sndptr, numsamples)
        implicit none
        
        integer(2), intent(inout) :: sndptr(:)
        integer(4), intent(in)    :: numsamples
    
        integer(4) :: i
    
        do i = 1, numsamples, 1 
            !call displayDebug("FUCK!!!")
            call generateResampledOutput(sndptr(i))
            !sndptr = sndptr + 1
        end do
    
    end subroutine 

    subroutine testPlay
        integer(8)      :: last
        integer(1)      :: rc

        last          = 0
        bufferSize    = 0
        bufferIndex   = 1

        if (allocated(outBufferFull)) then
            deallocate(outBufferFull, stat = rc)
            if (rc /= 0) call displayDebug("Failed to deallocate full Adlib buffer!")
        end if

        allocate(outBufferFull(chunkSize), stat = rc)
        if (rc /= 0) call displayDebug("Failed to allocate full Adlib buffer!")

        outBufferFull = 0
        bufferSize    = chunkSize / 2

        call playMusicInit(outBufferFull, bufferSize, .TRUE.)

        do while (last <= adlibD%ind)
           call generateAdlib() 
           last = adlibD%ind
        end do

        deallocate(outBufferFull, stat = rc)
        if (rc /= 0) call displayDebug("Failed to deallocate full Adlib buffer!")

    end subroutine 

    subroutine buffer2Buffer()
        integer(8)                  :: lastIndex
        character(40)               :: test        

        lastIndex = bufferIndex + size(adlibD%outBuffer) - 1
         
        outBufferFull(bufferIndex:lastIndex) = adlibD%outBuffer(1:size(adlibD%outBuffer))
        bufferIndex = bufferIndex + size(adlibD%outBuffer)

        call AppendSamples("fos.bin", adlibD%outBuffer)

        if (lastIndex > bufferSize) then
            call wavFeedBuffer(outBufferFull, lastIndex)
            bufferIndex = 1
            call musicLoop()
        end if

    end subroutine         

    subroutine AppendSamples(filename, data)
    
        implicit none
    
        character(*), intent(in) :: filename
        integer(2), intent(in)   :: data(:)
    
        integer :: unit
    
        unit = 10
    
        open( &
            unit=unit, &
            file=filename, &
            access='stream', &
            form='unformatted', &
            status='unknown', &
            position='append')
    
        write(unit) data
    
        close(unit)
    
    end subroutine

    subroutine generateAdlib()
        implicit none
        integer(8)          :: waitTime, numOfFrames
        integer(1)          :: RC

        if (chip%counter%timerEnded() .EQ. .TRUE.) then
            if (adlibD%ind <= adlibD%numOfBytes) then
                adlibD%ind  = adlibD%ind + 1
                waitTime    = 0

                test        = ""
                select case(adlibD%songBytes(adlibD%ind))
                case(Z'66')  
                    ! EndByte    
                      if (testDebug .EQV. .TRUE.) &  
                      write(test, "(I8.8, '# Endbyte found!' )") adlibD%ind  

                      adlibD%ind = adlibD%loopByte
                case(Z'05')
                      waitTime   =  adlibD%songBytes(adlibD%ind + 1) + &
                                   (adlibD%songBytes(adlibD%ind + 2) * 256)  

                      if (testDebug .EQV. .TRUE.) &  
                      write(test, "(I8.8, '# wait for ', I8.8 ' ms!' )") adlibD%ind, &
                                     adlibD%songBytes(adlibD%ind + 1) + &
                                   (adlibD%songBytes(adlibD%ind + 2) * 256)           

                      adlibD%ind = adlibD%ind + 2
                case(Z'06')
                      waitTime   =  adlibD%songBytes(adlibD%ind + 1)

                      if (testDebug .EQV. .TRUE.) &  
                      write(test, "(I8.8, '# wait for ', I8.8 ' ms!' )") adlibD%ind, &
                                     adlibD%songBytes(adlibD%ind + 1)            

                      adlibD%ind = adlibD%ind + 1 
                case(Z'10':Z'1F')
                      waitTime = minWait
                      call writeRegBuffered(adlibD%songBytes(adlibD%ind) + Z'90', Z'00')    
                      !adlibD%ind = adlibD%ind + 1 

                case(Z'D0':Z'DF')
                      waitTime = minWait
                      !adlibD%ind = adlibD%ind + 1 
                      call writeRegBuffered(adlibD%songBytes(adlibD%ind) - Z'20', Z'00')   
                case default

                      waitTime = minWait
                      call writeRegBuffered(adlibD%songBytes(adlibD%ind),   & 
                                            adlibD%songBytes(adlibD%ind + 1))   
                      adlibD%ind = adlibD%ind + 1 

                end select
                
                if (testDebug .EQV. .TRUE.) then 
                    IF (test /= "") call displayDebug(test)
                end if

                if (allocated(adlibD%outBuffer)) then
                   deallocate(adlibD%outBuffer, stat = rc)  
                   if (rc /= 0) call displayDebug("Failed to deallocate outBuffer of Adlib data! #2") 
                end if

                if (waitTime > 0) then 
                    call chip%counter%timerStart(waitTime)
                    numOfFrames = ms2Frames(waitTime )
                    
                    !write(test, "('Num of Frames: ', I0)") numOfFrames 
                    !call displayDebug(test)

                    allocate(adlibD%outBuffer(numOfFrames), stat = rc)  
                    if (rc /= 0) call displayDebug("Failed to allocate outBuffer of Adlib data!")    
                    
                    call generateStream(adlibD%outBuffer, numOfFrames)
                    call buffer2Buffer() 

                end if
            else
                adlibD%ind = adlibD%loopByte
            end if
        end if
  
    end subroutine

    function ms2Frames(ms) result(frames) 
        integer(8)          :: ms
        integer(8)          :: frames   
        real                :: r

        r      = ms * RATE / 1000000
        frames = floor(r)

    end function

END MODULE adlib
