!===============================================================================
! Nuked OPL3 - Fortran 2003 translation
!
! Original: Nuked OPL3 emulator (C), Copyright (C) 2013-2020 Nuke.YKT
! Licensed under LGPL 2.1 or later (see original opl3.c / opl3.h header).
! This file is a line-for-line-equivalent translation of that emulator's
! logic from C to Fortran 2003, version 1.8 baseline, default build
! configuration (OPL_ENABLE_STEREOEXT = 0).
!
! Translation notes:
!   * C has no derived types with pointer "wires" between struct fields
!     that survive well in Fortran without TARGET/POINTER fragility, so
!     every C pointer used as an internal signal route (slot->mod,
!     channel->out[4], slot->trem, slot->channel, channel->pair, ...) is
!     replaced by a plain integer index (1-based) into the owning chip's
!     slot(:)/channel(:) arrays, plus a small integer "kind" tag where a
!     C pointer could point at one of several different targets.
!   * C's uint8_t/uint16_t/uint32_t are all stored as default 32-bit
!     signed INTEGER (kind i32); C's uint64_t as 64-bit signed INTEGER
!     (kind i64). Two's-complement wraparound arithmetic on a 32-bit
!     signed integer produces the identical bit pattern as unsigned
!     32-bit wraparound, so plain +,-,* reproduce C's unsigned overflow
!     behaviour exactly (gfortran does not trap on signed overflow).
!   * ISHFT is a logical shift (zero-fill in both directions) and is
!     used for every C ">>"/"<<" on an effectively-unsigned quantity,
!     which is nearly all of them. The two places in the original code
!     where a signed (possibly negative) 16-bit quantity is genuinely
!     right-shifted with sign extension use the ASHR helper below.
!   * Every point where the C code assigns to, or returns, an int16_t
!     value is passed through TRUNC_I16, which reinterprets the low 16
!     bits of the 32-bit working value as a signed 16-bit quantity -
!     the exact effect of a C (int16_t) narrowing conversion.
!===============================================================================
module opl3_mod
  use debugWindow
  use winapis

  implicit none
  private

  integer, parameter :: i32 = selected_int_kind(9)
  integer, parameter :: i64 = selected_int_kind(18)

  integer(i32), parameter :: OPL_WRITEBUF_SIZE  = 1024
  integer(i32), parameter :: OPL_WRITEBUF_DELAY = 2
  integer(i32), parameter :: RSM_FRAC = 10

  ! Channel types
  integer(i32), parameter :: ch_2op  = 0
  integer(i32), parameter :: ch_4op  = 1
  integer(i32), parameter :: ch_4op2 = 2
  integer(i32), parameter :: ch_drum = 3

  ! Envelope key types
  integer(i32), parameter :: egk_norm = 1  ! 0x01
  integer(i32), parameter :: egk_drum = 2  ! 0x02

  ! Envelope generator states
  integer(i32), parameter :: envelope_gen_num_attack  = 0
  integer(i32), parameter :: envelope_gen_num_decay   = 1
  integer(i32), parameter :: envelope_gen_num_sustain = 2
  integer(i32), parameter :: envelope_gen_num_release = 3

  ! "mod" pointer-replacement kinds (opl3_slot%mod_kind)
  integer(i32), parameter :: MOD_ZERO  = 0
  integer(i32), parameter :: MOD_FBMOD = 1
  integer(i32), parameter :: MOD_OUT   = 2

  ! "out[4]" pointer-replacement kinds (opl3_channel%out_kind)
  integer(i32), parameter :: OUT_ZERO = 0
  integer(i32), parameter :: OUT_SLOT = 1

  public :: opl3_chip
  public :: OPL3_Generate, OPL3_GenerateResampled, OPL3_Reset
  public :: OPL3_WriteReg, OPL3_WriteRegBuffered, OPL3_GenerateStream
  public :: OPL3_Generate4Ch, OPL3_Generate4ChResampled, OPL3_Generate4ChStream
  public :: ym3812  
  public :: OPL3_GenerateStreamMono  

  !-----------------------------------------------------------------------
  ! logsin table
  !-----------------------------------------------------------------------
  integer(i32), parameter :: logsinrom(0:255) = (/ &
    2137, 1731, 1543, 1419, 1326, 1252, 1190, 1137, 1091, 1050, &
    1013,  979,  949,  920,  894,  869,  846,  825,  804,  785, &
     767,  749,  732,  717,  701,  687,  672,  659,  646,  633, &
     621,  609,  598,  587,  576,  566,  556,  546,  536,  527, &
     518,  509,  501,  492,  484,  476,  468,  461,  453,  446, &
     439,  432,  425,  418,  411,  405,  399,  392,  386,  380, &
     375,  369,  363,  358,  352,  347,  341,  336,  331,  326, &
     321,  316,  311,  307,  302,  297,  293,  289,  284,  280, &
     276,  271,  267,  263,  259,  255,  251,  248,  244,  240, &
     236,  233,  229,  226,  222,  219,  215,  212,  209,  205, &
     202,  199,  196,  193,  190,  187,  184,  181,  178,  175, &
     172,  169,  167,  164,  161,  159,  156,  153,  151,  148, &
     146,  143,  141,  138,  136,  134,  131,  129,  127,  125, &
     122,  120,  118,  116,  114,  112,  110,  108,  106,  104, &
     102,  100,   98,   96,   94,   92,   91,   89,   87,   85, &
      83,   82,   80,   78,   77,   75,   74,   72,   70,   69, &
      67,   66,   64,   63,   62,   60,   59,   57,   56,   55, &
      53,   52,   51,   49,   48,   47,   46,   45,   43,   42, &
      41,   40,   39,   38,   37,   36,   35,   34,   33,   32, &
      31,   30,   29,   28,   27,   26,   25,   24,   23,   23, &
      22,   21,   20,   20,   19,   18,   17,   17,   16,   15, &
      15,   14,   13,   13,   12,   12,   11,   10,   10,    9, &
       9,    8,    8,    7,    7,    7,    6,    6,    5,    5, &
       5,    4,    4,    4,    3,    3,    3,    2,    2,    2, &
       2,    1,    1,    1,    1,    1,    1,    1,    0,    0, &
       0,    0,    0,    0,    0,    0 /)

  !-----------------------------------------------------------------------
  ! exp table
  !-----------------------------------------------------------------------
  integer(i32), parameter :: exprom(0:255) = (/ &
    2042, 2037, 2031, 2026, 2020, 2015, 2010, 2004, 1999, 1993, &
    1988, 1983, 1977, 1972, 1966, 1961, 1956, 1951, 1945, 1940, &
    1935, 1930, 1924, 1919, 1914, 1909, 1904, 1898, 1893, 1888, &
    1883, 1878, 1873, 1868, 1863, 1858, 1853, 1848, 1843, 1838, &
    1833, 1828, 1823, 1818, 1813, 1808, 1803, 1798, 1794, 1789, &
    1784, 1779, 1774, 1769, 1765, 1760, 1755, 1750, 1746, 1741, &
    1736, 1732, 1727, 1722, 1717, 1713, 1708, 1704, 1699, 1694, &
    1690, 1685, 1681, 1676, 1672, 1667, 1663, 1658, 1654, 1649, &
    1645, 1640, 1636, 1631, 1627, 1623, 1618, 1614, 1609, 1605, &
    1601, 1596, 1592, 1588, 1584, 1579, 1575, 1571, 1566, 1562, &
    1558, 1554, 1550, 1545, 1541, 1537, 1533, 1529, 1525, 1520, &
    1516, 1512, 1508, 1504, 1500, 1496, 1492, 1488, 1484, 1480, &
    1476, 1472, 1468, 1464, 1460, 1456, 1452, 1448, 1444, 1440, &
    1436, 1433, 1429, 1425, 1421, 1417, 1413, 1409, 1406, 1402, &
    1398, 1394, 1391, 1387, 1383, 1379, 1376, 1372, 1368, 1364, &
    1361, 1357, 1353, 1350, 1346, 1342, 1339, 1335, 1332, 1328, &
    1324, 1321, 1317, 1314, 1310, 1307, 1303, 1300, 1296, 1292, &
    1289, 1286, 1282, 1279, 1275, 1272, 1268, 1265, 1261, 1258, &
    1255, 1251, 1248, 1244, 1241, 1238, 1234, 1231, 1228, 1224, &
    1221, 1218, 1214, 1211, 1208, 1205, 1201, 1198, 1195, 1192, &
    1188, 1185, 1182, 1179, 1176, 1172, 1169, 1166, 1163, 1160, &
    1157, 1154, 1150, 1147, 1144, 1141, 1138, 1135, 1132, 1129, &
    1126, 1123, 1120, 1117, 1114, 1111, 1108, 1105, 1102, 1099, &
    1096, 1093, 1090, 1087, 1084, 1081, 1078, 1075, 1072, 1069, &
    1066, 1064, 1061, 1058, 1055, 1052, 1049, 1046, 1044, 1041, &
    1038, 1035, 1032, 1030, 1027, 1024 /)

  ! freq mult table multiplied by 2: 1/2,1,2,3,4,5,6,7,8,9,10,10,12,12,15,15
  integer(i32), parameter :: mt(0:15) = (/ &
    1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 20, 24, 24, 30, 30 /)

  ! ksl table
  integer(i32), parameter :: kslrom(0:15) = (/ &
    0, 32, 40, 45, 48, 51, 53, 55, 56, 58, 59, 60, 61, 62, 63, 64 /)

  integer(i32), parameter :: kslshift(0:3) = (/ 8, 1, 2, 0 /)

  ! envelope generator constants, eg_incstep(rate_lo, eg_timer_lo), both 0:3.
  ! TRANSPOSE(RESHAPE(row_major_data, [ncols,nrows])) is the standard idiom
  ! to port a C 2D array literal C[NR][NC] into Fortran F(0:NR-1,0:NC-1)
  ! such that F(i,j) == C[i][j] for all 0-based i,j.
  integer(i32), parameter :: eg_incstep(0:3, 0:3) = reshape((/ &
        0, 0, 0, 0, &
        1, 0, 0, 0, &
        1, 0, 1, 0, &
        1, 1, 1, 0 /), (/ 4,4 /), order=(/2,1/))

  ! address decoding: ad_slot(0:31) gives the 1-based slot index
  ! (C value + 1), or 0 to mean "invalid" (C's -1 sentinel).
  integer(i32), parameter :: ad_slot(0:31) = (/ &
     1,  2,  3,  4,  5,  6,  0,  0,  7,  8,  9, 10, 11, 12,  0,  0, &
    13, 14, 15, 16, 17, 18,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 /)

  ! ch_slot(1:18): 1-based first-operand slot index for each 1-based
  ! channel number (C value + 1).
  integer(i32), parameter :: ch_slot(1:18) = (/ &
     1,  2,  3,  7,  8,  9, 13, 14, 15, 19, 20, 21, 25, 26, 27, 31, 32, 33 /)

  !-----------------------------------------------------------------------
  ! opl3_slot: replaces C's struct _opl3_slot.
  !
  ! chan_idx replaces "channel" pointer (1-based index into chip%channel,
  ! 0 = unset). There is no "chip" back-pointer: every procedure that
  ! needs it receives the chip object as an explicit argument instead.
  !-----------------------------------------------------------------------
  type :: opl3_slot
    integer(i32) :: chan_idx = 0
    integer(i32) :: out_     = 0   ! int16_t out
    integer(i32) :: fbmod    = 0   ! int16_t fbmod
    integer(i32) :: mod_kind = MOD_ZERO
    integer(i32) :: mod_idx  = 0   ! meaning depends on mod_kind
    integer(i32) :: prout    = 0   ! int16_t prout
    integer(i32) :: eg_rout  = 0   ! uint16_t
    integer(i32) :: eg_out   = 0   ! uint16_t
    integer(i32) :: eg_inc   = 0   ! uint8_t  (unused by original logic; kept for structural parity)
    integer(i32) :: eg_gen   = 0   ! uint8_t
    integer(i32) :: eg_rate  = 0   ! uint8_t  (unused by original logic; kept for structural parity)
    integer(i32) :: eg_ksl   = 0   ! uint8_t
    logical      :: trem_is_tremolo = .false.  ! replaces uint8_t* trem
    integer(i32) :: reg_vib  = 0
    integer(i32) :: reg_type = 0
    integer(i32) :: reg_ksr  = 0
    integer(i32) :: reg_mult = 0
    integer(i32) :: reg_ksl  = 0
    integer(i32) :: reg_tl   = 0
    integer(i32) :: reg_ar   = 0
    integer(i32) :: reg_dr   = 0
    integer(i32) :: reg_sl   = 0
    integer(i32) :: reg_rr   = 0
    integer(i32) :: reg_wf   = 0
    integer(i32) :: key      = 0
    integer(i32) :: pg_reset = 0
    integer(i32) :: pg_phase = 0    ! uint32_t
    integer(i32) :: pg_phase_out = 0
    integer(i32) :: slot_num = 0    ! kept 0-based, matches hardware slot numbering
  end type opl3_slot

  !-----------------------------------------------------------------------
  ! opl3_channel: replaces C's struct _opl3_channel.
  !
  ! slotz_idx(2) replaces "slotz[2]" pointers, pair_idx replaces "pair"
  ! pointer, out_kind/out_idx(4) replace "out[4]" pointers.
  !-----------------------------------------------------------------------
  type :: opl3_channel
    integer(i32) :: slotz_idx(2) = 0
    integer(i32) :: pair_idx     = 0
    integer(i32) :: out_kind(4)  = OUT_ZERO
    integer(i32) :: out_idx(4)   = 0
    integer(i32) :: chtype = ch_2op
    integer(i32) :: f_num  = 0    ! uint16_t
    integer(i32) :: block  = 0
    integer(i32) :: fb     = 0
    integer(i32) :: con    = 0
    integer(i32) :: alg    = 0
    integer(i32) :: ksv    = 0
    integer(i32) :: cha = 0, chb = 0, chc = 0, chd = 0   ! uint16_t
    integer(i32) :: ch_num = 0    ! kept 0-based, matches hardware channel numbering
  end type opl3_channel

  !-----------------------------------------------------------------------
  ! opl3_writebuf: replaces C's opl3_writebuf.
  !-----------------------------------------------------------------------
  type :: opl3_writebuf
    integer(i64) :: time = 0
    integer(i32) :: reg  = 0
    integer(i32) :: data = 0
  end type opl3_writebuf

  !-----------------------------------------------------------------------
  ! opl3_chip: replaces C's struct _opl3_chip. slot/channel are plain
  ! data arrays owned directly by the chip, exactly as in the C struct
  ! (they are inline arrays there too, not separately allocated).
  !-----------------------------------------------------------------------
  type :: opl3_chip
    type(opl3_channel) :: channel(18)
    type(opl3_slot)    :: slot(36)
    integer(i32) :: timer = 0        ! uint16_t
    integer(i64) :: eg_timer = 0      ! uint64_t
    integer(i32) :: eg_timerrem = 0
    integer(i32) :: eg_state = 0
    integer(i32) :: eg_add = 0
    integer(i32) :: eg_timer_lo = 0
    integer(i32) :: newm = 0
    integer(i32) :: nts = 0
    integer(i32) :: rhy = 0
    integer(i32) :: vibpos = 0
    integer(i32) :: vibshift = 0
    integer(i32) :: tremolo = 0
    integer(i32) :: tremolopos = 0
    integer(i32) :: tremoloshift = 0
    integer(i32) :: noise = 0        ! uint32_t
    integer(i32) :: zeromod = 0      ! int16_t, always 0; canonical "zero" target
    integer(i32) :: mixbuff(4) = 0   ! int32_t
    integer(i32) :: rm_hh_bit2 = 0, rm_hh_bit3 = 0, rm_hh_bit7 = 0, rm_hh_bit8 = 0
    integer(i32) :: rm_tc_bit3 = 0, rm_tc_bit5 = 0
    ! OPL3L resampler state
    integer(i32) :: rateratio = 0
    integer(i32) :: samplecnt = 0
    integer(i32) :: oldsamples(4) = 0   ! int16_t
    integer(i32) :: samples(4) = 0      ! int16_t
    ! buffered register writes
    integer(i64) :: writebuf_samplecnt = 0
    integer(i32) :: writebuf_cur = 0
    integer(i32) :: writebuf_last = 0
    integer(i64) :: writebuf_lasttime = 0
    type(opl3_writebuf) :: writebuf(OPL_WRITEBUF_SIZE)
  end type opl3_chip

  type(opl3_chip) :: ym3812    

contains

  subroutine manualReset()
        integer(2) :: ind

        ym3812%timer = 0        ! uint16_t
        ym3812%eg_timer = 0      ! uint64_t
        ym3812%eg_timerrem = 0
        ym3812%eg_state = 0
        ym3812%eg_add = 0
        ym3812%eg_timer_lo = 0
        ym3812%newm = 0
        ym3812%nts = 0
        ym3812%rhy = 0
        ym3812%vibpos = 0
        ym3812%vibshift = 0
        ym3812%tremolo = 0
        ym3812%tremolopos = 0
        ym3812%tremoloshift = 0
        ym3812%noise = 0        ! uint32_t
        ym3812%zeromod = 0      ! int16_t, always 0; canonical "zero" target
        ym3812%mixbuff(4) = 0   ! int32_t
        ym3812%rm_hh_bit2 = 0
        ym3812%rm_hh_bit3 = 0
        ym3812%rm_hh_bit7 = 0
        ym3812%rm_hh_bit8 = 0
        ym3812%rm_tc_bit3 = 0
        ym3812%rm_tc_bit5 = 0
        ! OPL3L resampler state
        ym3812%rateratio = 0
        ym3812%samplecnt = 0
        ym3812%oldsamples(4) = 0   ! int16_t
        ym3812%samples(4) = 0      ! int16_t
        ! buffered register writes
        ym3812%writebuf_samplecnt = 0
        ym3812%writebuf_cur = 0
        ym3812%writebuf_last = 0
        ym3812%writebuf_lasttime = 0  

        do ind = 1, 18, 1
            ym3812%channel(ind)%slotz_idx(2) = 0
            ym3812%channel(ind)%pair_idx     = 0
            ym3812%channel(ind)%out_kind(4)  = OUT_ZERO
            ym3812%channel(ind)%out_idx(4)   = 0
            ym3812%channel(ind)%chtype = ch_2op
            ym3812%channel(ind)%f_num  = 0    ! uint16_t
            ym3812%channel(ind)%block  = 0
            ym3812%channel(ind)%fb     = 0
            ym3812%channel(ind)%con    = 0
            ym3812%channel(ind)%alg    = 0
            ym3812%channel(ind)%ksv    = 0
            ym3812%channel(ind)%cha = 0
            ym3812%channel(ind)%chb = 0
            ym3812%channel(ind)%chc = 0 
            ym3812%channel(ind)%chd = 0   ! uint16_t
        end do

        do ind = 1, 36, 1
            ym3812%slot(ind)%out_     = 0   ! int16_t out
            ym3812%slot(ind)%fbmod    = 0   ! int16_t fbmod
            ym3812%slot(ind)%mod_kind = MOD_ZERO
            ym3812%slot(ind)%mod_idx  = 0   ! meaning depends on mod_kind
            ym3812%slot(ind)%prout    = 0   ! int16_t prout
            ym3812%slot(ind)%eg_rout  = 0   ! uint16_t
            ym3812%slot(ind)%eg_out   = 0   ! uint16_t
            ym3812%slot(ind)%eg_inc   = 0   ! uint8_t  (unused by original logic; kept for structural parity)
            ym3812%slot(ind)%eg_gen   = 0   ! uint8_t
            ym3812%slot(ind)%eg_rate  = 0   ! uint8_t  (unused by original logic; kept for structural parity)
            ym3812%slot(ind)%eg_ksl   = 0   ! uint8_t
            ym3812%slot(ind)%trem_is_tremolo = .false.  ! replaces uint8_t* trem
            ym3812%slot(ind)%reg_vib  = 0
            ym3812%slot(ind)%reg_type = 0
            ym3812%slot(ind)%reg_ksr  = 0
            ym3812%slot(ind)%reg_mult = 0
            ym3812%slot(ind)%reg_ksl  = 0
            ym3812%slot(ind)%reg_tl   = 0
            ym3812%slot(ind)%reg_ar   = 0
            ym3812%slot(ind)%reg_dr   = 0
            ym3812%slot(ind)%reg_sl   = 0
            ym3812%slot(ind)%reg_rr   = 0
            ym3812%slot(ind)%reg_wf   = 0
            ym3812%slot(ind)%key      = 0
            ym3812%slot(ind)%pg_reset = 0
            ym3812%slot(ind)%pg_phase = 0    ! uint32_t
            ym3812%slot(ind)%pg_phase_out = 0
        end do

        do ind = 1, OPL_WRITEBUF_SIZE, 1
            ym3812%writebuf(ind)%time = 0
            ym3812%writebuf(ind)%reg  = 0
            ym3812%writebuf(ind)%data = 0
        end do

  end subroutine
    


  !=======================================================================
  ! Bit-manipulation helpers
  !=======================================================================

  ! Reinterpret the low 16 bits of x as a signed 16-bit value - the
  ! exact effect of C's "(int16_t) x" narrowing conversion.
  pure function trunc_i16(x) result(r)
    integer(i32), intent(in) :: x
    integer(i32) :: r
    r = iand(x, 65535_i32)
    if (r >= 32768_i32) r = r - 65536_i32
  end function trunc_i16

  ! Arithmetic (sign-extending) right shift of a signed value by n bits
  ! (n >= 0). Used only at the two places in the original C source where
  ! a possibly-negative int is right-shifted relying on sign extension.
  pure function ashr(x, n) result(r)
    integer(i32), intent(in) :: x, n
    integer(i32) :: r
    if (n <= 0) then
      r = x
    else if (x >= 0) then
      r = ishft(x, -n)
    else
      r = -ishft(-x - 1_i32, -n) - 1_i32
    end if
  end function ashr

  !=======================================================================
  ! "mod" / "out" / "trem" pointer-replacement dispatchers
  !=======================================================================

  pure function slot_mod_value(chip, islot) result(v)
    type(opl3_chip), intent(in) :: chip
    integer(i32), intent(in) :: islot
    integer(i32) :: v
    select case (chip%slot(islot)%mod_kind)
    case (MOD_FBMOD)
      v = chip%slot(chip%slot(islot)%mod_idx)%fbmod
    case (MOD_OUT)
      v = chip%slot(chip%slot(islot)%mod_idx)%out_
    case default
      v = chip%zeromod
    end select
  end function slot_mod_value

  pure function channel_out_value(chip, ich, k) result(v)
    type(opl3_chip), intent(in) :: chip
    integer(i32), intent(in) :: ich, k
    integer(i32) :: v
    if (chip%channel(ich)%out_kind(k) == OUT_SLOT) then
      v = chip%slot(chip%channel(ich)%out_idx(k))%out_
    else
      v = chip%zeromod
    end if
  end function channel_out_value

  pure function slot_trem_value(chip, islot) result(v)
    type(opl3_chip), intent(in) :: chip
    integer(i32), intent(in) :: islot
    integer(i32) :: v
    if (chip%slot(islot)%trem_is_tremolo) then
      v = chip%tremolo
    else
      v = chip%zeromod
    end if
  end function slot_trem_value

  !=======================================================================
  ! Envelope generator
  !=======================================================================

  pure function OPL3_EnvelopeCalcExp(level_in) result(r)
    integer(i32), intent(in) :: level_in
    integer(i32) :: r, level, idx, shiftamt, val
    level = level_in
    if (level > 8191_i32) level = 8191_i32
    idx = iand(level, 255_i32)
    shiftamt = ishft(level, -8)
    val = ishft(exprom(idx), 1)
    r = trunc_i16(ishft(val, -shiftamt))
  end function OPL3_EnvelopeCalcExp

  pure function OPL3_EnvelopeCalcSin0(phase_in, envelope) result(r)
    integer(i32), intent(in) :: phase_in, envelope
    integer(i32) :: r, phase, out_, neg
    out_ = 0; neg = 0
    phase = iand(phase_in, 1023_i32)
    if (iand(phase, 512_i32) /= 0) neg = 65535_i32
    if (iand(phase, 256_i32) /= 0) then
      out_ = logsinrom(ieor(iand(phase, 255_i32), 255_i32))
    else
      out_ = logsinrom(iand(phase, 255_i32))
    end if
    r = trunc_i16(ieor(OPL3_EnvelopeCalcExp(out_ + ishft(envelope, 3)), neg))
  end function OPL3_EnvelopeCalcSin0

  pure function OPL3_EnvelopeCalcSin1(phase_in, envelope) result(r)
    integer(i32), intent(in) :: phase_in, envelope
    integer(i32) :: r, phase, out_
    out_ = 0
    phase = iand(phase_in, 1023_i32)
    if (iand(phase, 512_i32) /= 0) then
      out_ = 4096_i32
    else if (iand(phase, 256_i32) /= 0) then
      out_ = logsinrom(ieor(iand(phase, 255_i32), 255_i32))
    else
      out_ = logsinrom(iand(phase, 255_i32))
    end if
    r = OPL3_EnvelopeCalcExp(out_ + ishft(envelope, 3))
  end function OPL3_EnvelopeCalcSin1

  pure function OPL3_EnvelopeCalcSin2(phase_in, envelope) result(r)
    integer(i32), intent(in) :: phase_in, envelope
    integer(i32) :: r, phase, out_
    out_ = 0
    phase = iand(phase_in, 1023_i32)
    if (iand(phase, 256_i32) /= 0) then
      out_ = logsinrom(ieor(iand(phase, 255_i32), 255_i32))
    else
      out_ = logsinrom(iand(phase, 255_i32))
    end if
    r = OPL3_EnvelopeCalcExp(out_ + ishft(envelope, 3))
  end function OPL3_EnvelopeCalcSin2

  pure function OPL3_EnvelopeCalcSin3(phase_in, envelope) result(r)
    integer(i32), intent(in) :: phase_in, envelope
    integer(i32) :: r, phase, out_
    out_ = 0
    phase = iand(phase_in, 1023_i32)
    if (iand(phase, 256_i32) /= 0) then
      out_ = 4096_i32
    else
      out_ = logsinrom(iand(phase, 255_i32))
    end if
    r = OPL3_EnvelopeCalcExp(out_ + ishft(envelope, 3))
  end function OPL3_EnvelopeCalcSin3

  pure function OPL3_EnvelopeCalcSin4(phase_in, envelope) result(r)
    integer(i32), intent(in) :: phase_in, envelope
    integer(i32) :: r, phase, out_, neg
    out_ = 0; neg = 0
    phase = iand(phase_in, 1023_i32)
    if (iand(phase, 768_i32) == 256_i32) neg = 65535_i32
    if (iand(phase, 512_i32) /= 0) then
      out_ = 4096_i32
    else if (iand(phase, 128_i32) /= 0) then
      out_ = logsinrom(iand(ishft(ieor(phase, 255_i32), 1), 255_i32))
    else
      out_ = logsinrom(iand(ishft(phase, 1), 255_i32))
    end if
    r = trunc_i16(ieor(OPL3_EnvelopeCalcExp(out_ + ishft(envelope, 3)), neg))
  end function OPL3_EnvelopeCalcSin4

  pure function OPL3_EnvelopeCalcSin5(phase_in, envelope) result(r)
    integer(i32), intent(in) :: phase_in, envelope
    integer(i32) :: r, phase, out_
    out_ = 0
    phase = iand(phase_in, 1023_i32)
    if (iand(phase, 512_i32) /= 0) then
      out_ = 4096_i32
    else if (iand(phase, 128_i32) /= 0) then
      out_ = logsinrom(iand(ishft(ieor(phase, 255_i32), 1), 255_i32))
    else
      out_ = logsinrom(iand(ishft(phase, 1), 255_i32))
    end if
    r = OPL3_EnvelopeCalcExp(out_ + ishft(envelope, 3))
  end function OPL3_EnvelopeCalcSin5

  pure function OPL3_EnvelopeCalcSin6(phase_in, envelope) result(r)
    integer(i32), intent(in) :: phase_in, envelope
    integer(i32) :: r, phase, neg
    neg = 0
    phase = iand(phase_in, 1023_i32)
    if (iand(phase, 512_i32) /= 0) neg = 65535_i32
    r = trunc_i16(ieor(OPL3_EnvelopeCalcExp(ishft(envelope, 3)), neg))
  end function OPL3_EnvelopeCalcSin6

  pure function OPL3_EnvelopeCalcSin7(phase_in, envelope) result(r)
    integer(i32), intent(in) :: phase_in, envelope
    integer(i32) :: r, phase, out_, neg
    out_ = 0; neg = 0
    phase = iand(phase_in, 1023_i32)
    if (iand(phase, 512_i32) /= 0) then
      neg = 65535_i32
      phase = ieor(iand(phase, 511_i32), 511_i32)
    end if
    out_ = ishft(phase, 3)
    r = trunc_i16(ieor(OPL3_EnvelopeCalcExp(out_ + ishft(envelope, 3)), neg))
  end function OPL3_EnvelopeCalcSin7

  ! Dispatch replacing the C envelope_sin[8] function-pointer table.
  pure function envelope_sin(wf, phase, envelope) result(r)
    integer(i32), intent(in) :: wf, phase, envelope
    integer(i32) :: r
    select case (wf)
    case (0); r = OPL3_EnvelopeCalcSin0(phase, envelope)
    case (1); r = OPL3_EnvelopeCalcSin1(phase, envelope)
    case (2); r = OPL3_EnvelopeCalcSin2(phase, envelope)
    case (3); r = OPL3_EnvelopeCalcSin3(phase, envelope)
    case (4); r = OPL3_EnvelopeCalcSin4(phase, envelope)
    case (5); r = OPL3_EnvelopeCalcSin5(phase, envelope)
    case (6); r = OPL3_EnvelopeCalcSin6(phase, envelope)
    case default; r = OPL3_EnvelopeCalcSin7(phase, envelope)
    end select
  end function envelope_sin

  subroutine OPL3_EnvelopeUpdateKSL(chip, islot)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot
    integer(i32) :: ich, ksl
    ich = chip%slot(islot)%chan_idx
    ksl = ishft(kslrom(ishft(chip%channel(ich)%f_num, -6)), 2) &
        - ishft(8_i32 - chip%channel(ich)%block, 5)
    if (ksl < 0) ksl = 0
    chip%slot(islot)%eg_ksl = iand(ksl, 255_i32)
  end subroutine OPL3_EnvelopeUpdateKSL

  subroutine OPL3_EnvelopeCalc(chip, islot)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot
    integer(i32) :: ich
    integer(i32) :: nonzero, rate, rate_hi, rate_lo, reg_rate, ks
    integer(i32) :: eg_shift, shift, eg_rout, eg_inc, eg_off, reset

    ich = chip%slot(islot)%chan_idx

    chip%slot(islot)%eg_out = iand( &
        chip%slot(islot)%eg_rout + ishft(chip%slot(islot)%reg_tl, 2) &
      + ishft(chip%slot(islot)%eg_ksl, -kslshift(chip%slot(islot)%reg_ksl)) &
      + slot_trem_value(chip, islot), 65535_i32)

    reg_rate = 0
    reset = 0
    if (chip%slot(islot)%key /= 0 .and. chip%slot(islot)%eg_gen == envelope_gen_num_release) then
      reset = 1
      reg_rate = chip%slot(islot)%reg_ar
    else
      select case (chip%slot(islot)%eg_gen)
      case (envelope_gen_num_attack)
        reg_rate = chip%slot(islot)%reg_ar
      case (envelope_gen_num_decay)
        reg_rate = chip%slot(islot)%reg_dr
      case (envelope_gen_num_sustain)
        if (chip%slot(islot)%reg_type == 0) reg_rate = chip%slot(islot)%reg_rr
      case (envelope_gen_num_release)
        reg_rate = chip%slot(islot)%reg_rr
      end select
    end if
    chip%slot(islot)%pg_reset = reset

    ks = ishft(chip%channel(ich)%ksv, -ishft(ieor(chip%slot(islot)%reg_ksr, 1), 1))
    nonzero = merge(1, 0, reg_rate /= 0)
    rate = ks + ishft(reg_rate, 2)
    rate_hi = ishft(rate, -2)
    rate_lo = iand(rate, 3_i32)
    if (iand(rate_hi, 16_i32) /= 0) rate_hi = 15_i32
    eg_shift = iand(rate_hi + chip%eg_add, 255_i32)
    shift = 0
    if (nonzero /= 0) then
      if (rate_hi < 12_i32) then
        if (chip%eg_state /= 0) then
          select case (eg_shift)
          case (12); shift = 1
          case (13); shift = iand(ishft(rate_lo, -1), 1_i32)
          case (14); shift = iand(rate_lo, 1_i32)
          end select
        end if
      else
        shift = iand(rate_hi, 3_i32) + eg_incstep(rate_lo, chip%eg_timer_lo)
        if (iand(shift, 4_i32) /= 0) shift = 3
        if (shift == 0) shift = chip%eg_state
      end if
    end if

    eg_rout = chip%slot(islot)%eg_rout
    eg_inc = 0
    eg_off = 0
    ! Instant attack
    if (reset /= 0 .and. rate_hi == 15_i32) eg_rout = 0
    ! Envelope off
    if (iand(chip%slot(islot)%eg_rout, 504_i32) == 504_i32) eg_off = 1
    if (chip%slot(islot)%eg_gen /= envelope_gen_num_attack .and. reset == 0 .and. eg_off /= 0) then
      eg_rout = 511_i32
    end if

    select case (chip%slot(islot)%eg_gen)
    case (envelope_gen_num_attack)
      if (chip%slot(islot)%eg_rout == 0) then
        chip%slot(islot)%eg_gen = envelope_gen_num_decay
      else if (chip%slot(islot)%key /= 0 .and. shift > 0 .and. rate_hi /= 15_i32) then
        eg_inc = trunc_i16(ashr(not(chip%slot(islot)%eg_rout), 4 - shift))
      end if
    case (envelope_gen_num_decay)
      if (ishft(chip%slot(islot)%eg_rout, -4) == chip%slot(islot)%reg_sl) then
        chip%slot(islot)%eg_gen = envelope_gen_num_sustain
      else if (eg_off == 0 .and. reset == 0 .and. shift > 0) then
        eg_inc = ishft(1_i32, shift - 1)
      end if
    case (envelope_gen_num_sustain, envelope_gen_num_release)
      if (eg_off == 0 .and. reset == 0 .and. shift > 0) then
        eg_inc = ishft(1_i32, shift - 1)
      end if
    end select

    chip%slot(islot)%eg_rout = iand(eg_rout + eg_inc, 511_i32)

    ! Key off
    if (reset /= 0) chip%slot(islot)%eg_gen = envelope_gen_num_attack
    if (chip%slot(islot)%key == 0) chip%slot(islot)%eg_gen = envelope_gen_num_release
  end subroutine OPL3_EnvelopeCalc

  subroutine OPL3_EnvelopeKeyOn(chip, islot, type_)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot, type_
    chip%slot(islot)%key = ior(chip%slot(islot)%key, type_)
  end subroutine OPL3_EnvelopeKeyOn

  subroutine OPL3_EnvelopeKeyOff(chip, islot, type_)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot, type_
    chip%slot(islot)%key = iand(chip%slot(islot)%key, not(type_))
  end subroutine OPL3_EnvelopeKeyOff

  !=======================================================================
  ! Phase generator
  !=======================================================================

  subroutine OPL3_PhaseGenerate(chip, islot)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot
    integer(i32) :: ich, f_num, basefreq, rm_xor, n_bit, noise, phase
    integer(i32) :: range, vibpos

    ich = chip%slot(islot)%chan_idx
    f_num = chip%channel(ich)%f_num

    if (chip%slot(islot)%reg_vib /= 0) then
      range = iand(ishft(f_num, -7), 7_i32)
      vibpos = chip%vibpos

      if (iand(vibpos, 3_i32) == 0) then
        range = 0
      else if (iand(vibpos, 1_i32) /= 0) then
        range = ishft(range, -1)
      end if
      range = ishft(range, -chip%vibshift)

      if (iand(vibpos, 4_i32) /= 0) range = -range

      f_num = iand(f_num + range, 65535_i32)
    end if

    basefreq = ishft(ishft(f_num, chip%channel(ich)%block), -1)
    phase = iand(ishft(chip%slot(islot)%pg_phase, -9), 65535_i32)
    if (chip%slot(islot)%pg_reset /= 0) chip%slot(islot)%pg_phase = 0
    chip%slot(islot)%pg_phase = chip%slot(islot)%pg_phase &
        + ishft(basefreq * mt(chip%slot(islot)%reg_mult), -1)

    ! Rhythm mode
    noise = chip%noise
    chip%slot(islot)%pg_phase_out = phase
    if (chip%slot(islot)%slot_num == 13) then   ! hh
      chip%rm_hh_bit2 = iand(ishft(phase, -2), 1_i32)
      chip%rm_hh_bit3 = iand(ishft(phase, -3), 1_i32)
      chip%rm_hh_bit7 = iand(ishft(phase, -7), 1_i32)
      chip%rm_hh_bit8 = iand(ishft(phase, -8), 1_i32)
    end if
    if (chip%slot(islot)%slot_num == 17 .and. iand(chip%rhy, 32_i32) /= 0) then   ! tc
      chip%rm_tc_bit3 = iand(ishft(phase, -3), 1_i32)
      chip%rm_tc_bit5 = iand(ishft(phase, -5), 1_i32)
    end if
    if (iand(chip%rhy, 32_i32) /= 0) then
      rm_xor = ior(ior(ieor(chip%rm_hh_bit2, chip%rm_hh_bit7), &
                        ieor(chip%rm_hh_bit3, chip%rm_tc_bit5)), &
                   ieor(chip%rm_tc_bit3, chip%rm_tc_bit5))
      select case (chip%slot(islot)%slot_num)
      case (13)   ! hh
        chip%slot(islot)%pg_phase_out = ishft(rm_xor, 9)
        if (ieor(rm_xor, iand(noise, 1_i32)) /= 0) then
          chip%slot(islot)%pg_phase_out = ior(chip%slot(islot)%pg_phase_out, 208_i32)  ! 0xd0
        else
          chip%slot(islot)%pg_phase_out = ior(chip%slot(islot)%pg_phase_out, 52_i32)   ! 0x34
        end if
      case (16)   ! sd
        chip%slot(islot)%pg_phase_out = ior(ishft(chip%rm_hh_bit8, 9), &
            ishft(ieor(chip%rm_hh_bit8, iand(noise, 1_i32)), 8))
      case (17)   ! tc
        chip%slot(islot)%pg_phase_out = ior(ishft(rm_xor, 9), 128_i32)   ! 0x80
      end select
    end if
    n_bit = iand(ieor(ishft(noise, -14), noise), 1_i32)
    chip%noise = ior(ishft(noise, -1), ishft(n_bit, 22))
  end subroutine OPL3_PhaseGenerate

  !=======================================================================
  ! Slot
  !=======================================================================

  subroutine OPL3_SlotWrite20(chip, islot, data)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot, data
    chip%slot(islot)%trem_is_tremolo = (iand(ishft(data, -7), 1_i32) /= 0)
    chip%slot(islot)%reg_vib  = iand(ishft(data, -6), 1_i32)
    chip%slot(islot)%reg_type = iand(ishft(data, -5), 1_i32)
    chip%slot(islot)%reg_ksr  = iand(ishft(data, -4), 1_i32)
    chip%slot(islot)%reg_mult = iand(data, 15_i32)
  end subroutine OPL3_SlotWrite20

  subroutine OPL3_SlotWrite40(chip, islot, data)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot, data
    chip%slot(islot)%reg_ksl = iand(ishft(data, -6), 3_i32)
    chip%slot(islot)%reg_tl  = iand(data, 63_i32)
    call OPL3_EnvelopeUpdateKSL(chip, islot)
  end subroutine OPL3_SlotWrite40

  subroutine OPL3_SlotWrite60(chip, islot, data)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot, data
    chip%slot(islot)%reg_ar = iand(ishft(data, -4), 15_i32)
    chip%slot(islot)%reg_dr = iand(data, 15_i32)
  end subroutine OPL3_SlotWrite60

  subroutine OPL3_SlotWrite80(chip, islot, data)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot, data
    chip%slot(islot)%reg_sl = iand(ishft(data, -4), 15_i32)
    if (chip%slot(islot)%reg_sl == 15_i32) chip%slot(islot)%reg_sl = 31_i32
    chip%slot(islot)%reg_rr = iand(data, 15_i32)
  end subroutine OPL3_SlotWrite80

  subroutine OPL3_SlotWriteE0(chip, islot, data)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot, data
    chip%slot(islot)%reg_wf = iand(data, 7_i32)
    if (chip%newm == 0) chip%slot(islot)%reg_wf = iand(chip%slot(islot)%reg_wf, 3_i32)
  end subroutine OPL3_SlotWriteE0

  subroutine OPL3_SlotGenerate(chip, islot)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot
    chip%slot(islot)%out_ = envelope_sin(chip%slot(islot)%reg_wf, &
        chip%slot(islot)%pg_phase_out + slot_mod_value(chip, islot), &
        chip%slot(islot)%eg_out)
  end subroutine OPL3_SlotGenerate

  subroutine OPL3_SlotCalcFB(chip, islot)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot
    integer(i32) :: ich
    ich = chip%slot(islot)%chan_idx
    if (chip%channel(ich)%fb /= 0) then
      chip%slot(islot)%fbmod = trunc_i16(ashr( &
          chip%slot(islot)%prout + chip%slot(islot)%out_, &
          9 - chip%channel(ich)%fb))
    else
      chip%slot(islot)%fbmod = 0
    end if
    chip%slot(islot)%prout = chip%slot(islot)%out_
  end subroutine OPL3_SlotCalcFB

  !=======================================================================
  ! Channel
  !
  ! Small helpers below replace the C idiom "target->mod = &source_expr"
  ! and "channel->out[k] = &source_expr" with explicit (kind, index)
  ! pairs, per the pointer-replacement scheme in the opl3_slot /
  ! opl3_channel type definitions above.
  !=======================================================================

  subroutine set_mod_zero(chip, islot)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot
    chip%slot(islot)%mod_kind = MOD_ZERO
    chip%slot(islot)%mod_idx = 0
  end subroutine set_mod_zero

  subroutine set_mod_fbmod(chip, islot, src)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot, src
    chip%slot(islot)%mod_kind = MOD_FBMOD
    chip%slot(islot)%mod_idx = src
  end subroutine set_mod_fbmod

  subroutine set_mod_out(chip, islot, src)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot, src
    chip%slot(islot)%mod_kind = MOD_OUT
    chip%slot(islot)%mod_idx = src
  end subroutine set_mod_out

  subroutine set_out_zero(chip, ich, k)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: ich, k
    chip%channel(ich)%out_kind(k) = OUT_ZERO
    chip%channel(ich)%out_idx(k) = 0
  end subroutine set_out_zero

  subroutine set_out_slot(chip, ich, k, src)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: ich, k, src
    chip%channel(ich)%out_kind(k) = OUT_SLOT
    chip%channel(ich)%out_idx(k) = src
  end subroutine set_out_slot

  subroutine set_out_zero_all(chip, ich)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: ich
    integer(i32) :: k
    do k = 1, 4
      call set_out_zero(chip, ich, k)
    end do
  end subroutine set_out_zero_all

  subroutine OPL3_ChannelSetupAlg(chip, ich)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: ich
    integer(i32) :: pich, s0, s1, ps0, ps1

    s0 = chip%channel(ich)%slotz_idx(1)
    s1 = chip%channel(ich)%slotz_idx(2)

    if (chip%channel(ich)%chtype == ch_drum) then
      if (chip%channel(ich)%ch_num == 7 .or. chip%channel(ich)%ch_num == 8) then
        call set_mod_zero(chip, s0)
        call set_mod_zero(chip, s1)
        return
      end if
      select case (iand(chip%channel(ich)%alg, 1_i32))
      case (0)
        call set_mod_fbmod(chip, s0, s0)
        call set_mod_out(chip, s1, s0)
      case (1)
        call set_mod_fbmod(chip, s0, s0)
        call set_mod_zero(chip, s1)
      end select
      return
    end if

    if (iand(chip%channel(ich)%alg, 8_i32) /= 0) return

    if (iand(chip%channel(ich)%alg, 4_i32) /= 0) then
      pich = chip%channel(ich)%pair_idx
      ps0 = chip%channel(pich)%slotz_idx(1)
      ps1 = chip%channel(pich)%slotz_idx(2)
      call set_out_zero_all(chip, pich)
      select case (iand(chip%channel(ich)%alg, 3_i32))
      case (0)
        call set_mod_fbmod(chip, ps0, ps0)
        call set_mod_out(chip, ps1, ps0)
        call set_mod_out(chip, s0, ps1)
        call set_mod_out(chip, s1, s0)
        call set_out_slot(chip, ich, 1, s1)
        call set_out_zero(chip, ich, 2)
        call set_out_zero(chip, ich, 3)
        call set_out_zero(chip, ich, 4)
      case (1)
        call set_mod_fbmod(chip, ps0, ps0)
        call set_mod_out(chip, ps1, ps0)
        call set_mod_zero(chip, s0)
        call set_mod_out(chip, s1, s0)
        call set_out_slot(chip, ich, 1, ps1)
        call set_out_slot(chip, ich, 2, s1)
        call set_out_zero(chip, ich, 3)
        call set_out_zero(chip, ich, 4)
      case (2)
        call set_mod_fbmod(chip, ps0, ps0)
        call set_mod_zero(chip, ps1)
        call set_mod_out(chip, s0, ps1)
        call set_mod_out(chip, s1, s0)
        call set_out_slot(chip, ich, 1, ps0)
        call set_out_slot(chip, ich, 2, s1)
        call set_out_zero(chip, ich, 3)
        call set_out_zero(chip, ich, 4)
      case (3)
        call set_mod_fbmod(chip, ps0, ps0)
        call set_mod_zero(chip, ps1)
        call set_mod_out(chip, s0, ps1)
        call set_mod_zero(chip, s1)
        call set_out_slot(chip, ich, 1, ps0)
        call set_out_slot(chip, ich, 2, s0)
        call set_out_slot(chip, ich, 3, s1)
        call set_out_zero(chip, ich, 4)
      end select
    else
      select case (iand(chip%channel(ich)%alg, 1_i32))
      case (0)
        call set_mod_fbmod(chip, s0, s0)
        call set_mod_out(chip, s1, s0)
        call set_out_slot(chip, ich, 1, s1)
        call set_out_zero(chip, ich, 2)
        call set_out_zero(chip, ich, 3)
        call set_out_zero(chip, ich, 4)
      case (1)
        call set_mod_fbmod(chip, s0, s0)
        call set_mod_zero(chip, s1)
        call set_out_slot(chip, ich, 1, s0)
        call set_out_slot(chip, ich, 2, s1)
        call set_out_zero(chip, ich, 3)
        call set_out_zero(chip, ich, 4)
      end select
    end if
  end subroutine OPL3_ChannelSetupAlg

  subroutine OPL3_ChannelUpdateRhythm(chip, data)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: data
    integer(i32) :: ch6, ch7, ch8, chnum

    ch6 = 7; ch7 = 8; ch8 = 9   ! 1-based indices for C channels 6, 7, 8

    chip%rhy = iand(data, 63_i32)
    if (iand(chip%rhy, 32_i32) /= 0) then
      call set_out_slot(chip, ch6, 1, chip%channel(ch6)%slotz_idx(2))
      call set_out_slot(chip, ch6, 2, chip%channel(ch6)%slotz_idx(2))
      call set_out_zero(chip, ch6, 3)
      call set_out_zero(chip, ch6, 4)

      call set_out_slot(chip, ch7, 1, chip%channel(ch7)%slotz_idx(1))
      call set_out_slot(chip, ch7, 2, chip%channel(ch7)%slotz_idx(1))
      call set_out_slot(chip, ch7, 3, chip%channel(ch7)%slotz_idx(2))
      call set_out_slot(chip, ch7, 4, chip%channel(ch7)%slotz_idx(2))

      call set_out_slot(chip, ch8, 1, chip%channel(ch8)%slotz_idx(1))
      call set_out_slot(chip, ch8, 2, chip%channel(ch8)%slotz_idx(1))
      call set_out_slot(chip, ch8, 3, chip%channel(ch8)%slotz_idx(2))
      call set_out_slot(chip, ch8, 4, chip%channel(ch8)%slotz_idx(2))

      do chnum = 7, 9
        chip%channel(chnum)%chtype = ch_drum
      end do
      call OPL3_ChannelSetupAlg(chip, ch6)
      call OPL3_ChannelSetupAlg(chip, ch7)
      call OPL3_ChannelSetupAlg(chip, ch8)

      ! hh
      if (iand(chip%rhy, 1_i32) /= 0) then
        call OPL3_EnvelopeKeyOn(chip, chip%channel(ch7)%slotz_idx(1), egk_drum)
      else
        call OPL3_EnvelopeKeyOff(chip, chip%channel(ch7)%slotz_idx(1), egk_drum)
      end if
      ! tc
      if (iand(chip%rhy, 2_i32) /= 0) then
        call OPL3_EnvelopeKeyOn(chip, chip%channel(ch8)%slotz_idx(2), egk_drum)
      else
        call OPL3_EnvelopeKeyOff(chip, chip%channel(ch8)%slotz_idx(2), egk_drum)
      end if
      ! tom
      if (iand(chip%rhy, 4_i32) /= 0) then
        call OPL3_EnvelopeKeyOn(chip, chip%channel(ch8)%slotz_idx(1), egk_drum)
      else
        call OPL3_EnvelopeKeyOff(chip, chip%channel(ch8)%slotz_idx(1), egk_drum)
      end if
      ! sd
      if (iand(chip%rhy, 8_i32) /= 0) then
        call OPL3_EnvelopeKeyOn(chip, chip%channel(ch7)%slotz_idx(2), egk_drum)
      else
        call OPL3_EnvelopeKeyOff(chip, chip%channel(ch7)%slotz_idx(2), egk_drum)
      end if
      ! bd
      if (iand(chip%rhy, 16_i32) /= 0) then
        call OPL3_EnvelopeKeyOn(chip, chip%channel(ch6)%slotz_idx(1), egk_drum)
        call OPL3_EnvelopeKeyOn(chip, chip%channel(ch6)%slotz_idx(2), egk_drum)
      else
        call OPL3_EnvelopeKeyOff(chip, chip%channel(ch6)%slotz_idx(1), egk_drum)
        call OPL3_EnvelopeKeyOff(chip, chip%channel(ch6)%slotz_idx(2), egk_drum)
      end if
    else
      do chnum = 7, 9
        chip%channel(chnum)%chtype = ch_2op
        call OPL3_ChannelSetupAlg(chip, chnum)
        call OPL3_EnvelopeKeyOff(chip, chip%channel(chnum)%slotz_idx(1), egk_drum)
        call OPL3_EnvelopeKeyOff(chip, chip%channel(chnum)%slotz_idx(2), egk_drum)
      end do
    end if
  end subroutine OPL3_ChannelUpdateRhythm

  subroutine OPL3_ChannelWriteA0(chip, ich, data)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: ich, data
    integer(i32) :: pich
    if (chip%newm /= 0 .and. chip%channel(ich)%chtype == ch_4op2) return
    chip%channel(ich)%f_num = ior(iand(chip%channel(ich)%f_num, 768_i32), data)
    chip%channel(ich)%ksv = ior(ishft(chip%channel(ich)%block, 1), &
        iand(ishft(chip%channel(ich)%f_num, -(9 - chip%nts)), 1_i32))
    call OPL3_EnvelopeUpdateKSL(chip, chip%channel(ich)%slotz_idx(1))
    call OPL3_EnvelopeUpdateKSL(chip, chip%channel(ich)%slotz_idx(2))
    if (chip%newm /= 0 .and. chip%channel(ich)%chtype == ch_4op) then
      pich = chip%channel(ich)%pair_idx
      chip%channel(pich)%f_num = chip%channel(ich)%f_num
      chip%channel(pich)%ksv = chip%channel(ich)%ksv
      call OPL3_EnvelopeUpdateKSL(chip, chip%channel(pich)%slotz_idx(1))
      call OPL3_EnvelopeUpdateKSL(chip, chip%channel(pich)%slotz_idx(2))
    end if
  end subroutine OPL3_ChannelWriteA0

  subroutine OPL3_ChannelWriteB0(chip, ich, data)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: ich, data
    integer(i32) :: pich
    if (chip%newm /= 0 .and. chip%channel(ich)%chtype == ch_4op2) return
    chip%channel(ich)%f_num = ior(iand(chip%channel(ich)%f_num, 255_i32), &
        ishft(iand(data, 3_i32), 8))
    chip%channel(ich)%block = iand(ishft(data, -2), 7_i32)
    chip%channel(ich)%ksv = ior(ishft(chip%channel(ich)%block, 1), &
        iand(ishft(chip%channel(ich)%f_num, -(9 - chip%nts)), 1_i32))
    call OPL3_EnvelopeUpdateKSL(chip, chip%channel(ich)%slotz_idx(1))
    call OPL3_EnvelopeUpdateKSL(chip, chip%channel(ich)%slotz_idx(2))
    if (chip%newm /= 0 .and. chip%channel(ich)%chtype == ch_4op) then
      pich = chip%channel(ich)%pair_idx
      chip%channel(pich)%f_num = chip%channel(ich)%f_num
      chip%channel(pich)%block = chip%channel(ich)%block
      chip%channel(pich)%ksv = chip%channel(ich)%ksv
      call OPL3_EnvelopeUpdateKSL(chip, chip%channel(pich)%slotz_idx(1))
      call OPL3_EnvelopeUpdateKSL(chip, chip%channel(pich)%slotz_idx(2))
    end if
  end subroutine OPL3_ChannelWriteB0

  subroutine OPL3_ChannelUpdateAlg(chip, ich)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: ich
    integer(i32) :: pich
    chip%channel(ich)%alg = chip%channel(ich)%con
    if (chip%newm /= 0) then
      if (chip%channel(ich)%chtype == ch_4op) then
        pich = chip%channel(ich)%pair_idx
        chip%channel(pich)%alg = ior(ior(4_i32, ishft(chip%channel(ich)%con, 1)), &
            chip%channel(pich)%con)
        chip%channel(ich)%alg = 8_i32
        call OPL3_ChannelSetupAlg(chip, pich)
      else if (chip%channel(ich)%chtype == ch_4op2) then
        pich = chip%channel(ich)%pair_idx
        chip%channel(ich)%alg = ior(ior(4_i32, ishft(chip%channel(pich)%con, 1)), &
            chip%channel(ich)%con)
        chip%channel(pich)%alg = 8_i32
        call OPL3_ChannelSetupAlg(chip, ich)
      else
        call OPL3_ChannelSetupAlg(chip, ich)
      end if
    else
      call OPL3_ChannelSetupAlg(chip, ich)
    end if
  end subroutine OPL3_ChannelUpdateAlg

  subroutine OPL3_ChannelWriteC0(chip, ich, data)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: ich, data
    chip%channel(ich)%fb = ishft(iand(data, 14_i32), -1)
    chip%channel(ich)%con = iand(data, 1_i32)
    call OPL3_ChannelUpdateAlg(chip, ich)
    if (chip%newm /= 0) then
      chip%channel(ich)%cha = merge(65535_i32, 0_i32, iand(ishft(data, -4), 1_i32) /= 0)
      chip%channel(ich)%chb = merge(65535_i32, 0_i32, iand(ishft(data, -5), 1_i32) /= 0)
      chip%channel(ich)%chc = merge(65535_i32, 0_i32, iand(ishft(data, -6), 1_i32) /= 0)
      chip%channel(ich)%chd = merge(65535_i32, 0_i32, iand(ishft(data, -7), 1_i32) /= 0)
    else
      chip%channel(ich)%cha = 65535_i32
      chip%channel(ich)%chb = 65535_i32
      chip%channel(ich)%chc = 0_i32
      chip%channel(ich)%chd = 0_i32
    end if
  end subroutine OPL3_ChannelWriteC0

  subroutine OPL3_ChannelKeyOn(chip, ich)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: ich
    integer(i32) :: pich
    if (chip%newm /= 0) then
      if (chip%channel(ich)%chtype == ch_4op) then
        pich = chip%channel(ich)%pair_idx
        call OPL3_EnvelopeKeyOn(chip, chip%channel(ich)%slotz_idx(1), egk_norm)
        call OPL3_EnvelopeKeyOn(chip, chip%channel(ich)%slotz_idx(2), egk_norm)
        call OPL3_EnvelopeKeyOn(chip, chip%channel(pich)%slotz_idx(1), egk_norm)
        call OPL3_EnvelopeKeyOn(chip, chip%channel(pich)%slotz_idx(2), egk_norm)
      else if (chip%channel(ich)%chtype == ch_2op .or. chip%channel(ich)%chtype == ch_drum) then
        call OPL3_EnvelopeKeyOn(chip, chip%channel(ich)%slotz_idx(1), egk_norm)
        call OPL3_EnvelopeKeyOn(chip, chip%channel(ich)%slotz_idx(2), egk_norm)
      end if
    else
      call OPL3_EnvelopeKeyOn(chip, chip%channel(ich)%slotz_idx(1), egk_norm)
      call OPL3_EnvelopeKeyOn(chip, chip%channel(ich)%slotz_idx(2), egk_norm)
    end if
  end subroutine OPL3_ChannelKeyOn

  subroutine OPL3_ChannelKeyOff(chip, ich)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: ich
    integer(i32) :: pich
    if (chip%newm /= 0) then
      if (chip%channel(ich)%chtype == ch_4op) then
        pich = chip%channel(ich)%pair_idx
        call OPL3_EnvelopeKeyOff(chip, chip%channel(ich)%slotz_idx(1), egk_norm)
        call OPL3_EnvelopeKeyOff(chip, chip%channel(ich)%slotz_idx(2), egk_norm)
        call OPL3_EnvelopeKeyOff(chip, chip%channel(pich)%slotz_idx(1), egk_norm)
        call OPL3_EnvelopeKeyOff(chip, chip%channel(pich)%slotz_idx(2), egk_norm)
      else if (chip%channel(ich)%chtype == ch_2op .or. chip%channel(ich)%chtype == ch_drum) then
        call OPL3_EnvelopeKeyOff(chip, chip%channel(ich)%slotz_idx(1), egk_norm)
        call OPL3_EnvelopeKeyOff(chip, chip%channel(ich)%slotz_idx(2), egk_norm)
      end if
    else
      call OPL3_EnvelopeKeyOff(chip, chip%channel(ich)%slotz_idx(1), egk_norm)
      call OPL3_EnvelopeKeyOff(chip, chip%channel(ich)%slotz_idx(2), egk_norm)
    end if
  end subroutine OPL3_ChannelKeyOff

  subroutine OPL3_ChannelSet4Op(chip, data)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: data
    integer(i32) :: bit, chnum, chnum_f

    do bit = 0, 5
      chnum = bit
      if (bit >= 3) chnum = chnum + 6
      chnum_f = chnum + 1
      if (iand(ishft(data, -bit), 1_i32) /= 0) then
        chip%channel(chnum_f)%chtype = ch_4op
        chip%channel(chnum_f + 3)%chtype = ch_4op2
        call OPL3_ChannelUpdateAlg(chip, chnum_f)
      else
        chip%channel(chnum_f)%chtype = ch_2op
        chip%channel(chnum_f + 3)%chtype = ch_2op
        call OPL3_ChannelUpdateAlg(chip, chnum_f)
        call OPL3_ChannelUpdateAlg(chip, chnum_f + 3)
      end if
    end do
  end subroutine OPL3_ChannelSet4Op

  !=======================================================================
  ! Top-level generate / reset / register-write entry points
  !=======================================================================

  pure function OPL3_ClipSample(sample) result(r)
    integer(i32), intent(in) :: sample
    integer(i32) :: r
    r = sample
    if (r > 32767_i32) then
      r = 32767_i32
    else if (r < -32768_i32) then
      r = -32768_i32
    end if
  end function OPL3_ClipSample

  subroutine OPL3_ProcessSlot(chip, islot)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: islot
    call OPL3_SlotCalcFB(chip, islot)
    call OPL3_EnvelopeCalc(chip, islot)
    call OPL3_PhaseGenerate(chip, islot)
    call OPL3_SlotGenerate(chip, islot)
  end subroutine OPL3_ProcessSlot

  ! Replaces C's "inline void OPL3_Generate4Ch". The slot-processing loop
  ! is deliberately split into four ranges (1:15, 16:18, 19:33, 34:36),
  ! interleaved with the two stereo mixdown passes, to reproduce the
  ! "channel sample delay" quirk of the original hardware/emulator
  ! (OPL_QUIRK_CHANNELSAMPLEDELAY, which is on by default since
  ! OPL_ENABLE_STEREOEXT defaults to 0). Do not reorder these loops.
  subroutine OPL3_Generate4Ch(chip, buf4)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(out) :: buf4(4)
    integer(i32) :: ii, accm, shift, widx
    integer(i32) :: mix(2)

    buf4(2) = OPL3_ClipSample(chip%mixbuff(2))
    buf4(4) = OPL3_ClipSample(chip%mixbuff(4))

    do ii = 1, 15
      call OPL3_ProcessSlot(chip, ii)
    end do

    mix(1) = 0; mix(2) = 0
    do ii = 1, 18
      accm = trunc_i16( channel_out_value(chip, ii, 1) + channel_out_value(chip, ii, 2) &
                       + channel_out_value(chip, ii, 3) + channel_out_value(chip, ii, 4) )
      mix(1) = mix(1) + trunc_i16(iand(accm, chip%channel(ii)%cha))
      mix(2) = mix(2) + trunc_i16(iand(accm, chip%channel(ii)%chc))
    end do
    chip%mixbuff(1) = mix(1)
    chip%mixbuff(3) = mix(2)

    do ii = 16, 18
      call OPL3_ProcessSlot(chip, ii)
    end do

    buf4(1) = OPL3_ClipSample(chip%mixbuff(1))
    buf4(3) = OPL3_ClipSample(chip%mixbuff(3))

    do ii = 19, 33
      call OPL3_ProcessSlot(chip, ii)
    end do

    mix(1) = 0; mix(2) = 0
    do ii = 1, 18
      accm = trunc_i16( channel_out_value(chip, ii, 1) + channel_out_value(chip, ii, 2) &
                       + channel_out_value(chip, ii, 3) + channel_out_value(chip, ii, 4) )
      mix(1) = mix(1) + trunc_i16(iand(accm, chip%channel(ii)%chb))
      mix(2) = mix(2) + trunc_i16(iand(accm, chip%channel(ii)%chd))
    end do
    chip%mixbuff(2) = mix(1)
    chip%mixbuff(4) = mix(2)

    do ii = 34, 36
      call OPL3_ProcessSlot(chip, ii)
    end do

    if (iand(chip%timer, 63_i32) == 63_i32) then
      chip%tremolopos = mod(chip%tremolopos + 1, 210_i32)
    end if
    if (chip%tremolopos < 105_i32) then
      chip%tremolo = ishft(chip%tremolopos, -chip%tremoloshift)
    else
      chip%tremolo = ishft(210_i32 - chip%tremolopos, -chip%tremoloshift)
    end if

    if (iand(chip%timer, 1023_i32) == 1023_i32) then
      chip%vibpos = iand(chip%vibpos + 1, 7_i32)
    end if

    chip%timer = iand(chip%timer + 1, 65535_i32)   ! uint16_t wraparound

    if (chip%eg_state /= 0) then
      shift = 0
      do while (shift < 13 .and. iand(ishft(chip%eg_timer, -shift), 1_i64) == 0_i64)
        shift = shift + 1
      end do
      if (shift > 12) then
        chip%eg_add = 0
      else
        chip%eg_add = shift + 1
      end if
      chip%eg_timer_lo = int(iand(chip%eg_timer, 3_i64), i32)
    end if

    if (chip%eg_timerrem /= 0 .or. chip%eg_state /= 0) then
      if (chip%eg_timer == 68719476735_i64) then   ! 0xfffffffff
        chip%eg_timer = 0_i64
        chip%eg_timerrem = 1
      else
        chip%eg_timer = chip%eg_timer + 1_i64
        chip%eg_timerrem = 0
      end if
    end if

    chip%eg_state = ieor(chip%eg_state, 1_i32)

    do
      widx = chip%writebuf_cur + 1
      if (chip%writebuf(widx)%time > chip%writebuf_samplecnt) exit
      if (iand(chip%writebuf(widx)%reg, 512_i32) == 0) exit
      chip%writebuf(widx)%reg = iand(chip%writebuf(widx)%reg, 511_i32)
      call OPL3_WriteReg(chip, chip%writebuf(widx)%reg, chip%writebuf(widx)%data)
      chip%writebuf_cur = mod(chip%writebuf_cur + 1, OPL_WRITEBUF_SIZE)
    end do
    chip%writebuf_samplecnt = chip%writebuf_samplecnt + 1_i64
  end subroutine OPL3_Generate4Ch

  subroutine OPL3_Generate(chip, buf)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(out) :: buf(2)
    integer(i32) :: samples(4)
    call OPL3_Generate4Ch(chip, samples)
    buf(1) = samples(1)
    buf(2) = samples(2)
  end subroutine OPL3_Generate

  subroutine OPL3_Generate4ChResampled(chip, buf4)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(out) :: buf4(4)
    integer(i32) :: k

    do while (chip%samplecnt >= chip%rateratio)
      chip%oldsamples = chip%samples
      call OPL3_Generate4Ch(chip, chip%samples)
      chip%samplecnt = chip%samplecnt - chip%rateratio
    end do

    do k = 1, 4
      buf4(k) = trunc_i16( (chip%oldsamples(k) * (chip%rateratio - chip%samplecnt) &
                           + chip%samples(k) * chip%samplecnt) / chip%rateratio )
    end do
    chip%samplecnt = chip%samplecnt + ishft(1_i32, RSM_FRAC)
  end subroutine OPL3_Generate4ChResampled

  subroutine OPL3_GenerateResampled(chip, buf)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(out) :: buf(2)
    integer(i32) :: samples(4)
    call OPL3_Generate4ChResampled(chip, samples)
    buf(1) = samples(1)
    buf(2) = samples(2)
  end subroutine OPL3_GenerateResampled

  subroutine OPL3_Reset(chip, samplerate)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: samplerate
    integer(i32) :: islot, ich, local_ch_slot, channum0, m

    call manualReset()

    do islot = 1, 36
      call set_mod_zero(chip, islot)
      chip%slot(islot)%eg_rout = 511_i32
      chip%slot(islot)%eg_out  = 511_i32
      chip%slot(islot)%eg_gen  = envelope_gen_num_release
      chip%slot(islot)%trem_is_tremolo = .false.
      chip%slot(islot)%slot_num = islot - 1   ! kept 0-based, matches hardware
    end do

    do ich = 1, 18
      channum0 = ich - 1
      local_ch_slot = ch_slot(ich)
      chip%channel(ich)%slotz_idx(1) = local_ch_slot
      chip%channel(ich)%slotz_idx(2) = local_ch_slot + 3
      chip%slot(local_ch_slot)%chan_idx = ich
      chip%slot(local_ch_slot + 3)%chan_idx = ich

      m = mod(channum0, 9)
      if (m < 3) then
        chip%channel(ich)%pair_idx = channum0 + 3 + 1
      else if (m < 6) then
        chip%channel(ich)%pair_idx = channum0 - 3 + 1
      end if

      call set_out_zero_all(chip, ich)
      chip%channel(ich)%chtype = ch_2op
      chip%channel(ich)%cha = 65535_i32
      chip%channel(ich)%chb = 65535_i32
      chip%channel(ich)%ch_num = channum0   ! kept 0-based, matches hardware
      call OPL3_ChannelSetupAlg(chip, ich)
    end do

    chip%noise = 1_i32
    chip%rateratio = int( (int(samplerate, i64) * ishft(1_i64, RSM_FRAC)) / 49716_i64, i32)
    chip%tremoloshift = 4
    chip%vibshift = 1
  end subroutine OPL3_Reset

  subroutine OPL3_WriteReg(chip, reg, v)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: reg, v
    integer(i32) :: high, regm

    high = iand(ishft(reg, -8), 1_i32)
    regm = iand(reg, 255_i32)

    select case (iand(regm, 240_i32))   ! 0xf0
    case (0)
      if (high /= 0) then
        select case (iand(regm, 15_i32))
        case (4)
          call OPL3_ChannelSet4Op(chip, v)
        case (5)
          chip%newm = iand(v, 1_i32)
        end select
      else
        select case (iand(regm, 15_i32))
        case (8)
          chip%nts = iand(ishft(v, -6), 1_i32)
        end select
      end if
    case (32, 48)   ! 0x20, 0x30
      if (ad_slot(iand(regm, 31_i32)) >= 1) then
        call OPL3_SlotWrite20(chip, 18 * high + ad_slot(iand(regm, 31_i32)), v)
      end if
    case (64, 80)   ! 0x40, 0x50
      if (ad_slot(iand(regm, 31_i32)) >= 1) then
        call OPL3_SlotWrite40(chip, 18 * high + ad_slot(iand(regm, 31_i32)), v)
      end if
    case (96, 112)  ! 0x60, 0x70
      if (ad_slot(iand(regm, 31_i32)) >= 1) then
        call OPL3_SlotWrite60(chip, 18 * high + ad_slot(iand(regm, 31_i32)), v)
      end if
    case (128, 144) ! 0x80, 0x90
      if (ad_slot(iand(regm, 31_i32)) >= 1) then
        call OPL3_SlotWrite80(chip, 18 * high + ad_slot(iand(regm, 31_i32)), v)
      end if
    case (224, 240) ! 0xe0, 0xf0
      if (ad_slot(iand(regm, 31_i32)) >= 1) then
        call OPL3_SlotWriteE0(chip, 18 * high + ad_slot(iand(regm, 31_i32)), v)
      end if
    case (160)      ! 0xa0
      if (iand(regm, 15_i32) < 9_i32) then
        call OPL3_ChannelWriteA0(chip, 9 * high + iand(regm, 15_i32) + 1, v)
      end if
    case (176)      ! 0xb0
      if (regm == 189_i32 .and. high == 0) then   ! 0xbd
        chip%tremoloshift = ishft(ieor(ishft(v, -7), 1_i32), 1) + 2
        chip%vibshift = ieor(iand(ishft(v, -6), 1_i32), 1_i32)
        call OPL3_ChannelUpdateRhythm(chip, v)
      else if (iand(regm, 15_i32) < 9_i32) then
        call OPL3_ChannelWriteB0(chip, 9 * high + iand(regm, 15_i32) + 1, v)
        if (iand(v, 32_i32) /= 0) then
          call OPL3_ChannelKeyOn(chip, 9 * high + iand(regm, 15_i32) + 1)
        else
          call OPL3_ChannelKeyOff(chip, 9 * high + iand(regm, 15_i32) + 1)
        end if
      end if
    case (192)      ! 0xc0
      if (iand(regm, 15_i32) < 9_i32) then
        call OPL3_ChannelWriteC0(chip, 9 * high + iand(regm, 15_i32) + 1, v)
      end if
    end select
  end subroutine OPL3_WriteReg

  subroutine OPL3_WriteRegBuffered(chip, reg, v)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(in) :: reg, v
    integer(i32) :: writebuf_last, widx
    integer(i64) :: time1, time2

    writebuf_last = chip%writebuf_last
    widx = writebuf_last + 1

    if (iand(chip%writebuf(widx)%reg, 512_i32) /= 0) then
      call OPL3_WriteReg(chip, iand(chip%writebuf(widx)%reg, 511_i32), chip%writebuf(widx)%data)
      chip%writebuf_cur = mod(writebuf_last + 1, OPL_WRITEBUF_SIZE)
      chip%writebuf_samplecnt = chip%writebuf(widx)%time
    end if

    chip%writebuf(widx)%reg = ior(reg, 512_i32)
    chip%writebuf(widx)%data = v
    time1 = chip%writebuf_lasttime + int(OPL_WRITEBUF_DELAY, i64)
    time2 = chip%writebuf_samplecnt
    if (time1 < time2) time1 = time2

    chip%writebuf(widx)%time = time1
    chip%writebuf_lasttime = time1
    chip%writebuf_last = mod(writebuf_last + 1, OPL_WRITEBUF_SIZE)
  end subroutine OPL3_WriteRegBuffered

  subroutine OPL3_Generate4ChStream(chip, sndptr1, sndptr2, numsamples)
    type(opl3_chip), intent(inout) :: chip
    integer(i32), intent(inout) :: sndptr1(:), sndptr2(:)
    integer(i32), intent(in) :: numsamples
    integer(i32) :: i, samples(4)

    do i = 1, numsamples
      call OPL3_Generate4ChResampled(chip, samples)
      sndptr1(2 * i)     = samples(1)
      sndptr1(2 * i + 1) = samples(2)
      sndptr2(2 * i)     = samples(3)
      sndptr2(2 * i + 1) = samples(4)
    end do
  end subroutine OPL3_Generate4ChStream

  subroutine OPL3_GenerateStream(chip, sndptr, numsamples)
    type(opl3_chip), intent(inout) :: chip
    integer(2), intent(inout) :: sndptr(:)
    integer(i32), intent(in) :: numsamples
    integer(i32) :: i, buf(2)

    do i = 1, numsamples
      call OPL3_GenerateResampled(chip, buf)
      sndptr(2 * i - 1) = buf(1)
      sndptr(2 * i)     = buf(2)

    end do

  end subroutine 

  subroutine OPL3_GenerateStreamMono(chip, sndptr, numsamples)
    type(opl3_chip), intent(inout) :: chip
    integer(2), intent(inout) :: sndptr(:)
    integer(i32), intent(in) :: numsamples
    integer(i32) :: i, buf(2)

    do i = 1, numsamples, 1
      call OPL3_GenerateResampled(chip, buf)
      sndptr(i) = buf(1)
    end do

  end subroutine 
end module opl3_mod
