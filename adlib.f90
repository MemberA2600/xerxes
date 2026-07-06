MODULE adlib
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
    public                      :: initAdlibData, fillAdlibData


    type adlibData
         character(4)                          :: header
         integer(2)                            :: nameLen
         character(255)                        :: name 
         integer(8)                            :: numOfReads, numOfBytes, loopByte
         integer(2), dimension(:), allocatable :: songBytes
    end type 

    type(adlibData)              :: adlibD 

    integer(2), dimension(124), parameter     :: oplAddresses = &

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

    type YM3812C


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

    contains

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
          Ychip%AMDepth        = AMDepthValues      (1)  

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
