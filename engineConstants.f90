MODULE engineConstants

    IMPLICIT NONE

    INTEGER(KIND=2), DIMENSION(9,2), PARAMETER   ::  & 
    standards = reshape((/ &
                320,  240, &   ! EGA
                640,  480, &   ! VGA
                800,  600, &   ! SVGA
                1024, 768, &   ! XGA
                1152, 864, &   ! XGA+
                1280, 960, &   ! Packed Desktop
                1400, 1050, &  ! SXGA+
                1600, 1200, &  ! UXGA
                2048, 1536  &  ! QXGA
                /), shape(standards), order=(/2,1/))

    INTEGER(KIND=1), PARAMETER    :: maxNumberOfScreenSizes = 9
    INTEGER(kind=2)               :: wOfScreenBuffer        = 2048, &
                                     hOfScreenBuffer        = 1536  
    INTEGER(KIND = 1), PARAMETER  :: MFPS = 120             ! Maximum frames per second
    INTEGER, PARAMETER            :: MAX_PATH_LEN = 255
    integer, parameter            :: NAME_MAX_LEN = 25

    real(8), parameter            :: pi = 3.141592653589793238462643383279502884197
    integer(4), parameter         :: chunkSize = 32767
    character(4), parameter       :: OPL2_FILE_TYPE = 'OPL2'
    character(4), parameter       :: TIA_FILE_TYPE = 'TIA '

    integer(1), parameter         :: IND_KEY_LEFT           = 1 , &  
                                     IND_KEY_RIGHT          = 2 , &  
                                     IND_KEY_UP             = 3 , &  
                                     IND_KEY_DOWN           = 4 , &  
                                     IND_KEY_ATTACK         = 5 , &  
                                     IND_KEY_CHARGE         = 6 , &  
                                     IND_KEY_SPELL1         = 7 , &  
                                     IND_KEY_SPELL2         = 8 , &
                                     IND_KEY_SPELL3         = 9 , &
                                     IND_KEY_MENU           = 10, &
                                                                            
                                     IND_JOY_LEFT           = 1 , &  
                                     IND_JOY_RIGHT          = 2 , &  
                                     IND_JOY_UP             = 3 , &  
                                     IND_JOY_DOWN           = 4 , &  
                                     IND_JOY_BUTTON1        = 5 , &  
                                     IND_JOY_BUTTON2        = 6 , &  
                                     IND_JOY_BUTTON3        = 7 , &  
                                     IND_JOY_BUTTON4        = 8 , &
                                     IND_JOY_BUTTON5        = 9 , &
                                     IND_JOY_BUTTON6        = 10  

END MODULE engineConstants
