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

    integer(2), parameter         :: numOfColors = 256 
    INTEGER(KIND=1), PARAMETER    :: maxNumberOfScreenSizes = 9
    INTEGER(kind=2)               :: wOfScreenBuffer        = 640, &
                                     hOfScreenBuffer        = 480  
    INTEGER(KIND = 1), PARAMETER  :: MFPS = 120             ! Maximum frames per second
    INTEGER, PARAMETER            :: MAX_PATH_LEN = 255
    integer, parameter            :: NAME_MAX_LEN = 25

    real(8), parameter            :: pi = 3.141592653589793238462643383279502884197
    integer(4), parameter         :: chunkSize = 32767
    character(4), parameter       :: OPL2_FILE_TYPE   = 'OPL2', &
                                     TIA_FILE_TYPE    = 'TIA ', &
                                     CONF_FILE_TYPE   = 'CONF', &
                                     IMG_FILE_TYPE    = 'IMG '                

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

    integer(1), parameter            :: configVersion       = 0
    character(8), parameter          :: configName          = "conf.xxc"    

    integer(1), parameter            :: NO_FILTER           = 0,  &
                                        FILTER_RAINBOW      = 1,  &
                                        FILTER_NIGHT        = 2,  &
                                        FILTER_RED0         = 3,  &
                                        FILTER_RED1         = 4,  &
                                        FILTER_RED2         = 5,  &
                                        FILTER_BLUE0        = 6,  &
                                        FILTER_BLUE1        = 7,  &
                                        FILTER_BLUE2        = 8,  &
                                        FILTER_GREEN0       = 9,  &
                                        FILTER_GREEN1       = 10, &
                                        FILTER_GREEN2       = 11, &
                                        FILTER_YELLOW0      = 12, &
                                        FILTER_YELLOW1      = 13, &
                                        FILTER_YELLOW2      = 14
END MODULE engineConstants
