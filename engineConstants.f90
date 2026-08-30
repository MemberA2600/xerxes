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

    integer                       :: PERFECT_WAIT = 50000

    integer(1), parameter         :: layerNum          = 6, &      
                                     LAYER_BACKGROUND  = 1, & 
                                     LAYER_PLAYGROUND  = 2, &
                                     LAYER_SKY         = 3, &   
                                     LAYER_WEATHER     = 4, &   
                                     LAYER_FOREGROUND  = 5, &  
                                     LAYER_INTERFACE   = 6

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
                                        FILTER_RED          = 2,  &
                                        FILTER_RED1         = 3,  &
                                        FILTER_RED2         = 4,  &
                                        FILTER_BLUE         = 5,  &
                                        FILTER_BLUE1        = 6,  &
                                        FILTER_BLUE2        = 7,  &
                                        FILTER_GREEN        = 8,  &
                                        FILTER_GREEN1       = 9,  &
                                        FILTER_GREEN2       = 10, &
                                        FILTER_YELLOW       = 11, &
                                        FILTER_YELLOW1      = 12, &
                                        FILTER_YELLOW2      = 13, &
                                        FILTER_DARK         = 14, &
                                        FILTER_DARK1        = 15, &
                                        FILTER_DARK2        = 16, &
                                        FILTER_LIGHT        = 17, &
                                        FILTER_LIGHT1       = 18, &
                                        FILTER_LIGHT2       = 19, &
                                        FILTER_SHADOW       = 20, &
                                        FILTER_TRANSP       = 21, &
                                        FILTER_PINK         = 22, &
                                        FILTER_PINK1        = 23, &
                                        FILTER_PINK2        = 24, &
                                        FILTER_TEAL         = 25, &
                                        FILTER_TEAL1        = 26, &
                                        FILTER_TEAL2        = 27
    integer(1), parameter            :: TYPE_EMPTY          = 0

END MODULE engineConstants
