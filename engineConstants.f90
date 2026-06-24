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


END MODULE engineConstants
