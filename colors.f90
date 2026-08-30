MODULE colors

   USE WINTERACTER
   USE RESID
   USE debugWindow
   USE engineConstants  
   USE subs
   USE winapis

   IMPLICIT NONE

   PRIVATE
   PUBLIC               :: generateColors, getColorRGB,     &
                           getColorValue,  getColorHex,     &
                           getColorBlue,   getColorGreen,   &
                           getColorRed,    getClosestColor, & 
                           setUpTo256,     getUpTo256,      &
                           start256Timer,  stupidTimerEnded,&
                           c24btoC256
   !
   !  Redefine 8bit palette for Spectrum Extra. :) 
   !

   TYPE colorHolder
      INTEGER(KIND=2), DIMENSION(3) :: RGB 
      INTEGER                       :: trueValue
      CHARACTER (6)                 :: hexValue
   END TYPE colorHolder

   TYPE(counterTimer)                                :: timer256

   TYPE(colorHolder), DIMENSION(numOfColors), target  :: colorList
   integer(2)                                         :: upTo256 = 0, upTo256_C = 0
       

   CONTAINS

   subroutine start256Timer()
        call timer256%timerStart(PERFECT_WAIT)
   end subroutine  

   SUBROUTINE setUpTo256()
        if (timer256%timerEnded() .EQV. .TRUE.) then
            upTo256 = modulo(upTo256 + 1, 257) 
            call timer256%timerRestart()
        end if
   end subroutine   

   FUNCTION stupidTimerEnded() result(r)
        logical :: r

        r = (upTo256 /= upTo256_C)
        upTo256_C = upTo256        

   end FUNCTION 

   FUNCTION getUpTo256() result(r)
        integer(2)      :: r
        r = upTo256 

   end FUNCTION 

   FUNCTION getColorRGB(num) result(RGB)
       INTEGER(KIND=2)               :: num
       INTEGER(KIND=2), DIMENSION(3) :: RGB 

       RGB = colorList(num)%RGB

   END FUNCTION

   FUNCTION getColorValue(num) result(val)
       INTEGER(KIND=2) :: num
       INTEGER         :: val 

       val = colorList(num)%trueValue

   END FUNCTION

   FUNCTION getColorHex(num) result(val)
       INTEGER(KIND=2) :: num
       CHARACTER (6)   :: val 

       val = colorList(num)%hexValue

   END FUNCTION

   FUNCTION getColorBlue(num) result(val)
       INTEGER(KIND=2) :: num
       INTEGER(kind=2) :: val 

       val = colorList(num)%RGB(3)

   END FUNCTION

   FUNCTION getColorGreen(num) result(val)
       INTEGER(KIND=2) :: num
       INTEGER(kind=2) :: val 

       val = colorList(num)%RGB(2)

   END FUNCTION

   FUNCTION getColorRed(num) result(val)
       INTEGER(KIND=2) :: num
       INTEGER(kind=2) :: val 

       val = colorList(num)%RGB(1)

   END FUNCTION

   FUNCTION c24btoC256(bit24) result(r)
        integer         :: bit24
        integer(4)      :: ind
        integer(2)      :: r

        r = -1
        
        do ind = 1, numOfColors, 1
           if (colorList(ind)%trueValue == bit24) then
               r = ind  
               exit 
            end if
        end do

   end function 

   FUNCTION getClosestColor(r, g, b) result(closest)
        integer(2)              :: closest
        integer(8)              :: diff, smallestDiff
        integer(2)              :: ind
        integer(2)              :: r , g , b
        integer(2)              :: r2, g2, b2

        character(60)           :: test

        closest      = 0
        smallestDiff = 9223372036854775807!

        do ind = 1, numOfColors, 1
           r2 = getColorRed(  ind)
           b2 = getColorBlue( ind)
           g2 = getColorGreen(ind)

           diff = (30 * (abs(r - r2) ** 2) ) + &
                  (59 * (abs(g - g2) ** 2) ) + &
                  (11 * (abs(b - b2) ** 2) )            

           !write(test, "(I0, '|', I0, '|', I0, '|', I0, '|', I0, '|', I0, '|', I0, '|', I0)") &
           !              ind, r, g, b, r2, g2, b2, diff
            
           !call displayDebug(test) 

           if (diff == 0) then
               closest = ind  
               exit
           end if  

           if (diff < smallestDiff) then
               smallestDiff = diff 
               closest      = ind     
           end if 
        end do
       

   END FUNCTION 

   SUBROUTINE generateColors
       INTEGER(KIND = 2)                 :: theIndex  
       INTEGER(KIND = 2)                 :: br, R, G, B
       INTEGER(KIND = 2)                 :: R2, G2, B2
       INTEGER                           :: smallest, otherWay
       TYPE(colorHolder)                 :: tempc 

       theIndex = 0
       do br = 0, 3, 1
          do R =  0, 3, 1
             do G =  0, 3, 1
                do B =  0, 3, 1
                   R2 = (IOR( ISHFT(R, 2), br)) * 17
                   G2 = (IOR( ISHFT(G, 2), br)) * 17
                   B2 = (IOR( ISHFT(B, 2), br)) * 17

                   theIndex                          = theIndex + 1 

                   colorList(theIndex)%RGB(1)        = R2 
                   colorList(theIndex)%RGB(2)        = G2 
                   colorList(theIndex)%RGB(3)        = B2 

                   colorList(theIndex)%trueValue     = WRGB(R2, G2, B2)
                   write(colorList(theIndex)%hexValue, '(Z2.2, Z2.2, Z2.2)') R2, G2, B2
       
                end do
             end do
          end do
       end do

       !call printPalette()

   END SUBROUTINE

   SUBROUTINE printPalette()
       INTEGER(KIND = 2)                 :: theIndex
       CHARACTER (40)                    :: msgString

       open(19, FILE = "src/colorCodes.txt", action = 'WRITE', STATUS = "REPLACE") 
       do theIndex = 1, numOfColors, 1
          write(msgString, "(I3.3, ' ', I3.3, ' ', I3.3, ' ', I3.3)") theIndex, &
                             colorList(theIndex)%RGB(1), &
                             colorList(theIndex)%RGB(2), &
                             colorList(theIndex)%RGB(3)   
          write(19, '(A)') msgString  
       end do
       close(19) 

   end subroutine  

END MODULE colors