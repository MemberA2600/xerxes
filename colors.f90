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
                           c24btoC256,     indexToGridVals, &
                           gridValsToIndex,getNightColor
   !
   !  Redefine 8bit palette for Spectrum Extra. :) 
   !

   TYPE colorHolder
      INTEGER(KIND=2), DIMENSION(3) :: RGB 
      INTEGER                       :: trueValue
      CHARACTER (6)                 :: hexValue
      integer(2)                    :: night  
   END TYPE colorHolder

   TYPE(counterTimer)                        :: timer256


   TYPE(colorHolder), DIMENSION(numOfColors) :: colorList
   integer(2)                                :: upTo256 = 0, upTo256_C = 0

   CONTAINS

   function getNightColor(ind) result(r)
        integer(2)          :: ind
        integer(2)          :: r

        r = colorList(ind)%night  

   end function 

   function indexToGridVals(ind) result(r)
        integer(2)               :: ind
        integer(2), dimension(3) :: r

        integer(2)               :: group, red, green, blue

        group = (ind - 1) / 64
        
        red   = (group + 4 * mod((ind-1) / 16, 4))
        green = (group + 4 * mod((ind-1) /  4, 4))
        blue  = (group + 4 * mod((ind-1),      4))

        r = (/ red, green, blue /)

   end function 

    function gridValsToIndex(red, green, blue) result(ind)
        integer(2), intent(in) :: red, green, blue
        integer(2)             :: ind
        integer(2)             :: group
        integer(2)             :: redDigit, greenDigit, blueDigit
    
        ! All three values must belong to the same palette group.
        group = mod(red, 4)
    
        if (mod(green, 4) /= group .or. &
            mod(blue,  4) /= group) then
            ind = 0  
            call displayDebug("Color not in palette!")
            return
        end if
    
        redDigit   = (red   - group) / 4
        greenDigit = (green - group) / 4
        blueDigit  = (blue  - group) / 4
    
        ind = group         * 64 + &
              redDigit      * 16 + &
              greenDigit    * 4  + &
              blueDigit          + 1
    end function

   subroutine start256Timer()
        call timer256%timerStart(50000)
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
       INTEGER(KIND = 2)                 :: theIndex, putHere, smallPoz 
       INTEGER(KIND = 2)                 :: br, R, G, B
       INTEGER(KIND = 2)                 :: R2, G2, B2
       CHARACTER (40)                    :: msgString
       INTEGER                           :: smallest, otherWay
       TYPE(colorHolder)                 :: tempc 

       !open(19, FILE = "colorCodes.txt", action = 'WRITE', STATUS = "REPLACE") 

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

                   !write(msgString, '(I0, " | ", A)') theIndex, colorList(theIndex)%hexValue  
                   !call displayDebug(msgString)     

                   !write(msgString, "(I3.3, ' ', I3.3, ' ', I3.3, ' ', I3.3)") theIndex, R2, G2, B2
                   !write(19, '(A)') msgString
        
                   colorList(theIndex)%night         = getClosestColor(R2 / 12, G2 / 4, B2) 

                end do
             end do
          end do
       end do

       !close(19) 

       do putHere = 1, numOfColors, 1
          smallest = 99999999       
          smallPoz = 0
  
          do theIndex = putHere, numOfColors, 1
             otherWay = colorList(theIndex)%RGB(1) * 255 * 255 + &
                        colorList(theIndex)%RGB(2) * 255       + &
                        colorList(theIndex)%RGB(3) 

 
             if (otherWay   < smallest) then
                 smallest   = otherWay
                 smallPoz   = theIndex
             end if
          end do

          tempC%RGB                     = colorList(smallPoz)%RGB
          tempC%trueValue               = colorList(smallPoz)%trueValue
          tempC%hexValue                = colorList(smallPoz)%hexValue 

          colorList(smallPoz)%RGB       = colorList(putHere)%RGB
          colorList(smallPoz)%trueValue = colorList(putHere)%trueValue
          colorList(smallPoz)%hexValue  = colorList(putHere)%hexValue
 
          colorList(putHere)%RGB        = tempC%RGB 
          colorList(putHere)%trueValue  = tempC%trueValue 
          colorList(putHere)%hexValue   = tempC%hexValue

       end do

       !do theIndex = 1, 256, 1
       !   write(msgString, '(I0, " | ", A)') theIndex, colorList(theIndex)%hexValue 
       !   call displayDebug(msgString) 
       !end do

   END SUBROUTINE

END MODULE colors