MODULE colors

   USE WINTERACTER
   USE RESID
   USE debugWindow
   USE engineConstants  
   USE subs
   USE winapis

   IMPLICIT NONE

   PRIVATE
   PUBLIC               :: generateColors, getColorRGB,    &
                           getColorValue,  getColorHex,    &
                           getColorBlue,   getColorGreen,  &
                           getColorRed,    getClosestColor 
                           
   !
   !  Redefine 8bit palette for Spectrum Extra. :) 
   !

   TYPE colorHolder
      INTEGER(KIND=2), DIMENSION(3) :: RGB 
      INTEGER                       :: trueValue
      CHARACTER (6)                 :: hexValue

   END TYPE colorHolder

   TYPE(colorHolder), DIMENSION(numOfColors) :: colorList

   CONTAINS

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

   FUNCTION getClosestColor(r, g, b) result(closest)
        integer(2)              :: closest
        integer(8)              :: diff, smallestDiff
        integer(2)              :: ind
        integer(2)              :: r , g , b
        integer(2)              :: r2, g2, b2

        character(60)           :: test

        closest      = 0
        smallestDiff = 9223372036854775807

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
       CHARACTER (20)                    :: msgString
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

                   !write(msgString, '(I0, " | ", A)') theIndex, colorList(theIndex)%hexValue  
                   !call displayDebug(msgString)     

                end do
             end do
          end do
       end do

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