MODULE screen
    USE debugWindow
    USE engineConstants
    USE colors
    USE WINTERACTER
    USE RESID

    IMPLICIT NONE

    PRIVATE
    PUBLIC    :: initScreenBuff, eraseBuff, initRealScreen,  &
                 getGameScreenSize, buffer2Real
             
    INTEGER(KIND = 4), DIMENSION(:,:,:), &
                       ALLOCATABLE  :: screenBuffers
                             
    INTEGER(KIND = 4), DIMENSION(:),      &
                       ALLOCATABLE  :: screenData 

    INTEGER(KIND = 2), DIMENSION(2) :: screenSize = (/ 0, 0 /) 
    INTEGER(KIND = 2)               :: layers     = 0, test = 0, slow = 0
    CHARACTER(50)                   :: msgString
    INTEGER                         :: onlyBitMap
    LOGICAL                         :: bitmapCreated = .FALSE.

    CONTAINS 
    SUBROUTINE initScreenBuff(numOfLayers)
        INTEGER(KIND = 2) :: numOfLayers    
        INTEGER(KIND = 4) :: stat

        if (allocated(screenBuffers)) deallocate(screenBuffers, stat = stat)
        
        if (stat /= 0) then
            call displayDebug("Failed to deallocate screen buffer array!") 
        else
            allocate(screenBuffers(numOfLayers, wOfScreenBuffer,  &
                                   hOfScreenBuffer), stat = stat)
            if (stat /= 0) then
                call displayDebug("Failed to allocate screen buffer array!") 
            else
                layers = numOfLayers
            end if 

        end if

        call eraseBuff()

    END SUBROUTINE initScreenBuff

    SUBROUTINE eraseBuff()
    
         screenBuffers = -1
 
    END SUBROUTINE eraseBuff  
 
    SUBROUTINE testPattern1()
        INTEGER(KIND = 2) :: X, Y

        !write(msgString, '("Starter: ", I0)') test
        !call displayDebug(msgString) 

        do y    = 1, hOfScreenBuffer, 1
           do x = 1, wOfScreenBuffer, 1
             
              if (slow == 0) then
                  test = test + 1
                  if (test > 256 .OR. test < 1) test = 1
              end if

              slow = slow + 1
              slow = mod(slow, 13)

              screenBuffers(0,x,y) = getColorValue(test) 
              !write(msgString, '("Starter: ", I0, " | ", I0, "|", A)') x, y, getColorHEX(test) 
              !call displayDebug(msgString) 
           end do
        end do 

    END SUBROUTINE testPattern1

    SUBROUTINE testPattern2()
        INTEGER(KIND = 2) :: X, Y

        slow = slow + 1
        if (mod(slow, 8) == 0) then
            test = test + 1
        end if

        if (test > 256 .OR. test < 1) test = 1

        do y    = 1, hOfScreenBuffer, 1
           do x = 1, wOfScreenBuffer, 1

              if (mod(x, 8) == 0) then
                 screenBuffers(0,x,y) = getColorValue(test)

              else
                 screenBuffers(0,x,y) = getColorValue(1)    
              end if  

           end do
        end do 


    END SUBROUTINE testPattern2

    SUBROUTINE testPattern3()
        INTEGER(KIND = 2) :: X, Y

        !slow = slow + 1
        !if (mod(slow, 2) == 0) then
        test = test + 1
        !end if

        if (test > 512 .OR. test < 1) test = 1

        do y    = 1, hOfScreenBuffer, 1
           do x = 1, wOfScreenBuffer, 1

              if (test > 256) then
                  screenBuffers(0,x,y) = getColorValue(mod((y + (test - 256)) / 8, 256) + 1)
              else
                  screenBuffers(0,x,y) = getColorValue(mod((x +  test)        / 8, 256) + 1)
              end if 

           end do
        end do 


    END SUBROUTINE testPattern3

    SUBROUTINE initRealScreen(w, h)
        INTEGER(KIND = 4) :: w, h  
        INTEGER(KIND = 4) :: stat

        !if (allocated(realScreen)) deallocate(realScreen, stat = stat)
        if (allocated(screenData)) deallocate(screenData , stat = stat)

        if (stat /= 0) then
            call displayDebug("Failed to deallocate screen array!") 
        else
            !allocate(realScreen(w, h), stat = stat)
            allocate(screenData(w * h), stat = stat)
 
            if (stat /= 0) then
                call displayDebug("Failed to allocate screen array!") 
            else
                screenSize = (/w, h/)
                !write(msgString, '(I0, " | ", I0)') w, h
                !call displayDebug(msgString) 

                if (bitmapCreated) then 
                   CALL WBitmapDestroy(onlyBitMap) 
                   bitmapCreated = .FALSE. 
                end if 
              
                call WBitmapCreate(onlyBitMap, screenSize(1), screenSize(2)) 
                bitmapCreated = .TRUE. 

            end if 

        end if

    END SUBROUTINE initRealScreen 

    FUNCTION getGameScreenSize () result(s)
        INTEGER(KIND = 2), DIMENSION(2) :: s

        s = screenSize

    END FUNCTION

    SUBROUTINE buffer2Real()
        INTEGER(kind=4), DIMENSION(2048, 1536) :: five2One 
        INTEGER(kind=4)  :: layerIndex, lineIndex, pixelIndex, srcLineIndex, srcPixelIndex          
        INTEGER          :: counter 

        ! call testPattern3()

        five2One = -1 

        do layerIndex       =  1, layers          , 1
           do lineIndex     =  1, hOfScreenBuffer , 1 
              do pixelIndex =  1, wOfScreenBuffer , 1         
                 
                 if (screenBuffers(layerIndex, pixelIndex, lineIndex) /= -1) then
                     five2One     (pixelIndex, lineIndex) = &
                     screenBuffers(layerIndex, pixelIndex, lineIndex)
                 else
                     if (layerIndex == layers) five2One (pixelIndex, lineIndex) = 0
                 end if

              end do
           end do
        end do

        counter = 0
             
        do lineIndex       =  1, screenSize(2), 1 
          do pixelIndex    =  1, screenSize(1), 1 
                 
             srcPixelIndex = (pixelIndex * wOfScreenBuffer) / screenSize(1)
             srcLineIndex  = (lineIndex  * hOfScreenBuffer) / screenSize(2)

             counter             = counter + 1 
             screenData(counter) = five2One  (srcPixelIndex, srcLineIndex)
 
          end do                       
        end do


        call WBitmapGetData(onlyBitMap,screenData)
        CALL WBitmapPut(onlyBitMap)

    END SUBROUTINE buffer2Real


!    FUNCTION RGB2LIN(RGB) result(lin)
!        INTEGER(KIND = 2)     :: RGB
!        REAL(KIND=4)          :: lin, temp
!
!        temp = RGB / 255   
!        if (temp <= 0.04045) then 
!            lin = temp / 12.92
!        else
!            lin = ((temp + 0.055) / 1.055) ** 2.4   
!        end if    
!
!    END FUNCTION RGB2LIN
!
!    FUNCTION LIN2RGB(lin) result(RGB)
!        INTEGER(KIND = 2)     :: RGB
!        REAL(KIND=4)          :: lin, temp
!
!        if (lin <= 0.0031308) then 
!            temp = lin * 12.92;
!        else 
!            temp = 1.055 * (lin ** (1.0/2.4)) - 0.055; 
!        end if
!
!        RGB = temp * 255     
!    END FUNCTION LIN2RGB 
 
END MODULE screen
