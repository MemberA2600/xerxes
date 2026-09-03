MODULE screen
    USE debugWindow
    USE engineConstants
    USE colors
    USE WINTERACTER
    USE RESID

    IMPLICIT NONE

    PRIVATE
    PUBLIC    :: initScreenBuff, eraseBuff, initRealScreen,  &
                 getGameScreenSize, buffer2Real, setBufferPixel, &
                 displayPalette
             
    INTEGER(KIND = 4), DIMENSION(:,:,:), &
                       ALLOCATABLE  :: screenBuffers
                             
    INTEGER(KIND = 4), DIMENSION(:),      &
                       ALLOCATABLE  :: screenData 

    INTEGER(KIND = 2), DIMENSION(2) :: screenSize = (/ 0, 0 /) 
    INTEGER(KIND = 2)               :: layers     = 0, test = 0, slow = 0
    CHARACTER(50)                   :: msgString
    INTEGER                         :: onlyBitMap
    LOGICAL                         :: bitmapCreated = .FALSE., dontDelete = .FALSE.

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
         dontDelete = .FALSE.
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

              screenBuffers(1,x,y) = getColorValue(test) 
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
                 screenBuffers(1,x,y) = getColorValue(test)

              else
                 screenBuffers(1,x,y) = getColorValue(1)    
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
                  screenBuffers(1,x,y) = getColorValue(mod((y + (test - 256)) / 8, 256) + 1)
              else
                  screenBuffers(1,x,y) = getColorValue(mod((x +  test)        / 8, 256) + 1)
              end if 

           end do
        end do 


    END SUBROUTINE testPattern3

    SUBROUTINE displayPalette()
        integer(2)      :: gridX, gridY, w, h, c, cI
        integer(2)      :: x, y
        character(40)   :: t

        dontDelete = .TRUE.

        c = (numOfColors / 16)

        h = (hOfScreenBuffer / c)
        w = (wOfScreenBuffer / c)

        !write(t, '(I0, " ", I0)') h, hOfScreenBuffer 
        !call displayDebug(t)

        do y = 0,     hOfScreenBuffer - 1, 1
            do x = 0, wOfScreenBuffer - 1, 1
               gridX = x / w 
               gridY = y / h 
 
               cI = (gridX + (c * gridY)) + 1

               screenBuffers(1 ,x + 1 ,y + 1) = getColorValue(cI)
            end do

            !write(t, '(I0, " ", I0)') y, gridY 
            !call displayDebug(t)

        end do

        dontDelete = .TRUE.

    END SUBROUTINE 

    subroutine setBufferPixel(n, x, y, c)
        integer(2) :: n, x, y, c
        
        dontDelete = .FALSE.

        if (c > 0) then
            screenBuffers(n, x, y) = getColorValue(c)
        end if

    end subroutine

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
        INTEGER(kind=4), DIMENSION(640, 480) :: all2One 
        INTEGER(kind=4)  :: layerIndex, lineIndex, pixelIndex, srcLineIndex, srcPixelIndex          
        INTEGER          :: counter 
        character(40)    :: test

        all2One = -1       
      
        do layerIndex       =  1, layers          , 1
           do lineIndex     =  1, hOfScreenBuffer , 1 
              do pixelIndex =  1, wOfScreenBuffer , 1         
                 
                 !if (layerIndex > 1) then
                 !write(test, "(I0, ' ', I0, ' ', I0)") layerIndex, lineIndex, pixelIndex     
                 !call displayDebug(test)
                 !end if

                 if (screenBuffers(layerIndex, pixelIndex, lineIndex) /= -1) then
                     all2One     (pixelIndex, lineIndex) = &
                     screenBuffers(layerIndex, pixelIndex, lineIndex)
                 else
                     if (layerIndex == layers .AND. all2One(pixelIndex, lineIndex) == -1) then
                         all2One (pixelIndex, lineIndex) = getColorValue(1)
                     end if
                 end if

              end do
           end do
        end do
             
        counter = 0
        do lineIndex       =  1, screenSize(2), 1 
          do pixelIndex    =  1, screenSize(1), 1 
                
             !if (screenSize(1) <= wOfScreenBuffer) then
             srcPixelIndex = (pixelIndex * wOfScreenBuffer) / screenSize(1) 
             srcLineIndex  = (lineIndex  * hOfScreenBuffer) / screenSize(2) 
             !else
    
             !end if

             counter             = counter + 1 
             if (all2One(srcPixelIndex, srcLineIndex) == -1) &
                 all2One(srcPixelIndex, srcLineIndex) = getColorValue(1)   

             screenData(counter) = all2One  (srcPixelIndex, srcLineIndex)
                 
          end do                       
        end do

        CALL WBitmapclear(onlyBitMap)           
        call WBitmapGetData(onlyBitMap,screenData)
        CALL WBitmapPut(onlyBitMap)
        if (dontDelete .EQV. .FALSE.) CALL eraseBuff()

    END SUBROUTINE buffer2Real
 
END MODULE screen
