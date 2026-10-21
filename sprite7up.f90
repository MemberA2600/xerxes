MODULE sprite7up

    USE debugWindow
    USE dataLoader
    USE WINTERACTER
    USE RESID
    USE subs
    USE engineConstants
    USE winapis
    use IFPORT
    use imageFactory
    use colors
    use screen
    use gameobject
    use inputreader

    implicit None

    private    
    public                        :: initBlockMaps, putSpritesOnBuffer, createSpriteObjPlayGround,        &
                                     createSpriteObjBackGround, setOffset, addToOffset,                   &
                                     createSpriteObjSky, addTempFilter, getAllIndByName, getAllIndByType, &
                                     addTempFiltertoAllByName, addTempFiltertoAllByType,                  &
                                     setWeather, spritePoz, getOffsetX, getOffsetY, setSize, &
                                     changeSprite, changeSpriteEditor, addPointerToLastSpritePoz, &
                                     checkCollision, addPointerByNameXY, layerBlocks, checkAllCollisions, &
                                     DROPALLUNITS, reorderPoz, checkForPossibleOverlaps, addPointerByName, &
                                     SETPOINTERBYNAME, changeXY

    type SpriteObj 
         integer(2)               :: w, h, spriteI
         type(imageFile), pointer :: imageF => null()      
         integer(1)               :: filter, bufferNum, tempFilter, tempFilterCountDown  
         integer(4)               :: ind
         logical                  :: active, unique
         type(counterTimer)       :: timer

         contains 
         procedure                :: drawImage     => drawImage 
         procedure                :: drawDrawDraw  => drawDrawDraw  
         procedure                :: addTempFilter => addTempFilter       
         procedure                :: animateSprite => animateSprite

    end type

    type SpritePoz
         character(NAME_MAX_LEN)   :: name
         integer(4)                :: typFlag
         integer(4)                :: x, y, yh, xw, ind, hitBoxW, hitBoxH, hitBoxAlign 
         integer(2)                :: fly     
         integer(1)                :: bufferNum
         type(objectData), pointer :: objectD
         logical                   :: dontCountMe 
         type(counterTimer)        :: idleCounter
            
    
         contains        

         procedure                :: killMe             => killMe
         procedure                :: changeSprite       => changeSprite 
         procedure                :: dropMySprite       => dropMySprite
         procedure                :: checkCollision     => checkCollision
         procedure                :: checkAllCollisions => checkAllCollisions 
         procedure                :: changeSpriteEditor => changeSpriteEditor 
         procedure                :: getHitBoxCenter    => getHitBoxCenter
         procedure                :: changeXY           => changeXY
    end type 

    type BlockMap
        type(SpriteObj), dimension(:), allocatable :: spriteList
        type(spritePoz), dimension(:), allocatable :: pozList
        integer(4)                                 :: nextIndexS, nextIndexP
    end type

    type(BlockMap), dimension(layerNum), target :: layerBlocks

    integer(2), dimension(layerNum, 2)          :: layerDimensions

    integer(4)                                  :: XOffset, YOffset, wSize, hSize

    integer(1), parameter                       :: BLOCKMAP_1     = 0, &
                                                   BLOCKMAP_INF   = 1, & 
                                                   BLOCKMAP_FIX   = 0, &
                                                   BLOCKMAP_EXP   = 1                                                

    integer(1), parameter, dimension(6)         :: tempFilterValueChangers = &
                (/ 0, 1, 2, 2, 1 , 0/)                            

    integer(1)                                  :: defaultFilter = NO_FILTER

    contains

    !       
    !   SpritePoz Things
    !

    subroutine changeXY(this, AddX, AddY)
        integer(2)          :: addX, addY                
        class(SpritePoz)    :: this
        
        this%x  = this%x  + addX 
        this%y  = this%y  + addY
        this%xw = this%xw + addX
        this%yh = this%yh + addY

        if (this%x < 0) then 
            this%x   = 0
            this%xw  = layerBlocks(this%bufferNum)%spriteList(this%ind)%w
        end if 

        if (this%y < 0) then 
            this%y   = 0
            this%yh  = layerBlocks(this%bufferNum)%spriteList(this%ind)%h
        end if 

        if (this%xw > getOffsetX() + wOfScreenBuffer) then
            this%xw = getOffsetX() + wOfScreenBuffer
            this%x  = this%xw - layerBlocks(this%bufferNum)%spriteList(this%ind)%w
        end if

        if (this%yh > getOffsetY() + hOfScreenBuffer) then
            this%yh = getOffsetY() + hOfScreenBuffer
            this%y  = this%yh - layerBlocks(this%bufferNum)%spriteList(this%ind)%h
        end if

    end subroutine

    subroutine setPointerByName(b, n, p)
        character(*)                            :: n
        integer(1)                              :: b
        type(SpritePoz), pointer, intent(inout) :: p
        integer(8)                              :: ind        
        !character(30)                           :: test

        do ind = 1, layerBlocks(b)%NextIndexP, 1
           !write(test, "(A, L)") layerBlocks(b)%pozList(ind)%name // ": ", layerBlocks(b)%pozList(ind)%objectD%triggerable             
           !call displayDebug(test)
 
           if (layerBlocks(b)%pozList(ind)%objectD%triggerable .EQV. .FALSE.) cycle 

           if (layerBlocks(b)%pozList(ind)%name == n) then   
               p => layerBlocks(b)%pozList(ind)
               return 
           end if       
        end do

        call displayDebug("No pointer set on name " // n // "!")

    end subroutine

    function checkForPossibleOverlaps(b, x, y, w, h, typ, fly, img) result(r)
        integer(1)               :: b, typ
        integer(4)               :: x, y
        integer(2)               :: w, h, fly
        logical                  :: r
        type(spritePoz), pointer :: that
        integer(4)               :: ind
        type(spritePoz)          :: dummy
        integer                  :: x1 , x2 , y1 , y2
        type(imageData), pointer :: iD2, iD1
        type(imageFile), pointer :: img

        r = .FALSE.

        do ind = 1, layerBlocks(b)%nextIndexP, 1
           that => layerBlocks(b)%pozList(ind) 
   
           if (that%dontCountMe) cycle    

           if ((associated(layerblocks(that%bufferNum)%spriteList(that%ind)%imageF) .EQV. .FALSE.) .OR. &
                          (layerblocks(that%bufferNum)%spriteList(that%ind)%active  .EQV. .FALSE.)) cycle

           if ((that%x < XOffset .OR. that%x > XOffset + wSize)  .AND. &   
               (that%y < YOffset .OR. that%y > YOffset + hSize)) cycle 
           if ((typ          == TYPE_FLOOR .OR. typ          == TYPE_FLOOR_DECORATION) .AND.  &
               (that%typFlag == TYPE_FLOOR .OR. that%typFlag == TYPE_FLOOR_DECORATION)) cycle 

           dummy%x       = x 
           dummy%y       = y 
           dummy%xw      = x + w - 1
           dummy%yh      = y + h - 1 
           dummy%fly     = fly
           dummy%typFlag = typ 

           call setHitBox(dummy, w, h, .TRUE.) 
           call calculateHBXY(dummy, x1, y1, w, h)

           call calculateHBXY(that , x2, y2, layerblocks(that%bufferNum)%spriteList(that%ind)%w, &
                                             layerblocks(that%bufferNum)%spriteList(that%ind)%h)


           r = x1 < x2 + that%hitBoxW  .AND. &
               x2 < x1 + dummy%hitBoxW .AND. &
               y1 < y2 + that%hitBoxH  .AND. &
               y2 < y1 + dummy%hitBoxH
     
           if (r .EQV. .FALSE.) cycle

           iD1 => img%img
           iD2 => layerblocks(that%bufferNum)%spriteList(that%ind)%imageF%img
    
           r = pixelColl(iD1, iD2, x1, y1, x2, y2, &
                          dummy%hitBoxW, dummy%hitBoxH, that%hitBoxW, that%hitBoxH, LAYER_PLAYGROUND, &
                          dummy%x, dummy%y, that%x, that%y, dummy%fly, that%fly)

           if (r .EQV. .TRUE.) return
        end do

    end function

    subroutine getHitBoxCenter(this, centerX, centerY)  
        class(SpritePoz)    :: this
        integer(4)          :: centerX, centerY

        call calculateHBXY(this, centerX, centerY, this%hitBoxW, this%hitBoxH)

        centerX = centerX + (this%hitBoxW / 2)
        centerY = centerY + (this%hitBoxH / 2)

    end subroutine 

    function checkAllCollisions(this, justCheck) result(colliding)
        class(spritePoz), target :: this
        type(spritePoz), pointer :: that
        logical                  :: justCheck

        integer(4)       :: ind
        logical          :: colliding

        colliding = .FALSE.

        do ind = 1, layerBlocks(this%bufferNum)%nextIndexP, 1
           that      => layerBlocks(this%bufferNum)%pozList(ind)               
           if (associated(that, this)) cycle          

           colliding =  this%checkCollision(layerBlocks(this%bufferNum)%pozList(ind))     

           if (colliding) then
              !call displayDebug(this%name // ' | ' // that%name) 
               !call displayDebug(this%name // ' | ' // that%name) 
               if (justCheck) return
           end if 

        end do

    end function

    function checkCollision(this, that) result(r)
        class(spritePoz) :: this
        type(spritePoz)  :: that
        logical          :: r, debug = .FALSE.
        integer          :: x1 , x2 , y1 , y2    
        type(imageData), pointer :: iD1, iD2
 
        r = .FALSE.
    
        if (.NOT. (this%objectD%solid .AND. that%objectD%solid)) return
        if (this%name == that%name) return

        if (this%dontCountMe .OR. that%dontCountMe) return    

        if ((associated(layerblocks(this%bufferNum)%spriteList(this%ind)%imageF) .EQV. .FALSE.) .OR. &
                       (layerblocks(this%bufferNum)%spriteList(this%ind)%active  .EQV. .FALSE.)) return

        if ((associated(layerblocks(that%bufferNum)%spriteList(that%ind)%imageF) .EQV. .FALSE.) .OR. &
                       (layerblocks(that%bufferNum)%spriteList(that%ind)%active  .EQV. .FALSE.)) return

        if ((this%x < XOffset .OR. this%x > XOffset + wSize)  .AND. &
            (this%y < YOffset .OR. this%y > YOffset + hSize)) return 

        if ((that%x < XOffset .OR. that%x > XOffset + wSize)  .AND. &
            (that%y < YOffset .OR. that%y > YOffset + hSize)) return 

        call calculateHBXY(this , x1, y1, layerblocks(this%bufferNum)%spriteList(this%ind)%w, &
                                          layerblocks(this%bufferNum)%spriteList(this%ind)%h)
        call calculateHBXY(that , x2, y2, layerblocks(that%bufferNum)%spriteList(that%ind)%w, &
                                          layerblocks(that%bufferNum)%spriteList(that%ind)%h)
    

        r = x1 < x2 + that%hitBoxW .AND. &
            x2 < x1 + this%hitBoxW .AND. &
            y1 < y2 + that%hitBoxH .AND. &
            y2 < y1 + this%hitBoxH
    
        if (isAKeyPressed(8) .AND. (debug)) then
            call displayDebugNumTxt("x1: ", x1)
            call displayDebugNumTxt("x2: ", x2)
            call displayDebugNumTxt("spX1: ", this%x)
            call displayDebugNumTxt("spX2: ", that%x)
            call displayDebugNumTxt("spXW1: ", this%xw)
            call displayDebugNumTxt("spXW2: ", that%xw)


            call displayDebugNumTxt("y1: ", y1)
            call displayDebugNumTxt("y2: ", y2)
            call displayDebugNumTxt("spY1: ", this%y)
            call displayDebugNumTxt("spY2: ", that%y)
            call displayDebugNumTxt("spYH1: ", this%yh)
            call displayDebugNumTxt("spYH2: ", that%yh)

            call displayDebugNumTxt("spFly1: ", this%fly)
            call displayDebugNumTxt("spFly2: ", that%fly)


            call displayDebugNumTxt("hW1: ", this%hitBoxW)
            call displayDebugNumTxt("hH1: ", this%hitBoxH)
            call displayDebugNumTxt("hW2: ", that%hitBoxW)
            call displayDebugNumTxt("hH2: ", that%hitBoxH)

            call displayDebugNumTxt('x2 < that%hitBoxW', x2 + that%hitBoxW)
            call displayDebugNumTxt('x1 < this%hitBoxW', x1 + this%hitBoxW)
            call displayDebugNumTxt('y2 < that%hitBoxH', y2 + that%hitBoxH)
            call displayDebugNumTxt('y1 < this%hitBoxH', y1 + this%hitBoxH)

        end if

        if (r .EQV. .FALSE.) return

        iD1 => layerblocks(this%bufferNum)%spriteList(this%ind)%imageF%img
        iD2 => layerblocks(that%bufferNum)%spriteList(that%ind)%imageF%img

        r = pixelColl(iD1, iD2, x1, y1, x2, y2, &
                      this%hitBoxW, this%hitBoxH, that%hitBoxW, that%hitBoxH, LAYER_PLAYGROUND, &
                      this%x, this%y, that%x, that%y, this%fly, that%fly)
   

    end function

    function pixelColl(iD1, iD2, x1, y1, x2, y2, hitBoxW1, hitBoxH1, hitBoxW2, hitBoxH2, bufferNum,&
                       spX1, spY1, spX2, spY2, spFly1, spFly2) result(r)
        logical          :: r
        integer          :: x1 , x2 , y1 , y2 , checkX, checkY, &
                            pX1, pX2, pY1, pY2, startX, startY, spX1, spY1, spX2, spY2 
        integer(2)       :: boxW, boxH 
        integer(2)       :: hitBoxW1, hitBoxH1, hitBoxW2, hitBoxH2, spFly1, spFly2
        integer(1)       :: bufferNum
    
        type(imageData), pointer :: iD1, iD2

        r = .FALSE.

     !  Choose the smaller hitbox.
        if (hitBoxW1 * hitBoxH1 <= &
            hitBoxW2 * hitBoxH2) then
    
            startX = x1
            startY = y1
            boxW = hitBoxW1
            boxH = hitBoxH1
        else
            startX = x2
            startY = y2
            boxW = hitBoxW2
            boxH = hitBoxH2
        end if

        do checkY = startY, startY + boxH - 1
            do checkX = startX, startX + boxW - 1
    
              ! Skip pixels outside the shared hitbox area.
                if (checkX < x1 .OR. checkX >= x1 + hitBoxW1) cycle
                if (checkY < y1 .OR. checkY >= y1 + hitBoxH1) cycle
                if (checkX < x2 .OR. checkX >= x2 + hitBoxW2) cycle
                if (checkY < y2 .OR. checkY >= y2 + hitBoxH2) cycle
    
              ! Convert screen coordinates to sprite-local coordinates.
              ! Assumes x and y are each sprite's top-left screen position.
                pX1 = checkX -  spX1 + 1
                pY1 = checkY - (spY1 - spFly1) + 1
    
                pX2 = checkX -  spX2 + 1
                pY2 = checkY - (spY2 - spFly2) + 1
    
                ! Check the two sprite pixels here.
                if (pX1 < 1 .OR. pX1 > iD1%width ) cycle
                if (pY1 < 1 .OR. pY1 > iD1%height) cycle
                if (pX2 < 1 .OR. pX2 > iD2%width ) cycle
                if (pY2 < 1 .OR. pY2 > iD2%height) cycle                

                if (iD1%returnColorOfPixel(1, pX1, pY1) /= -1 .AND. &
                    iD2%returnColorOfPixel(1, pX2, pY2) /= -1) then 
                    r = .TRUE.
                    return
                end if
            end do
        end do


    end function 

    subroutine calculateHBXY(sp, x, y, w, h)
        type(spritePoz), intent(in) :: sp
        integer, intent(out)        :: x, y
        integer(2)                  :: w, h

        select case(sp%hitBoxAlign)
        case(ALIGN_CENTER)  
            x = sp%xw - (w / 2) - (sp%hitBoxW / 2) 
            y = sp%yh - (h / 2) - (sp%hitBoxH / 2) 

        case(ALIGN_BOTTOM)
            x = sp%xw - (w / 2) - (sp%hitBoxW / 2) 
            y = sp%yh - sp%hitBoxH + 1

        end select

        if (abs(x - sp%x) == 1) x = sp%x
        if (abs(y - sp%y) == 1) y = sp%y

        y = y - sp%fly

    end subroutine

    subroutine addPointerToLastSpritePoz(p, b)
        type(spritePoz), pointer, intent(inout) :: p
        integer(1)                              :: b        
  
        p => layerBlocks(b)%pozList(layerBlocks(b)%nextIndexP)
  
    end subroutine

    subroutine addPointerByNameXY(p, b, n, x, y)
        type(spritePoz), pointer, intent(inout) :: p
        integer(1)                              :: b  
        character(*)                            :: n
        integer(4)                              :: x, y, num      

        do num = 1, layerBlocks(b)%nextIndexP, 1
           if (n == layerBlocks(b)%pozList(num)%name .AND. & 
               x == layerBlocks(b)%pozList(num)%x    .AND. &  
               y == layerBlocks(b)%pozList(num)%y    ) then
               p => layerBlocks(b)%pozList(num)
               return
           end if  
        end do

        call displayDebug("No pointer was set for Map Object " // n // " !")

    end subroutine

    subroutine addPointerByName(p, b, n)
        type(spritePoz), pointer, intent(inout) :: p
        integer(1)                              :: b  
        character(*)                            :: n
        integer(4)                              :: x, y, num      

        do num = 1, layerBlocks(b)%nextIndexP, 1
           if (n == layerBlocks(b)%pozList(num)%name) then
               p => layerBlocks(b)%pozList(num)
               return
           end if  
        end do

        call displayDebug("No pointer was set for Map Object " // n // " !")

    end subroutine

    subroutine changeSpriteEditor(this, wind)
        class(spritePoz), intent(inout) :: this        
        logical                         :: wind
       
        call this%changeSprite(this%objectD%getEditorSprite(wind, .FALSE., &
             layerBlocks(this%bufferNum)%spriteList(this%ind)%imageF%name  ))

    end subroutine

    subroutine changeSprite(this, imageName)
        class(spritePoz), intent(inout) :: this        
        character(*)                    :: imageName
        logical                         :: unique    
        integer(1)                      :: filter

        unique = layerBlocks(this%bufferNum)%spriteList(this%ind)%unique
        filter = layerBlocks(this%bufferNum)%spriteList(this%ind)%filter

        call this%dropMySprite()

        call AddSpriteThing(this%name, imageName, this%bufferNum, this%x, this%y, & 
                            filter, this%fly, unique, this%objectD%name, .TRUE.)

        this%ind = layerBlocks(this%bufferNum)%nextIndexS       


    end subroutine

    subroutine dropMySprite(this)
        class(spritePoz), intent(inout) :: this        
        integer(4)                      :: pozInd = 0

        if  (layerBlocks(this%bufferNum)%spriteList(this%ind)%unique .EQV. .TRUE.) then 
            layerBlocks(this%bufferNum)%spriteList(this%ind)%active = .FALSE.
        else
            pozInd = 0
            pozInd = getPozInd(pozInd, this%ind, this%bufferNum)
            pozInd = getPozInd(pozInd, this%ind, this%bufferNum)

            !
            !  If the sprite had only one occurences, delete it.
            !

            if (pozInd == -1) layerBlocks(this%bufferNum)%spriteList(this%ind)%active = .FALSE.

        end if

    end subroutine

    subroutine killMe(this, reord)
        class(spritePoz), intent(inout) :: this        
        logical                         :: reord

        call this%dropMySprite()
        this%dontCountMe = .TRUE.

        if (reord) then
           if (layerBlocks(this%bufferNum)%nextIndexP > 1) call reorderPoz(this%bufferNum)
        end if

    end subroutine       

    !       
    !   SpriteObj Things
    !

    subroutine addTempFilter(this, f, t)
        class(SpriteObj)        :: this
        integer(1)              :: f, t
        
        if (this%tempFilter == -1 .OR. this%tempFilterCountDown <= t) then 
            this%tempFilter          = f
            this%tempFilterCountDown = t * FILTER_MULTI
        end if

    end subroutine       

    subroutine drawDrawDraw(this, x, y, fly, typ)
        class(SpriteObj)        :: this
        integer(4)              :: x, y
        integer(2)              :: fly, typ, b
        integer(1)              :: filter    

        filter = this%filter
        
        b = this%bufferNum
        if (typ == TYPE_FLOOR .OR. typ == TYPE_FLOOR_DECORATION) b = LAYER_BACKGROUND

        if (this%tempFilter > 0) then
            if (this%tempFilterCountDown == 0) then
                this%tempFilter = -1
            else
                filter = this%tempFilter + tempFilterValueChangers(this%tempFilterCountDown * FILTER_MULTI)
                this%tempFilterCountDown = this%tempFilterCountDown - 1
            end if
        end if

        if (b == LAYER_PLAYGROUND) then
            if (fly > 0) then
                call this%imageF%addToScreenBuffer(this%spriteI, b - 1, &
                                                   x, y, FILTER_SHADOW)
    
                call this%imageF%addToScreenBuffer(this%spriteI, b, &
                                                   x, y, FILTER_SHADOW)

                call this%imageF%addToScreenBuffer(this%spriteI, b, &
                                                   x, y - fly, filter)                
            else
                call this%imageF%addToScreenBuffer(this%spriteI, b, &
                                                   x, y, filter)
            end if
        else
            call this%imageF%addToScreenBuffer(this%spriteI, b, &
                                               x, y, filter)
        end if
    end subroutine

    function getPozInd(p, i, b) result(res)
        integer(4)              :: i
        integer(4)              :: p
        integer(1)              :: b
        integer(4)              :: ind, res

        res = -1

        do ind = p + 1, layerBlocks(b)%nextIndexP, 1
           if (layerBlocks(b)%pozList(ind)%ind == i) then 
                res = ind 
                exit
           end if 
        end do

    end function

    subroutine drawImage(this, pozInd)
        class(SpriteObj)        :: this
        integer(4)              :: x, y, typ
        integer(4)              :: pozInd
        character(40)           :: test
        integer(2)              :: fly

        if ((associated(this%imageF) .EQV. .FALSE.)  .OR. &
            (this%active             .EQV. .FALSE.)  .OR. pozInd == -1) return
    
        typ = layerBlocks(this%bufferNum)%pozList(pozInd)%typFlag

        if (layerDimensions(this%bufferNum,1) /= BLOCKMAP_1) then  
    
                x   = layerBlocks(this%bufferNum)%pozList(pozInd)%x
                y   = layerBlocks(this%bufferNum)%pozList(pozInd)%y
                fly = layerBlocks(this%bufferNum)%pozList(pozInd)%fly
            
                if (layerDimensions(this%bufferNum,2) /= BLOCKMAP_FIX) then
                    if ((x + this%w - 1 < XOffset) .OR. (x > XOffset + wSize)  .OR. & 
                        (y + this%h - 1 < YOffset) .OR. (Y > YOffset + hSize)) return
            
                    x = x - XOffset 
                    y = y - YOffset 
                end if
    
    
                call this%drawDrawDraw(x, y, fly, typ)
        
        else

                do y = layerBlocks(this%bufferNum)%pozList(pozInd)%y -   & 
                       layerBlocks(this%bufferNum)%spriteList(this%ind)%h, &
                       hSize + layerBlocks(this%bufferNum)%spriteList(this%ind)%h, &
                       layerBlocks(this%bufferNum)%spriteList(this%ind)%h 
                    do x = layerBlocks(this%bufferNum)%pozList(pozInd)%x -   & 
                           layerBlocks(this%bufferNum)%spriteList(this%ind)%w, &
                           wSize + layerBlocks(this%bufferNum)%spriteList(this%ind)%w, &
                           layerBlocks(this%bufferNum)%spriteList(this%ind)%w 
       
    
                        call this%drawDrawDraw(x - modulo(XOffset, layerBlocks(this%bufferNum)%spriteList(this%ind)%w) &
                                             , y - modulo(YOffset, layerBlocks(this%bufferNum)%spriteList(this%ind)%h) &
                                             , 0, typ)
    
                    end do
                end do
    
        end if
    
        if (this%timer%getDiffCheck() /= getWaitTime(this%bufferNum, this%imageF%name)) then
            call this%timer%timerStart(getWaitTime(this%bufferNum, this%imageF%name))    
            call this%animateSprite()                  
        else
            if (this%timer%timerEnded() .EQV. .TRUE. ) call this%animateSprite()             
        end if  

        !end do
    end subroutine

    function getWaitTime(layer, name) result(waitTime)
        integer(1)          :: layer 
        integer             :: waitTime
        character(*)        :: name
        integer(1)          :: num

        if (len_trim(name) >= 5) then
            if (name(1:5) == "Water") then
                if (name == "WaterWind") then
                    waitTime = PERFECT_WAIT * getSpeed() 
                else
                    waitTime = PERFECT_WAIT * 1.5 * getSpeed() 
                end if
                return 
            end if

            do num = 1, len_trim(name) - 3, 1
               if (name(num:num + 3) == "Wind") then 
                   waitTime = PERFECT_WAIT * 0.75 * getSpeed() 
                   return 
               end if 
            end do 

        end if

        select case(layer)
        case(LAYER_WEATHER)
            waitTime = PERFECT_WAIT / 3 * getSpeed()      
        case default
            waitTime = PERFECT_WAIT * getSpeed()
        end select
    
    end function

    subroutine animateSprite(this)
       class(SpriteObj)        :: this

       if (this%imageF%img%numOfFrames > 1) then
            this%spriteI = this%spriteI + 1

            if (this%spriteI > this%imageF%img%numOfFrames) then
               this%spriteI = 1
            end if

            !call this%timer%timerStart(getWaitTime(this%bufferNum, this%imageF%name)) 
            call this%timer%timerReStart() 
        else
            this%spriteI = 1
        end if  

    end subroutine

    !
    !   BlockMap Stuff
    !

    subroutine addTempFiltertoAllByName(b, n, f, t)
        integer(1)                           :: f, t, b
        character(*)                         :: n
        integer                              :: ind, rc        

        integer, dimension(:,:), allocatable :: l
        !character(40)                        :: test    

        call getAllIndByName(b, n, l)

        do ind = 1, size(l, 1), 1
           call layerBlocks(b)%spriteList(l(ind, 2))%addTempFilter(f, t) 
        end do

        deallocate(l, stat = RC)
        if (rc /= 0) call displayDebug("Failed to dealloc list of indexes!")

    end subroutine  

    subroutine addTempFiltertoAllByType(b, typ, f, t)
        integer(1)                           :: f, t, b
        integer                              :: typ
        integer                              :: ind, rc        

        integer, dimension(:,:), allocatable :: l

        call getAllIndByType(b, typ, l)
        do ind = 1, size(l, 1), 1
           call layerBlocks(b)%spriteList(l(ind, 2))%addTempFilter(f, t) 
        end do

        deallocate(l, stat = RC)
        if (rc /= 0) call displayDebug("Failed to dealloc list of indexes!")
    end subroutine       

    subroutine getAllIndByName(b, n, r) 
        integer(1)                                        :: b

        integer, dimension(:,:), allocatable, intent(out) :: r
        integer, dimension(:,:), allocatable              :: temp

        integer                                           :: l, i, iP
        integer(1)                                        :: rc
        character(*)                                      :: n
 
        l = 0

        if (allocated(r)) then
            deallocate(r, stat = RC)
            if (rc /= 0) call displayDebug("Failed to deallocate temp for All finder!")                 
        end if 

        allocate(temp(layerBlocks(b)%nextIndexP, 2), stat = RC)
        if (rc /= 0) call displayDebug("Failed to allocate temp for All finder!")                 

        do i = 1, layerBlocks(b)%nextIndexP, 1
           iP = layerBlocks(b)%pozList(i)%ind  

           if (layerBlocks(b)%pozList(i)%name /= n .AND. n /= "") cycle 
                        
           if ((layerBlocks(b)%spriteList(ip)%active             .EQV. .TRUE.) .AND. &
               (associated(layerBlocks(b)%spriteList(ip)%imageF) .EQV. .TRUE.)) then    
                l = l + 1    
                temp(l, 1) = i
                temp(l, 2) = iP
            end if

        end do

        allocate(r(l, 2), stat = RC)
        if (rc /= 0) call displayDebug("Failed to allocate result for All finder!")                 
        
        do i = 1, l, 1
           r(i, 1) = temp(i, 1)
           r(i, 2) = temp(i, 2)
        end do
      
        deallocate(temp, stat = RC)
        if (rc /= 0) call displayDebug("Failed to deallocate temp for All finder!")   

    end subroutine 

    subroutine getAllIndByType(b, t, r) 
        integer(1)                                        :: b

        integer, dimension(:,:), allocatable, intent(out) :: r
        integer, dimension(:,:), allocatable              :: temp

        integer                                           :: l, i, iP    
        integer(1)                                        :: rc
        integer(4)                                        :: t
 
        l = 0

        if (allocated(r)) then
            deallocate(r, stat = RC)
            if (rc /= 0) call displayDebug("Failed to deallocate temp for All finder!")                 
        end if 

        allocate(temp(layerBlocks(b)%nextIndexP, 2), stat = RC)
        if (rc /= 0) call displayDebug("Failed to allocate temp for All finder!")                 

        do i = 1, layerBlocks(b)%nextIndexP, 1
           iP = layerBlocks(b)%pozList(i)%ind  

           if (layerBlocks(b)%pozList(i)%typFlag /= t) cycle 

           if ((layerBlocks(b)%spriteList(ip)%active             .EQV. .TRUE.) .AND. &
               (associated(layerBlocks(b)%spriteList(ip)%imageF) .EQV. .TRUE.)) then    
                l = l + 1    
                temp(l, 1) = i
                temp(l, 2) = iP
            end if

        end do

        allocate(r(l, 2), stat = RC)
        if (rc /= 0) call displayDebug("Failed to allocate result for All finder!")                 

        do i = 1, l, 1
           r(i, 1) = temp(i, 1)
           r(i, 2) = temp(i, 2)
        end do
      
        deallocate(temp, stat = RC)
        if (rc /= 0) call displayDebug("Failed to deallocate temp for All finder!")   

    end subroutine 

    subroutine offSetCorr()

        if (xOffset < 0) xOffset = 0
        if (yOffset < 0) yOffset = 0

        if (xOffset > wSize - wOfScreenBuffer) xOffset = wSize - wOfScreenBuffer
        if (yOffset > hSize - hOfScreenBuffer) yOffset = hSize - hOfScreenBuffer

    end subroutine

    function getOffsetX() result(r)
        integer(4)          :: r

        r = xOffset 
    end function 

    function getOffsetY() result(r)
        integer(4)          :: r

        r = yOffset     
    end function 

    subroutine setOffset(x, y)
        integer(4)          :: x, y
        
        xOffset = x
        yOffset = y

        call offsetCorr()

    end subroutine

    subroutine addToOffset(x, y)
        integer(4)          :: x, y
        
        xOffset = xOffset + x
        yOffset = yOffset + y

        call offsetCorr()

    end subroutine

    subroutine setSize(w, h)
        integer(4)          :: w, h

        wSize = w
        hSize = h

        !call offsetCorr()
        call setOffset(0, 0)

    end subroutine

    subroutine setWeather(w, wind)
        integer(1)      :: w
        logical         :: wind
        integer(4)      :: bufferN, ind

        layerBlocks(LAYER_WEATHER)%spriteList(1)%active = .FALSE.
        
        select case(w)
        case(WEATHER_DAY_NORM)    
             defaultFilter = NO_FILTER

        case(WEATHER_NIGHT_NORM)    
             defaultFilter = FILTER_BLUE

        case(WEATHER_DAY_RAIN)    
             defaultFilter = NO_FILTER

             if (wind) then 
                call createSpriteObjWeather("RainWind", "RainWind", defaultFilter, "")
             else
                call createSpriteObjWeather("Rain", "Rain", defaultFilter, "")
             end if
        case(WEATHER_NIGHT_RAIN)    
             defaultFilter = FILTER_BLUE  

             if (wind) then              
                call createSpriteObjWeather("RainWind", "RainWind", defaultFilter, "")
             else
                call createSpriteObjWeather("Rain", "Rain", defaultFilter, "")
             end if
        end select

        do bufferN = 1, layerNum, 1
           if (bufferN /= LAYER_FOREGROUND .AND. bufferN /= LAYER_INTERFACE &
         .AND. bufferN /= LAYER_TRANSITION) then
               do ind = 1, size(layerBlocks(bufferN)%spriteList), 1 
                  if ((layerBlocks(bufferN)%spriteList(ind)%active .EQV. .TRUE.) .AND. &
          (associated(layerBlocks(bufferN)%spriteList(ind)%imageF) .EQV. .TRUE.)) then
                       layerBlocks(bufferN)%spriteList(ind)%filter = defaultFilter 
                  end if      
               end do
           end if 
        end do

    end subroutine  

    subroutine createSpriteObjWeather(spriteName, imageName, filter, gameObjName)
         character(*)  :: imageName, spriteName, gameObjName   
         integer(1)    :: filter

         call createSpriteObj(spriteName, imageName, LAYER_WEATHER, 1, 1, filter, 0, .TRUE., gameObjName)

    end subroutine 

    subroutine createSpriteObjSky(spriteName, imageName, x, y, filter, fly, unique, gameObjName)
         character(*)  :: imageName, spriteName, gameObjName   
         integer(4)    :: x, y
         integer(1)    :: filter
         integer(2)    :: fly
         logical       :: unique
        
      !
      !  Sky units are basically ground units. If fly = 0, the main unit is the creature, but if it flies,
      !  the shadow becomes the main unit and the creature is just drawn on the SKY layer.
      !

         call createSpriteObj(spriteName, imageName, LAYER_PLAYGROUND, x, y, filter, fly, unique, gameObjName)

    end subroutine 

    subroutine createSpriteObjPlayGround(spriteName, imageName, x, y, filter, unique, gameObjName)
         character(*)  :: imageName, spriteName, gameObjName   
         integer(4)    :: x, y
         integer(1)    :: filter
         logical       :: unique

         call createSpriteObj(spriteName, imageName, LAYER_PLAYGROUND, x, y, filter, 0, unique,gameObjName   )

    end subroutine 

    subroutine createSpriteObjBackGround(spriteName, imageName, filter, gameObjName)
         character(*)  :: imageName, spriteName, gameObjName   
         integer(1)    :: filter

         call createSpriteObj(spriteName, imageName, LAYER_BACKGROUND, 1, 1, filter, 0, .TRUE., gameObjName)

    end subroutine 
   
    subroutine createSpriteObj(spriteName, imageName, bufferNum, x, y, filter, fly, unique, gameObjName)
         character(*)          :: imageName, spriteName, gameObjName   
         integer(4)            :: x, y, f
         integer(1)            :: filter, bufferNum  
         integer(4)            :: typFlag
         integer(1)            :: rc
         integer(2)            :: fly
         logical               :: unique
  
         if (bufferNum /= LAYER_FOREGROUND .AND. bufferNum /= LAYER_INTERFACE &
       .AND. bufferNum /= LAYER_TRANSITION) then
             if (filter == NO_FILTER) then
                 f = defaultFilter 
             else
                 f = filter
             end if      
         else   
             f = filter
         end if

         call AddSpriteThing(spriteName, imageName, bufferNum, x, y, f, fly, unique, gameObjName, .FALSE.)   

         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%ind = &
         layerBlocks(bufferNum)%nextIndexS

         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%y   = y
         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%yh  = y - 1 + &
         layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%h
         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%x   = x
         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%xw  = x - 1 + &
         layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%w 

         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%fly      = fly
         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%name     = spriteName
         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%bufferNum = bufferNum  

         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%typFlag  = &
         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%objectD%objType
         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%dontCountMe = .FALSE.

         call setHitBox(layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)     , &
                        layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%w, &
                        layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%h, &
                        layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%objectD%solid)   

         call layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%idleCounter%timerStart(&
              (PERFECT_WAIT * getSpeed())) 


         if (layerBlocks(bufferNum)%nextIndexP > 1) call reorderPoz(bufferNum)


         !call displayDebugNumTxt("Poz:    ", layerBlocks(bufferNum)%NextIndexP) 
         !call displayDebugNumTxt("Sprite: ", layerBlocks(bufferNum)%NextIndexS) 

    end subroutine

    subroutine setHitBox(poz, w, h, solid)
         type(SpritePoz), intent(inout) :: poz
         logical                        :: solid
         integer(1), parameter          :: FULL = 0, BOTTOM = 1
         integer(1)                     :: hitBoxTyp
         integer(2)                     :: w, h

         if (solid) then
             select case(poz%typFlag)
             case(TYPE_FLOOR)
                 hitBoxTyp = FULL
             case(TYPE_FLOOR_DECORATION)
                 hitBoxTyp = FULL
             case(TYPE_PLAYER_SPAWNER)
                 hitBoxTyp = FULL
             case default 
                 hitBoxTyp = BOTTOM

             end select

             select case(hitBoxTyp)
             case(FULL)
                 poz%hitBoxW     = w    
                 poz%hitBoxH     = h
                 poz%hitBoxAlign = ALIGN_CENTER

             case default
                 poz%hitBoxW     = w     
                 poz%hitBoxH     = h / 8
                 poz%hitBoxAlign = ALIGN_BOTTOM

                 if (poz%hitBoxH == 0) poz%hitBoxH = 1

             end select
         else
             poz%hitBoxW     =  0
             poz%hitBoxH     =  0
             poz%hitBoxAlign =  0

         end if

    end subroutine

    subroutine AddSpriteThing(spriteName, imageName, bufferNum, x, y, filter, & 
                              fly, unique, gameObjName, spriteOnly)
         character(*)  :: imageName, spriteName, gameObjName   
         integer(4)    :: x, y
         integer(1)    :: filter, bufferNum  
         integer(4)    :: typFlag
         integer(1)    :: rc
         integer(2)    :: fly
         logical       :: unique, spriteOnly       
         logical       :: alreadyThere
         integer(4)    :: ind

         type(SpriteObj), dimension(:), allocatable :: spriteListTemp
         type(spritePoz), dimension(:), allocatable :: pozListTemp

         alreadyThere = .FALSE.

         if (layerDimensions(bufferNum,1) /= BLOCKMAP_1) then
             if (unique .EQV. .FALSE.) then  
                 do ind = 1, size(layerBlocks(bufferNum)%spriteList), 1
                    if ((layerBlocks(bufferNum)%spriteList(ind)%active             .EQV. .TRUE.) .AND. &
                        (associated(layerBlocks(bufferNum)%spriteList(ind)%imageF) .EQV. .TRUE.) .AND. &
                        (layerBlocks(bufferNum)%spriteList(ind)%unique .EQV. .FALSE. )) then

                         if (layerBlocks(bufferNum)%spriteList(ind)%imageF%name == imageName) then
                             layerBlocks(bufferNum)%nextIndexS = layerBlocks(bufferNum)%spriteList(ind)%ind
                             alreadyThere = .TRUE.
                             exit
                         end if
                    end if        
                 end do   
             end if

             if (alreadyThere .EQV. .FALSE.) then
                 layerBlocks(bufferNum)%nextIndexS = size(layerBlocks(bufferNum)%spriteList) 
  
                 do ind = 1, size(layerBlocks(bufferNum)%spriteList), 1
                    if ((layerBlocks(bufferNum)%spriteList(ind)%active             .EQV. .FALSE.) .OR. &
                        (associated(layerBlocks(bufferNum)%spriteList(ind)%imageF) .EQV. .FALSE.)) then
                         layerBlocks(bufferNum)%nextIndexS = ind - 1   
                         exit   
                    end if            
                 end do
       
                 if (layerBlocks(bufferNum)%nextIndexS == size(layerBlocks(bufferNum)%spriteList)) then
        
                     allocate(spriteListTemp(layerBlocks(bufferNum)%nextIndexS + SIZE_ADD), stat = RC)
                     if (rc /= 0) call displayDebug("Failed to allocate temporal spriteList!")
        
                     spriteListTemp(1:layerBlocks(bufferNum)%nextIndexS) = layerBlocks(bufferNum)%spriteList
        
                     call move_alloc(spriteListTemp, layerBlocks(bufferNum)%spriteList)
                
                     do ind = layerBlocks(bufferNum)%nextIndexS + 1, size(layerBlocks(bufferNum)%spriteList), 1
                        layerBlocks(bufferNum)%spriteList(ind)%active = .FALSE.
                        nullify(layerBlocks(bufferNum)%spriteList(ind)%imageF)
                     end do

                 end if 

                 layerBlocks(bufferNum)%nextIndexS = & 
                 layerBlocks(bufferNum)%nextIndexS + 1       
             end if         

             if (spriteOnly .EQV. .FALSE.) then

                 if (layerBlocks(bufferNum)%nextIndexP == size(layerBlocks(bufferNum)%pozList)) then
        
                     allocate(pozListTemp(layerBlocks(bufferNum)%nextIndexP + SIZE_ADD), stat = RC)
                     if (rc /= 0) call displayDebug("Failed to allocate temporal pozList!")
        
                     pozListTemp(1:layerBlocks(bufferNum)%nextIndexP) = layerBlocks(bufferNum)%pozList
        
                     call move_alloc(pozListTemp, layerBlocks(bufferNum)%pozList)
                 end if                 
                   
                 layerBlocks(bufferNum)%nextIndexP = & 
                 layerBlocks(bufferNum)%nextIndexP + 1   
             end if
         else
             layerBlocks(bufferNum)%nextIndexS = 1            
             layerBlocks(bufferNum)%nextIndexP = 1   
         end if  

         if (alreadyThere .EQV. .FALSE.) then
             call assignSpriteToPointer(imageName, &
                  layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%imageF) 

             layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%w = &
             layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%imageF%img%width    
    
             layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%h = &
             layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%imageF%img%height  
    
             layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%spriteI    = 1
             layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%filter     = filter
             layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%tempFilter    = -1
             layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%tempFilterCountDown = 0
    
             layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%bufferNum = bufferNum   
             layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%active    = .TRUE.
    
             layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%ind = &
             layerBlocks(bufferNum)%nextIndexS
    
             layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%unique = unique

             if (layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%imageF%img%numOfFrames > 1) then

                 call layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%timer%timerStart( &
                      getWaitTime(bufferNum, imageName))
        
             end if
         end if

         if (spriteOnly .EQV. .FALSE.) then  
             if (gameObjName == "") then
                call getGameObjByName(&
                     layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%objectD, spriteName)   
             else
                call getGameObjByName(&
                     layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%objectD, gameObjName)   
             end if
         end if   
    end subroutine

    subroutine dropAllUnits()
        integer(1)              :: b
        integer(8)              :: ind
        
        do b = 1, layerNum, 1
           do ind = 1, layerBlocks(b)%nextIndexP, 1 
              if (layerBlocks(b)%pozList(ind)%dontCountMe .EQV. .FALSE.) call layerBlocks(b)%pozList(ind)%killMe(.FALSE.) 
           end do 

           layerBlocks(b)%nextIndexP = 0 
        end do


    end subroutine

    subroutine initBlockMaps(n, w, h)
        integer(1)              :: n, ind, rc
        character(40)           :: test
        integer(2)              :: w, h
        integer(4)              :: num
        
        call setDimensions()
        xOffset   = 0
        yOffset   = 0
        wSize     = w
        hSize     = h

        do ind = 1, n, 1

           if (allocated(layerBlocks(ind)%spriteList)) call deAllocBlockMap(ind)
    
           layerBlocks(ind)%nextIndexS = 0
           layerBlocks(ind)%nextIndexP = 0

           select case(layerDimensions(ind,1))
           case(BLOCKMAP_1)  
                allocate(layerBlocks(ind)%spriteList(1), stat = rc)
                if (rc /= 0) call displayDebug("Failed to allocate spriteList!") 
    
                allocate(layerBlocks(ind)%pozList   (1), stat = rc)
                if (rc /= 0) call displayDebug("Failed to allocate pozList!") 
    
                layerBlocks(ind)%spriteList(1)%active = .FALSE.
                layerBlocks(ind)%spriteList(1)%unique = .FALSE.
                nullify(layerBlocks(ind)%spriteList(1)%imageF)

           case(BLOCKMAP_INF) 
                allocate(layerBlocks(ind)%spriteList(SIZE_INIT), stat = rc)
                if (rc /= 0) call displayDebug("Failed to allocate spriteList!") 
    
                allocate(layerBlocks(ind)%pozList   (SIZE_INIT), stat = rc)
                if (rc /= 0) call displayDebug("Failed to allocate pozList!") 

                do num = 1, SIZE_INIT, 1
                    layerBlocks(ind)%spriteList(num)%active = .FALSE.
                    layerBlocks(ind)%spriteList(num)%unique = .FALSE.
                    nullify(layerBlocks(ind)%spriteList(num)%imageF)
                end do

           end select 
        end do 

    end subroutine

    subroutine setDimensions()

        layerDimensions(LAYER_BACKGROUND,1) =   BLOCKMAP_1
        layerDimensions(LAYER_BACKGROUND,2) =   BLOCKMAP_FIX

        layerDimensions(LAYER_PLAYGROUND,1) =   BLOCKMAP_INF       
        layerDimensions(LAYER_PLAYGROUND,2) =   BLOCKMAP_EXP     

        layerDimensions(LAYER_WEATHER   ,1) =   BLOCKMAP_1        
        layerDimensions(LAYER_WEATHER   ,2) =   BLOCKMAP_FIX

        layerDimensions(LAYER_FOREGROUND,1) =   BLOCKMAP_INF 
        layerDimensions(LAYER_FOREGROUND,2) =   BLOCKMAP_FIX

        layerDimensions(LAYER_INTERFACE ,1) =   BLOCKMAP_INF         
        layerDimensions(LAYER_INTERFACE ,2) =   BLOCKMAP_FIX     

        layerDimensions(LAYER_TRANSITION,1) =   BLOCKMAP_INF         
        layerDimensions(LAYER_TRANSITION,2) =   BLOCKMAP_FIX  

    end subroutine

    subroutine deAllocBlockMap(n)
        integer(1) :: rc, n
 
        deallocate(layerBlocks(n)%spriteList, stat = rc)
        if (rc /= 0) call displayDebug("Failed to deallocate spriteList!") 

        deallocate(layerBlocks(n)%pozList, stat = rc)
        if (rc /= 0) call displayDebug("Failed to deallocate pozList!") 

        layerBlocks(n)%nextIndexS = 0
        layerBlocks(n)%nextIndexP = 0

    end subroutine

    subroutine putSpritesOnBuffer(mapEditorActive)
        integer(1) :: n
        integer(4) :: ind, sInd
        integer(2) :: x, y
        !character(40)       :: test
        logical    :: mapEditorActive
        
        call eraseBuff()

        do n = 1, size(layerBlocks), 1

           do ind = 1, layerBlocks(n)%nextIndexP, 1 
              if (mapEditorActive .EQV. .FALSE.) then  
                  if (layerBlocks(n)%pozList(ind)%typFlag == TYPE_PLAYER_SPAWNER) cycle  
              end if

              sInd = layerBlocks(n)%pozList(ind)%ind  

              call layerBlocks(n)%spriteList(sind)%drawImage(ind)  
           end do 
        end do

    end subroutine

    subroutine reorderPoz(n)
         type(spritePoz), dimension(:), allocatable :: temp   
 
         integer                      :: from, to, sInd, smallest, smallestYh, smallestInd, last 
         integer(1)                   :: rc, filter    
         integer(1)                   :: n, nCopy
         !character(40)                :: test    
         integer(4)                   :: yhf 

         type(spritePoz)              :: tempPoz   

         nCopy = n

         allocate(temp(layerBlocks(n)%nextIndexP), stat = rc)
         if (rc /= 0) call displayDebug("Failed to allocate temp SpritePoz list!")

         to = 0
         do from = 1, layerBlocks(n)%nextIndexP, 1
            sInd = layerBlocks(n)%pozList(from)%ind  

            if ((layerBlocks(n)%spriteList(sind)%active   .EQV. .TRUE.)  .AND. &
     (associated(layerBlocks(n)%spriteList(sind)%imageF)  .EQV. .TRUE.)  .AND. &
                (layerBlocks(n)%pozList(from)%dontCountMe .EQV. .FALSE.)) then
                 to       = to + 1   
                 temp(to) = layerBlocks(n)%pozList(from)   
                 last     = to
            end if
         end do
 
  
         layerBlocks(n)%nextIndexP = to
         call move_alloc(temp, layerBlocks(nCopy)%pozList)  

         last        = 0
         do to = 1, layerBlocks(nCopy)%nextIndexP-1, 1
             smallest    = 0
             smallestYh  = 2147483647
             smallestInd = 2147483647

             do from = to, layerBlocks(nCopy)%nextIndexP, 1
                yhf = layerBlocks(nCopy)%pozList(from)%yh 
              !
              ! Decorations should never go behind the floor texture.
              !   
                if (layerBlocks(nCopy)%pozList(from)%typFlag == TYPE_FLOOR_DECORATION) yhf = yhf + 1   

                if ((yhf  < smallestYh)                               .OR. &
                    (yhf == smallestYh                               .AND. &
                     layerBlocks(nCopy)%pozList(from)%ind < smallestInd)) then
                     smallest    = from
                     smallestYh  = yhf
                     smallestInd = layerBlocks(nCopy)%pozList(from)%ind 
                end if
            end do

            if (smallest == 0) exit   

            tempPoz                              = layerBlocks(nCopy)%pozList(smallest)
            layerBlocks(nCopy)%pozList(smallest) = layerBlocks(nCopy)%pozList(to)
            layerBlocks(nCopy)%pozList(to)       = tempPoz

             !write(test, "(A, ' ', I0, ' ', I0)") & 
             !      layerBlocks(n)%pozList(smallest)%name, smallestYh, smallestInd 
             !call displayDebug(test)

         end do

    end subroutine

    function checkIfXYOccupied(b, x, y, typ) result(r)
        integer(1)              :: b, typ
        integer(4)              :: x, y
        integer(8)              :: ind
        logical                 :: r

        r = .FALSE.

        do ind = 1, layerBlocks(b)%nextIndexS, 1
           if (layerBlocks(b)%pozList(ind)%x == x .AND. layerBlocks(b)%pozList(ind)%y == y) then
               if (typ == 0 .OR. typ == layerBlocks(b)%pozList(ind)%typFlag) then 
                   r = .TRUE.
                   return  
               end if 
           end if
        end do

    end function 



END MODULE sprite7up
