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

    implicit None

    private    
    public                        :: initBlockMaps, putSpritesOnBuffer, createSpriteObjPlayGround, &
                                     createSpriteObjBackGround, setOffset, addToOffset, &
                                     createSpriteObjSky       

    type SpriteObj 
         integer(2)               :: w, h, spriteI
         type(imageFile), pointer :: imageF       
         integer(1)               :: filter, bufferNum  
         integer(4)               :: ind
         logical                  :: solid, active
         type(counterTimer)       :: timer

         contains 
         procedure                :: drawImage    => drawImage 
         procedure                :: drawDrawDraw => drawDrawDraw
    end type

    type spritePoz
         character(NAME_MAX_LEN)  :: name
         integer(4)               :: typFlag
         integer(4)               :: x, y, yh, ind 
         integer(2)               :: fly     
    end type 

    type BlockMap
        type(SpriteObj), dimension(:), allocatable :: spriteList
        type(spritePoz), dimension(:), allocatable :: pozList
        integer(4)                                 :: nextIndexS, nextIndexP
    end type

    type(BlockMap), dimension(layerNum)         :: layerBlocks

    integer(2), dimension(layerNum, 3)          :: layerDimensions

    integer(4)                                  :: XOffset, YOffset, wSize, hSize

    integer(1), parameter                       :: BLOCKMAP_1     = 0, &
                                                   BLOCKMAP_INF   = 1, & 
                                                   BLOCKMAP_REAL  = 0, &
                                                   BLOCKMAP_DUMMY = 1, & 
                                                   BLOCKMAP_FIX   = 0, &
                                                   BLOCKMAP_EXP   = 1                                                

    integer(1), parameter                       :: SIZE_INIT      = 64, &
                                                   SIZE_ADD       = 32
    !       
    !   SpriteObj Things
    !
    contains

    subroutine drawDrawDraw(this, x, y, fly)
        class(SpriteObj)        :: this
        integer(4)              :: x, y
        integer(2)              :: fly
    
        if (this%bufferNum == 2) then
            if (fly > 0) then
                call this%imageF%addToScreenBuffer(this%spriteI, this%bufferNum-1, &
                                                   x, y, FILTER_SHADOW)
    
                call this%imageF%addToScreenBuffer(this%spriteI, this%bufferNum, &
                                                   x, y - fly, this%filter)                
            else
                call this%imageF%addToScreenBuffer(this%spriteI, this%bufferNum, &
                                                   x, y, this%filter)
            end if
        else
            call this%imageF%addToScreenBuffer(this%spriteI, this%bufferNum, &
                                               x, y, this%filter)
        end if
    end subroutine

    function getPozInd(i, b) result(res)
        integer(4)              :: i
        integer(1)              :: b
        integer(4)              :: ind, res

        res = 0

        do ind = 1, layerBlocks(b)%nextIndexP, 1
           if (layerBlocks(b)%pozList(ind)%ind == i) then 
                res = ind 
                exit
           end if 
        end do

    end function

    subroutine drawImage(this)
        class(SpriteObj)        :: this
        integer(4)              :: x, y, pozInd
        character(40)           :: test
        integer(2)              :: fly

        if ((associated(this%imageF) .EQV. .FALSE.)  .OR. &
            (this%active             .EQV. .FALSE.)) return

        !write(test, "(A, ' ', I0, ' ', I0)") trim(layerBlocks(this%bufferNum)%pozList(pozInd)%name), x, y 
        !call DisplayDebug(test) 

        pozInd = getPozInd(this%ind, this%bufferNum)
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


            call this%drawDrawDraw(x, y, fly)
    
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
                                         , 0)

                end do
            end do

        end if

        if (this%imageF%img%numOfFrames > 1) then
            if (this%timer%timerEnded() .EQV. .TRUE. ) then             
                this%spriteI = this%spriteI + 1

                if (this%spriteI >= this%imageF%img%numOfFrames) then
                    this%spriteI = 1
                end if
                
                call this%timer%timerStart(PERFECT_WAIT * getSpeed())
            end if

        else
            this%spriteI = 1
        end if  
    

    end subroutine

    !
    !   BlockMap Stuff
    !

    subroutine offSetCorr()

        if (xOffset < 0) xOffset = 0
        if (yOffset < 0) yOffset = 0

        if (xOffset > wSize - wOfScreenBuffer) xOffset = wSize - wOfScreenBuffer
        if (yOffset > hSize - hOfScreenBuffer) yOffset = hSize - hOfScreenBuffer

    end subroutine

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

    subroutine createSpriteObjSky(spriteName, imageName, x, y, typFlag, solid, filter, fly)
         character(*)  :: imageName, spriteName   
         integer(4)    :: x, y
         integer(1)    :: filter
         integer(4)    :: typFlag
         logical       :: solid
         integer(2)    :: fly
        
      !
      !  Sky units are basically ground units. If fly = 0, the main unit is the creature, but if it flies,
      !  the shadow becomes the main unit and the creature is just drawn on the SKY layer.
      !

         call createSpriteObj(spriteName, imageName, LAYER_PLAYGROUND, x, y, typFlag, solid, filter, fly)

    end subroutine 

    subroutine createSpriteObjPlayGround(spriteName, imageName, x, y, typFlag, solid, filter)
         character(*)  :: imageName, spriteName   
         integer(4)    :: x, y
         integer(1)    :: filter
         integer(4)    :: typFlag
         logical       :: solid

         call createSpriteObj(spriteName, imageName, LAYER_PLAYGROUND, x, y, typFlag, solid, filter, 0)

    end subroutine 

    subroutine createSpriteObjBackGround(spriteName, imageName, filter)
         character(*)  :: imageName, spriteName   
         integer(1)    :: filter

         call createSpriteObj(spriteName, imageName, LAYER_BACKGROUND, 1, 1, TYPE_EMPTY, .FALSE., filter, 0)

    end subroutine 

    

    subroutine createSpriteObj(spriteName, imageName, bufferNum, x, y, typFlag, solid, filter, fly)
         character(*)  :: imageName, spriteName   
         integer(4)    :: x, y
         integer(1)    :: filter, bufferNum  
         integer(4)    :: typFlag
         logical       :: solid
         integer(1)    :: rc
         integer(2)    :: fly

         type(SpriteObj), dimension(:), allocatable :: spriteListTemp
         type(spritePoz), dimension(:), allocatable :: pozListTemp

         integer(4)    :: ind
         character(50) :: test

         if (layerDimensions(bufferNum,1) /= BLOCKMAP_1) then
             do ind = 1, layerBlocks(bufferNum)%nextIndexS, 1
                if ((layerBlocks(bufferNum)%spriteList(ind)%active .EQV. .FALSE.) .OR. &
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
             end if     
     
             if (layerBlocks(bufferNum)%nextIndexP == size(layerBlocks(bufferNum)%pozList)) then
    
                 allocate(pozListTemp(layerBlocks(bufferNum)%nextIndexP + SIZE_ADD), stat = RC)
                 if (rc /= 0) call displayDebug("Failed to allocate temporal pozList!")
    
                 pozListTemp(1:layerBlocks(bufferNum)%nextIndexP) = layerBlocks(bufferNum)%pozList
    
                 call move_alloc(pozListTemp, layerBlocks(bufferNum)%pozList)
             end if         
            
             layerBlocks(bufferNum)%nextIndexS = & 
             layerBlocks(bufferNum)%nextIndexS + 1            
               
             layerBlocks(bufferNum)%nextIndexP = & 
             layerBlocks(bufferNum)%nextIndexP + 1   
         else
             layerBlocks(bufferNum)%nextIndexS = 1            
             layerBlocks(bufferNum)%nextIndexP = 1   
         end if  

         call assignSpriteToPointer(imageName, &
              layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%imageF) 

         layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%w = &
         layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%imageF%img%width    

         layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%h = &
         layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%imageF%img%height  

         layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%spriteI   = 1
         layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%filter    = filter
         layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%bufferNum = bufferNum   
         layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%solid     = solid
         layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%active    = .TRUE.

         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%y   = y
         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%yh  = y + &
         layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%h

         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%x   = x
         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%ind = &
         layerBlocks(bufferNum)%nextIndexS

         layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%ind = &
         layerBlocks(bufferNum)%nextIndexS

         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%fly     = fly
         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%name    = spriteName
         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%typFlag = typFlag

         if (layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%imageF%img%numOfFrames > 1) then
             call layerBlocks(bufferNum)%spriteList(layerBlocks(bufferNum)%nextIndexS)%timer%timerStart( &
                  PERFECT_WAIT * getSpeed())
         end if

         !write(test, "(A, ' ', I0, ' ', I0, ' ', I0, ' ', I0)") &
         !      trim(spriteName), x, y, bufferNum ,layerBlocks(bufferNum)%nextIndexS   
         !call displayDebug(test)

    end subroutine

    subroutine initBlockMaps(n, w, h)
        integer(1)              :: n, ind, rc
        character(40)           :: test
        integer(2)              :: w, h

        call setDimensions()
        xOffset   = 0
        yOffset   = 0
        wSize     = w
        hSize     = h

        do ind = 1, n, 1

           if (allocated(layerBlocks(ind)%spriteList)) call deAllocBlockMap(ind)
    
           layerBlocks(ind)%nextIndexS = 0
           layerBlocks(ind)%nextIndexP = 0

           if (layerDimensions(ind,3) /= BLOCKMAP_DUMMY) then
               select case(layerDimensions(ind,1))
               case(BLOCKMAP_1)  
                    allocate(layerBlocks(ind)%spriteList(1), stat = rc)
                    if (rc /= 0) call displayDebug("Failed to allocate spriteList!") 
    
                    allocate(layerBlocks(ind)%pozList   (1), stat = rc)
                    if (rc /= 0) call displayDebug("Failed to allocate pozList!") 
    
               case(BLOCKMAP_INF) 
                    allocate(layerBlocks(ind)%spriteList(SIZE_INIT), stat = rc)
                    if (rc /= 0) call displayDebug("Failed to allocate spriteList!") 
    
                    allocate(layerBlocks(ind)%pozList   (SIZE_INIT), stat = rc)
                    if (rc /= 0) call displayDebug("Failed to allocate pozList!") 
               end select 
            end if
        end do 

    end subroutine

    subroutine setDimensions()

        layerDimensions(LAYER_BACKGROUND,1) =   BLOCKMAP_1
        layerDimensions(LAYER_BACKGROUND,2) =   BLOCKMAP_FIX
        layerDimensions(LAYER_BACKGROUND,3) =   BLOCKMAP_REAL

        layerDimensions(LAYER_PLAYGROUND,1) =   BLOCKMAP_INF       
        layerDimensions(LAYER_PLAYGROUND,2) =   BLOCKMAP_EXP     
        layerDimensions(LAYER_PLAYGROUND,3) =   BLOCKMAP_REAL    

        layerDimensions(LAYER_SKY       ,1) =   BLOCKMAP_INF         
        layerDimensions(LAYER_SKY       ,2) =   BLOCKMAP_EXP     
        layerDimensions(LAYER_SKY       ,3) =   BLOCKMAP_DUMMY     

        layerDimensions(LAYER_WEATHER   ,1) =   BLOCKMAP_1        
        layerDimensions(LAYER_WEATHER   ,2) =   BLOCKMAP_FIX
        layerDimensions(LAYER_WEATHER   ,3) =   BLOCKMAP_REAL    

        layerDimensions(LAYER_FOREGROUND,1) =   BLOCKMAP_1
        layerDimensions(LAYER_FOREGROUND,2) =   BLOCKMAP_FIX
        layerDimensions(LAYER_FOREGROUND,3) =   BLOCKMAP_REAL    

        layerDimensions(LAYER_INTERFACE ,1) =   BLOCKMAP_INF         
        layerDimensions(LAYER_INTERFACE ,2) =   BLOCKMAP_FIX     
        layerDimensions(LAYER_INTERFACE ,3) =   BLOCKMAP_REAL    

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

    subroutine putSpritesOnBuffer()
        integer(1) :: ind, n, sInd
        integer(2) :: x, y

        call eraseBuff()

        do n = 1, size(layerBlocks), 1
           if (layerBlocks(n)%nextIndexP > 1) call reorderPoz(n)

           do ind = 1, layerBlocks(n)%nextIndexP, 1 
              sInd = layerBlocks(n)%pozList(ind)%ind  
              call layerBlocks(n)%spriteList(sind)%drawImage()  
           end do 
        end do

    end subroutine

    subroutine reorderPoz(n)
         type(spritePoz), dimension(:), allocatable :: temp   
 
         integer                      :: from, to, sInd, smallest, smallestYh, smallestInd, last 
         integer(1)                   :: rc   
         integer(1)                   :: n
         !character(40)                :: test    
         integer(4)                   :: yhf 

         type(spritePoz)              :: tempPoz   


         allocate(temp(layerBlocks(n)%nextIndexP), stat = rc)
         if (rc /= 0) call displayDebug("Failed to allocate temp SpritePoz list!")

         to = 0
         do from = 1, layerBlocks(n)%nextIndexP, 1
            sInd = layerBlocks(n)%pozList(from)%ind  

            if ((layerBlocks(n)%spriteList(sind)%active  .EQV. .TRUE.)  .AND. &
     (associated(layerBlocks(n)%spriteList(sind)%imageF) .EQV. .TRUE.)) then
                 to       = to + 1   
                 temp(to) = layerBlocks(n)%pozList(from)   
                 last     = to
            end if
         end do
   
         layerBlocks(n)%nextIndexP = last
         call move_alloc(temp, layerBlocks(n)%pozList)  

         last        = 0
         do to = 1, layerBlocks(n)%nextIndexP-1, 1
             smallest    = 0
             smallestYh  = 2147483647
             smallestInd = 2147483647

             do from = to, layerBlocks(n)%nextIndexP, 1
                yhf = layerBlocks(n)%pozList(from)%yh - & 
                      merge(1, 0, layerBlocks(n)%pozList(from)%fly > 0)

                if ((yhf  < smallestYh)                               .OR. &
                    (yhf == smallestYh                               .AND. &
                     layerBlocks(n)%pozList(from)%ind < smallestInd)) then
                     smallest    = from
                     smallestYh  = yhf
                     smallestInd = layerBlocks(n)%pozList(from)%ind 
                end if
            end do

            if (smallest == 0) exit   

            tempPoz                          = layerBlocks(n)%pozList(smallest)
            layerBlocks(n)%pozList(smallest) = layerBlocks(n)%pozList(to)
            layerBlocks(n)%pozList(to)       = tempPoz

             !write(test, "(A, ' ', I0, ' ', I0)") & 
             !      layerBlocks(n)%pozList(smallest)%name, smallestYh, smallestInd 
             !call displayDebug(test)

         end do

    end subroutine

END MODULE sprite7up
