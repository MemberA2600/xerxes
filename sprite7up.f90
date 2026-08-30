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
                                     createSpriteObjBackGround    

    type SpriteObj 
         integer(2)               :: w, h, spriteI
         type(imageFile), pointer :: imageF       
         integer(1)               :: filter, bufferNum  
         integer(4)               :: ind
         logical                  :: solid, active

         contains 
         procedure                :: drawImage => drawImage 

    end type

    type spritePoz
         character(NAME_MAX_LEN)  :: name
         integer(4)               :: typFlag
         integer(4)               :: x, y, yh, ind      
    end type 

    type BlockMap
        type(SpriteObj), dimension(:), allocatable :: spriteList
        type(spritePoz), dimension(:), allocatable :: pozList
        integer(4)                                 :: nextIndexS, nextIndexP
    end type

    type(BlockMap), dimension(layerNum)         :: layerBlocks

    integer(2), dimension(layerNum, 2)          :: layerDimensions

    integer(4)                                  :: XOffset, YOffset, wSize, hSize

    integer(1), parameter                       :: BLOCKMAP_1     = 0, &
                                                   BLOCKMAP_INF   = 1, & 
                                                   BLOCKMAP_FIX   = 0, &
                                                   BLOCKMAP_EXP   = 1                                                                                             

    integer(1), parameter                       :: SIZE_INIT      = 64, &
                                                   SIZE_ADD       = 32
    !       
    !   SpriteObj Things
    !
    contains

    subroutine drawImage(this)
        class(SpriteObj)        :: this
        integer(4)              :: x, y
        character(40)           :: test

        if ((associated(this%imageF) .EQV. .FALSE.)  .OR. &
            (this%active             .EQV. .FALSE.)) return

        if (layerDimensions(this%bufferNum,1) /= BLOCKMAP_1) then       
            x = layerBlocks(this%bufferNum)%pozList(this%ind)%x
            y = layerBlocks(this%bufferNum)%pozList(this%ind)%y

            if (layerDimensions(this%bufferNum,2) /= BLOCKMAP_FIX) then
                if ((x + this%w - 1 < XOffset) .OR. (x > XOffset + wSize)  .OR. & 
                    (y + this%h - 1 < YOffset) .OR. (Y > YOffset + hSize)) return
        
                x = x - XOffset 
                y = y - YOffset 
            end if

            call this%imageF%addToScreenBuffer(this%spriteI, this%bufferNum, &
                                               x, y, this%filter)
        else
            do y = layerBlocks(this%bufferNum)%pozList(this%ind)%y -   & 
                   layerBlocks(this%bufferNum)%spriteList(this%ind)%h, &
                   hSize, layerBlocks(this%bufferNum)%spriteList(this%ind)%h 
                do x = layerBlocks(this%bufferNum)%pozList(this%ind)%x -   & 
                       layerBlocks(this%bufferNum)%spriteList(this%ind)%w, &
                       wSize, layerBlocks(this%bufferNum)%spriteList(this%ind)%w 
    
                   !write(test, "(I0, ' ', I0)") x, y 
                   !call DisplayDebug(test) 

                   call this%imageF%addToScreenBuffer(this%spriteI, this%bufferNum, &
                                                      x, y, this%filter)
                end do
            end do

        end if

        if (stupidTimerEnded() .EQV. .TRUE. ) then   
            if (this%spriteI >= this%imageF%img%numOfFrames) then
                this%spriteI = 1
            else
                this%spriteI = this%spriteI + 1
            end if
        end if
    

    end subroutine

    !
    !   BlockMap Stuff
    !

    subroutine createSpriteObjPlayGround(spriteName, imageName, x, y, typFlag, solid, filter)
         character(*)  :: imageName, spriteName   
         integer(4)    :: x, y
         integer(1)    :: filter
         integer(4)    :: typFlag
         logical       :: solid

         call createSpriteObj(spriteName, imageName, LAYER_PLAYGROUND, x, y, typFlag, solid, filter)

    end subroutine 

    subroutine createSpriteObjBackGround(spriteName, imageName, filter)
         character(*)  :: imageName, spriteName   
         integer(1)    :: filter

         call createSpriteObj(spriteName, imageName, LAYER_BACKGROUND, 1, 1, TYPE_EMPTY, .FALSE., filter)

    end subroutine 

    subroutine createSpriteObj(spriteName, imageName, bufferNum, x, y, typFlag, solid, filter)
         character(*)  :: imageName, spriteName   
         integer(4)    :: x, y
         integer(1)    :: filter, bufferNum  
         integer(4)    :: typFlag
         logical       :: solid
         integer(1)    :: rc

         type(SpriteObj), dimension(:), allocatable :: spriteListTemp
         type(spritePoz), dimension(:), allocatable :: pozListTemp

         if (layerDimensions(bufferNum,1) /= BLOCKMAP_1) then
   
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

         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%name    = spriteName
         layerBlocks(bufferNum)%pozList(layerBlocks(bufferNum)%nextIndexP)%typFlag = typFlag

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
                if (rc /= 0) call displayDebug("Failed to allocate spriteList!") 
           end select 
        end do 

    end subroutine

    subroutine setDimensions()

        layerDimensions(LAYER_BACKGROUND,1) =   BLOCKMAP_1
        layerDimensions(LAYER_BACKGROUND,2) =   BLOCKMAP_FIX

        layerDimensions(LAYER_PLAYGROUND,1) =   BLOCKMAP_INF       
        layerDimensions(LAYER_PLAYGROUND,2) =   BLOCKMAP_EXP     
   
        layerDimensions(LAYER_SKY       ,1) =   BLOCKMAP_INF         
        layerDimensions(LAYER_SKY       ,2) =   BLOCKMAP_EXP     

        layerDimensions(LAYER_WEATHER   ,1) =   BLOCKMAP_1        
        layerDimensions(LAYER_WEATHER   ,2) =   BLOCKMAP_FIX

        layerDimensions(LAYER_INTERFACE ,1) =   BLOCKMAP_INF         
        layerDimensions(LAYER_INTERFACE ,2) =   BLOCKMAP_FIX     

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
         type(spriteObj), dimension(:), allocatable :: temo    
 
         integer                      :: from, to, sInd, smallest, smallestYh, smallestInd, last 
         integer(1)                   :: rc   
         integer(1)                   :: n
    
         allocate(temp(layerBlocks(n)%nextIndexP), stat = rc)
         if (rc /= 0) call displayDebug("Failed to allocate temp SpritePoz list!")

         allocate(temo(layerBlocks(n)%nextIndexS), stat = rc)
         if (rc /= 0) call displayDebug("Failed to allocate temp SpriteObj list!")

         smallest    = 0
         last        = 0
         smallestYh  = 2147483647
         smallestInd = 2147483647

         do to = 1, layerBlocks(n)%nextIndexP-1, 1
             do from = to, layerBlocks(n)%nextIndexP, 1
                sInd = layerBlocks(n)%pozList(from)%ind  
                if (layerBlocks(n)%spriteList(sind)%active .EQV. .TRUE.)  then
                    if ((layerBlocks(n)%pozList(from)%yh  < smallestYh)    .OR. &
                        (layerBlocks(n)%pozList(from)%yh == smallestYh    .AND. &
                         layerBlocks(n)%pozList(from)%ind < smallestInd)) then
                          smallest    = from
                          smallestYh  = layerBlocks(n)%pozList(from)%yh
                          smallestInd = layerBlocks(n)%pozList(from)%ind 
                    end if
                end if
             end do 

             if (smallest == 0) exit   
             temp(to) = layerBlocks(n)%pozList(from)   
   
             last     = to
         end do

         layerBlocks(n)%nextIndexP = last
         call move_alloc(temp, layerBlocks(n)%pozList)

         to = 0
         do from = 1, layerBlocks(n)%nextIndexS, 1
            if (layerBlocks(n)%spriteList(from)%active .EQV. .TRUE.) then              
                to       = to + 1
                temo(to) = layerBlocks(n)%spriteList(from)
            else
                nullify(layerBlocks(n)%spriteList(from)%imageF)        
            end if
         end do 

         layerBlocks(n)%nextIndexS = to
         call move_alloc(temo, layerBlocks(n)%spriteList)         

    end subroutine

END MODULE sprite7up
