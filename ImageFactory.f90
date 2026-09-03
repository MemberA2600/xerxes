MODULE ImageFactory

    USE debugWindow
    USE dataLoader
    USE WINTERACTER
    USE RESID
    USE subs
    USE engineConstants
    USE winapis
    use IFPORT
    use screen
    use colors
    USE inputReader
    USE KERNEL32, WinSleep => Sleep
    USE dict

    implicit none

    private
    public                  :: bitMapWindow, checkImageWindowFields, dropImageList, dropAllImages, &
                               initImageList, loadImageHeader, loadImageByName, addToSCRBuffByName, &
                               imageFile, assignSpriteToPointer, setSpeedScreen, testSpeedLoop

    !
    !   Images are pretty complex and compact.
    !   The beginning is typical: 
    !   'IMG ', lenght of name (1 byte), name.
    !
    !   Size of screen, each two bytes 
    !   (max: wOfScreenBuffer and hOfScreenBuffer)
    !   
    !   transpColor: This color is used for transparency, so should be a color that is
    !                not present on the picture. If 1, since black cannot be transparent,
    !                it means the picture is not transparent. By deafult, it's the top-left pixel
    !                of the first frame.
    !
    !   The number of frames. One byte, n+1 is the number, so it can go up to 256.    
    !
    !   The comes the actual picture data.
    !

    type imageData
         integer(2)                                :: numOfFrames, transpColor, &
                                                      width, height
         integer(2), dimension(:,:,:), allocatable :: frames

    end type

    type imageFile
         character(4)                              :: header = IMG_FILE_TYPE
         character(NAME_MAX_LEN)                   :: name 
         integer(1)                                :: nameLen
         character(MAX_PATH_LEN)                   :: fileName 
         type(imageData), allocatable              :: img

         contains

         procedure                                 :: dropImage         => dropImage      
         procedure                                 :: addToScreenBuffer => addToScreenBuffer 
         procedure                                 :: loadImage         => loadImage         
    end type

    type(imageFile)                                :: imageLoader
    logical                                        :: canKill = .FALSE., pickerActive = .FALSE., &
                                                      justACancel, pleaseStop = .TRUE.  

    type(imageFile), dimension(:), &
                           allocatable, target     :: imageList
    type(CounterTimer)                             :: counttimer

    integer                                        :: testIndex    

    contains

    subroutine assignSpriteToPointer(n, p)
        character(*)                            :: n
        type(imageFile), pointer, intent(inout) :: p
        integer                                 :: ind, foundInd 
        character(40)                           :: test        

        foundInd = 0
        nullify(p)

         do ind = 1, size(imageList), 1

            !write(test, "(A, '|', I0, '|', A, '|',I0)") &
            !      trim(imageList(ind)%name), len_trim(imageList(ind)%name), trim(n), len_trim(n)  

            !call displayDebug(test)

            if (imageList(ind)%name == n) then
                p => imageList(ind)
                foundInd = ind
                exit
            end if
         end do
    
         if (foundInd == 0) then 
            call displayDebug("Image " // trim(n) // " not found!")
         else   
            if (allocated(imageList(foundInd)%img) .EQV. .FALSE.) then
                call imageList(foundInd)%loadImage()
            end if              
         end if   

    end subroutine

    subroutine addToSCRBuffByName(n, frameNum, bufferNum, x, y, filter)
         integer(2)                      :: frameNum, bufferNum, x, y, filter
         integer                         :: ind   
         character(*)                    :: n   

         do ind = 1, size(imageList), 1
            if (imageList(ind)%name == n) then
                call imageList(ind)%addToScreenBuffer(frameNum, bufferNum, x, y, filter)
                exit
            end if
         end do

    end subroutine

    subroutine loadImageByName(n)
         integer                                  :: ind   
         character(*)                             :: n   

         do ind = 1, size(imageList), 1
            if (imageList(ind)%name == n) then
                call imageList(ind)%loadImage()
                exit
            end if
         end do

    end subroutine

    subroutine initImageList(n)
        integer                                   :: n 
        integer(1)                                :: rc

        allocate(imageList(n), stat = rc) 
        if (rc /= 0) call displayDebug("Failed to allocate imageList!")   

    end subroutine

    subroutine dropImageList()
       integer(1)                                 :: rc

       if (allocated(imageList)) then
           call dropAllImages() 

           deallocate(imageList, stat = rc) 
           if (rc /= 0) call displayDebug("Failed to deallocate imageList!")     

       end if 

    end subroutine

    subroutine dropAllImages()
        integer                                    :: ind
        integer(1)                                 :: rc

        if (allocated(imageList)) then
            do ind = 1, size(imageList), 1
               if (allocated(imageList(ind)%img)) then 
                   if (allocated(imageList(ind)%img%frames)) then
                       deallocate(imageList(ind)%img%frames, stat = rc) 
                       if (rc /= 0) call displayDebug("Failed to deallocate image frames of imageFile!")
                   end if  
                   deallocate(imageList(ind)%img, stat = rc) 
                   if (rc /= 0) call displayDebug("Failed to deallocate image of imageFile!") 
               end if 
            end do
        end if

    end subroutine
    
    function loadBMP() result(r)
         character(MAX_PATH_LEN)               :: fname, newFname
         integer(2)                            :: numOfFrames, dotPoz, ind, rc
         logical                               :: ex, r       

         r = .FALSE.

         pleaseStop = .TRUE.
         fname = FileDialog("", .FALSE., "bmp ")
         if (fname == "") then
             pleaseStop = (allocated(imageLoader%img) .EQV. .FALSE.)
             return
         end if

         call imageLoader%dropImage()            
         call eraseBuff()   

         do ind = len_trim(fname), 1, -1
            if (fname(ind:ind) == ".") then
                dotPoz = ind
                exit    
            end if
         end do       

         numOfFrames = 1

         if (fname(dotPoz - 3 : dotPoz - 1) == "000") then
             do ind = 1, 255, 1
                newFname = insertNum(fname, dotPoz - 3, ind)

                inquire(file=newFname, exist = ex)
                if (ex .EQV. .FALSE.) exit
                numOfFrames = numOfFrames + 1
             end do

         end if

        allocate(imageLoader%img, stat = rc) 
        if (rc /= 0) call displayDebug("Failed to allocate image data for tester!")

        imageLoader%img%numOfFrames = numOfFrames 

        call extractBMP(fname, 1)

        if (imageLoader%img%width > wOfScreenBuffer .OR. imageLoader%img%height > hOfScreenBuffer) then
            imageLoader%img%transpColor = 1
        else  
            imageLoader%img%transpColor = imageLoader%img%frames(1, 1, 1)  
        end if
   
        if (numOfFrames > 1) then
             do ind = 1, 255, 1
                newFname = insertNum(fname, dotPoz - 3, ind)

                inquire(file=newFname, exist = ex)
                if (ex .EQV. .FALSE.) exit
                call extractBMP(newFname, ind + 1)

             end do
        end if 
        r = .TRUE.
        pleaseStop = .FALSE.

    end function

    subroutine addToScreenBuffer(this, frameNum, bufferNum, x, y, filter)
        class(imageFile), intent(inout) :: this
        integer(2)                      :: frameNum, bufferNum, x, y, filter
        integer(2)                      :: xPix, yPix, color, xOnBuff, yOnBuff
        integer(2), dimension(3)        :: rgb        
        !character(40)                   :: test

        do yPix = 1, this%img%height, 1
           do xPix = 1, this%img%width, 1

              color = this%img%frames(frameNum, xPix, yPix)  

              xOnBuff = xPix + x - 1 
              yOnBuff = yPix + y - 1  
            
              if (xOnBuff > wOfScreenBuffer .OR. yOnBuff > hOfScreenBuffer .OR. & 
                  xOnBuff < 1               .OR. yOnBuff < 1) cycle    

              !write(test, "(I0, ' | ', I0)") this%img%transpColor, color
              !call displayDebug(test)  
              if (pickerActive .EQV. .FALSE.) then  
                  if (this%img%transpColor > 1 .AND. this%img%transpColor == color) then
                      !if (xOnBuff <= wOfScreenBuffer .AND. yOnBuff <= hOfScreenBuffer) then   
                      call setBufferPixel(bufferNum, xOnBuff, yOnBuff, -1)
                      cycle   
                      !end if
                  end if
              end if  

              select case(filter)
              case(FILTER_RAINBOW)
                   color = color + getUpTo256()
                   if (color > numOfColors) color = color - numOfColors
                    
              case(FILTER_RED)
                   color = changeRGB(color, 1, -1, -1)

              case(FILTER_RED1)
                   color = changeRGB(color, 2, -2, -2)

              case(FILTER_RED2)
                   color = changeRGB(color, 3, -3, -3)

              case(FILTER_BLUE)
                   color = changeRGB(color, -1, -1, 1)

              case(FILTER_BLUE1)
                   color = changeRGB(color, -2, -2, 2)

              case(FILTER_BLUE2)
                   color = changeRGB(color, -3, -3, 3)

              case(FILTER_GREEN)
                   color = changeRGB(color, -1, 1, -1)

              case(FILTER_GREEN1)
                   color = changeRGB(color, -2, 2, -2)

              case(FILTER_GREEN2)
                   color = changeRGB(color, -3, 3, -3)

              case(FILTER_YELLOW)
                   color = changeRGB(color,  1, 1, -1)

              case(FILTER_YELLOW1)
                   color = changeRGB(color,  2, 2, -2)

              case(FILTER_YELLOW2)
                   color = changeRGB(color,  3, 3, -3)

              case(FILTER_DARK)
                   color = changeRGB(color,  -1, -1, -1)

              case(FILTER_DARK1)
                   color = changeRGB(color,  -2, -2, -2)

              case(FILTER_DARK2)
                   color = changeRGB(color,  -3, -3, -3)

              case(FILTER_LIGHT)
                   color = changeRGB(color,   1,  1,  1)

              case(FILTER_LIGHT1)
                   color = changeRGB(color,   2,  2,  2)

              case(FILTER_LIGHT2)
                   color = changeRGB(color,   3,  3,  3)

              case(FILTER_PINK)
                   color = changeRGB(color,  1, -1, 1)

              case(FILTER_PINK1)
                   color = changeRGB(color,  2, -2, 2)

              case(FILTER_PINK2)
                   color = changeRGB(color,  3, -3, 3)

              case(FILTER_TEAL)
                   color = changeRGB(color,  -1, 1, 1)

              case(FILTER_TEAL1)
                   color = changeRGB(color,  -2, 2, 2)

              case(FILTER_TEAL2)
                   color = changeRGB(color,  -3, 3, 3)

              case(FILTER_SHADOW)
                   if (modulo((xPix + yPix), 2) == 0) then 
                       color = -1 
                   else 
                       color =  1
                   end if 

              case(FILTER_TRANSP)
                   if (modulo((xPix + yPix), 2) == 0) color = -1 

              end select

              !write(test, '(I0)') color
              !call displayDebug(test)  

              !if (xOnBuff <= wOfScreenBuffer .AND. yOnBuff <= hOfScreenBuffer) then   
              call setBufferPixel(bufferNum, xOnBuff, yOnBuff, color)   
              !end if

           end do 
        end do

    end subroutine    

    function changeRGB(ind, redShift, greenShift, blueShift) result(newInd)
        integer(2), intent(in) :: ind
        integer(2), intent(in) :: redShift, greenShift, blueShift
        integer(2)             :: newInd
    
        integer(2) :: n, group
        integer(2) :: redDigit, greenDigit, blueDigit
    
        n = ind - 1
    
        group      =     n / 64
        redDigit   = mod(n / 16, 4)
        greenDigit = mod(n / 4,  4)
        blueDigit  = mod(n,      4)
    
        redDigit   = max(0_2, min(3_2, redDigit   + redShift))
        greenDigit = max(0_2, min(3_2, greenDigit + greenShift))
        blueDigit  = max(0_2, min(3_2, blueDigit  + blueShift))
    
        newInd = group      * 64  + &
                 redDigit   * 16  + &
                 greenDigit * 4   + &
                 blueDigit        + 1
    end function

    subroutine extractBMP(fname, frameNum)
        character(MAX_PATH_LEN)               :: fname
        integer(2)                            :: frameNum
        integer(2)                            :: red, blue, green
        character(2)                          :: BM   
        integer(4)                            :: dataOffset, infoHeaderSize, w, h, bitsPerPixel, &
                                                 compr, wInd, hInd      
        !character(40)                         :: test   
        integer(2), dimension(:), allocatable :: d
        integer(8)                            :: offset, s   
        integer(1)                            :: rc, rowPadding  
        !integer(1)                            :: t

        offset = 1
        call loadBinary(fname, d, s, .FALSE.)        
        call read2CharFromBin(d, s, offset, BM)    

        if (BM /= "BM") then
            call displayDebug("Invalid BMP file!")
        else
            offset = 11
            
            call readIntFromBin(d, s, offset, dataOffset, 4)
            dataOffset = dataOffset + 1

            !write(test, "(Z4.4)") dataOffset
            !call displayDebug(test)

            call readIntFromBin(d, s, offset, infoHeaderSize, 4)
            if (infoHeaderSize /= 40) then 
                call displayDebug("Header type is not for Windows!")
            else
                call readIntFromBin(d, s, offset, w, 4)
                call readIntFromBin(d, s, offset, h, 4)

                !write(test, "(I0, ' | ', I0)") w, h
                !call displayDebug(test)

                if (frameNum == 1) then
                    imageLoader%img%width  = w 
                    imageLoader%img%height = h 
                else
                    if (imageLoader%img%width /= w .OR. imageLoader%img%height /= h) then  
                        call displayDebug("Image W & H is different from frame #1!")
                        return
                    end if
                end if

                offset = offset + 2
                call readIntFromBin(d, s, offset, bitsPerPixel  , 2)

                !write(test, "(I0)") bmpType
                !call displayDebug(test)       

                if (bitsPerPixel /= 24) then
                    call displayDebug("Must be a 24bit bitmap!")
                    return
                end if

                call readIntFromBin(d, s, offset, compr, 4)
                if (compr /= 0) then
                    call displayDebug("24bit bitmaps cannot be compressed!")
                    return
                end if    
                ! 
                ! The others are not required
                !

                offset = dataOffset

                if (frameNum == 1) then
                    allocate(imageLoader%img%frames( &
                             imageLoader%img%numOfFrames, &
                             w, h), stat = rc)
                    if (rc /= 0) call displayDebug("Failed to allocate image frames!")

                end if

                if (allocated(imageLoader%img%frames)) then
                    rowPadding = modulo(4 - modulo(w * 3, 4), 4)

                    do    hInd = h, 1, -1
                       do wInd = 1, w,  1 
                          blue  = d(offset    )
                          green = d(offset + 1)
                          red   = d(offset + 2)
                                                     
                          imageLoader%img%frames(frameNum, wInd, hInd) = &
                          getClosestColor(red, green, blue)    

                          offset = offset + 3  
                        
                       end do

                  ! Skip the padding at the end of the BMP scanline
                    offset = offset + rowPadding  

                    end do
                end if

            end if
        end if

    end subroutine    

    function insertNum(original, start, num) result(new)
         character(MAX_PATH_LEN)               :: original
         integer(2)                            :: start, num
         character(MAX_PATH_LEN)               :: new
         character(3)                          :: numText
         
         write(numText, "(I3.3)") num

         new = original(1 : start - 1) // numText // original(start + 3 : MAX_PATH_LEN )
         !call displayDebug(trim(new))    

    end function 

    subroutine dropImage(this)
         class(imageFile), intent(inout)  :: this
         integer(1)                       :: rc

         if (allocated(this%img)) then
             if (allocated(this%img%frames)) then
                 deallocate(this%img%frames, stat = rc)   
                        
                 if (rc /= 0) then
                     call displayDebug("Failed to deallocate image frames!")
                 end if
             end if
             
             deallocate(this%img, stat = rc)   
                        
             if (rc /= 0) then
                 call displayDebug("Failed to deallocate image data!")      
             end if     
         end if

    end subroutine

    subroutine setSpeedScreen(editMode)
       INTEGER                                 :: ITYPE, oldSpeed
       TYPE(WIN_MESSAGE)                       :: MESSAGE
       LOGICAL                                 :: editMode

       canKill      = .FALSE.   

       do
         if (WInfoDialog(CurrentDialog) == 0) exit
         call sleep(1)
       end do 

       CALL WDialogLoad(IDD_SpeedSetter)
       CALL WDialogTitle(getWordInCurrentLang("speedSettings")) 
       CALL WDialogPutString(ID_SpeedOK, getWordInCurrentLang("ok")) 
       CALL WDialogPutString(ID_Speedcancel, getWordInCurrentLang("cancel")) 


       oldSpeed  = 16 - getSpeed() 
       testIndex = 1
   
       call WDialogPutTrackbar(IDF_SpeedTrk, oldSpeed  )
       call WDialogPutInteger( IDF_SpeedVal, oldSpeed  )

       if (editMode .EQV. .TRUE.) then 
           if (allocated(imageLoader%img)) call imageLoader%dropImage()            

           imageLoader%name     = "Suika"  
           imageLoader%nameLen  = len_trim(imageLoader%name)
           imageLoader%fileName = "suika.xxp"

           call imageLoader%loadImage()

       end if 
        
       call counttimer.timerStart(PERFECT_WAIT * getSpeed()) 
 
       do
          CALL WDialogSelect(IDD_SpeedSetter)
          CALL WDialogShow(ITYPE=Modal)     
    
          if (WinfoDialog(CurrentDialog) == IDD_SpeedSetter) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     call setSpeed(oldSpeed)
                     EXIT
                  CASE(ID_SpeedCancel) 
                     call setSpeed(oldSpeed)
                     EXIT

                  CASE(ID_SpeedOK)
                     EXIT
                  END SELECT
              end if
       end do 

       canKill = .TRUE.        

    end subroutine

    subroutine testSpeedLoop(editMode)
         logical            :: editMode
         integer            :: s, v, t

         call WDialogGetInteger( IDF_SpeedVal, v)
         call WDialogGetTrackbar(IDF_SpeedTrk, t)

         s = 16 - getSpeed()

         if (v /= s) then
            !s = v
            call WDialogPutTrackbar(IDF_SpeedTrk, v)
            call setSpeed(16 - v)
         end if

         if (t /= s) then
            !t = v
            call WDialogPutInteger(IDF_SpeedVal, t)
            call setSpeed(16 - t)
         end if

         if (editMode .EQV. .TRUE.) then
            if (counttimer.timerEnded() .EQV. .TRUE.) then
               testIndex = testIndex + 1 
               call counttimer.timerStart(PERFECT_WAIT * getSpeed())
            end if 

            if (testIndex > imageLoader%img%numOfFrames) testIndex = 1 
            if (testIndex < 1) testIndex = imageLoader%img%numOfFrames 
    
            call imageLoader%addToScreenBuffer(testIndex , 1, 1, 1, NO_FILTER)
         end if   

         CALL setUpTo256()
         CALL buffer2Real() 
    
         if (canKill .EQV. .TRUE.) then 
            if (editMode .EQV. .TRUE.) call eraseBuff() 
            call imageLoader%dropImage()    
            CALL WDialogUnLoad()
            canKill = .FALSE.
        end if


    end subroutine 

    subroutine bitMapWindow()
       INTEGER                                 :: ITYPE, ind, filter
       TYPE(WIN_MESSAGE)                       :: MESSAGE
       !character(10)                  :: msgString
       integer(2)                              :: c 

       canKill      = .FALSE.   
       pickerActive = .FALSE.

       do
         if (WInfoDialog(CurrentDialog) == 0) exit
         call sleep(1)
       end do 

       CALL WDialogLoad(IDD_BMP2XXP)
       if (allocated(imageLoader%img)) call imageLoader%dropImage()            

       call counttimer.timerStart(PERFECT_WAIT * getSpeed()) 

       call onlyRunAfterLoad()
 
       CALL WDialogTitle(getWordInCurrentLang("bmpToXXP")) 
       CALL WDialogPutString(IDF_XXPLABEL1, getWordInCurrentLang("frameIndex")) 
       CALL WDialogPutString(IDF_ANIM, getWordInCurrentLang("animate")) 
       CALL WDialogPutString(IDF_XXPLABEL2, getWordInCurrentLang("transpColor")) 
       CALL WDialogPutString(IDF_TRANS, getWordInCurrentLang("transparent")) 
       CALL WDialogPutString(IDF_ColorPick, getWordInCurrentLang("pickAColor!")) 
       CALL WDialogPutString(IDF_XXPLABEL3, getWordInCurrentLang("testFilter")) 
       CALL WDialogPutString(ID_BMPLoad, getWordInCurrentLang("load")) 
       CALL WDialogPutString(ID_XXPSave, getWordInCurrentLang("save")) 

       justACancel = .FALSE.     
       pleaseStop  = .TRUE.
 
721    do
          CALL WDialogSelect(IDD_BMP2XXP)
          CALL WDialogShow(ITYPE=Modal)     
    
          if (WinfoDialog(CurrentDialog) == IDD_BMP2XXP) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     EXIT
                  CASE(ID_BMPLoad)
                     if (loadBMP() .EQV. .TRUE.) then
                         call setTrans()
                         call onlyRunAfterLoad()
                     end if

                  CASE(ID_XXPSave)
                     call saveBMP2XXP()

                  CASE(IDF_ColorPick)
                     pickerActive = .TRUE.

                     call WDialogGetInteger(IDF_SpriteIndex, ind)
                     call WDialogGetInteger(IDF_FilterInd  , filter)
                     call imageLoader%addToScreenBuffer(ind, 1, 1, 1, filter)

                     call WCursorShape(CurCrossHair)
                     call waitAndDraw()

                     c = pickColorFromScreen()   
                     pickerActive = .FALSE.
                     call WCursorShape(CurArrow)

                     if (c > 0) then 
                         imageLoader%img%transpColor = c 
                         call setTrans()
                         call onlyRunAfterLoad()
                     else
                         justACancel = .TRUE.
                     end if

                  END SELECT
              end if
       end do 

       if (justACancel .EQV. .TRUE.) then   
           justACancel = .FALSE.     
           goto 721 
       end if 

       canKill = .TRUE.        

    END SUBROUTINE

    subroutine waitAndDraw()
       integer                   :: ind, filter

        call WDialogGetInteger(IDF_SpriteIndex, ind)
        call WDialogGetInteger(IDF_FilterInd  , filter)

        call imageLoader%addToScreenBuffer(ind, 1, 1, 1, filter)
        CALL buffer2Real()   

    end subroutine

    subroutine setTrans()
        if (imageLoader%img%transpColor == 1) then
            call WDialogPutCheckBox(IDF_Trans , DISABLED)                    
            call WDialogPutInteger(IDF_TransColor , 2)
            call WDialogPutTrackbar(IDF_TransTrk  , 2)
        else
            call WDialogPutCheckBox(IDF_Trans , ENABLED)                    
            call WDialogPutInteger( IDF_TransColor, imageLoader%img%transpColor)
            call WDialogPutTrackbar(IDF_TransTrk  , imageLoader%img%transpColor)
        end if

    end subroutine

    subroutine onlyRunAfterLoad()
       integer(1)                  :: state, state2 

       if (allocated(imageLoader%img)) then
           state  = ENABLED
           if (imageLoader%img%numOfFrames > 1) then  
               state2 = ENABLED
           else
               state2 = DISABLED 
           end if 
 
       else  
           state  = DISABLED
           state2 = DISABLED 
       end if

       CALL WDialogFieldState(ID_XXPSave     , state)  
       CALL WDialogFieldState(IDF_ColorPick  , state)
       CALL WDialogFieldState(IDF_Trans      , state) 
       CALL WDialogFieldState(IDF_Anim       , state2)    
       CALL WDialogFieldState(IDF_FilterInd  , state)  

       call WDialogPutInteger(IDF_SpriteIndex, 1)
       call WDialogPutInteger(IDF_FilterInd  , 0)

    end subroutine 

    subroutine checkImageWindowFields()
       character(NAME_MAX_LEN)   :: name
       integer                   :: ind, filter, animSet, transSet, t1, t2

       if ((pickerActive .EQV. .FALSE.) .AND. (pleaseStop .EQV. .FALSE.)) then
 
           call WDialogGetString(     ID_XXAName, name   )
    
           if (allocated(imageLoader%img)) then 
               if (imageLoader%img%numOfFrames > 1) then  
                   call WDialogGetCheckBox(IDF_Anim   , animSet)
                   CALL WDialogFieldState(IDF_SpriteIndex, 1 - animSet)
               else
                   animSet = 0  
               end if 
    
               call WDialogGetCheckBox(IDF_Trans     , transSet)
               CALL WDialogFieldState(IDF_TransColor , transSet) 
               CALL WDialogFieldState(IDF_TransTrk   , transSet) 

               if (transSet == 1) then
                   CALL WDialogGetInteger( IDF_TransColor, t1)
                   CALL WDialogGetTrackbar(IDF_TransTrk, t2)
                   
                   if (t1 /= imageLoader%img%transpColor) then
                       imageLoader%img%transpColor = t1 
                       CALL WDialogPutTrackbar(IDF_TransTrk, t1)
                   end if 

                   if (t2 /= imageLoader%img%transpColor) then
                       imageLoader%img%transpColor = t2 
                       CALL WDialogPutInteger(IDF_TransColor, t2)
                   end if 

               else
                   imageLoader%img%transpColor = 1 
               end if  
    
               call WDialogGetInteger(IDF_SpriteIndex, ind)
               call WDialogGetInteger(IDF_FilterInd  , filter)

               if (animSet == 1) then
                   if (counttimer.timerEnded() .EQV. .TRUE.) then
                       ind = ind + 1 
                       call counttimer.timerStart(PERFECT_WAIT * getSpeed())
                   end if 
               end if     

               if (ind > imageLoader%img%numOfFrames) ind = 1 
               if (ind < 1) ind = imageLoader%img%numOfFrames 

               CALL WDialogPutInteger(IDF_SpriteIndex, ind) 

               call imageLoader%addToScreenBuffer(ind, 1, 1, 1, filter)
               CALL setUpTo256()
               CALL buffer2Real()    
           
           else 
               CALL WDialogFieldState(IDF_SpriteIndex, DISABLED) 
               CALL WDialogFieldState(IDF_TransColor , DISABLED) 
               CALL WDialogFieldState(IDF_TransTrk   , DISABLED) 

           end if  

        end if

        if (canKill .EQV. .TRUE.) then 
            call eraseBuff() 
            call imageLoader%dropImage()    
            CALL WDialogUnLoad()
            canKill = .FALSE.
        end if

    end subroutine
 
    subroutine saveBMP2XXP()
        integer(2), dimension(:), allocatable :: d
        integer(2)                            :: rc
        character(NAME_MAX_LEN)               :: name
        character(MAX_PATH_LEN)               :: fname
        integer(8)                            :: offset, s, f, x, y, fullS
        character(40)                         :: t   

        fname = FileDialog("img\", .TRUE., "xxp ")  
        call WDialogGetString(ID_XXPName,  name)
        if (fname == "") return
        
    !   4   bytes: 'IMG ' 
    !   1   byte : Lenght of Name
    !   lenOfName: Name 
    !   2 bytes  : w       
    !   2 bytes  : h       
    !   1 bytes  : transpColor - 1     
    !   1 bytes  : number of Frames - 1      
    !   (w * h)  * number of Frames  

        fullS = imageLoader%img%width
        fullS = fullS * imageLoader%img%height
        fullS = fullS * imageLoader%img%numOfFrames

        s = 5 + len_trim(name) + 6 + fullS

        allocate(d(s), stat = rc)
        if (rc /= 0) call displayDebug("Failed to allocate XXP bytes!")

        offset = 1

        call writeChars2Bin(d, IMG_FILE_TYPE, 1, 4)

        d(5) = len_trim(name)
        
        call writeChars2Bin(d, trim(name), 6, len_trim(name))
        offset = 6 + len_trim(name)

        call WriteInt2ToData(d, offset, imageLoader%img%width)
        call WriteInt2ToData(d, offset, imageLoader%img%height)

        d(offset    ) = imageLoader%img%transpColor - 1
        d(offset + 1) = imageLoader%img%numOfFrames - 1
        
        offset = offset + 1

        do f        = 1, imageLoader%img%numOfFrames, 1
           do y     = 1, imageLoader%img%height     , 1
               do x = 1, imageLoader%img%width      , 1
                    offset    = offset + 1   
                    d(offset) = imageLoader%img%frames(f, x, y) - 1
               end do  
           end do 
        end do

        !call writeBin2File(trim(CWD()) // "\img\test.xxp", d, .FALSE., .FALSE.)
        call writeBin2File(fname, d, .TRUE., .TRUE.)

    end subroutine

    subroutine loadImageHeader(num, fname)
        integer(1)                             :: rc
        integer(2)                             :: num
        character(*)                           :: fname
        integer(8)                             :: siz
        integer(2)                             :: stat
        integer(2), dimension(:), allocatable  :: d, temp
        integer(8)                             :: offset

        call loadBinary(trim(CWD()) // "\img\" // fname, d, siz, .TRUE.)
       
        offset = 1
        call read4CharFromBin(d, siz, offset, imageList(num)%header)  
        if (imageList(num)%header /= IMG_FILE_TYPE) then
            call displayDebug("This is not a valid bitmap file!")
            return
        end if    
    
        imageList(num)%nameLen = d(offset)
        offset                 = offset + 1

        call copyBytes(d, temp, offset, &
                       offset + imageList(num)%nameLen - 1, &
                       imageList(num)%nameLen) 

        offset = offset + imageList(num)%nameLen

        call bin2Char(imageList(num)%name, temp, imageList(num)%nameLen, .TRUE.) 
        imageList(num)%fileName = fname    

        deallocate(d, stat = stat)
        if (stat /= 0) call displayDebug("Failed to deallocate the loaded XXP!")

    end subroutine

    subroutine loadImage(this)
        class(imageFile), intent(inout)        :: this
        integer(8)                             :: siz
        integer(2)                             :: stat
        integer(2), dimension(:), allocatable  :: d
        integer(8)                             :: offset, f, x, y
        character(40)                          :: test

        if (allocated(this%img) .EQV. .TRUE.) call this%dropImage()

        call loadBinary(trim(CWD()) // "\img\" // trim(this%fileName), d, siz, .TRUE.)

        offset = 6 + this%nameLen

        allocate(this%img,  stat = stat)
        if (stat /= 0) then
            write(test, "(I0)") stat
            call displayDebug("Failed to allocate imageFile's ImageData! (" // trim(test) // ")")
        else    
            this%img%width       = ReadInt2FromData(d, offset) 
            this%img%height      = ReadInt2FromData(d, offset) 
            this%img%transpColor = d(offset)     + 1
            this%img%numOfFrames = d(offset + 1) + 1

            !write(test, "(I0, ' ', I0, ' ', I0, ' ', I0)") this%img%width, this%img%height, &
            !                                               this%img%transpColor, this%img%numOfFrames
            !
            !call displayDebug(trim(this%fileName) // " " // test)

            allocate(this%img%frames(this%img%numOfFrames, &
                                     this%img%width      , &
                                     this%img%height     ), stat = stat)

            if (stat /= 0) then
                !write(test, "(I0, '|', I0, '|', I0)") this%img%numOfFrames, this%img%width, this%img%height
                !call displayDebug("Dimensions: " // trim(test))

                write(test, "(I0)") stat
                call displayDebug("Failed to allocate imageFile's frames! (" // trim(test) // ")")
            else   
                offset = offset + 1
    
                do f        = 1, this%img%numOfFrames, 1
                   do y     = 1, this%img%height     , 1
                       do x = 1, this%img%width      , 1
                            offset                   = offset    + 1   
                            this%img%frames(f, x, y) = d(offset) + 1
                       end do  
                   end do 
                end do
            end if
        end if

        deallocate(d, stat = stat)
        if (stat /= 0) call displayDebug("Failed to deallocate the loaded XXP! #2")

    end subroutine

END MODULE ImageFactory
