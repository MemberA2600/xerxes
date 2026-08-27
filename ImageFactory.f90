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

    implicit none

    private
    public                  :: bitMapWindow, checkImageWindowFields

    !
    !   Images are pretty complex and compact.
    !   The beginning is typical: 
    !   'IMG ', lenght of name (1 byte), name.
    !
    !   Size of screen, each two bytes 
    !   (max: wOfScreenBuffer and hOfScreenBuffer)
    !   
    !   transpColor: This color is used for transparency, so should be a color that is
    !                not present on the picture. If 0, since black cannot be transparent,
    !                it means the picture is not transparent. By deafult, it's the top-left pixel
    !                of the first frame.
    !
    !   The number of frames. One byte, n+1 is the number, so it can go up to 256.    
    !
    !   The first frame (key) is always exact, so the file always holds all the pixels. The next ones
    !   holds only the number you have to add to the previous one the get the current one. This makes
    !   gzip more efficient with the lot of unchanged ones (0).
    !
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

    end type

    type(imageFile)                                :: imageLoader
    logical                                        :: canKill = .FALSE., pickerActive = .FALSE., &
                                                      justACancel, pleaseStop = .TRUE.  

    contains
    
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

        do yPix = 1, this%img%height, 1
           do xPix = 1, this%img%width, 1
              color = this%img%frames(frameNum, xPix, yPix)  

              xOnBuff = xPix + x - 1 
              yOnBuff = yPix + y - 1  
 
              if (this%img%transpColor > 1 .AND. this%img%transpColor == color) then
                  if (xOnBuff <= wOfScreenBuffer .AND. yOnBuff <= hOfScreenBuffer) then   
                      call setBufferPixel(bufferNum, xOnBuff, yOnBuff, -1)
                      cycle   
                  end if
              end if

              select case(filter)
              case(FILTER_RAINBOW)
                   color = color + getUpTo256()
                   if (color > numOfColors) color = color - numOfColors
                    
              case(FILTER_NIGHT)
                   color = getNightColor(color)  

              case(FILTER_RED0)
                   color = changeRGB(color, 1,  0,  0)

              case(FILTER_RED1)
                   color = changeRGB(color, 2, -1, -1)

              case(FILTER_RED2)
                   color = changeRGB(color, 4, -2, -3)

              case(FILTER_BLUE0)
                   color = changeRGB(color,  0,  0, 1)

              case(FILTER_BLUE1)
                   color = changeRGB(color, -2, -1, 2)

              case(FILTER_BLUE2)
                   color = changeRGB(color, -4, -2, 2)

              case(FILTER_GREEN0)
                   color = changeRGB(color,  0, 1, 0)

              case(FILTER_GREEN1)
                   color = changeRGB(color, -1, 1, -1)

              case(FILTER_GREEN2)
                   color = changeRGB(color, -1, 2, -1)

              case(FILTER_YELLOW0)
                   color = changeRGB(color,  1, 1, 0)

              case(FILTER_YELLOW1)
                   color = changeRGB(color,  2, 1, -1)

              case(FILTER_YELLOW2)
                   color = changeRGB(color,  4, 2, -1)

              end select

              if (xOnBuff <= wOfScreenBuffer .AND. yOnBuff <= hOfScreenBuffer) then   
                  call setBufferPixel(bufferNum, xOnBuff, yOnBuff, color)   
              end if

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

    subroutine bitMapWindow()
       INTEGER                                 :: ITYPE
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

       call onlyRunAfterLoad()
 
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

                  CASE(IDF_ColorPick)
                     pickerActive = .TRUE.
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
                   if (stupidTimerEnded() .EQV. .TRUE.) then
                       ind = ind + 1 
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
 

END MODULE ImageFactory
