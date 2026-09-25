MODULE GameObject

    USE, INTRINSIC :: ISO_C_BINDING
    USE debugWindow
    USE dataLoader
    USE WINTERACTER
    USE RESID
    USE subs
    USE engineConstants
    USE inpout
    USE dict
    USE ImageFactory

    implicit none

    private
    public   :: objManagerWindow, objectWindowThings, initSpriteNameListPointers

    logical               :: canKill, bullshit
    integer(1)            :: changedPage = 0
    integer(2), parameter :: typeNum = 3

    character(NAME_MAX_LEN), dimension(typeNum ), parameter:: typeNames =  &
                           (/ "floor", "playerMap", "tree" /)
    character(NAME_MAX_LEN), dimension(:), allocatable :: spriteNames 
    integer(2)                                         :: lastSelectedType, maxPages, currPage     

    type spriteNameListPointer
        character(NAME_MAX_LEN), dimension(:), pointer         :: spriteNameList 
    end type

    type(spriteNameListPointer), dimension(typeNum) :: spriteNameListPointers

    type objectData
         character(4)                            :: header
         integer(1)                              :: nameLen
         character(NAME_MAX_LEN)                 :: name 
         integer(1)                              :: objType, numOfSprites
         logical                                 :: solid

         character(NAME_MAX_LEN), dimension(:), allocatable :: spriteList

    end type

    type(objectData), dimension(:), allocatable, target :: gameObjectList
    type(objectData)                                    :: testData

    contains

    subroutine initSpriteNameListPointers()
        spriteNameListPointers(1)%spriteNameList => singleSpriteList 
        spriteNameListPointers(2)%spriteNameList => mapCharacterSpriteList 
        spriteNameListPointers(3)%spriteNameList => treeSpriteList 

    end subroutine

    subroutine objManagerWindow()
       INTEGER                                 :: ITYPE, rc
       TYPE(WIN_MESSAGE)                       :: MESSAGE
       !character(10)                  :: msgString
       bullshit  = .FALSE.  
       canKill   = .FALSE.   

       do
         if (WInfoDialog(CurrentDialog) == 0) exit
         call sleep(1)
       end do 

       CALL WDialogLoad(IDD_ObjectWindow)

       if (allocated(spriteNames)) then
           deallocate(spriteNames, stat = rc) 
           if (rc /= 0) call displayDebug("Failed to deallocate Sprite Name List!")
       end if 

       call getImageList(spriteNames)

       CALL WDialogPutString(ID_ObjName, OBJ_DEFAULT)  
       CALL WDialogTitle(getWordInCurrentLang("objManager")) 
       CALL WDialogPutString(ID_ObjLoad , getWordInCurrentLang("load")) 
       CALL WDialogPutString(ID_ObjSave , getWordInCurrentLang("save")) 
       CALL WDialogPutString(ID_ObjErase, getWordInCurrentLang("erase")) 

       CALL WDialogPutString(IDF_ObjTypeLabel, getWordInCurrentLang("objectType")) 
       CALL WDialogPutString(IDF_SolidBox    , getWordInCurrentLang("solid")) 
       CALL WDialogPutString(IDF_SpriteIndexLabel, getWordInCurrentLang("index")) 
       CALL WDialogPutString(IDF_SpriteNameLabel , getWordInCurrentLang("name")) 
       CALL WDialogPutString(IDF_AssignedLabel   , getWordInCurrentLang("assignedSprite")) 

       call wDialogPutMenu(IDF_ObjectTypeMenu , typeNames  , size(typeNames)  , 0)  
       call wDialogPutMenu(IDF_AssignedSprite1, spriteNames, size(spriteNames), 0)  
       call wDialogPutMenu(IDF_AssignedSprite2, spriteNames, size(spriteNames), 0)  
       call wDialogPutMenu(IDF_AssignedSprite3, spriteNames, size(spriteNames), 0)  
       call wDialogPutMenu(IDF_AssignedSprite4, spriteNames, size(spriteNames), 0)  
       call wDialogPutMenu(IDF_AssignedSprite5, spriteNames, size(spriteNames), 0)  
       call wDialogPutMenu(IDF_AssignedSprite6, spriteNames, size(spriteNames), 0)  
       call wDialogPutMenu(IDF_AssignedSprite7, spriteNames, size(spriteNames), 0)  
       call wDialogPutMenu(IDF_AssignedSprite8, spriteNames, size(spriteNames), 0)  

       call setDefaults() 
       changedPage = 0

       do
          CALL WDialogSelect(IDD_ObjectWindow)
          bullshit = .TRUE.  
          CALL WDialogShow(ITYPE=Modal)     
          bullshit = .FALSE.  

          if (WinfoDialog(CurrentDialog) == IDD_ObjectWindow) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     EXIT
                  CASE(ID_ObjLoad)
                       call loadObj()
                  CASE(ID_ObjSave)
                       call saveToList()
                       call saveObj() 
                  CASE(ID_ObjErase)
                       call setDefaults() 
                  CASE(IDF_DecrIndex)  
                       call saveToList()
                       changedPage = -1               
                  CASE(IDF_IncrIndex)  
                       call saveToList()
                       changedPage = 1              

                  END SELECT
              end if
       end do 

       canKill = .TRUE. 

    END SUBROUTINE

    subroutine loadObj()
        character(MAX_PATH_LEN)                :: fname

        fname = FileDialog("obj\", .FALSE., "xxo ")    
        if (fname /= "") call loadObjFile(0, fname, .FALSE.)

    end subroutine

    subroutine loadObjFile(N, fname, addCwd)
        character(*)                           :: fname
        integer(2)                             :: N
        integer(2), dimension(:), allocatable  :: d, temp
        integer(8)                             :: siz, offset
        integer(2)                             :: stat
        logical                                :: addCwd
        character(4)                           :: headerTyp
        integer(2)                             :: nameLen, numOfSprites              
        character(NAME_MAX_LEN)                :: name
        integer(4)                             :: solidBoxVal, typeVal  

        if (addCwd) then
            call loadBinary(trim(CWD()) // "\obj\" // fname, d, siz, .FALSE.)
        else
            call loadBinary(fname, d, siz, .FALSE.)
        end if
        
        offset = 1
        call read4CharFromBin(d, siz, offset, headerTyp)  

        if (headerTyp /= OBJ_FILE_TYPE) then
            call displayDebug("This is not a valid Xerxes Object file!")
            return
        end if    
    
        nameLen = d(offset)
        offset  = offset + 1

        allocate(temp(nameLen), stat = stat)
        if (stat /= 0) call displayDebug("Failed to allocate temp for Object Name on Load!")

        call copyBytes(d, temp, offset, offset + nameLen - 1, nameLen) 

        offset = offset + nameLen

        call bin2Char(name, temp, nameLen, .TRUE.) 

        if (name == OBJ_DEFAULT) call displayDebug(fname // " has the default OBJ name!")
 
        typeVal      = d(offset    )
        solidBoxVal  = d(offset + 1)
        numOfSprites = d(offset + 2)   

        offset = offset + 3          

        if (N == 0) then
            call setDefaults()
            lastSelectedType = -1

            call wDialogPutString(ID_ObjName, name)
            call WDialogPutOption(IDF_ObjectTypeMenu, typeVal)   
            call WDialogPutCheckBox(IDF_SolidBox    , solidBoxVal )

            call createSpriteList(d, numOfSprites, testData%spriteList, offset, siz)

        else 
            gameObjectList(N)%header       = headerTyp
            gameObjectList(N)%name         = name
            gameObjectList(N)%nameLen      = nameLen
            gameObjectList(N)%objType      = typeVal
            gameObjectList(N)%solid        = solidBoxVal  
            gameObjectList(N)%numOfSprites = numOfSprites 

            call createSpriteList(d, numOfSprites, gameObjectList(N)%spriteList, offset, siz)
        end if

        deallocate(d, stat = stat)
        if (stat /= 0) call displayDebug("Failed to deallocate the loaded Object data!")

    end subroutine

    subroutine createSpriteList(d, numOfSprites, spriteList, offset, siz)
        integer(2), dimension(:), allocatable  :: d
        integer(2), dimension(:), allocatable  :: temp
        integer(1)                             :: numOfSprites
        integer(1)                             :: num
        integer(8)                             :: siz
        integer(8), intent(inout)              :: offset
        integer(2)                             :: stat
        character(NAME_MAX_LEN), dimension(:), allocatable, intent(inout) :: spriteList
        integer(1)                             :: counter, ind, L

        if (allocated(spriteList)) then
            deallocate(spriteList, stat = stat)
            if (stat /= 0) call displayDebug("Failed to deallocate Sprite List on load!")    
        end if

        allocate(spriteList(numOfSprites), stat = stat)
        if (stat /= 0) call displayDebug("Failed to allocate Sprite List on load!")  

        num = 0
        do 
            L      = d(offset)
            offset = offset + 1
            num    = num + 1            

            allocate(temp(L), stat = stat)
            if (stat /= 0) call displayDebug("Failed to allocate temp for Sprite Name on Load!")

            call copyBytes(d, temp, offset, offset + L - 1, L) 
            call bin2Char(spriteList(num), temp, L, .TRUE.) 
            offset = offset + L

            if (num == numOfSprites) exit
        end do

    end subroutine

    subroutine saveObj()
        integer(2)                            :: stat, num
        character(NAME_MAX_LEN)               :: name
        character(MAX_PATH_LEN)               :: fname
        integer(2), dimension(:), allocatable :: fullD
        integer(8)                            :: siz, ind                                   
        integer(4)                            :: solidBoxVal, typeVal

        fname = FileDialog("obj\", .TRUE., "xxo ")  

        call WDialogGetString(ID_ObjName,  name)
        if (fname == "") return

        ! File Type ('OBJ ')     : 4 bytes
        ! Name Length (Max: 25)  : 1 byte
        ! Actual Name               
        ! Type                   : 1 byte
        ! Solid                  : 1 byte    
        ! Number of Sprites      : 1 bytes    
        ! Name Len + Sprite Name : 1 byte + many    

        siz = 4 + 1 + len_trim(name) + 1 + 1 + 1 

        do num = 1, size(testData%spriteList), 1
           siz = siz + 1 + len_trim(testData%spriteList(num))
        end do

        allocate(fullD(siz), stat = stat)
        if (stat /= 0) call displayDebug("Failed to allocate output bytes for saving XXA!")

        fullD = 0
        call writeChars2Bin(fullD, OBJ_FILE_TYPE, 1, 4)
        fullD(5) = len_trim(name) 
        call writeChars2Bin(fullD, trim(name), 6, len_trim(name))

        ind = 6 + len_trim(name)
        call WDialogGetCheckBox(IDF_SolidBox, solidBoxVal)

        call WDialogGetMenu(IDF_ObjectTypeMenu, typeVal) 
        fullD(ind) = typeVal
        ind = ind + 1

        fullD(ind) = solidBoxVal
        ind = ind + 1

        fullD(ind) = size(testData%spriteList)
        ind = ind + 1
        !call WriteInt4ToData(fullD, ind, size(testData%spriteList))
        do num = 1, size(testData%spriteList), 1
           fullD(ind) = len_trim(testData%spriteList(num))
           ind = ind + 1  
           call writeChars2Bin(fullD, trim(testData%spriteList(num)), ind, len_trim(testData%spriteList(num)))
           ind = ind + len_trim(testData%spriteList(num)) 
        end do

        call writeBin2File(fname, fullD, .TRUE., .FALSE.)

    end subroutine

    subroutine setDefaults()
        call wDialogPutString(ID_ObjName, obj_default)
        call WDialogPutOption(IDF_ObjectTypeMenu, 1)   
        call WDialogPutOption(IDF_AssignedSprite1, 0)   
        call WDialogPutOption(IDF_AssignedSprite2, 0)   
        call WDialogPutOption(IDF_AssignedSprite3, 0)   
        call WDialogPutOption(IDF_AssignedSprite4, 0)   
        call WDialogPutOption(IDF_AssignedSprite5, 0)   
        call WDialogPutOption(IDF_AssignedSprite6, 0)   
        call WDialogPutOption(IDF_AssignedSprite7, 0)   
        call WDialogPutOption(IDF_AssignedSprite8, 0)     
        call WDialogPutOption(IDF_AssignedSprite8, 0)     
        call WDialogPutCheckBox(IDF_SolidBox, 0)

        lastSelectedType = 0
        currPage         = 0   
        maxPages         = 0

    end subroutine

    subroutine fillListNames()
        integer(2)          :: num, num2
        character(2)        :: charNum

        num2 = 0 

        do num  = (currPage * 8) + 1, (currPage + 1)  * 8, 1
           num2 = num2 + 1 
 
           if (num > size(spriteNameListPointers(lastSelectedType)%spriteNameList)) then
               call WDialogPutString(IDF_SpriteName1 + (num2 - 1), "")           
               CALL WDialogFieldState(IDF_AssignedSprite1 + (num2 - 1), DISABLED) 
               call WDialogPutOption(IDF_AssignedSprite1 + (num2 - 1), 0)   
           else  
               call WDialogPutString(IDF_SpriteName1 + (num2 - 1), &
                    spriteNameListPointers(lastSelectedType)%spriteNameList(num)) 
               CALL WDialogFieldState(IDF_AssignedSprite1 + (num2 - 1), ENABLED) 

           end if 
           write(charNum, "(I2.2)") num 
           call WDialogPutString(IDF_SpriteIndexNum1 + (num2 - 1), charNum)
 
        end do

    end subroutine

    subroutine loadFromList()
        integer(2)                  :: num2
        integer(4)                  :: num3, num

        num2 = 0
        do num  = (currPage * 8) + 1, (currPage + 1)  * 8, 1
           num2 = num2 + 1 

           if (num <= size(spriteNameListPointers(lastSelectedType)%spriteNameList)) then
               do num3 = 1, size(spriteNames), 1  
                  if (spriteNames(num3) == testData%spriteList(num)) then
                      call WDialogPutOption(IDF_AssignedSprite1 + (num2 - 1), num3)  
                      exit  
                  end if 
               end do 
           else
               exit  
           end if 

        end do 

    end subroutine 

    subroutine saveToList()
        integer(2)                  :: rc, num, num2
        integer                     :: currSelectedSprite

        do num = 1, 8, 1
           num2 = num + (currPage * 8)  
               if (num2 > size(testData%spriteList)) exit                  
               call WDialogGetMenu(IDF_AssignedSprite1 + num - 1, currSelectedSprite) 
               testData%spriteList(num2) = spriteNames(currSelectedSprite)
        end do 

    end subroutine

    subroutine objectWindowThings()
        integer(2)                  :: rc
        integer                     :: currSelectedType
        logical                     :: hackMe

        if (WinfoDialog(CurrentDialog) == IDD_ObjectWindow) then 
           if (bullshit .EQV. .TRUE.) then 

               call WDialogGetMenu(IDF_ObjectTypeMenu, currSelectedType)    
    
               if (lastSelectedType /= currSelectedType) then
                   hackMe            = (lastSelectedType == -1) 

                   lastSelectedType  = currSelectedType
                   currPage          = 0   
                   maxPages          = ((size(spriteNameListPointers(lastSelectedType)%spriteNameList) - 1) / 8)  
                    
                   if (hackMe .EQV. .FALSE.) then
                       if (allocated(testData%spriteList)) then
                           deallocate(testData%spriteList, stat = rc) 
            
                           if (rc /= 0) call displayDebug("Failed to deallocate testData's Sprite List!")
                       end if  
        
                       allocate(testData%spriteList( & 
                            size(spriteNameListPointers(lastSelectedType)%spriteNameList)), stat = RC) 
        
                       if (rc /= 0) call displayDebug("Failed to allocate testData's Sprite List!")

                       testData%spriteList = spriteNames(1)
                   end if

                   call fillListNames() 
                   call loadFromList() 

               end if 

               if (changedPage /=0) then
                   currPage = currPage + changedPage 

                   call fillListNames()  
                   call loadFromList() 
    
                   changedPage = 0
               end if  

               if (maxPages == 0) then 
                   CALL WDialogFieldState(IDF_DecrIndex, DISABLED) 
                   CALL WDialogFieldState(IDF_IncrIndex, DISABLED) 
               else 
                   if (currPage == 0) then
                       CALL WDialogFieldState(IDF_DecrIndex, DISABLED) 
                   else 
                       CALL WDialogFieldState(IDF_DecrIndex, ENABLED) 
                   end if 
    
                   if (currPage == maxPages ) then
                       CALL WDialogFieldState(IDF_IncrIndex, DISABLED) 
                   else 
                       CALL WDialogFieldState(IDF_IncrIndex, ENABLED) 
                   end if 
    
               end if 
    
            end if
        end if

        if (canKill .EQV. .TRUE.) then 
            CALL WDialogUnLoad()

            if (allocated(spriteNames)) then
                deallocate(spriteNames, stat = rc) 
                if (rc /= 0) call displayDebug("Failed to deallocate Sprite Name List!")
            end if 

            canKill = .FALSE.
        end if

    end subroutine

END MODULE GameObject
