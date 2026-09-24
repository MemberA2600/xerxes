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

    logical   :: canKill
    integer(2), parameter :: typeNum = 3

    character(NAME_MAX_LEN), dimension(typeNum ), parameter:: typeNames =  &
                           (/ "floor", "playerMap", "obstacleMap" /)
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

         character(NAME_MAX_LEN), dimension(:,:), allocatable :: spriteList

    end type

    contains

    subroutine initSpriteNameListPointers()
        spriteNameListPointers(1)%spriteNameList => floorSprites 

    end subroutine

    subroutine objManagerWindow()
       INTEGER                                 :: ITYPE, rc
       TYPE(WIN_MESSAGE)                       :: MESSAGE
       !character(10)                  :: msgString

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

       !CALL WDialogPutString(ID_XXAName, OPL2_DEFAULT)  
       !CALL WDialogTitle(getWordInCurrentLang("vgmToXXA")) 
       !CALL WDialogPutString(ID_VGMLoad, getWordInCurrentLang("load")) 
       !CALL WDialogPutString(ID_XXASave, getWordInCurrentLang("save")) 
       !CALL WDialogPutString(ID_XXAPlay, getWordInCurrentLang("play")) 
       !CALL WDialogPutString(ID_XXAStop, getWordInCurrentLang("stop")) 

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

       do
          CALL WDialogSelect(IDD_ObjectWindow)
          CALL WDialogShow(ITYPE=Modal)     
    
          if (WinfoDialog(CurrentDialog) == IDD_ObjectWindow) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     EXIT
                  CASE(ID_ObjLoad)

                  CASE(ID_ObjSave)

                  CASE(ID_ObjErase)
                       call setDefaults() 
                  END SELECT
              end if
       end do 

       canKill = .TRUE. 

    END SUBROUTINE

    subroutine setDefaults()
        call wDialogPutString(ID_ObjName, obj_default)
        call WDialogPutOption(IDF_ObjectTypeMenu, 1)   
        call WDialogPutOption(IDF_AssignedSprite1, 1)   
        call WDialogPutOption(IDF_AssignedSprite2, 1)   
        call WDialogPutOption(IDF_AssignedSprite3, 1)   
        call WDialogPutOption(IDF_AssignedSprite4, 1)   
        call WDialogPutOption(IDF_AssignedSprite5, 1)   
        call WDialogPutOption(IDF_AssignedSprite6, 1)   
        call WDialogPutOption(IDF_AssignedSprite7, 1)   
        call WDialogPutOption(IDF_AssignedSprite8, 1)     
        call WDialogPutOption(IDF_AssignedSprite8, 1)     
        call WDialogPutCheckBox(IDF_SolidBox, 0)

        lastSelectedType = 0
        currPage         = 0   
        maxPages         = 0

    end subroutine

    subroutine fillListNames()
        integer(2)          :: num, num2

        num2 = 0 

        do num  = (currPage * 8) + 1, (currPage + 1)  * 8, 1
           num2 = num2 + 1 
 
           if (num > size(spriteNameListPointers(lastSelectedType)%spriteNameList)) then
               call WDialogPutString(IDF_SpriteName1 + (num2 - 1), "")           
               CALL WDialogFieldState(IDF_AssignedSprite1 + (num2 - 1), DISABLED) 
           else  
               call WDialogPutString(IDF_SpriteName1 + (num2 - 1), &
                    spriteNameListPointers(lastSelectedType)%spriteNameList(num)) 
               CALL WDialogFieldState(IDF_AssignedSprite1 + (num2 - 1), ENABLED) 

           end if 

        end do

    end subroutine

    subroutine objectWindowThings()
        integer(2)                  :: rc
        integer                     :: currSelectedType

        if (WinfoDialog(CurrentDialog) == IDD_ObjectWindow) then 
           call WDialogGetMenu(IDF_ObjectTypeMenu, currSelectedType)   
        
           if (lastSelectedType /= currSelectedType) then
               lastSelectedType  = currSelectedType
               currPage          = 0   
               maxPages          = ((size(spriteNameListPointers(lastSelectedType)%spriteNameList) - 1) / 8)  

               call fillListNames()  
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
