MODULE GameMap
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
    USE sprite7up    
    USE gameobject    
    USE inputreader
    USE screen
    USE winapis
    USE KERNEL32, WinSleep => Sleep
    USE adlib

    implicit none

    private
    public   :: initAllUnitLists, searchForDeadUnits, openBasicSettingsWindow, mapBasicSettings, &
                runGameLogic, doThingsOnMapEditor, closeMapEditor, FloorChooser, floorAdderCheck

    logical  :: canKill = .FALSE.
    character(NAME_MAX_LEN), dimension(:), allocatable :: baseFloorList, floorList, musicList 
    character(NAME_MAX_LEN), dimension(4), parameter   :: weatherNamesKeys = (/ & 
                                                       "dayNorm", "nightNorm", "dayRain","nightRain" /) 
    character(NAME_MAX_LEN), dimension(4)              :: weatherNames

    integer(1), parameter                              :: stepOnMap = 10
    integer(1)                                         :: lastNum

    type(counterTimer)                                 :: timerC

    logical                                            :: placeActive     = .FALSE., allowed
    character(NAME_MAX_LEN)                            :: placeObjName
    type(objectData), pointer                          :: placeObj         
    type(imageFile) , pointer                          :: placeImg
    integer(1)                                         :: placeTyp, placeFilter
    integer(4)                                         :: XOnScreen, YOnScreen 

    type Unit
         type(spritePoz), pointer      :: sp 
         integer(4)                    :: x, y
         integer(1)                    :: layerNum
         character(NAME_MAX_LEN)       :: name

         contains                  

         procedure                     :: killMe     => killMe
         procedure                     :: setUnit    => setUnit
         procedure                     :: setPointer => setPointer

    end type

    type UnitList
         integer(8)                            :: siz  
         type(Unit), dimension(:), allocatable :: units
        
         contains   

         procedure         initList        => initList
         procedure         dropList        => dropList 
         procedure         addUnit         => addUnit 
         procedure         removeDeads     => removeDeads
         procedure         removeOutsiders => removeOutsiders
         procedure         removeByName    => removeByName    

    end type

    type Map
         character(4)                          :: header
         integer(1)                            :: nameLen, typ = MAP_WALKING, &
                                                  defWeather = WEATHER_DAY_NORM, &
                                                  defFilter  = NO_FILTER      
         character(NAME_MAX_LEN)               :: name, musicPlaying, defaultMusic 
         integer(4)                            :: width, height
         logical                               :: wind = .FALSE., paused = .TRUE.
            
         type(unit)                            :: defaultFloor
         type(unitList)                        :: floors, realUnits

    end type

    type(map)                                  :: currentMap

    integer(1)                                 :: mapTypeOld, weatherOld, mapTypePrev
    integer(4)                                 :: widthOld, heightOld
    logical                                    :: windOld
    character(NAME_MAX_LEN)                    :: defaultFloorOld, defaultFloor

    contains

!
!   Unit Stuff
!

    subroutine killMe(this)
        class(Unit), intent(inout) :: this

        call this%setPointer()
        if (associated(this%sp)) call this%sp%killMe()
        nullify(this%sp)
        this%name = ""
        !call currentMap%killUnit(this%ind)
    
    end subroutine

    subroutine setPointer(this)
        class(Unit), intent(inout)            :: this

        !call displayDebug(this%name)

        nullify(this%sp)
        if (this%name == "") return

        call addPointerByNameXY(this%sp, this%layerNum, this%name, this%x, this%y)

        !call displayDebug("SZAR!!")

    end subroutine

    subroutine setUnit(this, name, x, y, layerNum)
        class(Unit), intent(inout)            :: this
        type(objectData), pointer             :: obj
        integer(1)                            :: rc
        integer(1)                            :: layerNum
        integer(4)                            :: x, y
        character(*)                          :: name

        call getGameObjByName(obj, name)

        select case(obj%objType)
        case(TYPE_FLOOR)
            if (layerNum == LAYER_BACKGROUND) then
                call createSpriteObjBackGround(name, obj%getEditorSprite(currentMap%wind, .TRUE., ""), &
                                               currentMap%defFilter, obj%name)
            else
                call createSpriteObjPlayGround(name, obj%getEditorSprite(currentMap%wind, .TRUE., ""), &
                                               x, y, currentMap%defFilter, .FALSE., obj%name)
            end if    

        case(TYPE_TREE)
            call createSpriteObjPlayGround(name, obj%getEditorSprite(currentMap%wind, .TRUE., ""), &
                                           x, y, currentMap%defFilter, .FALSE., obj%name)
        case(TYPE_PLAYER_MAP)   
            call createSpriteObjPlayGround(name, obj%getEditorSprite(currentMap%wind, .TRUE., ""), &
                                           x, y, currentMap%defFilter, .FALSE., obj%name)
        end select

        call addPointerToLastSpritePoz(this%sp, layerNum)
        this%x        = x
        this%y        = y
        this%name     = name
        this%layerNum = layerNum

    end subroutine

!
!   UnitList Stuff
 
    subroutine removeOutsiders(this, w, h)
        class(UnitList), intent(inout)        :: this
        integer(4)                            :: w, h
        integer(8)                            :: ind
        
        do ind = 1, size(this%units), 1
           call this%units(ind)%setPointer()
           if (associated(this%units(ind)%sp)) then 
               if (this%units(ind)%sp%xw > w .OR. this%units(ind)%sp%yh > h) call this%units(ind)%killMe() 
           end if 
        end do

    end subroutine

    subroutine removeByName(this, n)
        class(UnitList), intent(inout)        :: this
        character(*)                          :: n
        integer(8)                            :: ind
        
        do ind = 1, size(this%units), 1
           call this%units(ind)%setPointer()
           if (associated(this%units(ind)%sp)) then 
               if (this%units(ind)%sp%name == n) call this%units(ind)%killMe() 
           end if 
        end do

    end subroutine

    subroutine addUnit(this, name, x, y)
        class(UnitList), intent(inout)        :: this
        type(Unit), dimension(:), allocatable :: tempUnits
        integer(1)                            :: rc
        integer(4)                            :: x, y
        character(*)                          :: name
        type(objectData), pointer             :: obj
        integer(8)                            :: ind

        call getGameObjByName(obj, name)

        if (size(this%units) == this%siz) then

            allocate(tempUnits(size(this%units) + SIZE_ADD), stat = RC)
            if (rc /= 0) call displayDebug("Failed to allocate temporal unit list!")
        
            tempUnits(1:size(this%units)) = this%units        
            call move_alloc(tempUnits, this%units)

            do ind = this%siz + 1, size(this%units), 1
               if (associated(this%units(ind)%sp)) nullify(this%units(ind)%sp)
            end do
        end if

        this%siz = this%siz + 1

        call this%units(this%siz)%setUnit(name, x, y, LAYER_PLAYGROUND)
        
        do ind = 1, this%siz, 1
           call this%units(ind)%setPointer()  
        end do    

    end subroutine

    subroutine initList(this)
        class(UnitList), intent(inout) :: this
        integer(8)                     :: ind
        integer(1)                     :: rc
        
        if (allocated(this%units)) call this%dropList()
            
        allocate(this%units(SIZE_INIT), stat = rc)
        if (rc /= 0) call displayDebug("Failed to allocate unit list!") 

        do ind = 1, size(this%units), 1
           this%units(ind)%name = ""
           if (associated(this%units(ind)%sp)) nullify(this%units(ind)%sp)
        end do

        call timerC%timerStart(PERFECT_WAIT)
        lastNum = 1        

    end subroutine 

    subroutine dropList(this)
        class(UnitList), intent(inout) :: this
        integer(8)                     :: ind
        integer(1)                     :: rc
        
        if (allocated(this%units)) then
            do ind = 1, size(this%units), 1
               call this%units(ind)%killMe() 
            end do 

            deallocate(this%units, stat = rc)
            if (rc /= 0) call displayDebug("Failed to deallocate unit list!")        

        end if 

    end subroutine 

    subroutine removeDeads(this)
        class(UnitList), intent(inout)            :: this
        integer(8)                                :: ind, ind2, newSiz
        integer(1)                                :: rc

        type(Unit), dimension(:), allocatable     :: tempList

        if (this%siz == 0) return

        allocate(tempList(this%siz), stat = rc)
        if (rc /= 0) call displayDebug("Failed to allocate temp Unit List!")
        
        ind2   = 0
        newSiz = this%siz

        do ind = 1, this%siz, 1
           call this%units(ind)%setPointer()
           if (associated(this%units(ind)%sp)) then
               ind2           = ind2 + 1 
               tempList(ind2) = this%units(ind) 
           else                 
               newSiz = newSiz - 1 
           end if

        end do

        this%siz = newSiz
        call move_alloc(tempList, this%units)

        do ind = this%siz + 1, size(this%units), 1
           call this%units(ind)%setPointer()
           if (associated(this%units(ind)%sp)) call this%units(ind)%killMe()
        end do

    end subroutine 

!
!   Map Stuff
!
    subroutine initAllUnitLists()
        call currentMap%floors%initList()
        call currentMap%realUnits%initList()

    end subroutine    

    subroutine dropAllUnitLists()
        call currentMap%floors%dropList()
        call currentMap%realUnits%dropList()

    end subroutine    

    subroutine addUnitToList(name, x, y, typ)
        integer(4)                            :: x, y
        character(*)                          :: name
        integer(1)                            :: typ

        select case(typ)
        case(TYPE_FLOOR)
            call currentMap%floors%addUnit(   name, x, y) 
        case default
            call currentMap%realUnits%addUnit(name, x, y) 
        end select

    end subroutine

    subroutine searchForDeadUnits()
        call currentMap%floors%removeDeads()
        call currentMap%realUnits%removeDeads()           
    end subroutine

    function openBasicSettingsWindow(load) result(success)
       logical                 :: load

       integer(1)              :: success
       INTEGER                 :: ITYPE, selectedFloor, selectedWeather, windVal, musicVal
       TYPE(WIN_MESSAGE)       :: MESSAGE
       integer(1)              :: num 
       integer(2)              :: ind 
       integer                 :: w, h, m 

       call initAllUnitLists() 

       success           = 0
       canKill           = .FALSE.
       currentMap%paused = .TRUE.
       placeActive       = .FALSE.

       do
         if (WInfoDialog(CurrentDialog) == 0) exit
         call sleep(1)
       end do 
       
       call getUnitNames(baseFloorList, TYPE_FLOOR, 1) 
       call getMusicList(musicList) 

       CALL WDialogLoad(IDD_MAP_BASICSETTINGS)
       CALL WDialogTitle(getWordInCurrentLang("basicSettings")) 

       CALL WDialogPutString(ID_MAP_BASICSETTINGS_OK, getWordInCurrentLang("ok")) 

       if (load) then
           CALL WDialogPutString(ID_MAP_BASICSETTINGS_Cancel, getWordInCurrentLang("load")) 
           currentMap%typ        = MAP_WALKING 
           currentMap%defWeather = WEATHER_DAY_NORM
           currentMap%width      = wOfScreenBuffer
           currentMap%height     = hOfScreenBuffer
           currentMap%wind       = .FALSE.           
           defaultFloor          = "Grass"
           currentMap%name       = map_default 
       else 
           CALL WDialogPutString(ID_MAP_BASICSETTINGS_Cancel, getWordInCurrentLang("cancel")) 
           defaultFloor          = currentMap%defaultFloor%sp%name
       end if
 
       mapTypeOld      = currentMap%typ 
       mapTypePrev     = currentMap%typ  
       weatherOld      = currentMap%defWeather  
       widthOld        = currentMap%width
       heightOld       = currentMap%height 
       windOld         = currentMap%wind 
       defaultFloorOld = defaultFloor 

       CALL WDialogPutString(ID_MAP_BASICSETTINGS_NAME_LABEL, getWordInCurrentLang("mapName")) 
       CALL WDialogPutString(ID_MAP_BASICSETTINGS_TYPE_LABEL, getWordInCurrentLang("mapType")) 

       CALL WDialogPutString(IDF_MAP_TYPE_RADIO1, getWordInCurrentLang("walking")) 
       CALL WDialogPutString(IDF_MAP_TYPE_RADIO2, getWordInCurrentLang("fighting")) 
       CALL WDialogPutString(IDF_MAP_TYPE_RADIO3, getWordInCurrentLang("watching")) 

       CALL WDialogPutString(ID_MAP_BASICSETTINGS_WSIZE_L, getWordInCurrentLang("width")) 
       CALL WDialogPutString(ID_MAP_BASICSETTINGS_HSIZE_L, getWordInCurrentLang("height")) 
       CALL WDialogPutString(IDF_MAP_WIND, getWordInCurrentLang("wind")) 

       CALL WDialogPutString(ID_MAP_BASICSETTINGS_FLOOR_L,   getWordInCurrentLang("defaultFloorType")) 
       CALL WDialogPutString(ID_MAP_BASICSETTINGS_WEATHER_L, getWordInCurrentLang("defaultWeather")) 
       CALL WDialogPutString(ID_MAP_BASICSETTINGS_MUSIC_L , getWordInCurrentLang("defaultMusic")) 
       CALL WDialogPutString(ID_MAP_BASICSETTINGS_MUSIC_C , getWordInCurrentLang("clear")) 

       call wDialogPutMenu(IDF_MAP_DEFAULT_FLOOR, baseFloorList, size(baseFloorList), 0)  
        
       do num = 1, size(weatherNames), 1
          weatherNames(num) = getWordInCurrentLang(weatherNamesKeys(num))
       end do 

       call wDialogPutMenu(IDF_MAP_DEFAULT_WEATHER, weatherNames, size(weatherNames), 0)  
       call wDialogPutMenu(IDF_MAP_DEFAULT_MUSIC  , musicList   , size(musicList   ), 0)  

       select case(mapTypeOld) 
       case(1) 
            call WDialogPutRadioButton(IDF_MAP_TYPE_RADIO1)
       case(2) 
            call WDialogPutRadioButton(IDF_MAP_TYPE_RADIO2)
       case(3) 
            call WDialogPutRadioButton(IDF_MAP_TYPE_RADIO3)
       end select  

       if (windOld) then  
           call WDialogPutCheckBox(IDF_MAP_WIND, 1)
       else
           call WDialogPutCheckBox(IDF_MAP_WIND, 0)
       end if 

       CALL WDialogPutString( ID_MAP_BASICSETTINGS_NAME, currentMap%name  )        
       CALL WDialogPutInteger(IDF_MAP_WSIZE            , currentMap%width   / wOfScreenBuffer)        
       CALL WDialogPutInteger(IDF_MAP_HSIZE            , currentMap%height  / hOfScreenBuffer)        

       do selectedFloor = 1, size(baseFloorList), 1
          if (baseFloorList(selectedFloor) == defaultFloor) exit   
       end do  

       selectedWeather = weatherOld
 
       call WDialogPutOption(IDF_MAP_DEFAULT_FLOOR  , selectedFloor) 
       call WDialogPutOption(IDF_MAP_DEFAULT_WEATHER, selectedWeather) 

       do ind = 1, size(musicList), 1 
          if (musicList(ind) == currentMap%defaultMusic) then 
              musicVal = ind
              exit    
          end if  
       end do 

       call WDialogPutOption(IDF_MAP_DEFAULT_MUSIC  , musicVal) 

       do
          CALL WDialogSelect(IDD_MAP_BASICSETTINGS)
          CALL WDialogShow(ITYPE=Modal)     
    
          if (WinfoDialog(CurrentDialog) == IDD_MAP_BASICSETTINGS) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     EXIT
                  CASE(ID_MAP_BASICSETTINGS_Cancel) 
                     EXIT
                  CASE(ID_MAP_BASICSETTINGS_OK)
                     success = 1
                     EXIT
                  CASE(ID_MAP_BASICSETTINGS_MUSIC_C)
                     call WDialogPutOption(IDF_MAP_DEFAULT_MUSIC, 0) 
                  END SELECT
              end if
       end do 

       if (success == 1) then  

           call WDialogGetRadioButton(IDF_MAP_TYPE_RADIO1, m)
           CALL Wdialoggetinteger(IDF_MAP_WSIZE, w)
           CALL Wdialoggetinteger(IDF_MAP_HSIZE, h)        
    
           w = w * wOfScreenBuffer
           h = h * hOfScreenBuffer
    
           currentMap%width  = w
           currentMap%height = h
           currentMap%typ    = m
    
           call WDialogGetMenu(IDF_MAP_DEFAULT_FLOOR  , selectedFloor) 
           call WDialogGetMenu(IDF_MAP_DEFAULT_WEATHER, selectedWeather) 
    
           currentMap%defWeather = selectedWeather    
    
           call WDialogGetCheckBox(IDF_MAP_WIND, windVal)
           CALL wDialogGetString(ID_MAP_BASICSETTINGS_NAME, currentMap%name) 

           if (windVal == 1) then
               currentMap%wind = .TRUE.
           else
               currentMap%wind = .FALSE.
           end if   

           if ((defaultFloorOld /= baseFloorList(selectedFloor)) .OR. (load)) then 
               if (load .EQV. .FALSE.) call currentMap%defaultFloor%killMe() 
               call currentMap%defaultFloor%setUnit(baseFloorList(selectedFloor), 1, 1, 1)
           end if 

           if (selectedWeather /= weatherOld .OR. (currentMap%wind .NEQV. windOld)) then
               select case(selectedWeather)
               case(WEATHER_DAY_NORM)    
                    currentMap%defFilter = NO_FILTER
               case(WEATHER_NIGHT_NORM)    
                    currentMap%defFilter = FILTER_BLUE
               case(WEATHER_DAY_RAIN)    
                    currentMap%defFilter = NO_FILTER
               case(WEATHER_NIGHT_RAIN)    
                    currentMap%defFilter = FILTER_BLUE
               end select

               call setWeather(selectedWeather, currentMap%wind)
           end if  

           if (currentMap%width /= widthOld .OR. currentMap%height /= heightOld .OR. (load)) then
               call currentMap%floors%removeOutsiders(   currentMap%width, currentMap%height)
               call currentMap%realUnits%removeOutsiders(currentMap%width, currentMap%height)

               call setSize(currentMap%width, currentMap%height) 
           end if 

           if (defaultFloorOld /= defaultFloor .AND. (load .EQV. .FALSE.)) then
               call currentMap%realUnits%removeByName(defaultFloor)
           end if   

           call WDialogGetMenu(IDF_MAP_DEFAULT_MUSIC, musicVal) 
           currentMap%defaultMusic = musicList(musicVal) 

       else 
           if (load) then 
  
           end if 
       end if 

       canKill = .TRUE.        

    end function

    subroutine mapBasicSettings()
        integer(4)                  :: mapType

        if (WinfoDialog(CurrentDialog) == IDD_MAP_BASICSETTINGS) then
            call WDialogGetRadioButton(IDF_MAP_TYPE_RADIO1, mapType)

            if (mapType /= mapTypePrev) then
                select case(mapType)
                case(MAP_WALKING)
                     CALL WDialogFieldState(IDF_MAP_WSIZE, ENABLED) 
                     CALL WDialogFieldState(IDF_MAP_HSIZE, ENABLED) 
                case default
                     CALL WDialogFieldState(IDF_MAP_WSIZE, DISABLED) 
                     CALL WDialogFieldState(IDF_MAP_HSIZE, DISABLED) 

                     CALL Wdialogputinteger(IDF_MAP_WSIZE, 1)
                     CALL Wdialogputinteger(IDF_MAP_HSIZE, 1)
                end select
            end if

            mapTypePrev = mapType
            
        end if

        if (canKill .EQV. .TRUE.) then 
            CALL WDialogUnLoad()
            canKill = .FALSE.
        end if


    end subroutine

    subroutine runGameLogic()
        if (WinfoDialog(CurrentDialog) == 0) then



        end if
    end subroutine

    subroutine doThingsOnMapEditor()
        if (WinfoDialog(CurrentDialog) == 0 .OR. placeActive) then
            if (getInGameControl(PRESS_LEFT )) call addToOffSet(-1 * stepOnMap ,              0 )
            if (getInGameControl(PRESS_RIGHT)) call addToOffSet(     stepOnMap ,              0 )
            if (getInGameControl(PRESS_UP   )) call addToOffSet( 0             , -1 * stepOnMap )
            if (getInGameControl(PRESS_DOWN )) call addToOffSet( 0             ,      stepOnMap )

        end if

    end subroutine

    subroutine closeMapEditor()
        call dropAllUnitLists()
        call eraseBuff()
    end subroutine

    subroutine FloorChooser()
       TYPE(WIN_MESSAGE)       :: MESSAGE
       integer                 :: windVal 

       canKill           = .FALSE.

       do
         if (WInfoDialog(CurrentDialog) == 0) exit
         call sleep(1)
       end do 
       

       call getUnitNames(floorList, TYPE_FLOOR, 0) 
       CALL WDialogLoad(IDD_ADD_FLOOR)
       CALL WDialogTitle(getWordInCurrentLang("addFloor")) 

       CALL WDialogPutString(ID_FloorSelect    , getWordInCurrentLang("select")) 
       CALL WDialogPutString(ID_FloorExit      , getWordInCurrentLang("exit")) 
       CALL WDialogPutString(IDF_NO_WEATHER_BOX, getWordInCurrentLang("noWeather")) 

       call wDialogPutMenu(IDF_FLOORBOX, floorList, size(floorList), 1)  

       do
          CALL WDialogSelect(IDD_ADD_FLOOR)
          CALL WDialogShow(ITYPE=Modal)     
    
          if (WinfoDialog(CurrentDialog) == IDD_ADD_FLOOR) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     EXIT
                  CASE(ID_FloorSelect) 
                     placeActive   = .TRUE.
                     placeTyp      = TYPE_FLOOR

                     call WMessageEnable(MouseButDown, Disabled)
                     call WMessageEnable(MouseButUp,   Disabled)

                     call WDialogGetCheckBox(IDF_NO_WEATHER_BOX, windVal)
                     if (windVal == 1) then   
                         weatherOld      = currentMap%defWeather  
                         call setWeather(weatherOld, .FALSE.)
                         placeFilter     = NO_FILTER
                     else
                         placeFilter = currentMap%defFilter
                     end if 

                     call doThePlacer()

                     if (windVal == 1) call setWeather(currentMap%defWeather, currentMap%wind)

                     call WMessageEnable(MouseButDown, ENABLED)
                     call WMessageEnable(MouseButUp,   ENABLED)

                     placeActive       = .FALSE.
                  CASE(ID_FloorExit)
                     EXIT
                  END SELECT
              end if
       end do 

       canKill = .TRUE.

    end subroutine

    subroutine doThePlacer()

        DO
            call WinSleep(10)
            call doThingsOnMapEditor()
            if (isAKeyPressed(BUTTON_ESC) .OR. isAKeyPressed(BUTTON_MOUSE_R)) exit

            call putSpritesOnBuffer() 
            call placerDraw()
            call buffer2Real()

            if (isAKeyPressed(BUTTON_ENTER) .OR. getInGameControl(PRESS_ATTACK)) then

                select case(placeTyp) 
                case(TYPE_FLOOR) 
               
                    call currentMap%floors%addUnit(placeObj%name, XonScreen + getOffSetX(), &
                                                   YonScreen + getOffSetY()) 
                end select

            end if

        END DO

    end subroutine

    subroutine placerDraw()
        USE IFWIN

        type(T_POINT)       :: mousePos
        integer(BOOL)       :: rc
        integer(HANDLE)     :: hWnd
        TYPE(WIN_MESSAGE)   :: MESSAGE

        integer             :: X , Y, iType
        real                :: rX, rY
        character(40)       :: text
        integer(2)          :: color
        integer(1)          :: filter 

        rc = GetCursorPos(mousePos)
        
        if (rc /= 0) then
            hWnd = GetActiveWindow()
            rc = ScreenToClient(hWnd, mousePos)
            call WMessagePeek(itype, message)

            if (rc /= 0) then
                X  = mousePos%x        
                Y  = mousePos%y 

                call IGrUnitsFromPixels(X, Y, rX, rY)   
                rY = 1.0 - rY

                if (rx >= 0.0 .AND. rx <= 1.0 .AND. rY >= 0.0 .AND. rY <= 1.0) then
                    
                   X = min(wOfScreenBuffer - 1, int(       rX  * wOfScreenBuffer))
                   Y = min(hOfScreenBuffer, int((1.0 - rY) * hOfScreenBuffer))

                   select case(placeTyp) 
                   case(TYPE_FLOOR) 
                       color  = 253 
                       filter = placeFilter

                       XOnScreen = int(X / 32) * 32 + modulo(getOffsetX(), 32) 
                       YOnScreen = int(Y / 32) * 32 + modulo(getOffsetY(), 32)  

                       call drawSpriteWithRectagle(XOnScreen, YOnScreen , &
                                                   color, 1, placeImg, placeFilter) 
                    
                   end select
      
                end if
  
            end if
        end if

    end subroutine

    subroutine drawSpriteWithRectagle(x, y, c, s, img, f)
        type(imageFile) , pointer :: img
        integer(4)                :: x, y
        integer(2)                :: c
        integer(1)                :: s, f

        if (timerC%timerEnded()) then
            call timerC%timerRestart()  
            lastNum = lastNum + 1

            if (lastNum > 8) lastNum = 1 
        end if

        call img%addToScreenBuffer(1, LAYER_FOREGROUND, x, y, f) 

      ! x, y, w, h, c, b, fill, s   
        call drawRectangle(x - s, y - s, img%img%width + (s*2), img%img%height + (s*2), changeRGB(c, &
                           flashing(lastNum), flashing(lastNum), flashing(lastNum)), &
                           LAYER_FOREGROUND, .FALSE., s)

    end subroutine

    subroutine floorAdderCheck()
        integer(4)                :: selectedFloor
        character(NAME_MAX_LEN)   :: selectedFloorName, spriteName
        type(objectData), pointer :: obj         
        type(imageFile) , pointer :: img

        if (WinfoDialog(CurrentDialog) == IDD_ADD_FLOOR .AND. (placeActive .EQV. .FALSE.)) then

           call putSpritesOnBuffer() 

           call WDialogGetMenu(IDF_FLOORBOX, selectedFloor) 
           selectedFloorName = floorList(selectedFloor)
 
           if (currentMap%defaultFloor%sp%name == selectedFloorName) then
               call WDialogFieldState(ID_FloorSelect, DISABLED) 
           else
               call WDialogFieldState(ID_FloorSelect, ENABLED) 
 
               call getGameObjByName(obj, selectedFloorName) 
               spriteName =  obj%getEditorSprite(currentMap%wind, .TRUE., "") 
               call getImageFileByName(img, spriteName) 

               call drawSpriteWithRectagle(9, 9, 253, 1, img, NO_FILTER)
               placeImg     => img 
               placeObjName =  selectedFloorName 
               placeObj     => obj 

           end if  
           call buffer2Real()

        end if

        if (canKill .EQV. .TRUE.) then 
            CALL WDialogUnLoad()
            canKill = .FALSE.
        end if
    end subroutine

END MODULE GameMap
