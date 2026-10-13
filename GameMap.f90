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
    public   :: openBasicSettingsWindow, mapBasicSettings, initMapTimer, &
                runGameLogic, doThingsOnMapEditor, closeMapEditor, UnitChooser, UnitAdderCheck, &
                setSelection

    logical  :: canKill = .FALSE.
    character(NAME_MAX_LEN), dimension(:), allocatable :: baseFloorList, floorList, musicList, decoList, &
                                                          baseUnitList 
    character(NAME_MAX_LEN), dimension(4), parameter   :: weatherNamesKeys = (/ & 
                                                       "dayNorm", "nightNorm", "dayRain","nightRain" /) 
    character(NAME_MAX_LEN), dimension(4)              :: weatherNames

    integer(1), parameter :: typeNum = 3

    integer(1), parameter :: DIV_FLOOR = 32, DIV_UNIT = 8

    character(NAME_MAX_LEN), dimension(typeNum ), parameter :: typeNames =  &
                           (/ "floor", "floorDecoration", "commonUnit" /)

    integer(1)            :: selectedType

    integer(1), parameter                              :: stepOnMap = 10
    integer(2), parameter                              :: COLOR_GREEN = 253
    integer(1)                                         :: lastNum

    integer(1), parameter                              :: NO_SELECT          = 0, &
                                                          SELECT_FLOOR       = 1, &
                                                          SELECT_DECORATIONS = 2, &
                                                          SELECT_UNIT        = 3, &
                                                          SELECT_COMMON_UNIT = 3  
    

    integer(1)                                         :: selectMode = NO_SELECT

    type(counterTimer)                                 :: timerC

    logical                                            :: placeActive     = .FALSE., allowed
    character(NAME_MAX_LEN)                            :: placeObjName
    type(objectData), pointer                          :: placeObj         
    type(imageFile) , pointer                          :: placeImg
    integer(1)                                         :: placeTyp, placeFilter
    integer(4)                                         :: XOnScreen, YOnScreen 

    type Map
         character(4)                          :: header
         integer(1)                            :: nameLen, typ = MAP_WALKING, &
                                                  defWeather = WEATHER_DAY_NORM, &
                                                  defFilter  = NO_FILTER      
         character(NAME_MAX_LEN)               :: name, musicPlaying, defaultMusic 
         integer(4)                            :: width, height
         logical                               :: wind = .FALSE., paused = .TRUE.
            
    end type

    type(spritePoz), pointer                   :: selectedUnit
    type(map), target                          :: currentMap

    integer(1)                                 :: mapTypeOld, weatherOld, mapTypePrev
    integer(4)                                 :: widthOld, heightOld
    logical                                    :: windOld
    character(NAME_MAX_LEN)                    :: defaultFloorOld, defaultFloor

    contains
!
!   Unit Stuff
!
    subroutine setUnit(name, x, y, layerNum)
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
                                               x, y, currentMap%defFilter, obj%unique, obj%name)
            end if    
        case(TYPE_FLOOR_DECORATION)
            call createSpriteObjPlayGround(name, obj%getEditorSprite(currentMap%wind, .TRUE., ""), &
                                           x, y, currentMap%defFilter, obj%unique, obj%name)

        case(TYPE_TREE)
            call createSpriteObjPlayGround(name, obj%getEditorSprite(currentMap%wind, .TRUE., ""), &
                                           x, y, currentMap%defFilter, obj%unique, obj%name)
        case(TYPE_PLAYER_MAP)   
            call createSpriteObjPlayGround(name, obj%getEditorSprite(currentMap%wind, .TRUE., ""), &
                                           x, y, currentMap%defFilter, obj%unique, obj%name)
        end select

        !call displayDebug("Name: " // name)
        !call displayDebugNumTxt("X: ", x)
        !call displayDebugNumTxt("Y: ", y)
        !call displayDebugNumTxt("Size: ", layerBlocks(LAYER_PLAYGROUND)%NextIndexP)


    end subroutine

!
!   UnitList Stuff
 
    subroutine removeOutsiders(w, h)
        integer(1)                            :: b
        integer(4)                            :: w, h
        integer(8)                            :: ind
        logical                               :: was        

        do b = 1, layerNum, 1
           was = .FALSE. 

           do ind = 1, layerBlocks(b)%nextIndexP, 1 
              if ((layerBlocks(b)%pozList(ind)%xw > w .OR.layerBlocks(b)%pozList(ind)%yh > h) .AND. &
                 (layerBlocks(b)%pozList(ind)%dontCountMe .EQV. .FALSE.)) then 

                  call layerBlocks(b)%pozList(ind)%killMe(.FALSE.)  
                  was = .TRUE.  
              end if  
           end do 
           if (was) call reorderPoz(b) 

        end do

    end subroutine

    subroutine removeByName(n)
        character(*)                          :: n
        integer(8)                            :: ind
        integer(1)                            :: b
        logical                               :: was        
        
        do b = 1, layerNum, 1
           do ind = 1, layerBlocks(b)%nextIndexP, 1 
              if (layerBlocks(b)%pozList(ind)%name == n.AND. &
                 (layerBlocks(b)%pozList(ind)%dontCountMe .EQV. .FALSE.)) then 

                  call layerBlocks(b)%pozList(ind)%killMe(.FALSE.)  
                  was = .TRUE.  

              end if  
           end do 
           if (was) call reorderPoz(b) 

        end do

    end subroutine

!
!   Map Stuff
!

    subroutine initMapTimer()

        call timerC%timerStart(PERFECT_WAIT)

    end subroutine

    function openBasicSettingsWindow(load) result(success)
       logical                 :: load

       integer(1)              :: success
       INTEGER                 :: ITYPE, selectedFloor, selectedWeather, windVal, musicVal
       TYPE(WIN_MESSAGE)       :: MESSAGE
       integer(1)              :: num 
       integer(2)              :: ind 
       integer                 :: w, h, m 


       success           = 0
       canKill           = .FALSE.
       currentMap%paused = .TRUE.
       placeActive       = .FALSE.

       do
         if (WInfoDialog(CurrentDialog) == 0) exit
         call sleep(1)
       end do 
       
       call getUnitNames(baseFloorList, TYPE_FLOOR, COLLECT_BASEFLOOR) 
       call getMusicList(musicList) 

       CALL WDialogLoad(IDD_MAP_BASICSETTINGS)
       CALL WDialogTitle(getWordInCurrentLang("basicSettings")) 

       CALL WDialogPutString(ID_MAP_BASICSETTINGS_OK, getWordInCurrentLang("ok")) 

       selectMode            = NO_SELECT 
       nullify(selectedUnit) 

       if (load) then
           CALL WDialogPutString(ID_MAP_BASICSETTINGS_Cancel, getWordInCurrentLang("load")) 
           call dropAllUnits() 
           call initMapTimer() 

           currentMap%typ        = MAP_WALKING 
           currentMap%defWeather = WEATHER_DAY_NORM
           currentMap%width      = wOfScreenBuffer
           currentMap%height     = hOfScreenBuffer
           currentMap%wind       = .FALSE.           
           defaultFloor          = "Grass"
           currentMap%name       = map_default 
       else 
           CALL WDialogPutString(ID_MAP_BASICSETTINGS_Cancel, getWordInCurrentLang("cancel")) 
           defaultFloor          = layerBlocks(LAYER_BACKGROUND)%pozList(1)%name
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
       case(MAP_WALKING) 
            call WDialogPutRadioButton(IDF_MAP_TYPE_RADIO1)
       case(MAP_FIGHTING) 
            call WDialogPutRadioButton(IDF_MAP_TYPE_RADIO2)
       case(MAP_WATCHING) 
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
               if (load .EQV. .FALSE.) call layerBlocks(LAYER_BACKGROUND)%pozList(1)%killMe(.FALSE.) 
               call setUnit(baseFloorList(selectedFloor), 1, 1, LAYER_BACKGROUND)
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
               call removeOutsiders(currentMap%width, currentMap%height)

               call setSize(currentMap%width, currentMap%height) 
           end if 

           !if (defaultFloorOld /= defaultFloor .AND. (load .EQV. .FALSE.)) then
           !    call removeByName(defaultFloor)
           !end if   

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

        if (canKill .EQV. .TRUE.) then 
            CALL WDialogUnLoad()
            canKill = .FALSE.
        else
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
        end if

    end subroutine

    subroutine runGameLogic()
        if (WinfoDialog(CurrentDialog) == 0) then



        end if
    end subroutine

    subroutine doThingsOnMapEditor()
        real                :: rX, rY
        !logical             :: first

        if (WinfoDialog(CurrentDialog) == 0 .OR. placeActive) then
            if (getInGameControl(PRESS_LEFT )) call addToOffSet(-1 * stepOnMap ,              0 )
            if (getInGameControl(PRESS_RIGHT)) call addToOffSet(     stepOnMap ,              0 )
            if (getInGameControl(PRESS_UP   )) call addToOffSet( 0             , -1 * stepOnMap )
            if (getInGameControl(PRESS_DOWN )) call addToOffSet( 0             ,      stepOnMap )

            if ((placeActive .EQV. .FALSE.) .AND. selectMode > NO_SELECT) then
                 if (clicked()) then
                    nullify(selectedUnit)

                    call setRxRy(rX, rY, .FALSE.)   
                    if (rx >= 0.0 .AND. rx <= 1.0 .AND. rY >= 0.0 .AND. rY <= 1.0) then
                        !call displayDebugNum(selectMode)

                        select case(selectMode)
                        case(SELECT_FLOOR)
                            call setXYOnScreen(rX, rY, DIV_FLOOR) 
                            call setSelectedPointer(TYPE_FLOOR, LAYER_PLAYGROUND)
                        case(SELECT_DECORATIONS)
                            call setXYOnScreen(rX, rY, DIV_FLOOR) 
                            call setSelectedPointer(TYPE_FLOOR_DECORATION, LAYER_PLAYGROUND)
                        case(SELECT_UNIT)
                            call setXYOnScreen(rX, rY, DIV_UNIT) 
                            call setSelectedPointer(TYPE_COMMON_UNIT, LAYER_PLAYGROUND)
                        end select
                    end if
                 end if
                   
                 !first = .TRUE.   
                 if (associated(selectedUnit)) then
                     do 
                        call timerCT()  
                        call putSpritesOnBuffer() 
                        !if (first) then
                        !    first = .FALSE.
                        !    call displayDebug(selectedUnit%name)
                        !    call displayDebugNumTxt("X: ", selectedUnit%x                   - 1)
                        !    call displayDebugNumTxt("Y: ", selectedUnit%y                   - 1)
                        !    call displayDebugNumTxt("W: ", selectedUnit%xw - selectedUnit%x + 2)
                        !    call displayDebugNumTxt("H: ", selectedUnit%yh - selectedUnit%y + 2)
                        !end if

                      ! x, y, w, h, c, b, fill, s   
                         call drawRectangle(selectedUnit%x                       - 1, &
                                            selectedUnit%y                       - 1, &
                                            selectedUnit%xw - selectedUnit%x     + 2, &
                                            selectedUnit%yh - selectedUnit%y     + 2, &
                                            changeRGB(COLOR_GREEN,                                    &
                                            flashing(lastNum), flashing(lastNum), flashing(lastNum)), & 
                                            LAYER_FOREGROUND, .FALSE., 1)
                        call buffer2Real()
                        if (deleted()) then
                            call selectedUnit%killMe(.TRUE.)
                            nullify(selectedUnit)
                            !call displayDebugNum(layerBlocks(LAYER_PLAYGROUND)%NextIndexP)

                            exit
                        else if (cancelled()) then
                            nullify(selectedUnit)
                            exit
                        end if

                     end do
                end if
            else
                nullify(selectedUnit)
            end if
        end if

    end subroutine

    subroutine timerCT()
        if (timerC%timerEnded()) then
            call timerC%timerRestart()  
            lastNum = lastNum + 1
            
            if (lastNum > 8) lastNum = 1 
        end if
    end subroutine     

    subroutine setSelectedPointer(typ, b)
        integer(1)              :: typ, b
        integer(8)              :: ind

        nullifY(selectedUnit)

        do ind = 1, layerBlocks(b)%nextIndexP, 1
        
           if (xOnScreen >= layerBlocks(b)%pozList(ind)%x                    .AND. &
               yOnScreen >= layerBlocks(b)%pozList(ind)%y                    .AND. &
               xOnScreen <= layerBlocks(b)%pozList(ind)%xw                   .AND. &
               yOnScreen <= layerBlocks(b)%pozList(ind)%yh                   .AND. &
              (typ       == layerBlocks(b)%pozList(ind)%typFlag              .OR.  &
              (typ       == TYPE_COMMON_UNIT                                 .AND. & 
               layerBlocks(b)%pozList(ind)%typFlag /= TYPE_FLOOR             .AND. & 
               layerBlocks(b)%pozList(ind)%typFlag /= TYPE_FLOOR_DECORATION)).AND. &
              (layerBlocks(b)%pozList(ind)%dontCountMe .EQV. .FALSE.       )) then 
               selectedUnit => layerBlocks(b)%pozList(ind)
               return 
           end if 
        end do 
    
    end subroutine

    subroutine closeMapEditor()
        call dropAllUnits()
        call eraseBuff()
    end subroutine

    subroutine UnitChooser()
       TYPE(WIN_MESSAGE)       :: MESSAGE
       integer                 :: windVal 
       character(NAME_MAX_LEN), dimension(typeNum) :: typeNamesTrans
       integer(1)              :: num 

       canKill           = .FALSE.

       do
         if (WInfoDialog(CurrentDialog) == 0) exit
         call sleep(1)
       end do 
       
       do num = 1, typeNum, 1
          typeNamesTrans(num) = getWordInCurrentLang(typeNames(num))  
       end do 

       selectMode            = NO_SELECT 
       nullify(selectedUnit) 
       selectedType          = 1

       call getUnitNames(floorList    , TYPE_FLOOR           , COLLECT_NORMAL   ) 
       call getUnitNames(decoList     , TYPE_FLOOR_DECORATION, COLLECT_NORMAL   ) 
       call getUnitNames(baseUnitList , TYPE_EMPTY           , COLLECT_LEFTOVERS)
 
       CALL WDialogLoad(IDD_ADD_UNIT)
       CALL WDialogTitle(getWordInCurrentLang("addUnit")) 

       CALL WDialogPutString(ID_FloorSelect    , getWordInCurrentLang("select")) 
       CALL WDialogPutString(ID_FloorExit      , getWordInCurrentLang("exit")) 
       CALL WDialogPutString(IDF_NO_WEATHER_BOX, getWordInCurrentLang("noWeather")) 

       call wDialogPutMenu(IDF_UNITBOX, floorList, size(floorList), 1)  
       call wDialogPutMenu(IDF_UNITTYPES , typeNamesTrans, size(typeNames)  , 1)  

       do
          CALL WDialogSelect(IDD_ADD_UNIT)
          CALL WDialogShow(ITYPE=Modal)     
    
          if (WinfoDialog(CurrentDialog) == IDD_ADD_UNIT) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     EXIT
                  CASE(ID_FloorSelect) 
                     placeActive   = .TRUE.

                     select case(selectedType)
                     case(SELECT_FLOOR)
                        placeTyp   = TYPE_FLOOR
                     case(SELECT_DECORATIONS)   
                        placeTyp   = TYPE_FLOOR_DECORATION
                     case default
                        placeTyp   = TYPE_COMMON_UNIT
                     end select

                     call WMessageEnable(MouseButDown, Disabled)
                     call WMessageEnable(MouseButUp,   Disabled)

                     call WDialogGetCheckBox(IDF_NO_WEATHER_BOX, windVal)
                     if (windVal == 1) then   
                         !weatherOld      = currentMap%defWeather  
                         call setWeather(WEATHER_DAY_NORM, .FALSE.)
                         placeFilter     = NO_FILTER
                     else
                         placeFilter = currentMap%defFilter
                     end if 

                     call WMenuSetState(ID_MAPEDITOR , ItemEnabled, 0)  
                     call WMenuSetState(ID_SoundInput, ItemEnabled, 0)  

                     call doThePlacer()

                     if (windVal == 1) call setWeather(currentMap%defWeather, currentMap%wind)

                     call WMessageEnable(MouseButDown, ENABLED)
                     call WMessageEnable(MouseButUp,   ENABLED)
                     call WMenuSetState(ID_MAPEDITOR , ItemEnabled, 1)  
                     call WMenuSetState(ID_SoundInput, ItemEnabled, 1)  

                     placeActive       = .FALSE.
                  CASE(ID_FloorExit)
                     EXIT
                  END SELECT
              end if
       end do 

       canKill = .TRUE.
       
       select case(selectedType) 
       case(SELECT_FLOOR)
           selectMode = SELECT_FLOOR
       case(SELECT_DECORATIONS)   
           selectMode = SELECT_DECORATIONS
       case default 
           selectMode = SELECT_UNIT 
       end select

    end subroutine

    function clicked() result(r)
        logical :: r
        
        r = (isAKeyPressed(BUTTON_ENTER)    .OR.&
             getInGameControl(PRESS_ATTACK) .OR.&
             isAKeyPressed(BUTTON_MOUSE_L))
    end function

    function cancelled() result(r)
        logical :: r       

        r = (isAKeyPressed(BUTTON_ESC) .OR. isAKeyPressed(BUTTON_MOUSE_R))
    end function

    function deleted() result(r)
        logical :: r       

        r = (isAKeyPressed(BUTTON_DELETE))
    end function

    subroutine doThePlacer()
        integer         :: lastX, lastY

        lastX = -1
        lastY = -1

        DO
            call WinSleep(10)
            call doThingsOnMapEditor()
            if (cancelled()) exit

            call putSpritesOnBuffer() 
            call placerDraw()
            call buffer2Real()

            call readinput()

            if (clicked() .AND. (lastX /= XonScreen .OR. lastY /= YonScreen)) then

                select case(placeTyp) 
                case(TYPE_FLOOR) 
               
                    call deleteAllOnXY(XonScreen + getOffSetX(), &
                                       YonScreen + getOffSetY(), TYPE_FLOOR           , LAYER_PLAYGROUND)

                    call deleteAllOnXY(XonScreen + getOffSetX(), &
                                       YonScreen + getOffSetY(), TYPE_FLOOR_DECORATION, LAYER_PLAYGROUND)

                    call setUnit(placeObj%name, XonScreen + getOffSetX(), & 
                                 YonScreen + getOffSetY(), LAYER_PLAYGROUND) 

                case(TYPE_FLOOR_DECORATION) 

                    call deleteAllOnXY(XonScreen + getOffSetX(), &
                                       YonScreen + getOffSetY(), TYPE_FLOOR_DECORATION, LAYER_PLAYGROUND)

                    call setUnit(placeObj%name, XonScreen + getOffSetX(), & 
                                 YonScreen + getOffSetY(), LAYER_PLAYGROUND) 

                case(TYPE_COMMON_UNIT) 

                    call setUnit(placeObj%name, XonScreen + getOffSetX(), & 
                                 YonScreen + getOffSetY(), LAYER_PLAYGROUND) 

                end select

                lastX = XonScreen 
                lastY = YonScreen 

            end if

        END DO

    end subroutine

    subroutine deleteAllOnXY(x, y, typ, b)
        integer(4)          :: x, y
        integer(8)          :: ind
        integer(1)          :: b, typ

        do ind = 1, layerBlocks(b)%nextIndexP, 1 
           if (layerBlocks(b)%pozList(ind)%x == x .AND.layerBlocks(b)%pozList(ind)%y == y .AND. &
               layerBlocks(b)%pozList(ind)%typFlag == typ                                 .AND. &
              (layerBlocks(b)%pozList(ind)%dontCountMe .EQV. .FALSE.)) then 
               call layerBlocks(b)%pozList(ind)%killMe(.TRUE.)  
               exit 
           end if  
        end do 
   
    end subroutine    

    subroutine setRxRy(rx, ry, peek)
        USE IFWIN

        type(T_POINT)       :: mousePos
        integer(BOOL)       :: rc
        integer(HANDLE)     :: hWnd
        TYPE(WIN_MESSAGE)   :: MESSAGE
        logical             :: peek

        integer             :: X , Y, iType
        real, intent(out)   :: rX, rY

        rX = -1.0
        rY = -1.0

        rc = GetCursorPos(mousePos)
        
        if (rc /= 0) then
            hWnd = GetActiveWindow()
            rc = ScreenToClient(hWnd, mousePos)
            if (peek) call WMessagePeek(itype, message)

            if (rc /= 0) then
                X  = mousePos%x        
                Y  = mousePos%y 

                call IGrUnitsFromPixels(X, Y, rX, rY)   
                rY = 1.0 - rY
  
            end if
        end if
    end subroutine

    subroutine setXYOnScreen(rX, rY, divider)
        real                :: rX, rY
        integer             :: X , Y
        integer(2)          :: divider

        X = min(wOfScreenBuffer - 1, int(   rX  * wOfScreenBuffer))
        Y = min(hOfScreenBuffer, int((1.0 - rY) * hOfScreenBuffer))

        if (divider > 1) then
            XOnScreen = int(X / divider) * divider + modulo(getOffsetX(), divider) 
            YOnScreen = int(Y / divider) * divider + modulo(getOffsetY(), divider)  
        else
            XOnScreen = X + getOffsetX()
            YOnScreen = Y + getOffsetX()
        end if
    end subroutine

    subroutine placerDraw()
        real                :: rX, rY
        integer(2)          :: color
        integer(1)          :: filter 

        call setRxRy(rX, rY, .TRUE.)
        
        if (rx >= 0.0 .AND. rx <= 1.0 .AND. rY >= 0.0 .AND. rY <= 1.0) then
              
           select case(placeTyp) 
           case(TYPE_FLOOR) 
               color  = COLOR_GREEN
               filter = placeFilter

               call setXYOnScreen(rX, rY, 32) 

               call drawSpriteWithRectagle(XOnScreen, YOnScreen , &
                                                   color, 1, placeImg, filter )    
           case(TYPE_FLOOR_DECORATION) 
               color  = COLOR_GREEN
               filter = placeFilter

               call setXYOnScreen(rX, rY, 32) 

               call drawSpriteWithRectagle(XOnScreen, YOnScreen , &
                                                   color, 1, placeImg, filter )    
           case(TYPE_COMMON_UNIT) 
               color  = COLOR_GREEN
               filter = placeFilter

               call setXYOnScreen(rX, rY, 32) 

               call drawSpriteWithRectagle(XOnScreen, YOnScreen , &
                                                   color, 1, placeImg, filter )  
    
           end select
        end if
  
    end subroutine

    subroutine drawSpriteWithRectagle(x, y, c, s, img, f)
        type(imageFile) , pointer :: img
        integer(4)                :: x, y
        integer(2)                :: c
        integer(1)                :: s, f

        call timerCT()
        call img%addToScreenBuffer(1, LAYER_FOREGROUND, x, y, f) 

      ! x, y, w, h, c, b, fill, s   
        call drawRectangle(x - s, y - s, img%img%width + (s*2), img%img%height + (s*2), changeRGB(c, &
                           flashing(lastNum), flashing(lastNum), flashing(lastNum)), &
                           LAYER_FOREGROUND, .FALSE., s)

    end subroutine

    subroutine UnitAdderCheck()
        integer(4)                :: selectedUnit
        character(NAME_MAX_LEN)   :: selectedUnitName, spriteName
        type(objectData), pointer :: obj         
        type(imageFile) , pointer :: img
        integer(4)                :: selectedTypeTemp 

        if (canKill .EQV. .TRUE.) then 
            CALL WDialogUnLoad()
            canKill = .FALSE.
        else
           if (WinfoDialog(CurrentDialog) == IDD_ADD_UNIT .AND. (placeActive .EQV. .FALSE.)) then

               call WDialogGetMenu(IDF_UNITTYPES, selectedTypeTemp) 

               if (selectedTypeTemp /= selectedType) then
                   selectedType = selectedTypeTemp 
                   
                   select case(selectedType)
                   case(SELECT_FLOOR)   
                      call wDialogPutMenu(IDF_UNITBOX, floorList, size(floorList), 1)  
                      selectMode = SELECT_FLOOR  
             
                   case(SELECT_DECORATIONS)   
                      call wDialogPutMenu(IDF_UNITBOX, decoList, size(decoList), 1)  
                      selectMode = SELECT_DECORATIONS

                   case(SELECT_COMMON_UNIT)   
                      call wDialogPutMenu(IDF_UNITBOX, baseUnitList, size(baseUnitList), 1)  
                      selectMode = SELECT_UNIT

                   end select 
               end if 

               call putSpritesOnBuffer() 
    
               call WDialogGetMenu(IDF_UNITBOX, selectedUnit) 

               select case(selectedType)
               case(SELECT_FLOOR)   
                    selectedUnitName = floorList(selectedUnit)
               case(SELECT_DECORATIONS)   
                    selectedUnitName = decoList(selectedUnit)
               case(SELECT_COMMON_UNIT)   
                    selectedUnitName = baseUnitList(selectedUnit)
               end select 
     
               if (layerBlocks(LAYER_BACKGROUND)%pozList(1)%name == selectedUnitName) then
                   call WDialogFieldState(ID_FloorSelect, DISABLED) 
               else
                   call WDialogFieldState(ID_FloorSelect, ENABLED) 
     
                   call getGameObjByName(obj, selectedUnitName) 
                   spriteName =  obj%getEditorSprite(currentMap%wind, .TRUE., "") 
                   call getImageFileByName(img, spriteName) 
                  
                   call drawSpriteWithRectagle(9, 9, COLOR_GREEN, 1, img, NO_FILTER)
                   placeImg     => img 
                   placeObjName =  selectedUnitName
                   placeObj     => obj 
    
               end if  
               call buffer2Real()
            end if

        end if
    end subroutine

    subroutine setSelection(s)
        integer(1)              :: s

        selectMode = s

    end subroutine

END MODULE GameMap
