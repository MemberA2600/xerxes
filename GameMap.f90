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

    implicit none

    private
    public   :: initAllUnitLists, searchForDeadUnits, openBasicSettingsWindow, mapBasicSettings

    logical  :: canKill = .FALSE.
    character(NAME_MAX_LEN), dimension(:), allocatable :: baseFloorList 
    character(NAME_MAX_LEN), dimension(4), parameter   :: weatherNamesKeys = (/ & 
                                                       "dayNorm", "nightNorm", "dayRain","nightRain" /) 
    character(NAME_MAX_LEN), dimension(4)              :: weatherNames

    type Unit
         type(spritePoz), pointer      :: sp 

         contains                  

         procedure                     :: killMe  => killMe
         procedure                     :: setUnit => setUnit

    end type

    type UnitList
         integer(8)                            :: siz  
         type(Unit), dimension(:), allocatable :: units
        
         contains   

         procedure         initList    => initList
         procedure         dropList    => dropList 
         procedure         addUnit     => addUnit 
         procedure         removeDeads => removeDeads

    end type

    type Map
         character(4)                          :: header
         integer(1)                            :: nameLen, typ = MAP_WALKING, &
                                                  defWeather = WEATHER_DAY_NORM, &
                                                  defFilter  = NO_FILTER      
         character(NAME_MAX_LEN)               :: name 
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

        if (associated(this%sp)) call this%sp%killMe()
        !call currentMap%killUnit(this%ind)
    
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

    end subroutine

!
!   UnitList Stuff
 

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
               if (associated(this%units(ind)%sp)) call this%units(ind)%killMe()
            end do
        end if

        this%siz = this%siz + 1

        call this%units(this%siz)%setUnit(name, x, y, LAYER_PLAYGROUND)

    end subroutine

    subroutine initList(this)
        class(UnitList), intent(inout) :: this
        integer(8)                     :: ind
        integer(1)                     :: rc
        
        if (allocated(this%units)) call this%dropList()
            
        allocate(this%units(SIZE_INIT), stat = rc)
        if (rc /= 0) call displayDebug("Failed to allocate unit list!") 

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
       INTEGER                 :: ITYPE, selectedFloor, selectedWeather, windVal
       TYPE(WIN_MESSAGE)       :: MESSAGE
       integer(1)              :: num 

       integer                 :: w, h, m 

       success           = 0
       canKill           = .FALSE.
       currentMap%paused = .TRUE.

       do
         if (WInfoDialog(CurrentDialog) == 0) exit
         call sleep(1)
       end do 
       
       call getUnitNames(baseFloorList, TYPE_FLOOR, 1) 

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

       call wDialogPutMenu(IDF_MAP_DEFAULT_FLOOR, baseFloorList, size(baseFloorList), 0)  
        
       do num = 1, size(weatherNames), 1
          weatherNames(num) = getWordInCurrentLang(weatherNamesKeys(num))
       end do 

       call wDialogPutMenu(IDF_MAP_DEFAULT_WEATHER, weatherNames, size(weatherNames), 0)  

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

END MODULE GameMap
