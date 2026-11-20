MODULE TriggerAction

    USE, INTRINSIC :: ISO_C_BINDING
    USE debugWindow
    USE dataLoader
    USE WINTERACTER
    USE RESID
    USE subs
    USE engineConstants
    USE inpout
    USE dict

    implicit none
    
    private
    public                          :: triggerGroup, actionGroup, trigger, action, &
                                       removeTriggersAndActions, triggers, actions, &
                                       triggerActionWindow, triggerActionLoop   

    type trigger
         integer(kind=1)            :: typ
         logical                    :: or
         character(MAX_PARAM_LEN)   :: paramList   

         contains

         procedure :: generateFullTextT           => generateFullTextT

    end type

    type triggerGroup
        character(NAME_MAX_LEN)                  :: name, actionName  
        type(trigger), dimension(:), allocatable :: triggerList
        contains
        
        procedure :: dropTriggerList             => dropTriggerList
        procedure :: addNewTrigger               => addNewTrigger  
        procedure :: removeTrigger               => removeTrigger   
        procedure :: swapTriggers                => swapTriggers   
        procedure :: collectNamesT               => collectNamesT         
    end type

    integer(1)                      :: actionOrTrigger = TRIGGER_MODE, optionIndexOffset = 0
    logical                         :: canKill    

    type action
         integer(kind=1)            :: typ
         character(MAX_PARAM_LEN)   :: paramList  

         contains
         procedure :: generateFullTextA          => generateFullTextA     

    end type

    type actionGroup
        character(NAME_MAX_LEN)                 :: name
        type(action), dimension(:), allocatable :: actionList

        contains
        
        procedure :: dropACtionList              => dropActionList 
        procedure :: addNewAction                => addNewAction  
        procedure :: removeAction                => removeAction
        procedure :: swapActions                 => swapActions            
        procedure :: collectNamesA               => collectNamesA   
    end type

    type(triggerGroup), dimension(:), allocatable :: triggers
    type( actionGroup), dimension(:), allocatable :: actions    

    integer                                       :: selectedTriggerGroup, selectedActionGroup, &
                                                     selectedTrigger     , selectedAction

    character(NAME_MAX_LEN) , dimension(:), allocatable :: groupList
    character(MAX_PARAM_LEN), dimension(:), allocatable :: itemList 

    contains

    subroutine swapTriggers(this, s, n)
           class(triggerGroup), intent(inout) :: this 
           integer(4)                         :: s, n
           type(trigger)                      :: temp 

           if (s + n < 1 .OR. s + n > size(this%triggerList) &
          .OR.     s < 1 .OR.     s > size(this%triggerList)) return

           temp                     = this%triggerList(s)
           this%triggerList(s + n)  = this%triggerList(s) 
           this%triggerList(s)      = temp 

    end subroutine

    subroutine swapActions(this, s, n)
           class(actionGroup), intent(inout) :: this 
           integer(4)                        :: s, n
           type(action)                      :: temp 

           if (s + n < 1 .OR. s + n > size(this%actionList) &
          .OR.     s < 1 .OR.     s > size(this%actionList)) return

           temp                     = this%actionList(s)
           this%actionList(s + n)   = this%actionList(s) 
           this%actionList(s)       = temp 

    end subroutine

    subroutine dropTriggerList(this)    
           class(triggerGroup), intent(inout) :: this 
           integer(1)                         :: rc 
  
           if (allocated(this%triggerList)) then
               deallocate(this%triggerList, stat=rc)
               if (rc /= 0) call displayDebug("Failed to deallocate triggerList!")
           end if

           selectedTrigger = 0 

    end subroutine

    subroutine dropActionList(this)    
           class(actionGroup), intent(inout)  :: this 
           integer(1)                         :: rc 
  
           if (allocated(this%actionList)) then
               deallocate(this%actionList, stat=rc)
               if (rc /= 0) call displayDebug("Failed to deallocate actionList!")
           end if

           selectedAction = 0 

    end subroutine

    subroutine addNewTrigger(this, t)
           class(triggerGroup), intent(inout)       :: this 
           type(trigger)                            :: t 
           type(trigger), dimension(:), allocatable :: temp 
           integer(1)                               :: rc 
           
           if (allocated(this%triggerList)) then  
               allocate(temp(size(this%triggerList) + 1), stat = rc) 
                
               if (rc /= 0) call displayDebug("Failed to allocate temp TriggerList!") 
    
               temp(1:size(this%triggerList)) = this%triggerList 
    
               call move_alloc(temp, this%triggerList)
           else
               allocate(this%triggerList(1), stat = rc) 
               if (rc /= 0) call displayDebug("Failed to allocate TriggerList!") 

           end if 
         
           this%triggerList(size(this%triggerList)) = t 
           selectedTrigger = size(this%triggerList)

    end subroutine

    subroutine addNewAction(this, a)
           class(actionGroup), intent(inout)       :: this 
           type(action)                            :: a 
           type(action), dimension(:), allocatable :: temp 
           integer(1)                              :: rc 
            
           if (allocated(this%actionList)) then  
               allocate(temp(size(this%actionList) + 1), stat = rc) 
                
               if (rc /= 0) call displayDebug("Failed to allocate temp ActionList!") 
    
               temp(1:size(this%actionList)) = this%actionList
               call move_alloc(temp, this%actionList)
           else
               allocate(this%actionList(1), stat = rc) 
               if (rc /= 0) call displayDebug("Failed to allocate ActionList!") 

           end if 
             
           this%actionList(size(this%actionList)) = a 
           selectedAction = size(this%actionList)
 
    end subroutine

    subroutine removeTrigger(this, n)
           class(triggerGroup), intent(inout)       :: this 
           type(trigger), dimension(:), allocatable :: temp 
           integer(1)                               :: rc 
           integer                                  :: n         

           if (.not. allocated(this%triggerList))     return
           if (n < 1 .or. n > size(this%triggerList)) return

           if (size(this%triggerList) > 1) then 
               allocate(temp(size(this%triggerList) - 1), stat = rc) 
                
               if (rc /= 0) call displayDebug("Failed to allocate temp TriggerList!") 
               temp(1:n-1)        = this%triggerList(1  :n-1)  
               temp(n:size(temp)) = this%triggerList(n+1:size(this%triggerList))  
    
               call move_alloc(temp, this%triggerList)
           else 
               deallocate(this%triggerList, stat = rc)
               if (rc /= 0) call displayDebug("Failed to deallocate TriggerList!") 
           end if 

           if (n > size(this%triggerList)) then
               selectedTrigger = size(this%triggerList)
           else
               selectedTrigger = n
           end if

    end subroutine

    subroutine removeAction(this, n)
           class(actionGroup), intent(inout)       :: this 
           type(action), dimension(:), allocatable :: temp 
           integer(1)                              :: rc 
           integer                                 :: n    
     
           if (.not. allocated(this%actionList))     return
           if (n < 1 .or. n > size(this%actionList)) return

           if (size(this%actionList) > 1) then 
               allocate(temp(size(this%actionList) - 1), stat = rc) 
                
               if (rc /= 0) call displayDebug("Failed to allocate temp ActionList!") 
               temp(1:n-1)        = this%actionList(1  :n-1)  
               temp(n:size(temp)) = this%actionList(n+1:size(this%actionList))  
    
               call move_alloc(temp, this%actionList)
           else 
               deallocate(this%actionList, stat = rc)
               if (rc /= 0) call displayDebug("Failed to deallocate ActionList!") 
           end if 

           if (n > size(this%actionList)) then
               selectedAction = size(this%actionList)
           else
               selectedAction = n
           end if
    end subroutine

    subroutine removeTriggersAndActions()
        integer(1)           :: rc
        integer              :: num

        if (allocated(triggers)) then
            do num = 1, size(triggers)
                call triggers(num)%dropTriggerList()
            end do
    
            deallocate(triggers, stat=rc)
            if (rc /= 0) call displayDebug("Failed to deallocate triggers!")
        end if
    
        if (allocated(actions)) then
            do num = 1, size(actions)
                call actions(num)%dropActionList()
            end do
    
            deallocate(actions, stat=rc)
            if (rc /= 0) call displayDebug("Failed to deallocate actions!")
        end if

        selectedTriggerGroup = 0
        selectedActionGroup  = 0
        selectedTrigger      = 0
        selectedAction       = 0
        
    end subroutine

    subroutine addNewTriggerGroup(tg)
        integer(1)                                    :: rc
        type(triggerGroup)                            :: tg           
        type(triggerGroup), dimension(:), allocatable :: temp

        if (allocated(triggers)) then
           allocate(temp(size(triggers) + 1), stat = rc) 
           if (rc /= 0) call displayDebug("Failed to allocate temp triggerGroup!")
        
           temp(1:size(triggers)) = triggers(1:size(triggers)) 
           call move_alloc(temp, triggers) 
        else
           allocate(triggers(1), stat = rc)     
           if (rc /= 0) call displayDebug("Failed to allocate the triggerGroup!")
        end if

        triggers(size(triggers)) = tg 

        selectedTriggerGroup = size(triggers)
        selectedTrigger      = size(triggers(selectedTriggerGroup)%triggerList)

    end subroutine 

    subroutine addNewActionGroup(ag)
        integer(1)                                    :: rc
        type(actionGroup)                             :: ag           
        type(actionGroup), dimension(:), allocatable  :: temp

        if (allocated(actions)) then
           allocate(temp(size(actions) + 1), stat = rc) 
           if (rc /= 0) call displayDebug("Failed to allocate temp actionGroup!")
        
           temp(1:size(actions)) = actions(1:size(actions))
           call move_alloc(temp, actions) 
        else
           allocate(actions(1), stat = rc)     
           if (rc /= 0) call displayDebug("Failed to allocate the actionGroup!")
        end if

        actions(size(actions)) = ag 

        selectedActionGroup = size(actions)
        selectedAction      = size(actions(selectedActionGroup)%actionList)

    end subroutine

    subroutine removeTriggerGroup(n)
        integer                                       :: n
        integer(1)                                    :: rc
        type(triggerGroup), dimension(:), allocatable :: temp

        if (size(triggers) > 1) then
            allocate(temp(size(triggers) - 1), stat = rc)
            if (rc /= 0) call displayDebug("Failed to allocate the temp triggerGroup!")          
            
            temp(1:n - 1)              = triggers(1     :n - 1)   
            temp(n:size(triggers) - 1) = triggers(n + 1 : size(triggers))

            call move_alloc(temp, triggers)
        else
            deallocate(triggers, stat = rc)
            if (rc /= 0) call displayDebug("Failed to deallocate the triggerGroup!")          

        end if

        if (n > size(triggers)) then
            selectedTriggerGroup = size(triggers)
        else
            selectedTriggerGroup = n
        end if
        selectedTrigger      = size(triggers(selectedTriggerGroup)%triggerList)

    end subroutine

    subroutine removeActionGroup(n)
        integer                                       :: n
        integer(1)                                    :: rc
        type(actionGroup), dimension(:), allocatable  :: temp

        if (size(actions) > 1) then
            allocate(temp(size(actions) - 1), stat = rc)
            if (rc /= 0) call displayDebug("Failed to allocate the temp actionGroup!")          
            
            temp(1:n - 1)             = actions(1     :n - 1)   
            temp(n:size(actions) - 1) = actions(n + 1 : size(actions))

            call move_alloc(temp, actions)
        else
            deallocate(actions, stat = rc)
            if (rc /= 0) call displayDebug("Failed to deallocate the actionGroup!")          

        end if

        if (n > size(actions)) then
            selectedActionGroup = size(actions)
        else
            selectedActionGroup = n
        end if
        selectedAction      = size(actions(selectedActionGroup)%actionList)

    end subroutine

    subroutine triggerActionWindow(at)
       INTEGER                                            :: ITYPE, rc
       Integer(1)                                         :: at
       TYPE(WIN_MESSAGE)                                  :: MESSAGE
       logical                                            :: change 

       actionOrTrigger = at

88996  do
         if (WInfoDialog(CurrentDialog) == 0) exit
         call sleep(1)
       end do 
       canKill         = .FALSE.

       CALL WDialogLoad(IDD_Triggers)

       call SetTheFieldNames() 
       call collectAllGroupNames(actionOrTrigger , groupList, 1) 
       call setFieldStates() 

       if (actionOrTrigger == TRIGGER_MODE) then 
           if (selectedTrigger /= 0) call triggers(selectedTrigger)%collectNamesT(itemList, 1)
           if (selectedAction  /= 0) call actions (selectedTrigger)%collectNamesA(itemList, 1)
       end if

       do
          change = .FALSE.
          CALL WDialogSelect(IDD_Triggers)
          CALL WDialogShow(ITYPE=Modal)     

          if (WinfoDialog(CurrentDialog) == IDD_Triggers) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     EXIT
                  CASE(ID_TriggerOK)
                     EXIT   
                  CASE(ID_TriggerChange)
                     change = .TRUE.
                     EXIT   
                  END SELECT
              end if
       end do 

       canKill = .TRUE. 

       if (change) then 
           actionOrTrigger = 1 - actionOrTrigger 
           goto 88996
       end if   

    END SUBROUTINE

    subroutine collectAllGroupNames(at, groupL, selectThis)
        character(NAME_MAX_LEN), dimension(:), allocatable, intent(inout) :: groupL
        integer(1)                                                        :: rc, at
        integer                                                           :: num, selectThis

        if (allocated(groupL)) then
            deallocate (groupL, stat = rc)
            if (rc /= 0) call displayDebug("Failed to deallocate Group!")
        end if
        
        if (at == TRIGGER_MODE .AND. size(triggers) == 0) then
            selectedTriggerGroup = 0
            return
        end if    

        if (at == ACTION_MODE  .AND. size(actions ) == 0) then
            selectedActionGroup = 0
            return
        end if    

        if (at == TRIGGER_MODE) then
            allocate(groupL(size(triggers)), stat = rc)
            if (rc /= 0) call displayDebug("Failed to allocate Group as TriggerGroup!")

            do num = 1, size(triggers), 1
               groupL(num) = triggers(num)%name 
            end do

            if      (selectThis < 1) then
                     selectedTriggerGroup = 1
            else if (selectThis > size(triggers)) then
                     selectedTriggerGroup = size(triggers)   
            else
                     selectedTriggerGroup =  selectThis 
            end if
        else    
            allocate(groupL(size(actions)), stat = rc)
            if (rc /= 0) call displayDebug("Failed to allocate Group as ActionGroup!")

            do num = 1, size(actions), 1
               groupL(num) = actions(num)%name 
            end do

            if      (selectThis < 1) then
                     selectedActionGroup = 1
            else if (selectThis > size(actions)) then
                     selectedActionGroup = size(actions)   
            else
                     selectedActionGroup =  selectThis 
            end if

        end if

    end subroutine

    subroutine setFieldStates()
        integer(1)               :: changer
        integer(4)               :: num
        character(MAX_PARAM_LEN) :: tempString

        if (size(groupList) > 0) then
            call WDialogFieldState(ID_TriggerDeleteGroup, ENABLED)
        else
            call WDialogFieldState(ID_TriggerDeleteGroup, DISABLED)
        end if

        call WDialogGetString(ID_TriggerName, tempString)

        changer = ENABLED
        do num = 1, size(groupList), 1
           if (tempString == groupList(num)) then 
               changer = DISABLED 
               exit 
           end if 
        end do

        call WDialogFieldState(ID_TriggerAddGroup   ,     changer)
        call WDialogFieldState(ID_TriggerUpdateGroup, 1 - changer)

        if ((selectedTrigger == "" .AND. actionOrTrigger == TRIGGER_MODE)  .OR. &
            (selectedAction  == "" .AND. actionOrTrigger == ACTION_MODE )) then
            call WDialogFieldState(ID_TriggerDeleteGroup, DISABLED)
            call WDialogFieldState(ID_TriggerUpdateGroup, DISABLED)
        end if
  
    end subroutine

    subroutine collectNamesT(this, groupL, selectThis)
        class(triggerGroup), intent(inout)                                :: this
        character(MAX_PARAM_LEN), dimension(:), allocatable, intent(inout) :: groupL
        integer(1)                                                        :: rc
        integer                                                           :: num, selectThis        

        if (allocated(groupL)) then
            deallocate (groupL, stat = rc)
            if (rc /= 0) call displayDebug("Failed to deallocate Group for TriggerList!")
        end if

        if (size(this%triggerList) == 0) then
            selectedTrigger = 0
            return
        end if 

        allocate(groupL(size(this%triggerList)), stat = rc)
        if (rc /= 0) call displayDebug("Failed to allocate Group for TriggerList!")

        do num = 1, size(this%triggerList), 1 
           groupL(num) = this%triggerList(num)%generateFullTextT() 
        end do
    
        selectedTrigger = 1    

    end subroutine

    subroutine collectNamesA(this, groupL, selectThis)
        class(ActionGroup), intent(inout)                                 :: this
        character(MAX_PARAM_LEN), dimension(:), allocatable, intent(inout) :: groupL
        integer(1)                                                        :: rc
        integer                                                           :: num, selectThis        

        if (allocated(groupL)) then
            deallocate (groupL, stat = rc)
            if (rc /= 0) call displayDebug("Failed to deallocate Group for ACtionList!")
        end if

        if (size(this%actionList) == 0) then            
            selectedAction = 0
            return
        end if 

        allocate(groupL(size(this%actionList)), stat = rc)
        if (rc /= 0) call displayDebug("Failed to allocate Group for ActionList!")

        do num = 1, size(this%actionList), 1 
           groupL(num) = this%actionList(num)%generateFullTextA() 
        end do
    
        selectedAction = 1   

    end subroutine

    SUBROUTINE SetTheFieldNames()
       if (actionOrTrigger == TRIGGER_MODE) then
           CALL WDialogPutString(ID_TriggerName, TRIGGER_DEFAULT)  
           CALL WDialogTitle(getWordInCurrentLang("manageTriggers")) 
           CALL WDialogPutString(IDF_TriggerChooser_L, getWordInCurrentLang("triggerGroupChooser")) 

           CALL WDialogPutString(IDF_TriggeName_L     , getWordInCurrentLang("triggerGroupName")) 
           CALL WDialogPutString(IDF_TriggerList_L    , getWordInCurrentLang("triggerList"))
           CALL WDialogPutString(IDF_TriggerDetails_F , getWordInCurrentLang("triggerDetails"))

           CALL WDialogPutString(IDF_TriggerType_F    , getWordInCurrentLang("triggerType"))
           CALL WDialogPutString(IDF_TriggerDetails_F , getWordInCurrentLang("triggerDetails"))
           CALL WDialogPutString(ID_TriggerChange     , getWordInCurrentLang("changeToActions"))

       else 
           CALL WDialogPutString(ID_TriggerName, ACTION_DEFAULT)  
           CALL WDialogTitle(getWordInCurrentLang("manageActions")) 
           CALL WDialogPutString(IDF_TriggerChooser_L, getWordInCurrentLang("actionGroupChooser")) 

           CALL WDialogPutString(IDF_TriggeName_L     , getWordInCurrentLang("actionGroupName")) 
           CALL WDialogPutString(IDF_TriggerList_L    , getWordInCurrentLang("actionList"))
           CALL WDialogPutString(IDF_TriggerDetails_F , getWordInCurrentLang("actionDetails"))

           CALL WDialogPutString(IDF_TriggerType_F    , getWordInCurrentLang("actionType"))
           CALL WDialogPutString(IDF_TriggerDetails_F , getWordInCurrentLang("actionDetails"))
           CALL WDialogPutString(ID_TriggerChange     , getWordInCurrentLang("changeToTriggers"))

       end if 

       call setOptionButtons() 
       CALL WDialogPutString(IDF_CallAction_L     , getWordInCurrentLang("callAction"))
       CALL WDialogPutString(ID_TriggerOK         , getWordInCurrentLang("close"))
       CALL WDialogPutString(IDF_TRIGGEROR        , getWordInCurrentLang("or"))
       CALL WDialogPutString(ID_TriggerMoveUp     , getWordInCurrentLang("moveUp")) 
       CALL WDialogPutString(ID_TriggerMoveDown   , getWordInCurrentLang("moveDown")) 

       CALL WDialogPutString(ID_TriggerAdd        , getWordInCurrentLang("add")) 
       CALL WDialogPutString(ID_TriggerAddGroup   , getWordInCurrentLang("add")) 

       CALL WDialogPutString(ID_TriggerUpdateGroup, getWordInCurrentLang("update")) 
       CALL WDialogPutString(ID_TriggerDeleteGroup, getWordInCurrentLang("delete")) 
       CALL WDialogPutString(ID_TriggerDelete     , getWordInCurrentLang("delete")) 


    end SUBROUTINE

    SUBROUTINE setOptionButtons()
        integer(1)              :: num, num2
        character               :: nChar    

        num2 = 0    

        do num = 1 + optionIndexOffset, 3 + optionIndexOffset, 1
           write(nChar, "(I1.1)") num 

           CALL WDialogPutString(ID_TriggerOption1 + num2, &
                                 trim(getWordInCurrentLang("setOption")) // " " // nChar) 
           num2 = num2 + 1 

        end do

    end subroutine 

    SUBROUTINE triggerActionLoop()
        if (WinfoDialog(CurrentDialog) == IDD_TRIGGERS) then 

        end if

        if (canKill .EQV. .TRUE.) then 
            CALL WDialogUnLoad()

            canKill = .FALSE.
        end if

    END SUBROUTINE

    FUNCTION generateFullTextT(this) result(r)
        class(trigger), intent(inout)           :: this
        character(MAX_PARAM_LEN)                :: r

        r = generateFullText(TRIGGER_MODE, this%typ, this%paramList)
        if (this%OR) r = trim(r) // ' OR' 

    END FUNCTION 

    FUNCTION generateFullTextA(this) result(r)
        class(action ), intent(inout)           :: this
        character(MAX_PARAM_LEN)                :: r

        r = generateFullText(ACTION_MODE, this%typ, this%paramList)

    END FUNCTION 

    FUNCTION generateFullText(at, typ, paramList) result(r)
        character(MAX_PARAM_LEN)    :: r
        character(*)                :: paramList
        integer(kind = 1)           :: typ, at
        character(NAME_MAX_LEN)     :: name
        integer(2)                  :: startInd, endInd, startInd2, endInd2 
        logical                     :: lastOne, notFound             

        if (at == TRIGGER_MODE) then
            name = "trigger"
        else
            name = "action"
        end if
        
        write(name, "(A, I3.3)") trim(name), typ
        r = getWordInCurrentLang(name)
        
        if (paramList == "") then 
            return
        end if

        startInd  = 1
        endInd    = 1
        startInd2 = 1
        endInd2   = 1
        lastOne   = .FALSE.
        notFound  = .FALSE.

        do
            call getEnding(paramList, startInd, endInd, lastOne, delimiter)
            call getEnding(r, startInd2, endInd2, notFound, placeholder)
        
            if (notFound) exit
        
            r = r(:endInd2) // paramList(startInd:endInd) // &
                r(endInd2 + 1 + len_trim(placeholder):len_trim(r))
        
            if (lastOne) exit
        
            ! Resume the search immediately after the inserted parameter.
            startInd2 = endInd2 + (endInd - startInd + 1) + 1
            startInd  = endInd + len_trim(delimiter) + 1
        end do

    END FUNCTION    


    subroutine getEnding(txt, startInd, endInd, lastOne, replace)
        character(*), intent(in)    :: txt, replace
        integer(2),   intent(in)    :: startInd
        integer(2),   intent(inout) :: endInd
        logical,      intent(inout) :: lastOne
    
        integer :: ind, txtLen, replaceLen, wildcardPos, matchEnd
    
        txtLen      = len_trim(txt)
        replaceLen  = len_trim(replace)
        wildcardPos = index(replace, wildcard)
    
        lastOne = .true.
        endInd  = txtLen
    
        if (replaceLen == 0) return
    
        do ind = max(1, int(startInd)), txtLen - replaceLen + 1
            matchEnd = ind + replaceLen - 1
    
            if (wildcardPos == 0) then
                if (txt(ind:matchEnd) /= replace) cycle
            else
                if (wildcardPos > 1) then
                    if (txt(ind:ind + wildcardPos - 2) /= &
                        replace(:wildcardPos - 1)) cycle
                end if
    
                if (wildcardPos < replaceLen) then
                    if (txt(ind + wildcardPos:matchEnd) /= &
                        replace(wildcardPos + 1:replaceLen)) cycle
                end if
            end if
    
            lastOne = .false.
            endInd  = ind - 1
            return
        end do
    end subroutine 

END MODULE TriggerAction
