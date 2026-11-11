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
                                       removeTriggersAndActions, triggers, actions 

    type trigger
         integer(kind=1)            :: typ
         logical                    :: or
         character(MAX_PARAM_LEN)   :: paramList   
    end type

    type triggerGroup
        character(NAME_MAX_LEN)                  :: name, actionName  
        type(trigger), dimension(:), allocatable :: triggerList
        contains
        
        procedure :: dropTriggerList             => dropTriggerList
        procedure :: addNewTrigger               => addNewTrigger  
        procedure :: removeTrigger               => removeTrigger               
    end type

    !type(triggerGroup)              :: tempTriggerGroup
    !type(trigger)                   :: tempTrigger

    type action
         integer(kind=1)            :: typ
         character(MAX_PARAM_LEN)   :: paramList  
    end type

    type actionGroup
        character(NAME_MAX_LEN)                 :: name
        type(action), dimension(:), allocatable :: actionList

        contains
        
        procedure :: dropACtionList              => dropActionList 
        procedure :: addNewAction                => addNewAction  
        procedure :: removeAction                => removeAction

    end type

    !type(actionGroup)                             :: tempActionGroup
    !type(action)                                  :: tempAction

    type(triggerGroup), dimension(:), allocatable :: triggers
    type( actionGroup), dimension(:), allocatable :: actions    

    integer                                       :: selectedTriggerGroup, selectedActionGroup, &
                                                     selectedTrigger     , selectedAction

    contains

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


END MODULE TriggerAction
