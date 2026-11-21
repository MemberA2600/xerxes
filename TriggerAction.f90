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

    end type

    type triggerGroup
        character(NAME_MAX_LEN)                  :: name, actionName  
        type(trigger), dimension(:), allocatable :: triggerList
        contains
        
        procedure :: dropTriggerList             => dropTriggerList
        procedure :: addNewTrigger               => addNewTrigger  
       
    end type

    integer(1)                      :: actionOrTrigger = TRIGGER_MODE
    logical                         :: canKill    

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

    end type

    type(triggerGroup), dimension(:), allocatable :: triggers
    type( actionGroup), dimension(:), allocatable :: actions    

    contains

    subroutine dropTriggerList(this)    
           class(triggerGroup), intent(inout) :: this 
           integer(1)                         :: rc 
  
           if (allocated(this%triggerList)) then
               deallocate(this%triggerList, stat=rc)
               if (rc /= 0) call displayDebug("Failed to deallocate triggerList!")
           end if

    end subroutine

    subroutine dropActionList(this)    
           class(actionGroup), intent(inout)  :: this 
           integer(1)                         :: rc 
  
           if (allocated(this%actionList)) then
               deallocate(this%actionList, stat=rc)
               if (rc /= 0) call displayDebug("Failed to deallocate actionList!")
           end if

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

    end subroutine

    subroutine triggerActionWindow(at)
       INTEGER                                            :: ITYPE, rc
       Integer(1)                                         :: at
       TYPE(WIN_MESSAGE)                                  :: MESSAGE
       logical                                            :: change 

       actionOrTrigger = at

       do
         if (WInfoDialog(CurrentDialog) == 0) exit
         call sleep(1)
       end do 

       canKill         = .FALSE.

       CALL WDialogLoad(IDD_Triggers)

88996  call SetTheFieldNames() 

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
                  CASE(ID_TriggerImport)
                     call importText()
                  CASE(ID_TriggerExport)
                     call exportText()
                  END SELECT
              end if
       end do 

       if (change) then 
           actionOrTrigger = 1 - actionOrTrigger 
           goto 88996
       else
           canKill = .TRUE.  
       end if   

    END SUBROUTINE

    SUBROUTINE SetTheFieldNames()
       if (actionOrTrigger == TRIGGER_MODE) then
           CALL WDialogTitle(getWordInCurrentLang("manageTriggers"))
           CALL WDialogPutString(ID_TriggerChange, getWordInCurrentLang("changeToActions"))  
       else 
           CALL WDialogTitle(getWordInCurrentLang("manageActions")) 
           CALL WDialogPutString(ID_TriggerChange, getWordInCurrentLang("changeToTriggers"))  
       end if 

       CALL WDialogPutString(ID_TriggerImport, getWordInCurrentLang("import")) 
       CALL WDialogPutString(ID_TriggerExport, getWordInCurrentLang("export")) 
       CALL WDialogPutString(ID_TriggerValidate, getWordInCurrentLang("validate")) 
       CALL WDialogPutString(ID_TriggerOK, getWordInCurrentLang("close")) 

    end SUBROUTINE


    SUBROUTINE triggerActionLoop()
        if (WinfoDialog(CurrentDialog) == IDD_TRIGGERS) then 

        end if

        if (canKill .EQV. .TRUE.) then 
            CALL WDialogUnLoad()

            canKill = .FALSE.
        end if

    END SUBROUTINE


    subroutine importText()
        character(MAX_PATH_LEN) :: fname
        character(:), allocatable :: text
        integer :: unit, rc, closeRc, L
    
        fname = FileDialog("", .FALSE., "txt ")
        if (fname == "") return
    
        open(newunit=unit, file=fname, status='old', action='read', &
             access='stream', form='unformatted', iostat=rc)
        if (rc /= 0) then
            call displayDebug("Failed to open text file for import!")
            return
        end if
    
        inquire(unit=unit, size=L, iostat=rc)
        if (rc == 0) then
            if (L < 0) then
                rc = 1
            else
                allocate(character(len=L) :: text, stat=rc)
                if (rc == 0 .and. L > 0) read(unit, iostat=rc) text
            end if
        end if
    
        close(unit, iostat=closeRc)
        if (rc /= 0 .or. closeRc /= 0) then
            call displayDebug("Failed to import text file!")
            return
        end if
    
        if (L == 0) then
            call WDialogPutString(IDF_Code, ' ')
        else
            call WDialogPutString(IDF_Code, text)
        end if
    end subroutine
    
    subroutine exportText()
        character(MAX_PATH_LEN) :: fname
        character(:), allocatable :: text
        integer :: unit, rc, closeRc, L
    
        fname = FileDialog("", .TRUE., "txt ")
        if (fname == "") return
    
        call WDialogGetStringLength(IDF_Code, L)
        allocate(character(len=L) :: text, stat=rc)
        if (rc /= 0) then
            call displayDebug("Failed to allocate text buffer!")
            return
        end if
    
        if (L > 0) call WDialogGetString(IDF_Code, text)
    
        open(newunit=unit, file=fname, status='replace', action='write', &
             access='stream', form='unformatted', iostat=rc)
        if (rc /= 0) then
            call displayDebug("Failed to open text file for export!")
            return
        end if
    
        if (L > 0) write(unit, iostat=rc) text
        close(unit, iostat=closeRc)
    
        if (rc /= 0 .or. closeRc /= 0) then
            call displayDebug("Failed to export text file!")
        end if
    end subroutine

END MODULE TriggerAction
