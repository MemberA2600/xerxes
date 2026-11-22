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
                                       triggerActionWindow, triggerActionLoop, var, loadSyntax, &
                                       actionText, triggerText     

    character(:), allocatable, target          :: actionText, triggerText

    type command
         character(NAME_MAX_LEN)               :: name
         integer(1)                            :: allowedMode 
         logical, dimension(3)                 :: allowedMapType  
         integer(1), dimension(:), allocatable :: paramTypes, paramDetails   
    end type

    type(command), dimension(:), allocatable   :: commandList

    type var
         integer(1)                            :: typ
         integer(2), dimension(:), allocatable :: bytes

         contains

         procedure :: erase                    => erase
         procedure :: setAsChar                => setAsChar
         procedure :: setAsInt                 => setAsInt
         procedure :: getAsChar                => getAsChar
         procedure :: getAsInt                 => getAsInt

    end type

    type mapVar
         character(NAME_MAX_LEN)                  :: name
         logical                                  :: oneMapOnly = .TRUE.    
         type(var)                                :: var   
    end  type

    type trigger
         integer(kind=1)                      :: typ
         logical                              :: or
         type(var), dimension(:), allocatable :: paramList   

    end type

    type triggerGroup
        character(NAME_MAX_LEN)                  :: action 
        type(trigger), dimension(:), allocatable :: triggerList
        contains
        
        procedure :: dropTriggerList             => dropTriggerList
        procedure :: addNewTrigger               => addNewTrigger  
       
    end type

    integer(1)                      :: actionOrTrigger = TRIGGER_MODE
    logical                         :: canKill    

    type action
         integer(kind=1)                      :: typ
         type(var), dimension(:), allocatable :: paramList   
    end type

    type actionGroup
        character(NAME_MAX_LEN)                 :: name, nextAction
        type(action), dimension(:), allocatable :: actionList

        contains
        
        procedure :: dropACtionList              => dropActionList 
        procedure :: addNewAction                => addNewAction  

    end type

    type(triggerGroup), dimension(:), allocatable :: triggers
    type( actionGroup), dimension(:), allocatable :: actions    

    contains

    subroutine erase(this)
           class(var), intent(inout) :: this
           integer(1)                :: rc 

           this%typ                  = VAR_NULL

           if (allocated(this%bytes)) then
               deallocate(this%bytes, stat = rc) 
               if (rc /= 0) call displayDebug("Failed to deallocate var!") 
           end if   
    end subroutine

    subroutine setAsChar(this, c)
           class(var), intent(inout) :: this
           integer(1)                :: rc  
           character(*)              :: c 

           call this%erase()
           this%typ = VAR_CHAR
           
           allocate(this%bytes(NAME_MAX_LEN), stat = rc)     
           if (rc /= 0) call displayDebug("Failed to allocate var as char!") 
            
           this%bytes = 0 
           call writeChars2Bin(this%bytes, c, 1, len_trim(c))

           if (len_trim(c) /= NAME_MAX_LEN) this%bytes(len_trim(c) + 1 : NAME_MAX_LEN) = Z'20'  

    end subroutine

    subroutine setAsInt(this, i)
           class(var), intent(inout) :: this
           integer(1)                :: rc  
           integer(2)                :: i 

           call this%erase()
           this%typ = VAR_INT
           
           allocate(this%bytes(1), stat = rc)     
           if (rc /= 0) call displayDebug("Failed to allocate var as int!") 
            
           this%bytes(1) = i

    end subroutine

    function getAsChar(this) result(r)
           class(var), intent(inout) :: this
           character(NAME_MAX_LEN)   :: r 
                 
           call bin2Char(r, this%bytes, NAME_MAX_LEN, .FALSE.) 

    end function    

    function getAsInt(this) result(r)
           class(var), intent(inout) :: this
           integer(2)                :: r 

           r = this%bytes(1)
    end function

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

       if (actionOrTrigger == TRIGGER_MODE) then  
           call WDialogPutString(IDF_Code, triggerText) 
       else 
           call WDialogPutString(IDF_Code, actionText) 
       end if

       do
          change   = .FALSE.
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
                  case(ID_TriggerValidate)      
                     call compile(.TRUE.)   
                  CASE(ID_TriggerImport)
                     call importText()
                  CASE(ID_TriggerExport)
                     call exportText()
                  END SELECT
              end if
       end do 

       if (actionOrTrigger == TRIGGER_MODE) then  
           call WDialogGetStringA(IDF_Code, triggerText) 
       else 
           call WDialogGetStringA(IDF_Code, actionText) 
       end if

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
       
        if (L > 0) call WDialogGetStringA(IDF_Code, text)
    
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

    subroutine compile(validate)
        logical                   :: validate
        type(triggerGroup)        :: tempTriggerGroup
        type(actionGroup)         :: tempActionGroup
        character(:), allocatable :: txt, group, noComments

        integer(8)                :: startInd, endInd, ind
        logical                   :: insideComment, escaped

        if (validate) then
            call WDialogGetStringA(IDF_Code, txt)
        else
            if (actionOrTrigger == TRIGGER_MODE) then
                txt = triggerText
            else
                txt = actionText
            end if
        end if

        startInd      = 1
        insideComment = .FALSE.
        
        do ind = 1, len_trim(txt), 1
            if (txt(ind:ind) /= commentChar) cycle
        
            if (ind > 1) then
                if (txt(ind-1:ind-1) == escapeChar) cycle
            end if
        
            if (insideComment) then
                startInd = ind + 1
            else
                noComments = noComments // txt(startInd:ind-1)
            end if
        
            insideComment = .not. insideComment
        end do
        
        if (.not. insideComment) then
            noComments = noComments // txt(startInd:len_trim(txt))
        end if

        call WDialogPutString(IDF_Errors, noComments)

    end subroutine

    subroutine loadSyntax()
        integer(2), dimension(:), allocatable           :: bytes
        character(:), allocatable                       :: c
        integer(8)                                      :: L, ind, startInd, endInd, CR, insertNum, ind2, iii         
        integer(1)                                      :: rc

        character(NAME_MAX_LEN)                         :: name
        integer(1)                                      :: flags, whereToUse
        character(NAME_MAX_LEN), dimension(5)           :: paramStr
        logical                                         :: header

        call loadBinary("syntax.xxs", bytes, L, .TRUE.)

        allocate(character(len = L) :: c, stat = RC)
        if (rc /= 0) call displayDebug("Failed to allocate character for syntax bytes!")

        do ind = 1, L, 1
           c(ind:ind) = achar(bytes(ind))
        end do

        insertNum = 0        
        do ind = 1, L, 1
           if (c(ind:ind) == achar(10) .OR. ind == L) then 
               insertNum = insertNum + 1 
           end if 
        end do

        allocate(commandList(insertNum), stat = rc)
        if (rc /= 0) call displayDebug("Failed to allocate CommandList!")

        endInd    = 0
        insertNum = 0        
        header    = .TRUE.

        do 
            if (endInd == L) exit
            CR         = 0 
            do ind = endInd + 1, L, 1
               if (c(ind:ind) == achar(13)) CR = ind 
               if (c(ind:ind) == achar(10) .OR. ind == L) then
                   if (CR == 0) CR = ind 
                   startInd        = endInd + 1 
                   endInd          = ind  
                
                   if (startInd == endInd) exit 
                   if (header) then
                       header = .FALSE.
                   else     
                       read(c(startInd : CR), DECIMAL="COMMA", fmt=*, iostat = rc ) & 
                            name, flags, whereToUse, paramStr(1), paramStr(2), paramStr(3), paramStr(4), paramStr(5)      
                        
                       insertNum = insertNum + 1
                       if (rc /= 0) call displayDebug("Failed to read Syntax! " // c(startInd : CR))     
                    
                       commandList(insertNum)%name = name
                       do ind2 = 1, 3, 1
                          if (btest(flags, ind2 - 1)) then  
                             commandList(insertNum)%allowedMapType(ind2) = .TRUE.   
                          else
                             commandList(insertNum)%allowedMapType(ind2) = .FALSE.               
                          end if    
                       end do  

                       commandList(insertNum)%allowedMode = whereToUse

                       iii = 0

                       do ind2 = 1, size(paramStr), 1 
                          if (paramStr(ind2) /= "null") then   
                              iii = iii + 1  
                          else
                              exit  
                          end if  
                       end do 
                        
                       if (iii > 0) then
                           allocate(commandList(insertNum)%paramTypes(iii), stat = rc)  
                           if (rc /= 0) call displayDebug("Failed to allocate Paramtypes!")

                           allocate(commandList(insertNum)%paramDetails(iii), stat = rc)  
                           if (rc /= 0) call displayDebug("Failed to allocate Paramtypes!")  

                           do ind2 = 1, iii, 1
                              select case(paramStr(ind2))          
                              case("int")
                                    commandList(insertNum)%paramTypes(ind2)   = VAR_INT  
                                    commandList(insertNum)%paramDetails(ind2) = TYPE_EMPTY 
                              case("bool")
                                    commandList(insertNum)%paramTypes(ind2)   = VAR_INT  
                                    commandList(insertNum)%paramDetails(ind2) = TYPE_BOOL  
                              case("char")
                                    commandList(insertNum)%paramTypes(ind2)   = VAR_CHAR  
                                    commandList(insertNum)%paramDetails(ind2) = TYPE_EMPTY  
                              case("action")
                                    commandList(insertNum)%paramTypes(ind2)   = VAR_CHAR  
                                    commandList(insertNum)%paramDetails(ind2) = TYPE_ACTION
                              case("player")
                                    commandList(insertNum)%paramTypes(ind2)   = VAR_CHAR  
                                    commandList(insertNum)%paramDetails(ind2) = TYPE_PLAYER_MAP
                              case("spawner")
                                    commandList(insertNum)%paramTypes(ind2)   = VAR_CHAR  
                                    commandList(insertNum)%paramDetails(ind2) = TYPE_PLAYER_SPAWNER
                              case("follower")
                                    commandList(insertNum)%paramTypes(ind2)   = VAR_CHAR  
                                    commandList(insertNum)%paramDetails(ind2) = TYPE_FOLLOWER_MAP
                              case default
                                    call displayDebug("Invalid paramtype " // paramStr(ind2) // "!")
                              end select  
                           end do  
                         
                       end if  
                   end if 
               end if      
            end do
        end do

        deallocate(c, stat = rc)
        if (rc /= 0) call displayDebug("Failed to deallocate character for syntax bytes!")

    end subroutine

END MODULE TriggerAction
