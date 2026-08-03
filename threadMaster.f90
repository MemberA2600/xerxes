MODULE threadMaster
    use IFWIN
    use ISO_C_BINDING
    use debugWindow

    IMPLICIT NONE

    PRIVATE
    PUBLIC     :: initThreadList, addThread, killThread, isThreadRunning, closeAllThreads, &
                  isThreadPaused, getThreadCommand, pauseThread, NO_COMMAND, &
                  PAUSE_COMMAND, UNPAUSE_COMMAND  

    TYPE Thread   
         character(20)          :: name
         integer(HANDLE)        :: threadHandle
         integer(DWORD)         :: threadId  
         logical                :: running, paused
         integer                :: command 

    END TYPE

    integer(1), parameter       :: NO_COMMAND      = 0, &
                                   PAUSE_COMMAND   = 1, &
                                   UNPAUSE_COMMAND = 2

    TYPE(Thread), dimension(:), allocatable  :: threadList
    integer(2)                               :: listSize
    integer(2), parameter                    :: initSize = 20, &
                                                 addSize = 10
    abstract interface
        integer(DWORD) function ThreadEntry(lpParameter)
            use IFWIN
            implicit none
    
            integer(LPVOID), value :: lpParameter
        end function ThreadEntry
    end interface

    contains
    
    subroutine closeAllThreads() 
        integer(2)          :: ind

        do ind = 1, listSize, 1
           call killThread(threadList(ind)%name) 
        end do

    end subroutine 

    subroutine initThreadList
        integer(1)      :: rc
        
        allocate(threadList(initSize), stat = RC)
        if (rc /= 0) call displayDebug("Failed to allocate init threadlist!")
        
        listSize = 0

    end subroutine

    subroutine addThread(name, proc)
         character(*)           :: name
         procedure(ThreadEntry) :: proc   
         integer(1)             :: rc

         integer(HANDLE) :: threadHandle
         integer(DWORD)  :: threadId

         if (listSize == size(threadList)) call addMore()

         listSize = listSize + 1 
         threadList(listSize)%name    = name
         threadList(listSize)%running = .TRUE.          
         threadList(listSize)%paused  = .FALSE.          
         threadList(listSize)%command = 0          


         threadHandle = CreateThread( &
                        NULL,      &
                        0,         &
                        loc(proc), &
                        NULL,      &
                        0,         &
                        threadId)
    
         if (threadHandle == NULL) then
             call displayDebug("Failed to run thread!")
         end if
           
         threadList(listSize)%threadHandle = threadHandle          
         threadList(listSize)%threadId     = threadId 
         
    end subroutine

    subroutine addMore()
        integer(1)                               :: rc
        TYPE(Thread), dimension(:), allocatable  :: tempList
        integer(2)                               :: ind

        allocate(tempList(listSize), stat = RC)
        if (rc /= 0) call displayDebug("Failed to allocate temp list of threads!")

        do ind = 1, listSize, 1
           tempList(ind) = threadList(ind)  
        end do

        deallocate(threadList, stat = RC)
        if (rc /= 0) call displayDebug("Failed to deallocate threadlist!")  

        allocate(threadList(listSize + addSize), stat = RC)
        if (rc /= 0) call displayDebug("Failed to allocate threadlist!")     

        do ind = 1, listSize, 1
           threadList(ind) = tempList(ind)  
        end do        

        deallocate(tempList, stat = RC)
        if (rc /= 0) call displayDebug("Failed to deallocate tempList!")         

    end subroutine 

    function getThreadNum(name) result(i)
         character(*)           :: name
         integer(2)             :: ind, i

         i = 0

         do ind = 1, listSize, 1
            if (name == threadList(ind)%name) then
                if (threadList(ind)%threadHandle /= NULL) i = ind
                exit
            end if
         end do

    end function

    subroutine killThread(name)
         character(*)           :: name
         integer(2)             :: ind
         integer                :: RC

         ind = getThreadNum(name)
         if (ind > 0) then
             threadList(ind)%running = .FALSE.

             RC  = WaitForSingleObject(threadList(ind)%threadHandle, INFINITE)
             RC  = CloseHandle(threadList(ind)%threadHandle)
             threadList(ind)%threadHandle = NULL

         end if

    end subroutine

    function isThreadRunning(name) result(r)
         character(*)           :: name
         logical                :: r   
         integer(2)             :: ind

         r = .FALSE.
         ind = getThreadNum(name)
         if (ind > 0) r = threadList(ind)%running       

    end function

    function isThreadPaused(name) result(r)
         character(*)           :: name
         logical                :: r   
         integer(2)             :: ind

         r = .FALSE.
         ind = getThreadNum(name)
         if (ind > 0) r = threadList(ind)%paused       

    end function

    function getThreadCommand(name) result(r)
         character(*)           :: name
         integer(1)             :: r   
         integer(2)             :: ind

         r = NO_COMMAND
         ind = getThreadNum(name)
         if (ind > 0) r = threadList(ind)%command      

    end function

    subroutine setThreadCommand(name, c)
         character(*)           :: name
         integer(2)             :: ind
         integer(1)             :: c

         ind = getThreadNum(name)
         if (ind > 0) threadList(ind)%command = c   
    end subroutine

    subroutine pauseThread(name, p)
         character(*)           :: name
         integer(2)             :: ind
         logical                :: p

         ind = getThreadNum(name)
         if (ind > 0) then
             threadList(ind)%paused  = p   
             threadList(ind)%command = NO_COMMAND
         end if
    end subroutine

END MODULE threadMaster
