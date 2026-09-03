MODULE dict

    USE debugWindow
    USE engineConstants  
    USE subs
    USE winAPIs
    USE dataLoader
    use, intrinsic :: iso_c_binding

    USE IFWIN
    USE IFWINTY
    USE KERNEL32
    use WINMM

    implicit none

    private
    public                  :: getLang, setLang, setNumOfLangs, loadDict, getWordInCurrentLang

    integer(1)                           :: language = LANG_ENG, numOfLanguages
    character(4), parameter              :: splitter = " => "
    integer(8)                           :: lineCount = 0

    type textLine
        character(:), allocatable :: text
    end type textLine

    type(textLine), dimension(:)  , allocatable :: keys
    type(textLine), dimension(:,:), allocatable :: values         

    contains

    function getWordInCurrentLang(k) result(r)
        character(*)              :: k
        character(255)            :: r
        
        integer(8)                :: ind

        r = ""
        do ind = 1, lineCount, 1
           if (keys(ind)%text == k) then
              r = values(ind, language + 1)%text
              return
           end if  

        end do

        r = "!!!"

    end function

    subroutine setNumOfLangs(N)
        integer(1)          :: rc
        integer             :: N

        numOfLanguages      = N

        !allocate(dictList(N), stat = rc)
        !if (rc /= 0) call displayDebug("Failed to allocated dictoniaries!")

    end subroutine

    subroutine loadDict(N, fname)
        implicit none
    
        integer, intent(in)                         :: N
        character(*), intent(in)                    :: fname
    
        integer                                     :: rc
        integer(2), allocatable                     :: d(:)
        integer(8)                                  :: s, ind, splitIndex
    
        character(:), allocatable                   :: completeText
        type(textLine), allocatable                 :: lines(:)
    
        integer(8)                                  :: lineStart, lineEnd
        integer(8)                                  :: currentLine
         
        call loadBinary(trim(CWD()) // "\dict\" // trim(fname), &
                        d, s, .TRUE.)
      
        if (s == 0) then
            call displayDebug("Dictionary file is empty!")
            return
        end if
    
        allocate(character(len=s) :: completeText, stat=rc)
    
        if (rc /= 0) then
            call displayDebug("Failed to allocate completeText!")
            return
        end if
    
        ! Convert the decompressed ANSI bytes to a character string.
        do ind = 1, s
            completeText(ind:ind) = achar(iand(int(d(ind)), 255))
        end do
    
        deallocate(d, stat=rc)
    
        if (rc /= 0) then
            call displayDebug("Failed to deallocate raw bytes!")
        end if
    
        ! These variables must be initialized.
        lineStart  = 1
        currentLine = 0
    
        if (lineCount == 0) then 
            ! Count the lines.
            do ind = 1, s
                if (completeText(ind:ind) == achar(10)) then
                    lineCount = lineCount + 1
                end if
            end do
        
            ! Count the final line when the file does not end with LF.
            if (completeText(s:s) /= achar(10)) then
                lineCount = lineCount + 1
            end if
        end if
   
        allocate(lines(lineCount), stat=rc)
    
        if (rc /= 0) then
            call displayDebug("Failed to allocate lines!")
            return
        end if
    
        ! Split text at LF characters.
        do ind = 1, s
            if (completeText(ind:ind) == achar(10)) then
                currentLine = currentLine + 1
                lineEnd = ind - 1
    
                ! Remove CR from a Windows CR+LF line ending.
                if (lineEnd >= lineStart) then
                    if (completeText(lineEnd:lineEnd) == achar(13)) then
                        lineEnd = lineEnd - 1
                    end if
                end if
    
                if (lineEnd >= lineStart) then
                    lines(currentLine)%text = &
                        completeText(lineStart:lineEnd)
                else
                    lines(currentLine)%text = ""
                end if
    
                lineStart = ind + 1
            end if
        end do
    
        ! Handle the final line if it has no terminating LF.
        if (lineStart <= s) then
            currentLine = currentLine + 1
            lines(currentLine)%text = &
                completeText(lineStart:s)
        end if
    
        if (allocated(keys) .EQV. .FALSE.) then
            allocate(keys(lineCount), stat = rc)
            if (rc /= 0) then
                call displayDebug("Failed to allocate list of keys!")
            end if
        end if

        if (allocated(values) .EQV. .FALSE.) then
            allocate(values(lineCount, numOfLanguages), stat = rc)
            if (rc /= 0) then
                call displayDebug("Failed to allocate list of values!")
            end if
        end if

        do ind = 1, lineCount, 1
           do splitIndex = 1, len(lines(ind)%text)-3, 1 
              if (lines(ind)%text(splitIndex:splitIndex + 3) == splitter) then   
                  if (N == 1) keys(ind)%text = lines(ind)%text(1             :splitIndex - 1      )
                  values(ind, N)%text        = lines(ind)%text(splitIndex + 4:len(lines(ind)%text))

                  !call displayDebug(keys(ind)%text // "|||" // values(ind, N)%text)
              end if  
           end do 
        end do
    
        ! This also deallocates every lines(ind)%text component.
        deallocate(lines, stat=rc)
    
        if (rc /= 0) then
            call displayDebug("Failed to deallocate lines!")
        end if
    
        deallocate(completeText, stat=rc)
    
        if (rc /= 0) then
            call displayDebug("Failed to deallocate completeText!")
        end if
        
    end subroutine loadDict

    subroutine setLang(L)
        integer(1)     :: L

        language = L
    end subroutine

    function getLang() result(L)
        integer(1)     :: L

        L = language
    end function 

END MODULE dict
