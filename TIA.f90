MODULE TIA

    !
    ! Based on https://www.biglist.com/lists/stella/archives/200311/msg00156.html
    ! Written by Adam Wozniak (2003)
    !
    USE, INTRINSIC :: ISO_C_BINDING
    USE debugWindow
    USE dataLoader
    USE waveplayer
    USE WINTERACTER
    USE RESID
    USE subs

    implicit none

    private
    public :: TiaMaker

    type :: state_t
        integer :: offset, count, f
        integer :: rate
        logical :: last
    end type state_t

    
    
    integer, parameter :: infrequency = 31440 ! NTSC TIA
    integer, parameter :: outfrequency= 44100 ! wav
    integer, parameter :: channels    = 1     ! mono

    integer, parameter :: poly0(2) = [ 1, -1 ]

    integer, parameter :: poly1(3) = [ 1, 1, -1 ]

    integer, parameter :: poly2(3) = [ 16, 15, -1 ]

    integer, parameter :: poly4(9) = [ 1, 2, 2, 1, 1, 1, 4, 3, -1 ]

    integer, parameter :: poly5(17) = [ 1, 2, 1, 1, 2, 2, 5, 4, 2, 1, 3, 1, 1, 1, 1, 4, -1 ]

    integer, parameter :: poly9(257) = [    1, 4, 1, 3, 2, 4, 1, 2, 3, 2, 1, 1, 1, 1, 1, 1, &
                                            2, 4, 2, 1, 4, 1, 1, 2, 2, 1, 3, 2, 1, 3, 1, 1, &
                                            1, 4, 1, 1, 1, 1, 2, 1, 1, 2, 6, 1, 2, 2, 1, 2, &
                                            1, 2, 1, 1, 2, 1, 6, 2, 1, 2, 2, 1, 1, 1, 1, 2, &
                                            2, 2, 2, 7, 2, 3, 2, 2, 1, 1, 1, 3, 2, 1, 1, 2, &
                                            1, 1, 7, 1, 1, 3, 1, 1, 2, 3, 3, 1, 1, 1, 2, 2, &
                                            1, 1, 2, 2, 4, 3, 5, 1, 3, 1, 1, 5, 2, 1, 1, 1, &
                                            2, 1, 2, 1, 3, 1, 2, 5, 1, 1, 2, 1, 1, 1, 5, 1, &
                                            1, 1, 1, 1, 1, 1, 1, 6, 1, 1, 1, 2, 1, 1, 1, 1, &
                                            4, 2, 1, 1, 3, 1, 3, 6, 3, 2, 3, 1, 1, 2, 1, 2, &
                                            4, 1, 1, 1, 3, 1, 1, 1, 1, 3, 1, 2, 1, 4, 2, 2, &
                                            3, 4, 1, 1, 4, 1, 2, 1, 2, 2, 2, 1, 1, 4, 3, 1, &
                                            4, 4, 9, 5, 4, 1, 5, 3, 1, 1, 3, 2, 2, 2, 1, 5, &
                                            1, 2, 1, 1, 1, 2, 3, 1, 2, 1, 1, 3, 4, 2, 5, 2, &
                                            2, 1, 2, 3, 1, 1, 1, 1, 1, 2, 1, 3, 3, 3, 2, 1, &
                                            2, 1, 1, 1, 1, 1, 3, 3, 1, 2, 2, 3, 1, 3, 1, 8, &
                                            -1 ]

    integer, parameter :: poly68(17) = [ 5, 6, 4, 5, 10, 5, 3, 7, 4, 10, 6, 3, 6, 4, 9, 6, -1 ]

    integer, parameter :: poly465(129) = [  2,3,2,1,4,1,6,10,2,4,2,1,1,4,5, &
                                            9,3,3,4,1,1,1,8,5,5,5,4,1,1,1, &
                                            8,4,2,8,3,3,1,1,7,4,2,7,5,1,3, &
                                            1,7,4,1,4,8,2,1,3,4,7,1,3,7,3, &
                                            2,1,6,6,2,2,4,5,3,2,6,6,1,3,3, &
                                            2,5,3,7,3,4,3,2,2,2,5,9,3,1,5, &
                                            3,1,2,2,11,5,1,5,3,1,1,2,12,5,1, &
                                            2,5,2,1,1,12,6,1,2,5,1,2,1,10,6, &
                                            3,2,2,4,1,2,6,10,-1 ]

    integer, parameter :: divisors(16) = [ 1, 1, 15, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 1 ]

    Type TIATone
         integer(1) :: Vol, Chan, Freq, Length
    end type

    type TIASfx
         Type(TIATone), dimension(:), allocatable :: tones
         integer(2)                               :: length
         character(25)                            :: name

         contains   
         
         procedure :: initTIASfx   => initTIASfx   
         procedure :: createTIASfx => createTIASfx   
         procedure :: playTIASfx   => playTIASfx   
    end type

    contains

    ! TIASfx

    subroutine initTIASfx(this, L, N)   
        class(TIASfx), intent(inout) :: this
        integer(2)                   :: L 
        integer(2)                   :: stat
        character(*)                 :: N

        if (allocated(this%tones)) then
            deallocate(this%tones, stat = stat)
            if (stat /= 0) call displayDebug("Failed to deallocate tones!")
        end if 

        this%length = L
        this%name   = N

        if (this%length > 0) then
            allocate(this%tones(this%length), stat = stat)
            if (stat /= 0) call displayDebug("Failed to allocate tones!")
        end if

    end subroutine    

    subroutine createTIASfx(this, N, bytes)
        class(TIASfx), intent(inout)          :: this
        character(*)                          :: N
        integer(2), dimension(:), allocatable :: bytes
        integer(2)                            :: ind, stat, ind2 
        character(40)                         :: test

        if (mod(size(bytes), 4) /= 0) call displayDebug("Number of bytes is not dividable by 4!")

        this%length = size(bytes) / 4         
        call this%initTIASfx(this%length, N)
    
        ind2 = 0        
        do ind  = 1, size(bytes), 4
           ind2 = ind2 + 1 
           
           this%tones(ind2)%Vol    = bytes(ind)   
           this%tones(ind2)%Chan   = bytes(ind +1)   
           this%tones(ind2)%Freq   = bytes(ind +2)  
           this%tones(ind2)%Length = bytes(ind +3)          

           !write(test, "('Test: ', I0, ' ', I0)") ind, ind2
           !call displayDebug(test) 
        end do

    end subroutine
   
    subroutine playTIASfx(this, chan)
        class(TIASfx), intent(inout)          :: this
        integer(2)                            :: chan 
        integer(2)                            :: ind, stat 
        integer(2), dimension(:), allocatable :: temp, full, fullcopy
        logical                               :: copy
        integer                               :: ind2, newsize, adder
        character(40)                         :: test

        do ind = 1, this%length, 1
           if (allocated(temp) .EQV. .TRUE.) then
               deallocate(temp, stat = stat)
               if (stat /= 0) call displayDebug("Failed to deallocate temp for play TIA!")
           end if 

           !write(test, "(I0, ' | ', I0, ' | ', I0, '|', I0)") &
           !this%tones(ind)%Vol, this%tones(ind)%Chan, &
           !this%tones(ind)%Freq, this%tones(ind)%Length 

           !call displayDebug(test) 

           call TIA_gen(this%tones(ind)%Vol, this%tones(ind)%Chan, &
                        this%tones(ind)%Freq, this%tones(ind)%Length * 200, temp) 

           copy = .FALSE. 

           if (allocated(full) .EQV. .TRUE.) then
               copy = .TRUE.
                
               if(allocated(fullcopy) .EQV. .TRUE.) then
                  deallocate(fullcopy, stat = stat)  
                  if (stat /= 0) call displayDebug("Failed to deallocate fullcopy for play TIA!")
               end if 

               allocate(fullcopy(size(full)), stat = stat)  
               if (stat /= 0) call displayDebug("Failed to allocate fullcopy for play TIA!")

               do ind2 = 1, size(full), 1
                  fullcopy(ind2) = full(ind2) 
               end do  

               deallocate(full, stat = stat)  
               if (stat /= 0) call displayDebug("Failed to deallocate full for play TIA!")
  
               newsize = size(full) + size(temp)
           else
               newsize = size(temp) 
           end if 

           allocate(full(newsize), stat = stat)  
           if (stat /= 0) call displayDebug("Failed to allocate full for play TIA!")
 
           if (copy .EQV. .TRUE.) then
               do ind2 = 1, size(fullcopy), 1   
                  full(ind2) = fullcopy(ind2)   
               end do      

               adder = size(fullcopy) + 1

               deallocate(fullcopy, stat = stat)  
               if (stat /= 0) call displayDebug("Failed to deallocate fullcopy-2 for play TIA!")
            else
               adder = 0 
            end if                

            do ind2 = 1, size(temp), 1
               full(ind2 + adder) = temp(ind2)       
            end do

        end do

        call TIA2Wav(full, chan)
        
        deallocate(full, stat = stat)  
        if (stat /= 0) call displayDebug("Failed to deallocate full-2 for play TIA!")

    end subroutine


    ! Basic TIA stuff    

    subroutine TIA_init(s)
        type(state_t), intent(out) :: s

        s%offset = 1      ! Fortran arrays are 1-based
        s%count  = 0
        s%last   = .true.
        s%f      = 0
        s%rate   = 0
    end subroutine

    subroutine get_poly(C, poly)
        integer, intent(in) :: C
        integer, allocatable :: poly(:)
        integer              :: stat, s, ind

        if (allocated(poly)) then
            deallocate(poly, stat = stat)
            if (stat /= 0) call  displayDebug("Failed to deallocate Poly!")
        end if

        select case (C)
        case (0)
            s = size(poly0)
        case (1,2)
            s = size(poly4)
        case (3)
            s = size(poly465)
        case (4,5)
            s = size(poly1)
        case (6,10,14)
            s = size(poly2)
        case (7,9)
            s = size(poly5)
        case (8)
            s = size(poly9)
        case (11)
            s = size(poly0)
        case (12,13)
            s = size(poly1)
        case (15)
            s = size(poly68)
        end select

        allocate(poly(s), stat = stat)
        if (stat /= 0) call  displayDebug("Failed to deallocate Poly!")

        do ind = 1, s, 1
                select case (C)
                case (0)
                    poly(ind) = poly0(ind)
                case (1,2)
                    poly(ind) = poly4(ind)
                case (3)
                    poly(ind) = poly465(ind)
                case (4,5)
                    poly(ind) = poly1(ind)
                case (6,10,14)
                    poly(ind) = poly2(ind)
                case (7,9)
                    poly(ind) = poly5(ind)
                case (8)
                    poly(ind) = poly9(ind)
                case (11)
                    poly(ind) = poly0(ind)
                case (12,13)
                    poly(ind) = poly1(ind)
                case (15)
                    poly(ind) = poly68(ind)
                end select

        end do

    end subroutine 

    subroutine TIA_generate(F, V, C, buf, size, s)

        integer, intent(in) :: F, V, C
        integer, intent(in) :: size
        integer(kind=1), intent(inout) :: buf(:)
        type(state_t), intent(inout) :: s

        integer :: remaining
        integer :: idx
        integer, allocatable :: poly(:)

        call get_poly(C, poly)

        remaining = size
        idx = 1

        do while (remaining > 0)

            s%f = s%f + 1

            if (s%f == divisors(C+1) * (F + 1)) then

                s%f = 0
                s%count = s%count + 1

                if (s%count == poly(s%offset)) then
                    s%offset = s%offset + 1
                    s%count = 0

                    if (poly(s%offset) == -1) then
                        s%offset = 1
                    end if
                end if

                s%last = (iand(s%offset - 1, 1) == 0)
            end if

            s%rate = s%rate + outfrequency

            do while (s%rate >= infrequency .and. remaining > 0)

                if (s%last) then
                    buf(idx) = buf(idx) + shiftl(V, 3)
                end if

                s%rate = s%rate - infrequency

                idx = idx + channels
                remaining = remaining - channels
            end do

        end do

    end subroutine

    subroutine TIA_gen(V, C, F, L, out)
        integer(kind=1), allocatable :: buf(:)
        type(state_t) :: s
        integer :: F, V, C, L
        integer(kind=2), dimension(:), allocatable, intent(inout) :: out    
        integer :: stat, ind
   
        allocate(buf(L), stat = stat)
        if (stat /= 0) call  displayDebug("Failed to allocate TIA-buffer!")

        if (allocated(out)) then
            deallocate(out, stat = stat)
            if (stat /= 0) call displayDebug("Failed to deallocate TIA-out!")
        end if

        buf = 0
    
        call TIA_init(s)
        call TIA_generate(F, V, C, buf, size(buf), s)
    
        allocate(out(size(buf)), stat = stat)
        if (stat /= 0) call  displayDebug("Failed to allocate TIA-out!")

        do ind = 1, L, 1
           out(ind) = ior(ishft(int(buf(ind),2), 8), iand(int(buf(ind),2), z'FF')) 
        end do

        deallocate(buf, stat = stat)
        if (stat /= 0) call  displayDebug("Failed to deallocate TIA-buffer!")

    end subroutine

    subroutine TIA_test(V, C, F, L)
        integer :: F, V, C, L
        integer(kind=2), dimension(:), allocatable :: out 
        integer :: stat

        call TIA_gen(V, C, F, L, out)
        call TIA2Wav(out, 1)

        deallocate(out, stat = stat)
        if (stat /= 0) call  displayDebug("Failed to deallocate TIA-test-out!")

    end subroutine

    !
    !  Tia Maker Window
    !

    subroutine TIAMaker()
       INTEGER                                 :: ITYPE
       TYPE(WIN_MESSAGE)                       :: MESSAGE
       !character(10)                  :: msgString
       integer                                 :: c 


       CALL WDialogLoad(IDD_TIA)

       do
          CALL WDialogSelect(IDD_TIA)
          CALL WDialogShow(ITYPE=Modal)     
    
          if (WinfoDialog(CurrentDialog) == IDD_TIA) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     EXIT
                  CASE(ID_TIAErase)
                     CALL WDialogPutString(ID_TIAInput, "")   
                  CASE(ID_TIALoad)
                     call TiaLoad()
                  CASE(ID_TIASave)
                     call TiaSave()
                  CASE(ID_TIAPlay)
                     call TiaPlay()
                  END SELECT
              end if
       end do 

       CALL WDialogUnLoad()

    END SUBROUTINE

    subroutine TiaLoad()

    end subroutine

    subroutine inputBox2Data(d)
        integer, parameter                                   :: text_len = 1500
        character(text_len)                                  :: text
        integer(2), dimension(:), allocatable, intent(inout) :: d
        integer                                              :: stat, fromPoz, toPoz, ind, length, ind2    
        character                                            :: ch   
        character(10)                                        :: segment     

        if (allocated(d)) then
            deallocate(d, stat = stat)
            if (stat /= 0) call displayDebug("Failed to deallocate temp for grabbing text to data!")
        end if

        call WDialogGetString(ID_TIAInput, text)
            
        length = countCharInString(text, ",") + countCharInString(text, char(10)) + 1

        !write(segment, "('Fuck: ', I0)") length 
        !call displayDebug(segment)   

        allocate(d(length), stat = stat)
        if (stat /= 0) call displayDebug("Failed to allocate temp for grabbing text to data!")

        fromPoz = 1
        toPoz   = 1
        ind     = 0
        ind2    = 0

        do while(ind <= length)
           ind  = ind  + 1
           ind2 = ind2 + 1
    
           if (ind > 4) ind = 1 
           ch = ","
           if (ind == 4) ch = char(10)
                     
           toPoz = getNextPoz(text, ch, fromPoz)

           if (toPoz == -1) then 
               toPoz = len_trim(text)
           else
               toPoz = toPoz -1
               if (ind == 4) toPoz = toPoz - 1  
           end if  

           !write(segment, "(I0, '|', I0)") fromPoz, toPoz 
           !call displayDebug(segment)   
        
           segment = trim(text(fromPoz:toPoz))
           !call displayDebug("|" // segment // "|") 
           read(segment, "(I3)") d(ind2)

           if (toPoz == len_trim(text)) exit

           fromPoz = toPoz + 2 
           if (ind == 4) fromPoz = fromPoz + 1   
        end do 

    end subroutine

    subroutine TiaSave()
        integer(2), dimension(:), allocatable :: d

        call inputBox2Data(d)

    end subroutine

    subroutine TiaPlay()
        TYPE(Tiasfx)                          :: myTia 
        integer                               :: stat
        integer(2), dimension(:), allocatable :: d

        call inputBox2Data(d)
        call myTia%createTIASfx("", d)
        call myTia%playTIASfx(  1)
        call myTia%initTIASfx(  0, "")       

        deallocate(d, stat = stat)
        if (stat /= 0) call displayDebug("Failed to deallocate temp for play TIA on Editor!")
    end subroutine

END MODULE TIA
