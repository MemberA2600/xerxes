MODULE winFolderDialog

      use, intrinsic :: iso_c_binding
    
      USE WINTERACTER
      USE RESID
      USE debugWindow  
      USE engineConstants  
      USE IFWIN
      USE IFWINTY
      use IFPORT
      use dict
      use KERNEL32
      use SHELL32
      USE WINMM
    
      implicit none
      private
      public :: browse_for_folder
       
        integer(c_int), parameter :: BIF_RETURNONLYFSDIRS = int(Z'0001', c_int)
        integer(c_int), parameter :: BIF_NEWDIALOGSTYLE   = int(Z'0040', c_int)
        integer(c_int), parameter :: BFFM_INITIALIZED     = 1
        integer(c_int), parameter :: BFFM_SETSELECTIONA   = int(Z'0466', c_int)
    
        integer, parameter :: WIN_MAX_PATH = 260
    
        type, bind(C) :: BROWSEINFOA
            type(c_ptr)    :: hwndOwner
            type(c_ptr)    :: pidlRoot
            type(c_ptr)    :: pszDisplayName
            type(c_ptr)    :: lpszTitle
            integer(c_int) :: ulFlags
            type(c_funptr) :: lpfn
            type(c_ptr)    :: lParam
            integer(c_int) :: iImage
        end type BROWSEINFOA
    
        interface
    
            function SHBrowseForFolderA(lpbi) result(pidl)
                !DEC$ ATTRIBUTES STDCALL, DECORATE, &
                !DEC$ ALIAS:'SHBrowseForFolderA' :: SHBrowseForFolderA
    
                import :: c_ptr, c_intptr_t
    
                type(c_ptr), value  :: lpbi
                integer(c_intptr_t) :: pidl
            end function SHBrowseForFolderA
    
    
            function SHGetPathFromIDListA(pidl, pszPath) result(ok)
                !DEC$ ATTRIBUTES STDCALL, DECORATE, &
                !DEC$ ALIAS:'SHGetPathFromIDListA' :: SHGetPathFromIDListA
    
                import :: c_ptr, c_int, c_intptr_t
    
                integer(c_intptr_t), value :: pidl
                type(c_ptr), value          :: pszPath
                integer(c_int)              :: ok
            end function SHGetPathFromIDListA
    
    
            subroutine CoTaskMemFree(pv)
                !DEC$ ATTRIBUTES STDCALL, DECORATE, &
                !DEC$ ALIAS:'CoTaskMemFree' :: CoTaskMemFree
    
                import :: c_intptr_t
    
                integer(c_intptr_t), value :: pv
            end subroutine CoTaskMemFree
    
    
            function OleInitialize(pvReserved) result(hr)
                !DEC$ ATTRIBUTES STDCALL, DECORATE, &
                !DEC$ ALIAS:'OleInitialize' :: OleInitialize
    
                import :: c_ptr, c_long
    
                type(c_ptr), value :: pvReserved
                integer(c_long)    :: hr
            end function OleInitialize
    
    
            subroutine OleUninitialize()
                !DEC$ ATTRIBUTES STDCALL, DECORATE, &
                !DEC$ ALIAS:'OleUninitialize' :: OleUninitialize
            end subroutine OleUninitialize
    
    
            function SendMessageA(hWnd, Msg, wParam, lParam) result(r)
                !DEC$ ATTRIBUTES STDCALL, DECORATE, &
                !DEC$ ALIAS:'SendMessageA' :: SendMessageA
    
                import :: c_int, c_intptr_t
    
                integer(c_intptr_t), value :: hWnd
                integer(c_int), value      :: Msg
                integer(c_intptr_t), value :: wParam
                integer(c_intptr_t), value :: lParam
                integer(c_intptr_t)        :: r
            end function SendMessageA
    
        end interface
    
    contains
    
        function bff_callback(hwnd, umsg, lp, lpdata) bind(C) result(res)
            !DEC$ ATTRIBUTES STDCALL :: bff_callback
    
            integer(c_intptr_t), value :: hwnd
            integer(c_int), value      :: umsg
            integer(c_intptr_t), value :: lp
            integer(c_intptr_t), value :: lpdata
    
            integer(c_int)      :: res
            integer(c_intptr_t) :: dummy
    
            if (umsg == BFFM_INITIALIZED) then
    
                if (lpdata /= 0_c_intptr_t) then
                    dummy = SendMessageA( &
                        hwnd,               &
                        BFFM_SETSELECTIONA, &
                        1_c_intptr_t,       &
                        lpdata)
                end if
    
            end if
    
            res = 0
        end function bff_callback
    
    
        subroutine c_to_f_string(carr, fstr)
    
            character(kind=c_char), intent(in) :: carr(:)
            character(len=*), intent(out)      :: fstr
    
            integer :: i
    
            fstr = ' '
    
            do i = 1, min(size(carr), len(fstr))
                if (carr(i) == c_null_char) exit
                fstr(i:i) = carr(i)
            end do
    
        end subroutine c_to_f_string
    
    
        function browse_for_folder( &
            title, init_dir, selected_path) result(success)
    
            character(len=*), intent(in)  :: title
            character(len=*), intent(in)  :: init_dir
            character(len=*), intent(out) :: selected_path
    
            logical :: success
    
            character(kind=c_char), target :: &
                c_title(len_trim(title) + 1)
    
            character(kind=c_char), target :: &
                c_initdir(len_trim(init_dir) + 1)
    
            character(kind=c_char), target :: &
                c_path(WIN_MAX_PATH)
    
            type(BROWSEINFOA), target :: bi
    
            integer(c_intptr_t) :: pidl
            integer(c_int)      :: ok
            integer(c_long)     :: hr
            integer             :: i
            integer             :: n
    
            n = len_trim(title)
    
            do i = 1, n
                c_title(i) = title(i:i)
            end do
    
            c_title(n + 1) = c_null_char
    
            n = len_trim(init_dir)
    
            do i = 1, n
                c_initdir(i) = init_dir(i:i)
            end do
    
            c_initdir(n + 1) = c_null_char
    
            c_path = c_null_char
    
            bi%hwndOwner      = c_null_ptr
            bi%pidlRoot       = c_null_ptr
            bi%pszDisplayName = c_loc(c_path(1))
            bi%lpszTitle      = c_loc(c_title)
    
            bi%ulFlags = ior( &
                BIF_RETURNONLYFSDIRS, &
                BIF_NEWDIALOGSTYLE)
    
            bi%lpfn   = c_funloc(bff_callback)
            bi%lParam = c_loc(c_initdir(1))
            bi%iImage = 0
    
            hr = OleInitialize(c_null_ptr)
    
            pidl = SHBrowseForFolderA(c_loc(bi))
    
            success       = (pidl /= 0_c_intptr_t)
            selected_path = ' '
    
            if (success) then
    
                ok = SHGetPathFromIDListA( &
                    pidl, c_loc(c_path(1)))
    
                if (ok /= 0) then
                    call c_to_f_string(c_path, selected_path)
                else
                    success = .false.
                end if
    
                call CoTaskMemFree(pidl)
    
            end if
    
            if (hr >= 0_c_long) then
                call OleUninitialize()
            end if
    
        end function browse_for_folder
    
END MODULE winFolderDialog
