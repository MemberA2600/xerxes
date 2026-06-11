MODULE test

      USE WINTERACTER
      USE RESID
      USE debugWindow
      use, intrinsic :: iso_c_binding

      IMPLICIT NONE

      interface
        function testC(a,b) bind(C, name="testC") result(r)
          import                :: c_int
          integer(c_int), value :: a, b
          integer(c_int)        :: r
        end function
      end interface

END MODULE test
