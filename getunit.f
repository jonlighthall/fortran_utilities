c     This is a replacement for the Fortran 2008 function NEWUNIT for
c     systems running gfortran versions < 4.5
c
c     To use:
c       1) Replace open(newunit=unit) with open(getunit(unit))
c       2) Add getunit as an interger variable in the calling program
c       3) Add getunit.f to the compile line of the calling program
c
!     http://fortranwiki.org/fortran/show/newunit
!     This is a simple function to search for an available unit. lun_min
!     and lun_max define the range of possible luns to check. The unit
!     value is returned by the function, and also by the optional
!     argument. This allows the function to be used directly in an open
!     statement, and optionally save the result in a local variable. 
!     If no units are available, -1 is returned.
c
c     NB newunit() has been replace with getunit() to avoid confusion.
c
      integer function getunit(unit)
      integer, intent(out), optional :: unit
!     local
c     Note: Fortran 77 allows up to 64 logical units. In some situations
c     using logical unit 1 may cause problems.
      integer, parameter :: lun_min=2, lun_max=64
      logical :: i_open
      integer :: lun
!     begin
      getunit=-1
d     write(*,'(2(a,i2))')'testing units ',lun_min,' to ',lun_max
      do lun=lun_min,lun_max
d        write(*, '(a,i2,a)', advance = "no")' testing ',lun,'...'
         inquire(unit=lun,opened=i_open)
         if (.not. i_open) then
            getunit=lun
d           write(*,*)'available'
d           write(*,'(a,i2,a)')'logical unit ',lun,' available'
            exit
d        else
d           write(*,*)'not available'
         endif
      enddo
d     write(*,*)'done'
      if (present(unit)) unit=getunit
      end function getunit
