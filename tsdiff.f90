program tsdiff
  ! ts diff - calculate difference between two time series files
  !
  ! JCL Aug 2022

  implicit none
  interface
     integer function getunit(unit)
       integer, intent(out), optional :: unit
     end function getunit
  end interface
  integer,parameter :: srk = selected_real_kind(2)
  real(kind=srk), dimension(:), allocatable :: r1,r2
  real(kind=srk), dimension(:,:), allocatable :: tl1,tl2
  integer :: i,n1,io,ln1,ln2,unit1,unit2,n2,ln3,nerr,ns1,j,ls,ns2,nerr2,nerr3
  character(len=256) :: fname1, fname2, tlthresh,dummy
  real(kind=srk)::dtl,dtl_max
  ! ----------------------------------------------------------
  ! set thresholds
  real(kind=srk),parameter :: rdiff=0.01
  real(kind=srk)::tldiff=0.01
  real(kind=srk),parameter :: tl_red=0.1, comp_diff=0.001
  real(kind=srk),parameter :: tlmax=-20*log10(2.**(-23))
  ! ----------------------------------------------------------
  call get_command_argument(1,fname1,ln1)
  call get_command_argument(2,fname2,ln2)
  call get_command_argument(3,tlthresh,ln3)
100 format(a,f7.3)
  print 100,' tl max = ',tlmax
  print 100,' tl dif = ',tl_red
  print 100,' tl com = ',comp_diff

  !     set file names
  if (ln1.eq.0) then
     fname1='nspe02.asc'
  end if
  if (ln2.eq.0) then
     fname2='std/case2r.rtl'
  end if

  if (ln3.gt.0) then
     read(tlthresh,*)tldiff
  end if

  !     open files
  open (getunit(unit1), file = fname1, status = 'old')
  open (getunit(unit2), file = fname2, status = 'old')

  !     check file length
  n1=0  ! number of lines
  ns1=0 ! number of spaces (delimiters)
  ls=0  ! position of last space
  do
     if (n1.eq.0) then
        read(unit1,'(a)',iostat=io) dummy
        do j=1,len(trim(dummy))
           if(dummy(j:j) == ' ') then
              if (j.eq.(ls+1)) then ! same space
              else                  ! new space
                 ns1=ns1+1
              endif
              ls =j
           endif
        enddo
     else
        read(unit1,*,iostat=io)
     endif
     if (io/=0) exit
     n1=n1+1
  enddo
  print '(2a,i5,a,i3,a)',trim(fname1),' has ',n1,' lines and ',ns1,' delimiters'

  n2=0
  ns2=0
  ls=0
  do
     if (n2.eq.0) then
        read(unit2,'(a)',iostat=io) dummy
        do j=1,len(trim(dummy))
           if(dummy(j:j) == ' ') then
              if (j.eq.(ls+1)) then
              else
                 ns2=ns2+1
              endif
              ls =j
           endif
        enddo
     else
        read(unit2,*,iostat=io)
     endif
     if (io/=0) exit
     n2=n2+1
  enddo

  if (n1.eq.n2) then
     print *, 'file lengths match: ',n1
  else
     print *, 'file lengths do not match'
     print *, 'length file 1 = ',n1
     print *, 'length file 2 = ',n2
     close(unit1)
     close(unit2)
     stop 1
  endif

  if (ns1.eq.ns2) then
     print *, 'number of file delimiters match: ',ns1
  else
     print *, 'number of file delimiters do not match'
     print *, 'delim file 1 = ',ns1
     print *, 'delim file 2 = ',ns2
     close(unit1)
     close(unit2)
     stop 1
  endif

  !     read file
  allocate(r1(n1),tl1(n1,ns1))
  rewind(unit1)
  do i = 1,n1
     read(unit1,*) r1(i), (tl1(i,j), j=1,ns1)
  end do
  close(unit1)
  allocate(r2(n1),tl2(n1,ns1))
  rewind(unit2)
  do i = 1,n1
     read(unit2,*) r2(i), (tl2(i,j),j=1,ns1)
     if (abs(r1(i)-r2(i)).gt.rdiff) then
        print *, 'ranges do not match'
        print *, 'range ',i,' file 1 = ',i,r1(i)
        print *, 'range ',i,' file 2 = ',i,r2(i)
        close(unit1)
        close(unit2)
        stop 1
     endif
  end do
  close(unit2)
  print *, 'ranges match'

  !     print summary
  nerr=0
  nerr2=0
  nerr3=0
  dtl_max=0
  do i = 1,n1
     do j=1,ns1
        dtl=abs(tl1(i,j)-tl2(i,j))
        if (dtl.gt.dtl_max) dtl_max=dtl
        if(dtl.gt.tldiff) then
           if (nerr.eq.0) then ! print table header on first error
              print'(/a)','   ix   iz    range    tl1    tl2      | diff'
              print*, repeat('-',37),'+',repeat('-',6)
           endif
           nerr=nerr+1
           write(*,'(2i5,f9.2)',advance='no') i,j,r1(i)

           if(tl1(i,j).gt.tlmax)then
              write(*,'(a,f9.5,a)',advance='no') ''//achar(27)//'[31m',tl1(i,j),''//achar(27)//'[0m'
           else
              write(*,'(f9.5)',advance='no') tl1(i,j)
           endif

           if(tl2(i,j).gt.tlmax)then
              write(*,'(a,f9.5,a,a)',advance='no') ''//achar(27)//'[31m',tl2(i,j),''//achar(27)//'[0m',' | '
           else
              write(*,'(f9.5,a)',advance='no') tl2(i,j),' | '
           endif

           if(dtl.gt.(tl_red+comp_diff)) then
              print '(a,f8.5,a)',''//achar(27)//'[31m',dtl,''//achar(27)//'[0m'
              nerr2=nerr2+1
           else
              write(*,'(f8.5)') dtl
           endif

           if((tl1(i,j).lt.tlmax).and.(tl2(i,j).lt.tlmax).and.(dtl.gt.(tl_red+comp_diff)))then
              nerr3=nerr3+1
           endif
        endif
     enddo
  enddo
  print '(/a,f8.5,a,i0)',' number of errors found (>',tldiff,'): ',nerr
  if(tl_red.ge.tldiff) then
     print '(a,f8.5,a,i0)',' number of errors found (>',tl_red+comp_diff,'): ',nerr2
     print '(a,f8.5,a,f5.1,a,i0)',' number of errors found (>',tl_red+comp_diff,' and tl < ',tlmax,'): ',nerr3
  endif
  print '(a,f8.5)',' maximum error : ',dtl_max

  if (nerr3.gt.0) then
     stop 1
  endif
end program tsdiff
