program tldiff
  ! TL diff - calculate difference between two transmission loss files
  !
  ! The files are assumed to be formatted with range in the first column and TL
  ! in the remaining columns. First, the dimensions of the two files are
  ! compared. The number of lines and the number of columns (delimiters) must
  ! match. Next, the ranges are compared (first column). Each range value must
  ! match to withing the parameter rdiff. Then, the TL is compared, element by
  ! element and summary is printed. If any elements (less than the maximum TL
  ! value) differ by more than the sum of the parameters dtl_print and comp_diff,
  ! the program returns an error.
  !
  ! Aug 2022 JCL
  implicit none
  integer,parameter :: srk = selected_real_kind(2)
  real(kind=srk), dimension(:), allocatable :: r1,r2
  real(kind=srk), dimension(:,:), allocatable :: tl1,tl2
  integer :: i,n1,io,ln1,ln2,unit1,unit2,n2,ln3,nerr,ns1,j,ls,ns2,nerr2,nerr3
  character(len=256) :: fname1, fname2, tlthresh,dummy
  real(kind=srk)::dtl,dtl_max
  ! ----------------------------------------------------------
  ! set thresholds
  real(kind=srk),parameter :: rdiff=0.01
  real(kind=srk)::dtl_print=0.01
  real(kind=srk),parameter :: tl_red=0.1, comp_diff=0.001
  real(kind=srk),parameter :: tlmax=-20*log10(2.**(-23))
  real(kind=srk)::dtl_error
  ! ----------------------------------------------------------
  call get_command_argument(1,fname1,ln1)
  call get_command_argument(2,fname2,ln2)
  call get_command_argument(3,tlthresh,ln3)
100 format(a,f7.3)
  print 100,' tl max = ',tlmax
  print 100,' tl red = ',tl_red
  print 100,' tl com = ',comp_diff
  dtl_error=tl_red+comp_diff
  print 100,' tl err = ',dtl_error

  !     set file names
  if (ln1.eq.0) then
     fname1='nspe02.asc'
  end if
  if (ln2.eq.0) then
     fname2='std/case2r.rtl'
  end if

  if (ln3.gt.0) then
     read(tlthresh,*)dtl_print
     print *, 'using user-defined dtl_print'
  else
     print *, 'using default dtl_print'
  end if
  print 100,' tl dif = ',tl_diff

  !     open files
  open (newunit=unit1, file = fname1, status = 'old')
  open (newunit=unit2, file = fname2, status = 'old')

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

  print *, 'tl1 = ',trim(fname1)
  print *, 'tl2 = ',trim(fname2)

  ! print summary
  nerr=0
  nerr2=0
  nerr3=0
  dtl_max=0
  ! loop over lines
  do i = 1,n1
     ! loop over columns
     do j=1,ns1
        ! calculate difference in TL
        dtl=abs(tl1(i,j)-tl2(i,j))
        ! compare to maximum difference in TL
        if (dtl.gt.dtl_max) dtl_max=dtl
        ! compare to dtl_print
        if(dtl.gt.dtl_print) then
           if (nerr.eq.0) then ! print table header on first error
              print'(/a)','   ix   iz    range    tl1    tl2 | diff'
              print*, repeat('-',33),'+',repeat('-',6)
           endif
           nerr=nerr+1
           write(*,'(2i5,f9.2)',advance='no') i,j,r1(i)

           if(tl1(i,j).gt.tlmax)then
              write(*,'(a,f7.1,a)',advance='no') ''//achar(27)//'[33m',tl1(i,j),''//achar(27)//'[0m'
           else
              write(*,'(f7.1)',advance='no') tl1(i,j)
           endif

           if(tl2(i,j).gt.tlmax)then
              write(*,'(a,f7.1,a,a)',advance='no') ''//achar(27)//'[33m',tl2(i,j),''//achar(27)//'[0m',' | '
           else
              write(*,'(f7.1,a)',advance='no') tl2(i,j),' | '
           endif

           if(dtl.gt.(dtl_error)) then
              if((tl1(i,j).gt.tlmax).or.(tl2(i,j).gt.tlmax))then
                 print '(a,f4.1,a)',''//achar(27)//'[33m',dtl,''//achar(27)//'[0m'
              else
                 print '(a,f4.1,a)',''//achar(27)//'[31m',dtl,''//achar(27)//'[0m'
              endif
              nerr2=nerr2+1
           else
              write(*,'(f4.1)') dtl
           endif

           if((tl1(i,j).lt.tlmax).and.(tl2(i,j).lt.tlmax).and.(dtl.gt.(dtl_error)))then
              nerr3=nerr3+1
           endif
        endif
     enddo
  enddo
  print '(/a,f6.3,a,i0)',' number of errors found (>',dtl_print,'): ',nerr
  if(tl_red.ge.dtl_print) then
     print '(a,f6.3,a,i0)',' number of errors found (>',dtl_error,'): ',nerr2
     print '(a,f6.3,a,f5.1,a,i0)',' number of errors found (>',dtl_error,' and tl < ',tlmax,'): ',nerr3
  endif
  print '(a,f6.3)',' maximum error : ',dtl_max

  print *, 'tl1 = ',trim(fname1)
  print *, 'tl2 = ',trim(fname2)
  if (nerr3.gt.0) then
     print *, ''//achar(27)//'[31mERROR'//achar(27)//'[0m'
     stop 1
  else
     print *, ''//achar(27)//'[32mOK'//achar(27)//'[0m'
  endif
end program tldiff
