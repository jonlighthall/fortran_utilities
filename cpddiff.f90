program cpddiff
  ! cpd diff - calculate difference between two ducting probability files
  !
  ! Aug 2022 JCL
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
  real(kind=srk)::tl_diff=0.001
  real(kind=srk),parameter :: tl_red=0.01, comp_diff=0.001
  real(kind=srk),parameter :: tlmax=-20*log10(2.**(-23))
  ! ----------------------------------------------------------
  integer :: nm1,nm2
  real(kind=srk)::f1,f2
  integer,dimension(:),allocatable :: dp1,dp2,dp_check
  real,dimension(:),allocatable :: p_check
  integer :: ld,pln,fln,dln,wln,aln
  character fmt*64
  call get_command_argument(1,fname1,ln1)
  call get_command_argument(2,fname2,ln2)
  call get_command_argument(3,tlthresh,ln3)
100 format(a,f7.3)
  print 100,' tl max = ',tlmax
  print 100,' tl dif = ',tl_red
  print 100,' tl com = ',comp_diff

  !     set file names
  if (ln1.eq.0) then
     fname1='calcpduct.out'
  end if
  if (ln2.eq.0) then
     fname2='std/std0_calcpduct.out'
  end if

  if (ln3.gt.0) then
     read(tlthresh,*)tl_diff
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
        read(unit1,*) nm1,f1 ! read header line
        print '(a)', 'header line:'
        print '(a,i0)',' nm1 = ',nm1
        print '(a,f7.1)','  f1 = ',f1
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
        ! check precision
        allocate(dp1(ns1+1))
        i=0
        ls=0
        do j=1,len(trim(dummy))
           if(dummy(j:j)=='.')ld=j
           if(dummy(j:j)==' ')then
              if(j.eq.(ls+1))then ! same space
              else                ! new space
                 i=i+1
                 dp1(i)=j-ld-1
              endif
              ls=j
           endif
        enddo
        dp1(ns1+1)=j-ld-1
        print*,dp1
     else
        read(unit1,*,iostat=io)
     endif
     if (io/=0) exit
     n1=n1+1
  enddo
  print '(2a,i5,a,i3,a)',trim(fname1),' has ',n1,' lines and ',ns1,' delimiters'
  ! compare dimensions with header
  if (n1.eq.nm1) then
     print *, 'number of lines matches number of environments: ',n1
  else
     print *, 'number of lines does not match number of environments'
     print *, 'number of lines = ',n1
     print *, 'number of environments = ',nm1
     close(unit1)
     close(unit2)
     stop 'env'
  endif

  n2=0
  ns2=0
  ls=0
  do
     if (n2.eq.0) then
        read(unit2,*) nm2,f2 ! read header line
        print *,'nm2 = ',nm2
        print *,'f2 = ',f2

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
        ! check precision
        allocate(dp2(ns2+1))
        i=0
        ls=0
        do j=1,len(trim(dummy))
           if(dummy(j:j)=='.')ld=j
           if(dummy(j:j)==' ')then
              if(j.eq.(ls+1))then ! same space
              else                ! new space
                 i=i+1
                 dp2(i)=j-ld-1
              endif
              ls=j
           endif
        enddo
        dp2(ns2+1)=j-ld-1
        print*,dp2
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
  read(unit1,*)dummy ! read header line
  do i = 1,n1
     read(unit1,*) r1(i), (tl1(i,j), j=1,ns1)
  end do
  close(unit1)
  allocate(r2(n1),tl2(n1,ns1))
  rewind(unit2)
  read(unit2,*)dummy ! read header line
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

  ! determine precision to check
  allocate(dp_check(ns1+1),p_check(ns1+1))
  do i=1,ns1+1
     dp_check(i)=min(dp1(i),dp2(i))
     p_check(i)=10.**(-dp_check(i))
     print'(es10.2)',p_check(i)
  enddo
  print*,'minimum precisions per column'
  print*,dp_check

  !     print summary
  nerr=0
  nerr2=0
  nerr3=0
  dtl_max=0
  do i = 1,n1
     do j=1,ns1
        dtl=abs(tl1(i,j)-tl2(i,j))

        tl_diff=p_check(j+1)/2
        if(dtl-tl_diff.gt.epsilon(dtl)) then
                   if (dtl.gt.dtl_max) dtl_max=dtl
           if (nerr.eq.0) then ! print table header on first error
              print'(/a)','   ix   iz    range   tl1          tl2        |  diff        thresh'
              print*, repeat('-',45),'+',repeat('-',28)
           endif
           nerr=nerr+1
           write(*,'(2i5,f9.2)',advance='no') i,j,r1(i)
           pln=11
           wln=4
           dln=dp_check(j+1)+1
           aln=floor(log10(tl1(i,j)))
           fln=aln+1+dln+1
           write(fmt,'(a,i0,a,i0,a,i0,a,i0,a)')'(',wln-aln+1,'X,f',fln&
                &,'.',dln,',',max(0,pln-fln-(wln-aln))+1,'X)'
           write(*,fmt,advance='no') tl1(i,j)
           write(*,fmt,advance='no') tl2(i,j)
           write(*,'(a)',advance='no') ' | '
           write(*,fmt,advance='no') dtl

           write(*,fmt) tl_diff

           if((tl1(i,j).lt.tlmax).and.(tl2(i,j).lt.tlmax).and.(dtl.gt.(tl_red+comp_diff)))then
              nerr3=nerr3+1
           endif
        endif
     enddo
  enddo
  print '(/a,f6.3,a,i0)',' number of errors found (>',tl_diff,'): ',nerr
  if(tl_red.ge.tl_diff) then
     print '(a,f6.3,a,i0)',' number of errors found (>',tl_red+comp_diff,'): ',nerr2
     print '(a,f6.3,a,f5.1,a,i0)',' number of errors found (>',tl_red+comp_diff,' and tl < ',tlmax,'): ',nerr3
  endif
  print '(a,f8.5)',' maximum error : ',dtl_max

  if (nerr3.gt.0) then
     stop 1
  endif
end program cpddiff
