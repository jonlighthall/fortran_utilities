program readtest
  implicit none
  interface
     integer function getunit(unit)
       integer, intent(out), optional :: unit
     end function getunit
  end interface
  integer,parameter :: srk = selected_real_kind(2)
  real(kind=srk), dimension(:), allocatable :: r1,r2
  real(kind=srk), dimension(:,:), allocatable :: pr1,pi1,tl1,tl2,pr2,pi2
  complex(kind=srk), dimension(:,:), allocatable :: p1,p2
  integer :: i,n1,io,ln1,ln2,unit1,unit2,n2,ln3,nerr,ns1,j,ls,ns2,np1
  character(len=256) :: fname1, fname2, tlthresh,dummy
  real(kind=srk)::dp_max,dp
  ! set thresholds
  real(kind=srk),parameter :: rdiff=0.01
  real(kind=srk) :: pdiff=1e-3

  call get_command_argument(1,fname1,ln1)
  call get_command_argument(2,fname2,ln2)
  call get_command_argument(3,tlthresh,ln3)

  !     set file names
  if (ln1.eq.0) then
     fname1='nspe02.asc'
  end if
  if (ln2.eq.0) then
     fname2='std/case2r.rtl'
  end if

  if (ln3.gt.0) then
     read(tlthresh,*)pdiff
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
  np1=(ns1)/2

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
     print *, 'file lengths match'
  else
     print *, 'file lengths do not match'
     print *, 'length file 1 = ',n1
     print *, 'length file 2 = ',n2
     close(unit1)
     close(unit2)
     stop 'len'
  endif

  if (ns1.eq.ns2) then
     print *, 'file delimiters match'
  else
     print *, 'file delimiters do not match'
     print *, 'delim file 1 = ',ns1
     print *, 'delim file 2 = ',ns2
     close(unit1)
     close(unit2)
     stop 'delim'
  endif

  !     read file
  allocate(r1(n1),tl1(n1,ns1),p1(n1,np1),pr1(n1,np1),pi1(n1,np1))
  rewind(unit1)
  do i = 1,n1
     read(unit1,*) r1(i), (tl1(i,j), j=1,ns1)
  end do
  close(unit1)
  allocate(r2(n1),tl2(n1,ns1),p2(n1,np1),pr2(n1,np1),pi2(n1,np1))
  rewind(unit2)
  do i = 1,n1
     read(unit2,*) r2(i), (tl2(i,j),j=1,ns1)
     if (abs(r1(i)-r2(i)).gt.rdiff) then
        print *, 'ranges do not match'
        print *, 'range ',i,' file 1 = ',i,r1(i)
        print *, 'range ',i,' file 2 = ',i,r2(i)
        close(unit1)
        close(unit2)
        stop 'range'
     endif
  end do
  close(unit2)
  print *, 'ranges match'
  ! calculate pressure
  do i = 1,n1
     do j=1,np1
        pr1(i,j)=tl1(i,2*j-1)
        pi1(i,j)=tl1(i,2*j)
        p1(i,j)=cmplx(pr1(i,j),pi1(i,j))
     enddo
  enddo
  do i = 1,n1
     do j=1,np1
        pr2(i,j)=tl2(i,2*j-1)
        pi2(i,j)=tl2(i,2*j)
        p2(i,j)=cmplx(pr2(i,j),pi2(i,j))
     enddo
  enddo

  !     print summary
  nerr=0
  dp_max=0
  do i = 1,n1
     do j=1,np1
        dp=abs(p1(i,j)-p2(i,j))
        if (dp.gt.dp_max) dp_max=dp
        if(dp.gt.pdiff) then
           if (nerr.eq.0) then
              print'(/a)','   ix   iz    range    tl1    tl2 | diff'
              print*, repeat('-',33),'+',repeat('-',6)
           end if
           nerr=nerr+1
           write(*,'(2i5,f9.2, 4f9.5,a,f6.5)') i,j,r1(i), p1(i,j),p2(i,j),' | ',dp
        end if
     end do
  enddo
  print '(/a,f6.5,a,i0)',' number of errors found (>',pdiff,'): ',nerr
  print '(a,f6.4)',' maximum error : ',dp_max
end program readtest
