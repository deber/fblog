subroutine tot_list(pathname, buffer, status, msg)
  character(len=*), intent(in) :: pathname
  integer, intent(inout) :: status
  character(len=16), allocatable, intent(out) :: buffer(:)    
  character(len=256), intent(inout) :: msg
  integer :: i, io_rec_len, iostat, nextrec, u 
  character(len=256) :: iomsg
  type (header) h
  type (article) a
  logical :: exist, opened
  character(len=REC_LEN) :: fake
  inquire(iolength=io_rec_len) fake  
  inquire(file=pathname,opened= opened, exist=exist)
  if (opened .eqv. .true.) error stop "tot_list() " // pathname // " already opened"
  if (exist .eqv. .false.) error stop pathname // " do not exist"
  open(newunit=u, file=pathname, status='old', access='direct', recl=io_rec_len, iostat=iostat, iomsg=iomsg)
  if (iostat /= 0) then
    status = iostat ; msg = iomsg
    return
  end if
  read(u, rec=1) h ! Read base information
  if (h%total == 0) then  ! Case of empty data base
    status = -1
    msg = "No record available"
    close(u)
    return
  end if
  allocate(character(len=16) :: buffer(h%total))
  buffer = ""
  nextrec = h%first
  i = 1
  do
    read(u, rec=nextrec) a
    nextrec = a%next
    write(buffer(i),'(a)') a%id
    if (nextrec == 0) exit
    i= i +1
  end do
  close(u)
print'(a,g0)',"h%total=",h%total  
end subroutine tot_list
