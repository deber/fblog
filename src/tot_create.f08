subroutine tot_create(pathname, status, msg)
  character(len=*),intent(in) :: pathname
  integer, intent(out), optional :: status
  character(len=256), intent(out), optional :: msg
  integer :: io_rec_len, iostat, u
  character(len=256) :: iomsg
  type (header) h
  logical :: opened
  character(len=REC_LEN) :: fake
  inquire(iolength=io_rec_len) fake  
  inquire(file=pathname, opened=opened) 
  if (opened) error stop "tot_create(): " // pathname // " already opened"
  status = 0
  msg = ""
  open (newunit=u, file=pathname, status='new', access='direct', recl=io_rec_len , iostat=iostat, iomsg=iomsg)
  if (iostat /= 0) then
    status = iostat
    msg = iomsg
    close(u)
    return
  end if
  h%total = 0
  h%first = 0
  h%last = 0
  h%total_free = 0
  h%last_free = 0
  h%next_year = 0
  write(u, rec=1) h
  close(u)   
end subroutine tot_create
