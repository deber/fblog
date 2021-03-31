! INFO: manip craignos ! le backup est "tot_append.f08~" enregistré le 28 mars à 20:23
subroutine tot_append(pathname, id, status, msg)
  character(len=*), intent(in) :: pathname
  character(len=16), intent(in) :: id
  integer, intent(out), optional :: status  
  character(len=256), intent(out), optional :: msg
  integer ::  file_records_count, file_size, io_rec_len, iostat, new_rec, u !previous_free, u
  character(len=256) :: iomsg
  type (header) h
  type (article) previous_record, next_record
  logical :: exist, opened
  character(len=REC_LEN) :: fake
  inquire(iolength=io_rec_len) fake  
  inquire(file=pathname, opened=opened, exist=exist, size=file_size) 
  if (opened) error stop "add_article(): "// pathname // " already opened"
  if (exist .eqv. .false.) error stop pathname // " do not exist"
  file_records_count = file_size / rec_len
  open(newunit=u, file=pathname, status='old', access='direct', recl=io_rec_len, iostat=iostat, iomsg=iomsg)
  if (iostat /= 0) then
    status = iostat ; msg =iomsg
    return
  end if
  new_rec = tot_get_new_rec(u)
  read(u, rec=1) h
  if (h%first == 0) then   ! Empty data base
    h%first = new_rec
    h%last = new_rec
    h%total = 1
    write(u, rec=1) h
    next_record%id = id
    write(u, rec=new_rec) next_record
  else
    read(u,rec=h%last) previous_record
    previous_record%next = new_rec
    write(u, rec=h%last) previous_record
    next_record%id = id
    next_record%previous = h%last
    write(u, rec=new_rec) next_record
    h%total = h%total + 1
    h%last = new_rec
    write(u, rec=1) h
  end if
!  call tot_index_year(u, id)
  close(u)
end subroutine tot_append
