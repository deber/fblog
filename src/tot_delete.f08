subroutine tot_delete(pathname, timestamp, status, msg)
  character(len=*), intent(in) :: pathname
  character(len=16), intent(in) :: timestamp
  integer, intent(out), optional :: status  
  character(len=256), intent(out), optional :: msg  
  character(len=256) :: iomsg
  integer ::  io_rec_len, iostat, present_record, u
  type(article) a, previous_record, next_record
  type(header) h
  type(free_record) fr
  logical :: opened
  character(len=REC_LEN) :: fake
  inquire(iolength=io_rec_len) fake  
  inquire(file=pathname, opened= opened) 
  if (opened .eqv. .true.) error stop "tot_delete(): " // pathname // " already opened"  
  print'(2a)', "Received pathname:",pathname
  print'(2a)', "Received timestamp=",timestamp
  open(newunit=u, file=pathname, status='old', access='direct', recl=io_rec_len, iostat=iostat, iomsg=iomsg)
  if (iostat /= 0) then
    print'(a,g0,2a)', "Status=", iostat, " Msg: ",trim(iomsg)
    status = iostat ; msg =iomsg
  end if
  read(u, rec=1) h
  if (h%total == 0) then
    status = -1
    msg = "Empty data base"
    close(u)
    return
  end if
  read(u, rec = h%first) a
  if (h%total == 1) then                   ! Case of an unique record
    write(u, rec=h%last) repeat(char(0),rec_len)  ! Nullify record  
    h%total = 0; h%first = 0; h%last = 0   ! Update the base
    write(u, rec=1) h
    status = 0
    msg = a%id
    close(u)
    return    
  end if
  do 
    if (a%id == timestamp) then
      if (a%previous > 0) then
         read(u, rec=a%previous) previous_record
         present_record = previous_record%next
      end if 
      if (a%next > 0) then
         read(u, rec=a%next) next_record
         present_record = next_record%previous
      end if   
      if (a%previous == 0) then                   ! Case of oldest record
        next_record%previous =0
        write(u, rec=a%next) next_record
        h%first = a%next
      else  if (a%next == 0) then                 ! Case of newest record
        previous_record%next = 0
        write(u, rec=a%previous) previous_record
        h%last = a%previous
      else                                        ! General case
        read(u, rec=a%previous) previous_record
        previous_record%next = a%next
        write(u, rec=a%previous) previous_record
        read(u, rec=a%next) next_record
        next_record%previous = a%previous
        write(u, rec=a%next) next_record
      end if
      h%total = h%total - 1
      h%total_free = h%total_free + 1
      fr%previous = h%last_free
      write(u, rec=present_record) fr
      h%last_free = present_record
      write(u, rec=1) h
      status = 0
      msg = a%id
      close(u)
      return
    end if
    if (a%next == 0) exit
    read(u, rec=a%next) a
  end do
  status = -1
  msg = "Not found"
  close(u)
end subroutine tot_delete
