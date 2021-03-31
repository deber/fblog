subroutine tot_index(pathname, id, id_rec, status, msg)
  character(len=*), intent(in) :: pathname
  character(len=16), intent(in) :: id
  integer, intent(in) :: id_rec
  integer, intent(out), optional :: status  
  character(len=256), intent(out), optional :: msg
  integer :: iostat, io_rec_len, new_rec, next_day_rec, next_month_rec, u
  character(len=256) :: iomsg
  type (header) h
  type (year) y
  type (month) m
  type (day) d
  logical :: exist, opened
  character(len=REC_LEN) :: fake
  inquire(iolength=io_rec_len) fake  
  inquire(file=pathname, opened=opened, exist=exist)!, size=file_size) 
  if (opened) error stop "tot_index(): " // pathname // " already opened"
  if (exist .eqv. .false.) error stop pathname // " do not exist"
  open(newunit=u, file=pathname, status='old', access='direct', recl=io_rec_len, iostat=iostat, iomsg=iomsg)
  if (iostat /= 0) then
    status = iostat ; msg =iomsg
    return
  end if
  new_rec = tot_get_new_rec(u)
  read(u, rec=1) h
  if (h%next_year /= 0) then                                       ! General case
    read(u, rec=h%next_year) y
    if (id(1:4) == y%date(1:4)) then          ! Same year
      y%total_year = y%total_year + 1
      write(u, rec=h%next_year) y
      read(u, rec=y%next_month) m
      if (id(5:6) == m%date(5:6)) then        ! Same month of same year
        m%total_month = m%total_month + 1
        write(u, rec=y%next_month) m
        read(u, rec= m%next_day) d
        if (id(7:8) == d%date(7:8)) then      ! Same day than same month of same year
          d%total_day = d%total_day + 1
          if (d%next_article == 0) d%next_article = id_rec ! Case of first article of the day
          write(u, rec=m%next_day) d
        end if
        if (id(7:8) > d%date(7:8)) then       ! Day later than same month of same year
          d%total_day = 1
          d%next_article = id_rec
          write(u, rec=new_rec) d          
        end if
      else if (id(5:6) > m%date(5:6)) then    ! Month later of same year
        do while (id(5:6) == m%date(5:6))
          if (m%next_month == 0) then
          end if
        end do
        if (m%next_month /= 0) read(u, rec=m%next_month) m 
      else if (id(1:4) > y%date(1:4)) then    ! Year later
      end if
      close(u)
      return
    end if
  end if
  if (h%next_year == 0) then                                       ! Case of empty base
    d%date = id(1:8)
    d%total_day = 1
    d%next_day = 0
    d%next_article = id_rec
    write(u, rec=new_rec) d
    next_day_rec = new_rec
    new_rec = tot_get_new_rec(u)
    read(u, rec=1) h
    m%date = id(1:6) // "00"
    m%total_month = 1
    m%next_month = 0
    m%next_day = next_day_rec
    write(u, rec= new_rec) m
    next_month_rec = new_rec
    new_rec = tot_get_new_rec(u)
    read(u, rec=1) h
    y%date = id(1:4) // "0000"
    y%total_year = 1
    y%next_year = 0
    y%next_month = next_month_rec
    write(u, rec=new_rec) y
    h%next_year = new_rec
    write(u, rec=1) h
    close(u)
    return
  end if  
end subroutine tot_index