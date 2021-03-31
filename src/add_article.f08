recursive subroutine add_article
  integer :: cmdstat, iostat, u1, u2
!  integer(kind=8) :: fake_date = 0
  logical :: file_exist
  character(len=1) :: choice
  character(len=128) :: buf  
  character(len=256) :: cmdmsg, iomsg, title
  character(len=8) :: date
  character(len=10) :: time
  character(len=:), allocatable :: pathname, buffer
  type (article) :: a
  allocate(character :: pathname)
  call date_and_time(date, time)
  a%id = date // time(1:6) // time(8:9)
  inquire(file=trim(data_dir) // a%id // ".blog", exist=file_exist)
  if (file_exist) then
    print '(1x,a)', "? Error: file exist"
    call menu
  end if
!  write(*,'(/,1x,a)', advance='no') "> Enter date and time (YYYYMMDDHHMMSS) (optional): "
!  read(*,'(a)') buf
!  if (len_trim(buf) /= 0) then
!    read(buf, '(i14)', iostat=iostat, iomsg=iomsg) fake_date  ! Digit entry [0,9] only
!    if (iostat /=0) then
!      print'(2(1x,a))', "? " // trim(iomsg) // new_line('a') ,"! Bad entry, try again"
!      call add_article
!    end if  
!    if (len(adjustl(trim(buf))) < 14) then
!      print'(1x,a)', "! Incomplete date/time"
!      call add_article
!    else
!      a%id = adjustl(buf(1:14)) // "00"
!    end if  
!  end if  
  write(*,'(/,1x,a)', advance='no') "> Enter the title (optional): "
  read(*,'(a)') buf
  if (len_trim(buf) == 0) then
    title = a%id(1:4)//"-"//a%id(5:6)//"-"//a%id(7:8)//" "//a%id(9:10)//":"//a%id(11:12)//":"//a%id(13:14)//"."//a%id(15:16)
  else
    title = buf
  end if
  print'(1x,2a)', "* Title: ", trim(title)
  ! Create a new file "timestamp.blog"
  open(newunit=u1, file=trim(data_dir) // a%id // ".blog", status='new', iostat=iostat, iomsg=iomsg)  
  if (iostat /= 0) error stop trim(iomsg)  
  write(u1,'(a)',iostat=iostat, iomsg=iomsg) trim(title),&
    &"# Above line is text title. Begin text body at this line. Delete this message."
  if (iostat /= 0) error stop trim(iomsg)  
  close(u1)
  ! Create a new file Gopher "timestamp.g"
  open(newunit=u2, file=trim(data_dir) // a%id // ".g", status='new', iostat=iostat, iomsg=iomsg)
  if (iostat /= 0) error stop trim(iomsg)
  if (.not. allocated(buffer)) allocate(character :: buffer); buffer = ""
  buffer = "&GOPHER" // new_line('a') //' DESCRIPTION="' // trim(title) // '",' //new_line('a')&
            & // " /" // new_line('a')
  write (u2, '(a)') buffer
  close(u2)
  pathname = trim(data_dir) // SOFT // ".dat"
  call tot_append(pathname, a%id, iostat, iomsg)
  if (iostat /= 0) error stop trim(iomsg)  
  print '(/,(1x,a))',&
  "[0]  Return",&
  "[1]  Open editor and write article now",&
  "[2]  Write article later"
  write(*, '(/,1x,a)', advance='no') "> Your choice: "
  read(*,'(a)') choice
  select case (choice)
  case ("0", "2"); call menu
  case ("1")
  print'(2a)', "le choix est : ", choice
    call execute_command_line(command= trim(editor) // " " // trim(data_dir) // a%id // ".blog",&
                            cmdstat=cmdstat, cmdmsg=cmdmsg)
    if (cmdstat /= 0) error stop trim(cmdmsg)
   call menu
  case default; print '(1x,3a)', "? Wrong choice [", choice, "]"; call add_article
  end select
end subroutine add_article
