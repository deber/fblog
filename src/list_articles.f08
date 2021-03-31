subroutine list_articles
  integer :: exitstat=0, i, pos, status, size_list, tty_columns, tty_lines, tty_size(2), u1, u2,  viewport_lines  
  character(len=1) :: choice
  character(len=256) :: msg
  character(len=:), allocatable :: buf
  character(len=:), allocatable :: pathname
  character(len=16), allocatable :: buffer_list(:)
  character(len=*), parameter :: exec = "stty size > stty_size.txt"
  call execute_command_line(command=exec, exitstat=exitstat)
  if (exitstat == 0) then
    open(newunit=u1, file="stty_size.txt", status='old')
    read(u1,*) tty_size
    close(u1, status='delete')
    tty_lines = tty_size(1); tty_columns = tty_size(2)
  else
    tty_lines = 24 ; tty_columns = 80
  end if
  if (tty_columns < 80) then
    print '(1x,a,g0,a)', "! Your windows is ", tty_columns, " columns wide only, please enlarge&
    & your windows up to 80 columns or more."
    call menu
  end if
  allocate(character :: pathname)
  allocate(character(len=16) :: buffer_list(1))
  allocate(character(len=tty_columns) :: buf)
  pathname = trim(data_dir) // SOFT // ".dat"
  status = 0
  call tot_list(pathname, buffer_list, status, msg)
  if (allocated(pathname)) deallocate(pathname) ! Memory leak
  if (status /= 0) then
    print'(2a)', " ! ", trim(msg)
    if (allocated(buf)) deallocate(buf) ! Memory leak
    if (allocated(buffer_list)) deallocate(buffer_list) ! Memory leak
    call menu    
  end if
  size_list = size(buffer_list)
  viewport_lines = tty_lines - 4
  print*
  pos = size_list
  do
    if (pos < 1) cycle
    do i = pos, pos - viewport_lines, -1
      if (i == 0) exit
      read(buffer_list(i:i),'(a)') buf
      write(*,'(a,2x)', advance='no') buf(1:16)
      pathname = trim(data_dir) // buf(1:16) // ".g"
      open(newunit=u2, file=pathname, DELIM='QUOTE', status='old', iostat=status, iomsg=msg)
      if (status /= 0) error stop "list_articles_open_u2: " // trim(msg)
      read(u2, nml=gopher, iostat=status, iomsg=msg)
      if (status /= 0) then
        print'(a,g0)', "status=", status
        error stop "list_articles_read_u2: " // trim(msg)
      end if
      close(u2)
      write(*,'(a)') trim(description)
    end do
    print'(/,1x,a,"(",g0,"/",g0,")")', "[0] Return  [+] Scroll down  [-] Scroll up  ",pos, i+1
    write(*, '(1x,a)', advance='no') "> Your choice: "
    read(*,'(a)') choice
    select case (choice)
    case ("0");
      if (allocated(buf)) deallocate(buf) ! Memory leak
      if (allocated(buffer_list)) deallocate(buffer_list) ! Memory leak
      call menu
    case("+")
      if (i == 0) cycle
      pos = pos - 1 - viewport_lines
      cycle
    case ("-")
    if (i == (size_list - viewport_lines - 3)) cycle
    if (pos == size_list) cycle
    pos = pos + 1 + viewport_lines
      cycle
    case default; cycle
    end select
  end do
end subroutine list_articles
