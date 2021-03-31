program import
  use tot, only: article, tot_create, tot_append!, iostat, iomsg
  implicit none
  character(len=*), parameter :: VERSION = "0.1"
  integer :: argc, i, iostat, j=0, stat, u1, u2, u3
  !character(len=14) :: id
  character(len=16) :: id
  character(len=128) :: argv(4), buf1, buf2, title
  character(len=256) :: src_pathname, db_pathname, msg
  character(len=:), allocatable :: buffer
  argc = command_argument_count()
  if (argc == 0 .or. argc == 3 .or. argc > 4) call usage
  do i = 1, argc
    call get_command_argument(i,argv(i))
  end do
  select case (trim(argv(1)))
    case ("-h", "--help"); call usage
    case ("'-v", "--version") ; print '(a)', VERSION; stop 
    case ("-d", "--directory") ; src_pathname=trim(argv(2))
    case default; call usage
  end select
  select case (trim(argv(3)))
    case ("-t", "--table"); db_pathname = trim(argv(4))
    case default; call usage
  end select
  print'(a)',"Importation utility for TOT files version "//VERSION
  print'(2a)', " * Directory=", trim(src_pathname), " * Table=", trim(db_pathname)
  print'(a)', " * Copying list of files into tmp_file"
  call execute_command_line(command="ls -1 " // trim(src_pathname) // " > tmp_file")
  open(newunit=u1, file="tmp_file")
  call tot_create(db_pathname, stat, msg)
  if (stat /= 0) error stop trim(msg)
  allocate(character :: buffer); buffer = ""
  do
    read(u1, '(a)', iostat=iostat) buf1
    if (iostat /= 0) exit
    open(newunit=u2, file=trim(src_pathname) // "/" // trim(buf1))
    read(u2, '(a)') buf2
    title = trim(buf2)
    close(u2)
    id = buf1(1:14) // "00"
    print'(1x,a,2x,a)', id, trim(title)
    call tot_append(pathname=trim(db_pathname), id=id, status=stat, msg=msg)
    if (stat /= 0) print'(2a)', "tot_insert: ", trim(msg)
    open(newunit=u3, file= id // ".g")
    buffer = "&GOPHER" // new_line('a') //' DESCRIPTION="' // trim(title) // '",' //new_line('a')&
             & // " /" // new_line('a')
    write (u3, '(a)') buffer
    close(u3)
    j = j + 1
  end do
  print'(a,g0,a)'," * Imported ",j," records into TOT file " // trim(db_pathname)
  close(u1, status='delete')
  print'(a)'," * Deleted tmp_file"
  print'(a)',"Bye!"
contains
  subroutine usage
    print'(/,a)',&
    "USAGE: " // "import" // " <dir> <table>"  // new_line('a') // &
    "" // new_line('a') // &
    " OPTIONS" // new_line('a') // &
    "    -d   --directory     source directory" // new_line('a') // &
    "    -t   --table         pathname of output table" // new_line('a') // &    
    "    -h   --help          display this help and exit" // new_line('a') // &
    "    -v   --version       output version information and exit"  
    stop
  end subroutine usage
end program import
