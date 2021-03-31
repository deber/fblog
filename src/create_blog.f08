subroutine create_blog
  integer :: iostat, u,stat
  character(len=:), allocatable :: pathname
  character(len=256) :: iomsg, msg
  write(*,'(1x,a)', advance='no') "* Creating configuration file '" // SOFT //".conf' in directory " // trim(data_dir)
  open (newunit=u, file=trim(data_dir) // SOFT // '.conf',DELIM='QUOTE', status='new', iostat=iostat, iomsg=iomsg)
  if (iostat /= 0) then
    print'(/,1x,2a)', "? error: ", trim(iomsg)
    call menu_init
  end if  
  editor= "vi"
  max_articles = 5
  index_file = "index.html"
  blog_title = "My Fortran Blog"
  write(u, nml=settings, iostat=iostat, iomsg=iomsg)
  if (iostat /= 0) error stop trim(iomsg)
  print '(a)', " ...[OK]"
  close(u)
  write(*,'(1x,a)', advance='no') "* Creating data base file '" // SOFT // ".dat' in directory " // trim(data_dir)
  if (iostat /= 0) then
    print'(/,1x,2a)', "? error: ", trim(iomsg)
    call menu_init
  end if   
  print '(a)', " ...[OK]"
  allocate(character(len=len(trim(data_dir)//"fblog.dat")) :: pathname)
  pathname = trim(data_dir) // "fblog.dat"
  call tot_create(pathname, status=stat, msg=msg)
  call menu
end subroutine create_blog
