recursive subroutine menu_blog_setting
  integer :: iostat, u
  character :: choice
  character(len=256) :: buf, iomsg
  call read_conf
  config: do
    print '(1x,a)',&
      "",&
      "[0]  Return",&
      "[1]  Set new blog titre (" // trim(blog_title) // ")",&
      "[2]  Set new file name of home page (" // trim(index_file) // ")"
    print '(1x,a,i0,a)',&  
      "[3]  Set new number of articles displayed on home page (", max_articles, ")"
    print '(1x,3a,/)',&
      "[4]  Set prefered editor (", trim(editor), ")"
    write(*,'(1x,a)', advance='no') "> Your choice: "
    read(*,'(a)') choice
    select case (choice)
    case ("0"); call menu
    case ("1")
      write(*, '(/,1x,a)', advance='no')&
        "New titre: "
      read(*,'(a)') blog_title
      open (newunit=u, file=trim(data_dir) // SOFT //'.conf', DELIM='QUOTE', status='old', iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) error stop trim(iomsg)   
      write(u, nml=settings, iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) error stop trim(iomsg)
      print'(1x,2a)',"* Set blog titre to: ", trim(blog_title)
      close(u)
    case ("2")
      write(*,'(/,1x,a)', advance='no') "New file name: "
      read(*,'(a)') index_file
      open (newunit=u, file=trim(data_dir) // SOFT // '.conf', DELIM='QUOTE', status='old', iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) error stop trim(iomsg)
      write(unit=u, nml=settings, iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) error stop trim(iomsg)    
      print'(1x,2a)',"* Set file name home page to: ", trim(index_file)
      close(u)
    case ("3")
       write(*, '(/,2x,a)', advance='no')&
        "How many articles at home page: "
      read(*,'(i2)') max_articles
      open (newunit=u, file=trim(data_dir) // SOFT // '.conf', DELIM='QUOTE', status='old', iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) error stop trim(iomsg)
      write(u, nml=settings, iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) error stop trim(iomsg)    
      print'(1x,a,i0)',"* Set number of articles to: ", max_articles
      close(u)
    case ("4");
      write(*,'(/,1x,a)', advance='no') "New editor: "
      read(*,'(a)') buf
      if (len_trim(buf) <= 1) call menu_blog_setting
      editor = adjustl(trim(buf))
      open (newunit=u, file=trim(data_dir) // SOFT // '.conf', DELIM='QUOTE',status='old', iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) error stop trim(iomsg)
      write(u, nml=settings, iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) error stop trim(iomsg)    
      print'(1x,2a)',"* Set editor to: ", trim(editor)
      close(u)
    case default; print '(1x,3a,/)', "? Wrong choice [", choice, "]"; call menu_blog_setting
    end select
  end do config  
end subroutine menu_blog_setting
