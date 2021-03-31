recursive subroutine menu_init
  logical :: config_exist=.true.
  character(len=1) :: choice
  character(len=256) :: buf = ""
  print '(/,(1x,a))', "[0]  Quit", &
                  "[1]  Create a new blog",&
                  "[2]  Update the blog in this directory",&
                  "[3]  Update a blog in an other directory"
  write(*, '(/,1x,a)', advance='no') "> Your choice: "
  read(*,'(a)') choice
  select case (choice)
  case ("0"); stop  
  case ("1")  
    print*  
    print '(1x,a)', "[0]  Quit",&  
                    "[1]  Create a blog in this directory",&  
                    "[2]  Create a blog in an other directory",&  
                    "[3]  Return"  
    write(*, '(/,1x,a)', advance='no') "> Your choice: "  
    read(*,'(a)') choice  
    select case (choice)  
    case ("0"); stop  
    case ("1")  
      data_dir = ""  
      call create_blog  
    case ("2")  
      write(*, '(1x,a)', advance='no') "> Enter a directory name: "  
      read(*,'(a)') buf  
      if (len_trim(buf) == 0) call menu_init  
      if ( buf(len_trim(buf):len_trim(buf)) /= "/") buf = trim(buf) // "/"  
      data_dir = trim(buf)  
      call create_blog  
    case ("3"); call menu_init  
    case default; print '(1x,3a)', "? Wrong entry [", choice, "]"; call menu_init  
    end select  
  case("2")  
   data_dir = ""  
   call menu  
  case ("3")  
    write(*, '(1x,a)', advance='no') "> Enter its directory name: "  
    read(*,*) buf  
    if (len_trim(buf) == 0) call menu_init  
    if ( buf(len_trim(buf):len_trim(buf)) /= "/") buf = trim(buf) // "/"  
    data_dir = trim(buf)  
    inquire (exist=config_exist, file=trim(buf) // SOFT // ".conf")  
    if (.not.config_exist) then  
      print '(1x,2a)', "? There is no blog in directory ", trim(buf)  
      call menu_init  
    end if    
    call menu  
  case default; print '(1x,3a)', "? Wrong entry [", choice, "]"; call menu_init  
  end select
end subroutine menu_init
