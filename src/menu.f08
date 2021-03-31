recursive subroutine menu
  character(len=1) :: choice
  call read_conf
  print '(/,(1x,a))',&
    "[0]  Quit",&
    "[1]  Add an aricle",&
    "[2]  Modify an article (N/A)",&
    "[3]  Delete an article",&
    "[4]  Display the list of articles",&
    "[5]  Generate the blog (N/A)",&
    "[6]  Blog Settings"
  write(*, '(/,1x,a)', advance='no') "> Your choice: "
  read(*,'(a)') choice
  select case (choice)
  case ("0"); stop
  case ("1"); call add_article
  case ("3"); call del_article
  case ("4"); call list_articles
  case ("6"); call menu_blog_setting
  case default; print '(1x,3a)', "? Wrong entry [", choice, "]" ; call menu
  end select
end subroutine menu
