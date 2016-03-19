! Copyright (C) 2016 Denis Bernard.
! License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
! This is free software: you are free to change and redistribute it.
! There is NO WARRANTY, to the extent permitted by law.
! Written by Denis Bernard.
!
program fblog
  !
  use :: fblog_m, only: config_file, bad_argument, deleted, deleted_file, deleted_you_could_update_the_config, fblog_dir, i18n,&
       & is_a_wrong_argument, init_conf, path, no_fblog_directory_at, no_fblog_directory_here, not_modified, now_you_should_update&
       &, post_amount, show_version, terminate, there_is_not_directory, too_much_arguments, update
  use :: io_m, only: add_post, config, create_blog, delete_css, delete_page, delete_post, edit_css, edit_page, edit_post, help,&
       & list_css, list_pages, list_posts
  use :: tty_m, only: console
  !
  implicit none
  !
  logical :: config_has_been_recorded, css_has_been_recorded, file_exist, page_has_been_recorded, post_has_been_recorded
  integer :: argc, i, ios, len_path, rank_val
  character(len=80)  :: arg(0:2), char_rank
  character(len=512) :: css_2_delete, css_2_edit, page_2_edit, page_2_delete
  !
  argc = command_argument_count()
  if (argc > 2) call terminate(error_code = 1091, inf_msg = i18n(too_much_arguments))
  do i = 0, argc
     call get_command_argument(number = i, value = arg(i))
  end do
  if(argc == 0) then
     path = ""
     call console()
  end if
  if(argc == 1 .and. (arg(1)(1:1) /= '-')) then
     path = trim(arg(1))
     len_path = len(path)
     if(path(len_path:len_path) /= '/') path = path // '/'
     inquire(file = path // '.', exist = file_exist)
     if (.not. file_exist) call terminate(error_code = 1092, inf_msg = i18n(there_is_not_directory) // " " // path(:len(path)-1))
     call console()
  end if
  if(argc == 2 .and. (arg(2)(1:1) == '-')) call terminate(error_code = 1093, inf_msg = i18n(bad_argument) // '"' // trim(arg(2)) &
       &// '"')
  if(argc == 2) then
     path = trim(arg(2))
     len_path = len(path)
     if(path(len_path:len_path) /= '/') path = path // '/'
     inquire(file = path // '.', exist = file_exist)
     if (.not. file_exist) call terminate(error_code = 1094, inf_msg = i18n(there_is_not_directory) // " " // path)
  end if
  select case (trim(arg(1)))
  case ('-h', '-help', '--help', '-?', '?')
     print '(a)', help
     call terminate(error_code = 0)
  case ('-v', '--version')
     print '(a)', show_version
     call terminate(error_code = 0)
  end select
  inquire(file = path // config_file, exist = file_exist)
  if (file_exist) call init_conf()
  if (arg(1) == '--create') then
     call create_blog
     call terminate(error_code = 0)
  end if
  inquire(file = path // fblog_dir // '.', exist = file_exist)
  if ((.not. file_exist) .and. (len(path) > 0)) call terminate(error_code = 1095, inf_msg = i18n(no_fblog_directory_at) // " " //&
       & path)
  if ((.not. file_exist) .and. (len(path) == 0)) call terminate(error_code = 1096, inf_msg    = i18n(no_fblog_directory_here))
  if (arg(1)(1:12) == '--edit-post=') then
     char_rank = trim(arg(1)(13:))
     read(unit = char_rank, fmt = *, iostat = ios) rank_val
     if (ios /= 0) call terminate(error_code = 1097, inf_msg = trim(char_rank) // " " // i18n(is_a_wrong_argument))
     call edit_post(rank = rank_val, recorded = post_has_been_recorded)
     if (post_has_been_recorded) then
        call terminate(error_code = 0, inf_msg = i18n(now_you_should_update))
     else
        call terminate(error_code = 0, inf_msg = i18n(now_you_should_update))
     end if
  end if
  if (arg(1)(1:11) == '--edit-css=') then
     css_2_edit = trim(arg(1)(12:))
     call edit_css(css2edit = css_2_edit, recorded = css_has_been_recorded)
     if (css_has_been_recorded) then
        call terminate(error_code = 0, inf_msg = i18n(now_you_should_update))
     else
        call terminate(error_code = 0, inf_msg = i18n(not_modified))
     end if
  end if
  if (arg(1)(1:12) == '--edit-page=') then
     page_2_edit = trim(arg(1)(13:))
     call edit_page(page2edit = page_2_edit, recorded = page_has_been_recorded)
     if (page_has_been_recorded) then
        call terminate(error_code = 0, inf_msg = i18n(now_you_should_update))
     else
        call terminate(error_code = 0, inf_msg = i18n(not_modified))
     end if
  end if
  if (arg(1)(1:14) == '--delete-post=') then
     char_rank = trim(arg(1)(15:))
     read(unit = char_rank, fmt = *, iostat = ios) rank_val
     if (ios /= 0) call terminate(error_code = 1098, inf_msg = trim(char_rank) // " " // i18n(is_a_wrong_argument))
     call delete_post(rank = rank_val)
     call terminate(error_code = 0, inf_msg = i18n(deleted_file) // " " // trim(char_rank) // new_line('a') //&
          & i18n(now_you_should_update))
  end if
  if (arg(1)(1:14) == "--delete-page=") then
     page_2_delete = trim(arg(1)(15:))
     call delete_page(page2delete = page_2_delete)
     call terminate(error_code = 0, inf_msg = 'Page ' // trim(arg(1)(15:)) // " " // i18n(deleted_you_could_update_the_config))
  end if
  if (arg(1)(1:13) == "--delete-css=") then
     css_2_delete = trim(arg(1)(14:))
     call delete_css(css2delete = css_2_delete)
     call terminate(error_code = 0, inf_msg = 'Css ' // trim(arg(1)(14:)) // " " //i18n(deleted))
  end if
  select case (trim(arg(1)))
  case ('-L', '--list-all')
     call list_posts()
     call terminate(error_code = 0)
  case ('-l', '--list-last')
     call list_posts(limit = post_amount)
     call terminate(error_code = 0)
  case ('-u', '--update')
     call update()
     call terminate(error_code = 0)
  case ('-a', '--add-post')
     call add_post(recorded = post_has_been_recorded)
     if (post_has_been_recorded) then
        call terminate(error_code = 0, inf_msg =  i18n(now_you_should_update))
     else
        call terminate(error_code = 0, inf_msg = i18n(not_modified))
     end if
  case ('-c', '--config')
     call config(recorded = config_has_been_recorded)
     if (config_has_been_recorded) then
        call terminate(error_code = 0, inf_msg = i18n(now_you_should_update))
     else
        call terminate(error_code = 0, inf_msg = i18n(not_modified))
     end if
  case ('--list-css')
     call list_css()
     call terminate(error_code = 0)
  case ('--list-pages')
     call list_pages()
     call terminate(error_code = 0)
  case default
     call terminate(error_code = 1099, inf_msg = i18n(bad_argument) // ' "' // trim(arg(1)) // '"')
  end select
  error stop 'Internal error in Main'
end program fblog
