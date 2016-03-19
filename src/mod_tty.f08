! Copyright (C) 2016 Denis Bernard.
! License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
! This is free software: you are free to change and redistribute it.
! There is NO WARRANTY, to the extent permitted by law.
! Written by Denis Bernard.
!
!> Provide interactive menu using POSIX command stty.
!! There is **no use** of the well known programming library
!! [Curses](https://en.wikipedia.org/wiki/Curses_%28programming_library%29)).
!! Instead the command-line `stty` is used to set the terminal in *raw mode*
!! in order to catch a unique key stroke without the need to press the key
!! `<Enter>`. *(This could be done with a call to libc to set `canon / icanon`.)*
!!
!! Some ECMA-48 characters are emitted in order to change attributs (colors,
!!  font weight) of strings displayed in console menus. *(ECMA-48 is also known as
!! "ANSI escape sequences")*.
!!
!! References
!!
!!   * man pages stty(1) and console_codes(4),
!!   * [Open group](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap11.html),
!!   * [ECMA-48](http://www.ecma-international.org/publications/standards/Ecma-048.htm).
!
module tty_m
  !
  use :: fblog_m, only: create_new_blog, delete_a_css, delete_a_page, deleted_page, edit_a_css, edited_page,&
       & enter_the_filename_of_the_css_deleted, enter_the_filename_of_the_page_created, delete_a_post, deleted_file, edit_a_page,&
       & edit_settings, enter_the_filename_of_the_page_deleted, enter_the_rank_of_the_post_to_delete,&
       & enter_the_rank_of_the_post_to_modify, fblog_dir, i18n, init_conf, init_data, is_deleted, modify_a_post, new_post,&
       & not_modified, now_you_could_edit_settings, now_you_should_update, pages, path, post_amount, posts, quit, show_last_posts,&
       & show_full_list_css, show_full_list_pages, show_full_list_posts, show_version, styles, terminate,&
       & the_folowing_files_and_directories_will_be_created, there_is_already_a_blog, this_file_doesnt_exist, this_number_is_wrong&
       &, total_entries, up, update, update_all, wrong_entry
  use :: io_m, only: add_post, config, create_blog, delete_css, delete_page, delete_post, edit_css, edit_page, edit_post, list_css&
       & , list_pages, list_posts
  !
  implicit none
  !
  private
  public console
  !
  ! ECMA-48 (ANSI ESC sequence)
  character(len=*), parameter :: bye = char(27) // '[1m' // char(27) // '[33m' // 'BYE' // char(27) // '[0m'
  character(len=*), parameter :: ga = char(27) // '[1m' // char(27) // '[33m' // 'GA' // char(27) // '[0m'
  character(len=*), parameter :: ok = char(27) // '[1m' // char(27) // '[32m' // 'OK' // char(27) // '[0m'
  character(len=*), parameter :: reset = char(27) // '[0m'
  character(len=*), parameter :: set_blue = char(27) // '[1m' // char(27) // '[34m'
  character(len=*), parameter :: set_bold = char(27) // '[1m'
  character(len=*), parameter :: false_entry =  char(27) // '[1m' // char(27) // '[31m'
  !
  logical :: file_exist, directory_exist
  integer :: command_status = 0, exit_status = 0, ios, lu_stty_g
  character(len=1)   :: choice
  character(len=256) :: command_msg = "", system_msg
  character(len=512) :: terminal_settings
  !
contains
  !
  function console_menu(items)
    !! Auxilary routine for displaying the menus in console mode.
    character(len=*), intent(in) :: items
    character(len=1) console_menu
    integer :: command_status = 0, exit_status = 0, i, ios, j, k, pos
    character(len=len(items)) :: read_items
    character(len=80), dimension(10) :: item
    character(len=1)   :: keystroke
    !
    read_items = items
    j = 1
    do i = 1, 10
       pos = index(string = read_items(:), substring = ",")
       if ((pos == 0) .and. (len_trim(read_items) == 0)) exit
       if ((pos == 0) .and. (len(read_items) > 0)) then
          item(i) = read_items
          exit
       end if
       item(i) = read_items(:pos-1)
       read_items = read_items(pos+1:)
       j =  j + 1
    end do
    write(unit = *, fmt = '(a)', advance = 'no') set_bold
    do k = 1, j
       print '(2x,i1,a)', k-1, "  " // trim(item(k))
    end do
    print '(a)', ga
    call execute_command_line(command = 'stty raw', exitstat = exit_status, cmdstat = command_status, cmdmsg = command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1231, cmd_msg = command_msg)
    write(unit = *, fmt = '(a)', advance = 'no') set_blue
    read(unit = *, fmt = '(a)', advance = 'no', iostat  = ios) keystroke
    write(unit = *, fmt = '(a)', advance = 'no') reset
    call execute_command_line(command = 'stty ' // trim(terminal_settings), exitstat = exit_status, cmdstat = command_status,&
         & cmdmsg = command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1232, cmd_msg = command_msg)
    print *
    console_menu = keystroke
  end function console_menu
  !
  subroutine console()
    !! Head routine for console mode.
    !
    use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options
    !
    print '(a,2/,a,2/)', show_version, compiler_version() // ' (' // compiler_options() // ')'
    inquire(file = path // fblog_dir // '.', exist = file_exist)
    if (.not. file_exist) then
       inquire(file = path // FBLOG_DIR // '.', exist = directory_exist)
       if (.not. directory_exist) then
          call execute_command_line(command = 'stty -g > ' // path // 'stty_g', exitstat = exit_status, cmdstat = command_status,&
               & cmdmsg = command_msg)
          if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1131, cmd_msg = command_msg)
          open(newunit = lu_stty_g, file = path // 'stty_g', status = 'old', action = 'read', iostat = ios, iomsg = system_msg)
          if (ios /= 0) call terminate(error_code = 1132, sys_msg = system_msg)
          read(unit = lu_stty_g, fmt = '(a)') terminal_settings
          close(unit = lu_stty_g, status = 'delete')
       else if (directory_exist) then
          call execute_command_line(command = 'stty -g > ' // path // fblog_dir //'stty_g', exitstat = exit_status, cmdstat =&
               & command_status, cmdmsg = command_msg)
          if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1133, cmd_msg = command_msg)
          open(newunit = lu_stty_g, file = path // fblog_dir // 'stty_g', status = 'old', action = 'read', iostat = ios, iomsg =&
               & system_msg)
          if (ios /= 0) call terminate(error_code = 1134, sys_msg = system_msg)
          read(unit = lu_stty_g, fmt = '(a)') terminal_settings
          close(unit = lu_stty_g, status = 'delete')
       end if
       choice = console_menu(i18n(quit) // ',' // i18n(create_new_blog))
       select case (choice)
       case ('q', 'Q', '0')
          print '(a)', bye
          call terminate(error_code = 0)
       case ('1')
          call create_blog()
          print '(a)', ok
       case default
          call terminate(error_code = 1135, inf_msg = i18n(wrong_entry))
       end select
    end if
    call execute_command_line(command = 'stty -g > ' // trim(path) // fblog_dir //'stty_g', exitstat = exit_status, cmdstat =&
         & command_status, cmdmsg = command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1136, cmd_msg = command_msg)
    open(newunit = lu_stty_g, file = path // fblog_dir // 'stty_g', status = 'old', action = 'read', iostat = ios, iomsg =&
         & system_msg)
    if (ios /= 0) call terminate(error_code = 1137, sys_msg = system_msg)
    read(unit = lu_stty_g, fmt = '(a)') terminal_settings
    close(unit = lu_stty_g, status = 'delete')
    call init_conf()
    call menu_main()
    error stop 'Internal error in subroutine console()'
  end subroutine console
  !
  recursive subroutine menu_main()
    !! Provide top menu in console mode.
    use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options
    logical :: config_has_been_recorded
    character(len=1) :: choice
    !
    choice = console_menu(          &
         i18n(quit) // ',' //       &
         i18n(update_all) // ',' // &
         i18n(posts) // ',' //      &
         i18n(pages) // ',' //      &
         i18n(styles) // ',' //     &
         i18n(edit_settings)        &
         )
    select case (choice)
    case ('q', 'Q', '0')
       print '(a)', bye
       call terminate(error_code = 0)
    case ('1')
       call update()
       print '(a)', ok
       call menu_main()
    case ('2')
       call init_data()
       call menu_post()
    case ('3')
       call menu_page()
    case ('4')
       call menu_css()
    case ('5')
       call config(recorded = config_has_been_recorded)
       if (config_has_been_recorded) then
          print '(a)', i18n(now_you_should_update), ok
       else
          print '(a)', i18n(not_modified), ok
       end if
       call menu_main()
    case default
       print '(a)', false_entry // i18n(wrong_entry) // reset
       call menu_main()
    end select
  end subroutine menu_main
  !
  recursive subroutine menu_page()
    !! Provide sub menu for static pages in console mode.
    character(len=1)   :: choice
    character(len=512) :: page_2_delete, &
         page_2_edit
    logical :: page_has_been_recorded
    !
    choice = console_menu(                    &
         i18n(up) // ',' //                   &
         i18n(show_full_list_pages) // ',' // &
         i18n(edit_a_page) // ',' //          &
         i18n(delete_a_page)                  &
         )
    select case (choice)
    case ('q', 'Q')
       print '(a)', bye
       call terminate(error_code = 0)
    case ('0')
       call menu_main()
    case ('1')
       call list_pages
       print '(a)', ok
       call menu_page()
    case ('2')
       write(unit = *, fmt = '(a)', advance = 'no') i18n(enter_the_filename_of_the_page_created) // " "
       read(unit = *, fmt  = *) page_2_edit
       print '(a,g0,a)', set_blue, trim(page_2_edit), reset
       call edit_page(page2edit = page_2_edit, recorded = page_has_been_recorded)
       if (page_has_been_recorded) then
          print '(a)',  i18n(edited_page) // " " // trim(page_2_edit) // new_line('a') // i18n(now_you_could_edit_settings), ok
       else
          print '(a)', i18n(not_modified), ok
       end if
       call menu_page()
    case ('3')
       write(unit = *, fmt = '(a)', advance = 'no') i18n(enter_the_filename_of_the_page_deleted) // " "
       read(unit = *, fmt = *) page_2_delete
       print '(a,g0,a)', set_blue, trim(page_2_delete), reset
       call delete_page(page2delete = page_2_delete)
       print '(a)', i18n(deleted_page) // " " // trim(page_2_delete) // new_line('a') // i18n(now_you_could_edit_settings), ok
       call menu_page()
    case default
       print '(a)', false_entry // i18n(wrong_entry) // reset
       call menu_page()
    end select
  end subroutine menu_page
  !
  recursive subroutine menu_post()
    !! Provide sub menu for post entries in console mode.
    character(len=1) :: choice
    integer :: int_keystroke, ios, rank_val
    logical :: post_has_been_recorded
    !
    if (total_entries > 0) then
       choice = console_menu(               &
            i18n(up) // ',' //              &
            i18n(show_last_posts) // ',' // &
            i18n(new_post) // ',' //        &
            i18n(modify_a_post) // ',' //   &
            i18n(delete_a_post) // ',' //   &
            i18n(show_full_list_posts)      &
            )
       select case (choice)
       case ('q', 'Q')
          print '(a)', bye
          call terminate(error_code = 0)
       case ('0')
          call menu_main()
       case ('1')
          call list_posts(limit = post_amount)
          print '(a)', ok
          call menu_post()
       case ('2')
          print '(a)', ok
          call add_post(recorded = post_has_been_recorded)
          if (post_has_been_recorded) then
             print '(a)', i18n(now_you_should_update), ok
          else
             print '(a)', i18n(not_modified), ok
          end if
          call menu_main()
       case ('3')
          write(unit = *, fmt = '(a)', advance = 'no') i18n(enter_the_rank_of_the_post_to_modify) // " "
          read(unit = *, fmt = *, iostat = ios) int_keystroke
          if (ios /= 0) call terminate(error_code = 1141, inf_msg = i18n(wrong_entry))
          rank_val = int_keystroke
          print '(a,g0,a)', set_blue, rank_val, reset
          call edit_post(rank = rank_val, recorded = post_has_been_recorded)
          if (post_has_been_recorded) then
             print '(a)', i18n(now_you_should_update), ok
          else
             print '(a)', i18n(not_modified), ok
          end if
          call menu_main()
       case ('4')
          write(unit = *, fmt = '(a)', advance = 'no') i18n(enter_the_rank_of_the_post_to_delete) // " "
          read(unit = *, fmt = *, iostat = ios) int_keystroke
          if (ios /= 0) call terminate(error_code = 1142, inf_msg = i18n(wrong_entry))
          rank_val = int_keystroke
          print '(a,g0,a)', set_blue, rank_val, reset
          call delete_post(rank = rank_val)
          print '(a, g0, 2a)', i18n(deleted_file) // " ", rank_val, new_line('a'), i18n(now_you_should_update)
          print '(a)', ok
          call menu_main()
       case ('5')
          call list_posts()
          print '(a)', ok
          call menu_post()
       case default
          print '(a)', false_entry // i18n(wrong_entry) // reset
          call menu_post()
       end select
    else
       choice = console_menu(  &
            i18n(up) // ',' // &
            i18n(new_post)     &
            )
       select case (choice)
       case ('q', 'Q')
          print '(a)', bye
          call terminate(error_code = 0)
       case ('0')
          call menu_main()
       case ('1')
          call add_post(recorded = post_has_been_recorded)
          print '(a)', ok
          if (post_has_been_recorded) then
             print '(a)', i18n(now_you_should_update), ok
          else
             print '(a)', i18n(not_modified), ok
          end if
          call menu_main()
       case default
          print '(a)', false_entry
          call menu_post()
       end select
    end if
  end subroutine menu_post
  !
  recursive subroutine menu_css()
    !! Provide a sub menu for CSS in console mode.
    logical :: css_has_been_recorded
    character(len=1)   :: choice
    character(len=512) :: css_2_delete, css_2_edit
    !
    choice = console_menu(                  &
         i18n(up) // ',' //                 &
         i18n(show_full_list_css) // ',' // &
         i18n(edit_a_css) // ',' //         &
         i18n(delete_a_css)                 &
         )
    select case (choice)
    case ('q', 'Q')
       print '(a)', bye
       call terminate(error_code = 0)
    case ('0')
       call menu_main()
    case ('1')
       call list_css()
       print '(a)', ok
       call menu_css()
    case ('2')
       write(unit = *, fmt = '(a)', advance = 'no') i18n(enter_the_filename_of_the_page_created) // " "
       read(unit = *, fmt = '(a)') css_2_edit
       print '(a,g0,a)', set_blue, trim(css_2_edit), reset
       call edit_css(css2edit = css_2_edit, recorded = css_has_been_recorded)
       if (css_has_been_recorded) then
          print '(a)', i18n(now_you_should_update), ok
       else
          print '(a)', i18n(not_modified), ok
       end if
       call menu_css()
    case ('3')
       write(unit = *, fmt = '(a)', advance = 'no') i18n(enter_the_filename_of_the_css_deleted) // " "
       read(unit = *, fmt = *) css_2_delete
       print '(a,g0,a)', set_blue, trim(css_2_delete), reset
       call delete_css(css2delete = css_2_delete)
       print '(a)', i18n(is_deleted) // " " // trim(css_2_delete), ok
       call menu_css()
    case default
       print '(a)', false_entry // i18n(wrong_entry) // reset
       call menu_css()
    end select
  end subroutine menu_css
  !
end module tty_m
