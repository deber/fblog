! Copyright (C) 2016 Denis Bernard.
! License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
! This is free software: you are free to change and redistribute it.
! There is NO WARRANTY, to the extent permitted by law.
! Written by Denis Bernard.
!
!> Set of routines that are called either by command line interface
!! or by console interface.
!
module io_m
  !
  use :: fblog_m, only: config_file, data_dir, data_post, editor, empty_page, export_html_dir, fblog_dir, i18n, init_conf,&
       & init_data, no_post, pages_dir, path, styles_dir, the_folowing_files_and_directories_will_be_created,&
       & there_is_already_a_blog, this_file_doesnt_exist, this_number_is_wrong, terminate, total_entries, update, version
  !
  implicit none
  !
  character(len=*), parameter :: config_txt_1 =&
       include 'config_txt_1.inc'
  character(len=*), parameter :: config_txt_2 =&
       include 'config_txt_2.inc'
  character(len=*), parameter :: help =&
       include 'help.inc'
  character(len=*), parameter :: css_standard_1 =&
       include 'css_standard_1.inc'
  character(len=*), parameter :: css_standard_2 =&
       include 'css_standard_2.inc'
  character(len=*), parameter :: css_standard_3 =&
       include 'css_standard_3.inc'
  character(len=*), parameter :: css_standard_print =&
       include 'css_standard_print.inc'
  character(len=*), parameter :: css_standard = css_standard_1 // css_standard_2 // css_standard_3
  character(len=*), parameter :: lorem_ipsum =&
       include 'lorem_ipsum.inc'
  !
  character(len=:), allocatable :: config_txt
  !
  private
  public add_post, config, create_blog, delete_page, delete_css, delete_post, edit_css, edit_page, edit_post, help, list_css,&
       & list_pages, list_posts
  !
contains
  !
  subroutine add_post(recorded)
    !! Display editor to edit a new post entry.
    logical, intent(out) :: recorded
    integer :: command_status = 0, exit_status = 0, ios, lu_post, post_size_after, post_size_before
    character(len=8)   :: d
    character(len=10)  :: t
    character(len=19)  :: f_post
    character(len=256) :: command_msg = "", system_msg  = ""
    character(len=*), parameter :: put_title_and_text_here = 'The title here' // new_line('a') // '<p> The text from here. </p>'
    !
    recorded = .true. ! Initialisation needed.
    call init_conf()
    call date_and_time(date = d, time = t)
    f_post = d // t(1:6) // '.blog'
    open(newunit = lu_post, file = path // data_dir // f_post, status = 'new', action = 'write', iostat = ios, iomsg   =&
         & system_msg)
    if (ios /= 0) call terminate(error_code = 1001, sys_msg = system_msg)
    write(unit = lu_post, fmt = '(a)') put_title_and_text_here
    close(unit = lu_post)
    inquire(file = path // data_dir // f_post, size = post_size_before)
    call execute_command_line(command = editor // ' ' // path // data_dir  // f_post, exitstat = exit_status, cmdstat =&
         & command_status, cmdmsg = command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1002, cmd_msg    = command_msg)
    ! Check if the post has been recorded or not. If not, the post is removed. This verification is done by comparing the length
    ! of the file before and after.
    inquire(file = path // data_dir // f_post, size = post_size_after)
    if (post_size_before == post_size_after) then
       recorded = .false.
       call execute_command_line(command = 'rm ' // path // data_dir // f_post, exitstat = exit_status, cmdstat = command_status,&
            & cmdmsg = command_msg)
    end if
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1003, cmd_msg = command_msg)
  end subroutine add_post
  !
  subroutine config(recorded)
    !! Display the editor for modification of the configuration file.
    logical, intent(out) :: recorded
    integer :: command_status = 0, exit_status = 0, file_size_after, file_size_before
    character(len=256) :: command_msg = ""
    !
    recorded = .true. ! Initialisation required.
    call init_conf()
    inquire(file = path // config_file, size = file_size_before)
    call execute_command_line(command = editor // " " // path // config_file, exitstat = exit_status, cmdstat = command_status,&
         & cmdmsg = command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1011, cmd_msg = command_msg)
    call init_conf()
    ! Check if the size of the file has changed.
    inquire(file = path // config_file, size = file_size_after)
    if (file_size_before == file_size_after) recorded = .false.
  end subroutine config
  !
  subroutine create_blog()
    !! Create directories, files and install a new blog.
    !
    logical :: file_exist
    integer :: command_status = 0, exit_status = 0, i, ios, lu_config, lu_page, lu_post, lu_style, lu_style_print
    character(len=1)   :: char_i
    character(len=8)   :: d
    character(len=10)  :: t
    character(len=19)  :: file_name_entry = ""
    character(len=256) :: command_msg = "", system_msg  = ""
    !
    config_txt = config_txt_1 // "CSS=fBlog-" // version // ".css" // new_line('a') // "" // new_line('a') // "CSS_PRINT&
         &=fBlog_print-" // version // ".css" // new_line('a') // "" // new_line('a') // config_txt_2
    inquire(file = path // fblog_dir // '.', exist = file_exist)
    if (file_exist) call terminate(error_code = 1021, inf_msg = i18n(there_is_already_a_blog))
    print '(a)', i18n(the_folowing_files_and_directories_will_be_created), path // fblog_dir, path // config_file, path //&
         & data_dir, path // export_html_dir, path // styles_dir, path // styles_dir // "fBlog-" // version // ".css", path //&
         & styles_dir // "fBlog_print-" // version // ".css", path // pages_dir, path // pages_dir // "page1 to page3.html"
    ! Create main directory.
    call execute_command_line(command = "mkdir " // path // fblog_dir, exitstat = exit_status, cmdstat = command_status, cmdmsg =&
         & command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1022, cmd_msg = command_msg)
    ! Create subdirectory for data.
    call execute_command_line(command = "mkdir " // path // data_dir, exitstat = exit_status, cmdstat = command_status, cmdmsg =&
         & command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1023, cmd_msg = command_msg)
    ! Create subdirectory for output.
    call execute_command_line(command = "mkdir "// path // export_html_dir, exitstat = exit_status, cmdstat = command_status,&
         & cmdmsg = command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1024, cmd_msg = command_msg)
    ! Create subdirectory for CSSs.
    call execute_command_line(command = "mkdir " // path // styles_dir, exitstat = exit_status, cmdstat = command_status, cmdmsg =&
         & command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1025, cmd_msg = command_msg)
    ! Create subdirectory for static pages.
    call execute_command_line(command = "mkdir "// path // pages_dir, exitstat = exit_status, cmdstat = command_status, cmdmsg =&
         & command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1026, cmd_msg = command_msg)
    ! Begin config file generation.
    open(newunit = lu_config, file = path // config_file, status = 'new', action = 'write', iostat = ios, iomsg = system_msg)
    if (ios /= 0) call terminate(error_code = 1027, sys_msg = system_msg)
    if (ios == 0) write(unit = lu_config, fmt = '(a)') config_txt
    ! End of config file generation.
    close(unit = lu_config)
    ! Begin style sheets generation.
    open(newunit = lu_style, file  = path // styles_dir // "fBlog-" // version // ".css", status = 'new', action = 'write', iostat&
         & = ios, iomsg = system_msg)
    if (ios /= 0) call terminate(error_code = 1028, sys_msg = system_msg)
    if (ios == 0) write(unit = lu_style, fmt = '(a)')  css_standard
    ! End style sheet generation.
    close(unit = lu_style)
    ! Begin style sheets generation.
    open(newunit = lu_style_print, file = path // styles_dir // "fBlog_print-" // version // ".css", status = 'new', action =&
         & 'write', iostat = ios, iomsg = system_msg)
    if (ios /= 0) call terminate(error_code = 1029, sys_msg = system_msg)
    if (ios == 0) write(unit = lu_style_print, fmt = '(a)') css_standard_print
    ! End style sheet generation.
    close(unit = lu_style_print)
    create_3_pages: do i = 1, 3
       ! Create demo static pages.
       write(unit = char_i, fmt = '(i1)') i
       open(newunit = lu_page, file = path // pages_dir // "page" // char_i // ".html", status = 'new', action = 'write', iostat =&
            & ios)
       if (ios /= 0) cycle
       write(unit = lu_page, fmt = '(a)') "<h2>Page #" // char_i // "</h2>" // new_line('a') // "<h3>" // empty_page // "</h3>"
       close(unit = lu_page)
    end do create_3_pages
    ! Create 1 entry.
    call date_and_time(date = d, time = t)
    file_name_entry = d // t(1:6) // '.blog'
    open(newunit = lu_post, file = path // data_dir // file_name_entry, action = 'write', iostat = ios, iomsg = system_msg)
    if (ios /= 0) call terminate(error_code = 1030, sys_msg = system_msg)
    write(unit = lu_post, fmt = '(a)') lorem_ipsum
    close(unit = lu_post)
    if(allocated(config_txt)) deallocate(config_txt)
    call update()
  end subroutine create_blog
  !
  subroutine delete_css(css2delete)
    !! Delete a given cascading style sheet.
    character(len=512), intent(in) :: css2delete
    logical :: file_exist
    integer :: command_status = 0, exit_status = 0
    character(len=256) :: command_msg = ""
    !
    inquire(file = path // styles_dir // trim(css2delete), exist = file_exist)
    if (.not. file_exist) call terminate(error_code = 1031, inf_msg = i18n(this_file_doesnt_exist))
    call execute_command_line(command = "rm " // path //styles_dir // trim(css2delete), exitstat = exit_status, cmdstat =&
         & command_status, cmdmsg = command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1032, cmd_msg = command_msg)
  end subroutine delete_css
  !
  subroutine delete_page(page2delete)
    !! Delete a given static page.
    character(len=512), intent(in) :: page2delete
    logical :: file_exist
    integer :: command_status = 0, exit_status = 0
    character(len=256) :: command_msg = ""
    !
    inquire(file = path // pages_dir //trim(page2delete), exist = file_exist)
    if (.not. file_exist) call terminate(error_code = 1041, inf_msg = i18n(this_file_doesnt_exist))
    call execute_command_line(command = "rm " // path // pages_dir // trim(page2delete), exitstat = exit_status, cmdstat =&
         & command_status, cmdmsg = command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1042, cmd_msg = command_msg)
  end subroutine delete_page
  !
  subroutine delete_post(rank)
    !! Delete a given post entry.
    integer, intent(in) :: rank
    integer :: ios, lu_post
    character(len=256) :: system_msg
    character(len=512) :: del_file
    !
    call init_data
    if (rank > total_entries .or. rank <= 0) call terminate(error_code = 1051, inf_msg = i18n(this_number_is_wrong))
    del_file = data_post(rank,6)%date // '.blog'
    open(newunit = lu_post, file = path // data_dir // '/' // del_file(1:19), status = 'old', iostat = ios, iomsg = system_msg)
    if (ios /= 0) call terminate(error_code = 1052, sys_msg = system_msg)
    close(unit = lu_post, status = 'delete')
    del_file = data_post(rank,7)%post_title
  end subroutine delete_post
  !
  subroutine edit_css(css2edit, recorded)
    !! Display the editor to create or modify a cascading style sheeet.
    character(len = 512), intent(in)  :: css2edit
    logical             , intent(out) :: recorded
    integer :: command_status = 0, exit_status = 0, file_size_before, file_size_after
    character(len=256) :: command_msg = ""
    !
    recorded = .true. ! Initialisation required.
    inquire(file = path // styles_dir // trim(css2edit), size = file_size_before)
    call execute_command_line(command = editor // ' ' // path // styles_dir // trim(css2edit), exitstat = exit_status, cmdstat =&
         & command_status, cmdmsg = command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1061, cmd_msg = command_msg)
    ! Check if size of the file has changed. If not it is supposed that file wasn't modified. (Not always true!)
    inquire(file = path // styles_dir // trim(css2edit), size = file_size_after)
    if (file_size_before == file_size_after) recorded = .false.
  end subroutine edit_css
  !
  subroutine edit_page(page2edit, recorded)
    !! Display the editor to create or modify a static page.
    character(len=512), intent(in)  :: page2edit
    logical,            intent(out) :: recorded
    logical :: file_exist
    integer :: command_status = 0, exit_status = 0, file_size_after, file_size_before
    character(len=256) :: command_msg = ""
    !
    recorded = .true. ! Initialisation required.
    ! Check if file exist already and what size is it.
    inquire(file = path // pages_dir // trim(page2edit), exist = file_exist)
    if (file_exist) then
       inquire(file = path // pages_dir // trim(page2edit), size = file_size_before)
    else
       file_size_before = 0
    end if
    call execute_command_line(command = editor //' '// path // pages_dir // trim(page2edit), exitstat = exit_status, cmdstat =&
         & command_status, cmdmsg = command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1071, cmd_msg = command_msg)
    ! Check if size of the file has changed. If not, it is supposed that file wasn't modified. (Not always true!)
    inquire(file = path // pages_dir // trim(page2edit), size = file_size_after)
    if (file_size_before == file_size_after) recorded = .false.
  end subroutine edit_page
  !
  subroutine edit_post(rank, recorded)
    !! Display the editor to edit a post entry that already exist.
    integer, intent(in)  :: rank
    logical, intent(out) :: recorded
    integer :: command_status = 0, exit_status = 0, file_size_after, file_size_before
    character(len=19)  :: edit_file
    character(len=256) :: command_msg = ""
    !
    recorded = .true. ! Initialisation required.
    call init_conf()
    call init_data()
    if (rank > total_entries .or. rank <= 0 ) call terminate(error_code = 1081, inf_msg = i18n(this_number_is_wrong ))
    edit_file = data_post(rank,6)%date // '.blog'
    inquire(file = path // data_dir // trim(edit_file(1:19)), size = file_size_before)
    call execute_command_line(command = editor//' '// path // data_dir // trim((edit_file(1:19))), exitstat = exit_status, cmdstat&
         & = command_status, cmdmsg = command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1082, cmd_msg = command_msg)
    ! Check if size of the file has changed. If not, it is supposed that file wasn't modified. (Not always true!)
    inquire(file = path // data_dir // trim(edit_file(1:19)), size = file_size_after)
    if (file_size_before == file_size_after) recorded = .false.
  end subroutine edit_post
  !
  subroutine list_css()
    !! Display the list of all cascading style sheets avalaible.
    integer :: command_status = 0, exit_status = 0
    character(len=256) :: command_msg = ""
    !
    call execute_command_line(command = "ls -1 " // path // styles_dir, exitstat = exit_status, cmdstat = command_status, cmdmsg =&
         & command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1151, cmd_msg = command_msg)
  end subroutine list_css
  !
  subroutine list_pages()
    !! Display the list of all static pages.
    integer :: command_status = 0, exit_status = 0
    character(len=256) :: command_msg = ""
    !
    call execute_command_line(command = "ls -1 " // path // pages_dir, exitstat = exit_status, cmdstat = command_status, cmdmsg =&
         & command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1161, cmd_msg = command_msg)
  end subroutine list_pages
  !
  subroutine list_posts(limit)
    !! Display a list of post entries (last ones or all of them).
    integer,intent(in), optional :: limit
    logical :: ok_limit
    integer :: i
    !
    ok_limit = present(limit)
    call init_data()
    if (ok_limit) then
       if (limit < total_entries) total_entries = limit
    end if
    if (total_entries == 0) print'(a)', i18n(no_post)
    do i = total_entries, 1, -1
       print '(a,g0,a)', '#', i, ' [' // data_post(i,6)%date(1:4) // '-' // data_post(i,6)%date(5:6) // '-' // data_post(i,6)&
            &%date(7:8) // ' (' // data_post(i,6)%date(9:10) // ':' // data_post(i,6)%date(11:12) // ')] ' // trim(data_post(i,7)&
            &%post_title)
    end do
  end subroutine list_posts
  !
end module io_m
