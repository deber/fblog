! Copyright (C) 2016 Denis Bernard.
! License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
! This is free software: you are free to change and redistribute it.
! There is NO WARRANTY, to the extent permitted by law.
! Written by Denis Bernard.
!
!> The core of fBlog.
!
module fblog_m
  !
  implicit none
  !
  private
  public config_file, bad_argument, create_new_blog, delete_a_css, data_dir, data_post, delete_a_page, delete_a_post, deleted,&
       & deleted_file, deleted_page, deleted_you_could_update_the_config, edit_a_page, edit_a_css, edited_page, editor,&
       & edit_settings, enter_the_filename_of_the_css_deleted, enter_the_filename_of_the_page_created,&
       & enter_the_filename_of_the_page_deleted, enter_the_rank_of_the_post_to_delete, enter_the_rank_of_the_post_to_modify,&
       & empty_page, export_html_dir, fblog_dir, i18n, init_conf, init_data, is_a_wrong_argument, is_deleted, modify_a_post,&
       & new_post, no_fblog_directory_at, no_fblog_directory_here, no_post, not_modified, now_you_could_edit_settings,&
       & now_you_should_update, pages, pages_dir, path, posts, post_amount, quit, show_last_posts, show_full_list_css,&
       & show_full_list_pages, show_full_list_posts, show_version, styles, styles_dir, terminate, total_entries, update, version,&
       & the_folowing_files_and_directories_will_be_created, there_is_already_a_blog, there_is_not_directory,&
       & this_file_doesnt_exist, this_number_is_wrong, too_much_arguments, up, update_all, wrong_entry
  !
  character(len=*), parameter :: version = '0.6.0'
  character(len=*), parameter :: copyright = '2016'
  character(len=*), parameter :: config_file = 'fBlog/fblog.conf'
  character(len=*), parameter :: styles_dir = 'fBlog/styles/'
  character(len=*), parameter :: data_dir = 'fBlog/data/'
  character(len=*), parameter :: export_html_dir = 'fBlog/export_http/'
  character(len=*), parameter :: fblog_dir = "fBlog/"
  character(len=*), parameter :: pages_dir = 'fBlog/pages/'
  character(len=*), parameter :: empty_page = '[...]'
  character(len=*), parameter :: default_editor = 'vi'
  integer, parameter :: post_amount = 9
  integer entries_per_page, total_entries, year1, year2
  logical css, css_print
  character(len=3), save :: direction = 'ltr'
  character(len=80), dimension(12) :: monthes = ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"]
  character(:), allocatable :: archive, blog_title, charset, css_file, css_print_file, description, editor, footer, footer_1,&
       & header_1, header_2, home, language, menu_html, path, permalink, printer_friendly, str_i18n
  integer, allocatable :: stats(:,:)
  type       :: post_t
     integer :: year    = 0
     integer :: month   = 0
     integer :: day     = 0
     integer :: hour    = 0
     integer :: minute  = 0
     integer :: seconde = 0
     character(len=14)   :: date = ""
     character(len= 512) :: post_title = ""
  endtype post_t
  type(post_t), allocatable :: data_post(:,:)
  character(len=*), parameter :: nl = new_line('a')
  character(len=*), parameter :: show_version = 'fBlog ' // version // nl // 'Copyright (C) ' // copyright // ' Denis&
       & Bernard' // nl // nl // 'License GPLv3+: GNU GPL version 3 or later ' // '<http://gnu.org/licenses/gpl.html>.' // nl //&
       & 'This is free software: you are free to change and redistribute it.' // nl // 'There is NO WARRANTY, to the extent&
       & permitted by law.' // nl // nl // 'Written by Denis Bernard.'
  !
  enum, bind(c)
     enumerator bad_argument
     enumerator begin_update
     enumerator create_new_blog
     enumerator copy_css
     enumerator copy_css_print
     enumerator deleted
     enumerator delete_a_css
     enumerator delete_a_page
     enumerator delete_a_post
     enumerator deleted_file
     enumerator deleted_page
     enumerator deleted_you_could_update_the_config
     enumerator edit_a_page
     enumerator edit_a_css
     enumerator edited_page
     enumerator edit_settings
     enumerator end_clearing_subdirectory
     enumerator enter_the_filename_of_the_css_deleted
     enumerator enter_the_filename_of_the_page_created
     enumerator enter_the_filename_of_the_page_deleted
     enumerator enter_the_rank_of_the_post_to_delete
     enumerator enter_the_rank_of_the_post_to_modify
     enumerator error_css
     enumerator error_css_print
     enumerator files_generated_inside_the_subdirectory
     enumerator initialisation_reading_conf
     enumerator initialisation_reading_data
     enumerator is_a_wrong_argument
     enumerator is_deleted
     enumerator make_archive_pages
     enumerator make_clear_subdirectory
     enumerator make_home_page
     enumerator make_pages
     enumerator make_permalink_pages
     enumerator modify_a_post
     enumerator monthly_archive_pages_built
     enumerator new_post
     enumerator no_fblog_directory_at
     enumerator no_fblog_directory_here
     enumerator no_post
     enumerator not_modified
     enumerator now_you_could_edit_settings
     enumerator now_you_should_update
     enumerator pages
     enumerator pages_built
     enumerator permalink_pages_built
     enumerator posts
     enumerator quit
     enumerator show_last_posts
     enumerator show_full_list_css
     enumerator show_full_list_pages
     enumerator show_full_list_posts
     enumerator styles
     enumerator the_folowing_files_and_directories_will_be_created
     enumerator there_is_already_a_blog
     enumerator there_is_not_directory
     enumerator this_file_doesnt_exist
     enumerator this_number_is_wrong
     enumerator too_much_arguments
     enumerator up
     enumerator update_all
     enumerator update_done
     enumerator shell_said
     enumerator system_said
     enumerator warn_about_no_css
     enumerator warn_about_no_css_print
     enumerator wrong_entry
  end enum
  !
  enum, bind(c)
     enumerator en
     enumerator fr
  end enum
  !
  contains
  !
  function i18n(msg)
    !! Provide sentences in the language according the LANG environment variable.
    integer, intent(in) :: msg
    character(:), allocatable :: i18n
    integer :: l = 0, stat
    character(len=2) lang
    character(len=256), dimension(en:fr, 0:wrong_entry) :: msgstr = ""
    !
    include 'i18n.inc'
    !
    call get_environment_variable(name = "LANG", value = lang, status = stat)
    if (lang == "en") l = en
    if (lang == "fr") l = fr
    str_i18n = trim(msgstr(l,msg))
    if (len(str_i18n) == 0) str_i18n = trim(msgstr(en,msg))
    i18n = str_i18n
    deallocate(str_i18n)
  end function i18n
  !
  function clock()
    !! Print the computer clock time for update messages.
    character(len=10) :: raw_clock
    character(len=15) :: clock
    !
    call date_and_time(time = raw_clock)
    clock = '[' // raw_clock(1:2) // ':' // raw_clock(3:4) // ':' // raw_clock (5:10) // '] '
  end function clock
  !
  subroutine html_home(user_home)
    !! Make HTML code for home page.
    character(len=*), intent(in) :: user_home
    integer :: file_length, i, ios, lu_index, lu_post
    character(len=6)          :: current_entry_txt
    character(len=256)        :: system_msg
    character(:), allocatable :: post
    !
    open(newunit = lu_index, file = path // export_html_dir // "index.html", action = 'write', iostat = ios, iomsg = system_msg)
    if (ios /= 0) call terminate(error_code = 1171, sys_msg = system_msg)
    write(unit = lu_index, fmt  = '(a)') header_1, '<title>' // blog_title // ' | ' // trim(user_home) // '</title>' // header_2
    if (len(menu_html) > 0) write(unit = lu_index, fmt = '(a)') '<nav>', '<hr />', menu_html // '<hr />', '</nav>'
    if (entries_per_page > total_entries) entries_per_page = total_entries
    if (total_entries > 0) then
       do i = 1, entries_per_page
          post = ""
          inquire(file = path // data_dir // data_post(i,6)%date // '.blog', size = file_length)
          post = repeat(" ", file_length)
          open(newunit = lu_post, file = path // data_dir // data_post(i,6)%date // '.blog', access = 'stream', action = 'read',&
               & iostat = ios, iomsg = system_msg)
          if (ios /= 0) call terminate(error_code = 1173, sys_msg = system_msg)
          read(unit = lu_post) post
          close(unit = lu_post)
          write(unit = current_entry_txt, fmt = '(g0)') i
          write(unit = lu_index, fmt  = '(a)') '<article>'
          if (total_entries > 1) then
             write(unit = lu_index, fmt = '(a)', advance = 'no') '<header>', '<h3><a href="' // data_post(i,6)%date // '.html">' &
                  &// trim(adjustl(data_post(i,7)%post_title)) // '</a></h3>', '</header>'
          else
             write(unit = lu_index, fmt = '(a)', advance = 'no') '<header>', '<h3>' // trim(adjustl(data_post(i,7) %post_title)) &
                  &// '</h3>', '</header>'
          end if
          write(unit = lu_index, fmt = '(a)', advance = 'no') post(len_trim((data_post(i,7)%post_title)) + 2 : file_length)
          if (total_entries > 1) write(unit = lu_index, fmt  = '(g0)') '<footer>', '<a href="' // data_post(i,6)%date // '.html"&
               &>' // permalink // '</a>&nbsp;&nbsp;', '<a href="' // data_post(i,6)%date // '.htm">' // printer_friendly // '<&
               &/a                 >&nbsp;&nbsp;', '<samp dir="ltr"><time dir="ltr" datetime="'  // data_post(i,6)%date(1:4) //&
               & '-' // data_post(i,6) %date(5:6) // '-' // data_post(i,6)%date(7:8)//'T' // data_post(i,6)%date(9:10) // ':' //&
               & data_post(i,6) %date(11:12) // '">[' // data_post(i,6)%date(1:4) // '-' // data_post(i,6)%date(5:6) // '-' //&
               & data_post(i,6) %date(7:8)//' (' // data_post(i,6)%date(9:10) // ':' // data_post(i,6)%date(11:12)//')]' // '<&
               &/time> [# ' // trim(current_entry_txt) // ']</samp>', '</footer>'
          write(unit = lu_index, fmt = '(a)') '</article>'
          close(unit = lu_post)
       end do
    else
       write(unit = lu_index, fmt = '(a)') '<article><h3>' // empty_page // '</h3></article>'
    end if
    if (total_entries>entries_per_page) write(unit = lu_index, fmt = '(a)') '<nav>', '<hr />', '<a  rel="prev" href="' //&
         & data_post(entries_per_page + 1, 6)%date // '.html" >&nbsp;=>&nbsp;</a>', '</nav>'
    write(unit = lu_index, fmt = '(a)') footer_1
    close(unit = lu_index)
  end subroutine html_home
  !
  subroutine html_month_archive(n_pages)
    !! Make HTML code for monthly index pages.
    !  This routine is a shame. But it works... Maybe I'll rewrite it in the future.
    integer, intent(out) :: n_pages
    logical :: file_exist
    integer :: current_entry, i, ios, j, k, lu_m_archive
    character(len=6)   :: m_archive
    character(len=6)   :: m_archive1 = ""
    character(len=12)  :: m_archive2
    character(len=256) :: system_msg
    !
    n_pages = 0
    do current_entry = 1, total_entries
       m_archive = data_post(current_entry,6)%date(1:6)
       do i = 1, total_entries
          if (m_archive == m_archive1) cycle
          if (data_post(i,6)%date(1:6) /= m_archive) cycle
          if (i > 1) then
             if (data_post(i-1,6)%date(1:6) == m_archive) cycle
          end if
          inquire(file = path // export_html_dir // data_post(current_entry,6)%date(1:6) // ".html", exist = file_exist)
          if (file_exist) cycle
          m_archive1 = m_archive
          open(newunit = lu_m_archive, file = path // export_html_dir // data_post(current_entry,6)%date(1:6) // ".html", action&
               & = 'write', iostat = ios, iomsg = system_msg)
          if (ios /= 0) call terminate(error_code = 1181, sys_msg = system_msg)
          write(unit = lu_m_archive, fmt = '(a)') header_1, '<title>' // blog_title // ' | ' // trim(archive) // ' ' //&
               & data_post(current_entry,6)%date(1:4) // '-' // data_post(current_entry,6)%date(5:6) // '</title>' // header_2
          if (len(menu_html) > 0) write(unit = lu_m_archive, fmt  = '(a)') '<nav>', '<hr />', menu_html // '<hr />', '</nav>'
          write(unit = lu_m_archive, fmt = '(a)') '<article>','<header>','<h3>' // trim(archive) // '<span dir="ltr"><code> [' &
               &// data_post(current_entry,6)%date(1:4) // '-' // data_post(current_entry,6)%date(5:6) // ']</code></span></h3>'&
               &,'</header>', '<ol>'
          k=1
          do j = 1, total_entries
             m_archive2 = data_post(j,6)%date(1:12)
             if (m_archive2(1:6) /= m_archive1) cycle
             write(unit = lu_m_archive, fmt = '(a)') '<li><span dir="ltr"><code>[' // data_post(j,6)%date(1:4) // '-' //&
                  & data_post(j,6)%date(5:6) // '-' // data_post(j,6)%date(7:8) // ' (' // data_post(j,6)%date(9:10) // ':' //&
                  & data_post(j,6)%date(11:12) // ')] </code></span>&nbsp;<a href="' // data_post(j,6)%date(1:14) // '.html' //&
                  & '">' // trim(adjustl(data_post(j,7)%post_title)) // '</a></li>'
             K = k + 1
          end do
          write(unit = lu_m_archive, fmt = '(a)') '</ol>', '</article>', footer_1
          close(unit = lu_m_archive)
          n_pages = n_pages + 1
       end do
    end do
  end subroutine html_month_archive
  !
  subroutine html_page(n_pages)
    !! Make HTML code of extra Web pages
    use, intrinsic :: iso_fortran_env, only:&
         iostat_end
    integer, intent(out) :: n_pages
    integer :: file_length, ios, lu_data_page, lu_page, lu_page_list
    character(:), allocatable :: post
    character(len=256)        :: system_msg
    character(len=512)        :: readline
    !
    n_pages = 0
    call execute_command_line(command = 'ls ' // path // pages_dir // " > " // path // fblog_dir // 'pages_list.txt')
    open(newunit = lu_page_list, file = path // fblog_dir // 'pages_list.txt', status = 'old', action = 'read', iostat = ios,&
         & iomsg = system_msg)
    if (ios /= 0) call terminate(error_code = 1221, sys_msg = system_msg)
    build_pages: do
       post=""
       read(unit = lu_page_list, fmt = '(a)', iostat = ios) readline
       if (ios == iostat_end) exit build_pages
       inquire(file = path // pages_dir // trim(readline), size = file_length)
       post = repeat(" ", file_length)
       open(newunit = lu_data_page, file = path // PAGES_DIR // trim(readline), access = 'stream', action = 'read', iostat = ios,&
            & iomsg = system_msg)
       if (ios /= 0) call terminate(error_code = 1223, sys_msg = system_msg)
       read(unit = lu_data_page) post
       close(unit = lu_data_page)
       open(newunit = lu_page, file = path // export_html_dir // trim(readline), action = 'write', iostat = ios, iomsg =&
            & system_msg)
       if (ios /= 0) call terminate(error_code = 1222, sys_msg = system_msg)
       write(unit = lu_page, fmt = '(a)') header_1, '<title>' // trim(blog_title) // '</title>' // header_2, '<nav>', '<hr />',&
            & menu_html // '<hr />', '</nav>','<article>', post // '</article>', footer_1
       close(unit = lu_page)
       n_pages = n_pages + 1
    end do build_pages
    close(unit = lu_page_list, status = 'delete')
  end subroutine html_page
  !
  subroutine html_permalink()
    !! Make HTML code for permalink pages.
    integer :: current_entry, file_length, ios, lu_permalink, lu_post, lu_printer_friendly_page
    character(:), allocatable :: post
    character(len=6)          :: current_entry_txt
    character(len=256)        :: system_msg
    !
    do current_entry = 1, total_entries
       post=""
       write(unit = current_entry_txt, fmt = '(g0)') current_entry
       inquire(file = path // data_dir // data_post(current_entry,6)%date // '.blog', size = file_length)
       post = repeat(" ", file_length)
       open(newunit = lu_post, file = path // data_dir // data_post(current_entry,6)%date // '.blog', access = 'stream', action =&
            & 'read', iostat = ios, iomsg = system_msg)
       if (ios /= 0) call terminate(error_code = 1193, sys_msg = system_msg)
       read(unit = lu_post) post
       close(unit = lu_post)
       ! Printer friendly page
       open(newunit = lu_printer_friendly_page, file = path // export_html_dir // data_post(current_entry,6)%date //".htm", action&
            & = 'write', iostat = ios, iomsg = system_msg)
       if (ios /= 0) call terminate(error_code = 1241, sys_msg = system_msg)
       write(unit = lu_printer_friendly_page, fmt = '(a)') header_1, '<title>' // blog_title // ' | ' //&
            & trim(data_post(current_entry,7)%post_title) // '</title>'
       if (css_print) write(unit = lu_printer_friendly_page, fmt  = '(a)') '<link rel="stylesheet" href="' // css_print_file // '"&
            & type="text/css" media="all" />'
       write(unit = lu_printer_friendly_page, fmt = '(a)') '</head>', '<body>', '<article>', '<h3>',&
            & trim(adjustl(data_post(current_entry,7) %post_title)), '</h3>', post(len_trim(data_post(current_entry,7)%post_title)&
            &+2 : len(post)) // '</article>', '<footer>', '<hr />', '<address>' // blog_title // '</address>', '<samp dir="ltr">' &
            &// data_post(current_entry,6)%date(1:4) //'-' // data_post(current_entry,6)%date(5:6) // '-' //&
            & data_post(current_entry,6)%date(7:8) // ' (' // data_post(current_entry,6)%date(9:10) // ':' //&
            & data_post(current_entry,6)%date(11:12) //')' // '</samp>', '</footer>', '</body>', '</html>'
       close(unit = lu_printer_friendly_page)
       ! End printer friendly page
       open(newunit = lu_permalink, file = path // export_html_dir // data_post(current_entry,6)%date //".html", action = 'write',&
            & iostat = ios, iomsg = system_msg)
       if (ios /= 0) call terminate(error_code = 1191, sys_msg = system_msg)
       write(unit = lu_permalink, fmt = '(g0)')header_1, '<title>' // blog_title // ' | ' // trim(data_post(current_entry,7) &
            &%post_title) // '</title>' // header_2
       menu: if (len(menu_html) > 0) then
          if (current_entry < total_entries .and. current_entry /= 1) then
             write(unit = lu_permalink, fmt = '(g0)', advance = 'no') '<nav>' // nl // '<hr />' // nl // '<a rel="next" href="' //&
                  & data_post(current_entry-1,6)%date // '.html' // '">&nbsp;&lt;=&nbsp;</a> ' // ' <a rel&
                  &="prev" href="' // data_post(current_entry+1,6)%date // '.html">&nbsp;=>&nbsp;</a>' // nl // menu_html // '<hr&
                  & />' // nl // '</nav>' // nl
             exit menu
          end if
          if (current_entry == 1 .and. total_entries > 1) then
             write(unit = lu_permalink, fmt = '(g0)', advance = 'no') '<nav>' // nl // '<hr />' // nl // '<a rel="prev" href="' //&
                  & data_post(current_entry+1,6)%date // '.html' // '">&nbsp;=>&nbsp;</a>' // menu_html // '<hr&
                  &  />' // nl // '</nav>' // nl
             exit menu
          end if
          if (total_entries == current_entry .and. total_entries > 1) then
             write(unit = lu_permalink, fmt = '(g0)', advance = 'no') '<nav>' // nl // '<hr />' // nl// '<a rel="next" href="' //&
                  & data_post(current_entry-1,6)%date // '.html' // '">&nbsp;&lt;=&nbsp;</a>' // nl // menu_html // '<hr />' //&
                  & nl // '</nav>' // nl
             exit menu
          end if
          if (total_entries == 1) then
             write(unit = lu_permalink, fmt = '(g0)', advance = 'no') '<nav>' // nl // '<hr />'  // nl // menu_html // '<hr />' //&
                  & nl // '</nav>' // nl // '<hr />' // nl // '</nav>' // nl
             exit menu
          end if
       end if menu
       write(unit = lu_permalink, fmt = '(g0)') '<article>', '<header>','<h3>' // trim(adjustl(data_post(current_entry,7)&
            &%post_title)) // '</h3>', '</header>', post(len_trim(data_post(current_entry,7)%post_title)+2 : file_length) // '&
            &<footer>', '<a href="' // data_post(current_entry,6)%date // '.htm">' // printer_friendly // '</a>&nbsp;&nbsp;', '&
            &<samp dir="ltr"><time datetime="' // data_post(current_entry,6)%date(1:4) // '-' // data_post(current_entry,6)&
            &%date(5:6) // '-' // data_post(current_entry,6)%date(7:8) // 'T' // data_post(current_entry,6)%date(9:10) //':' //&
            & data_post(current_entry,6)%date(11:12) // '">[' // data_post(current_entry,6)%date(1:4) //'-' //&
            & data_post(current_entry,6)%date(5:6) // '-' // data_post(current_entry,6)%date(7:8) // ' (' //&
            & data_post(current_entry,6)%date(9:10) // ':' // data_post(current_entry,6)%date(11:12) //')]' // '</time>&nbsp;' //&
            & ' [# ' // trim(current_entry_txt) // ']</samp>', '</footer>', '</article>', footer_1
       close(unit = lu_permalink)
    end do
  end subroutine html_permalink
  !
  subroutine html_year_archive()
    !! Make HTML code of main index page.
    integer :: i, ios, j, lu_archive, total_month_entries
    character(len=256) :: system_msg
    !
    open(newunit = lu_archive, file = path // export_html_dir // archive // ".html", action = 'write', iostat = ios, iomsg =&
         & system_msg)
    if (ios /= 0) call terminate(error_code = 1201, sys_msg = system_msg)
    write(unit = lu_archive, fmt = '(a)') header_1 // nl // '<title>' //blog_title // ' | ' // trim(archive) // '</title>'//&
         & header_2
    if (len(menu_html) > 0) write(unit = lu_archive, fmt  = '(a)') '<nav>', '<hr />', menu_html // '<hr />', '</nav>'
    write(unit = lu_archive, fmt = '(a)') '<article>','<header>','<h3>' // archive // '</h3>','</header>', '<dl>'
    scan_year: do j = year1, year2, -1
       write (unit = lu_archive, fmt = '(a, i4, 2a)') '<dt>', stats(j,0), '</dt>', '<dd>'
       scan_month: do i = 1, 12
          total_month_entries = stats(j,i)
          if (total_month_entries == 0) cycle
          write(unit = lu_archive, fmt = '(a, i4, i2.2, 3a, I2.2, a)') '<a href="',j ,i, '.html">', trim(monthes(i)), '</a><code&
               & dir="ltr">[', total_month_entries, ']</code>'
       end do scan_month
       write(unit = lu_archive, fmt = '(a)') '</dd>'
    end do scan_year
    write(unit = lu_archive, fmt = '(a)') '</dl>', '</article>', footer_1
    close(unit = lu_archive)
  end subroutine html_year_archive
  !
  subroutine init_conf()
    !! Initialisation according the settings of the configuration file and environment.
    !! (This is the first step of two.)
    integer :: ios, lu_config, position_char
    character(len=512) :: readline
    character(len=256) :: system_msg
    character(:), allocatable :: label, url, user_editor
    !
    css_file         = ""
    css_print_file   = ""
    archive          = "Archive"
    home             = "Home"
    permalink        = "permalink"
    printer_friendly = "printer-friendly page"
    call get_environment_variable(name = "EDITOR", value = readline)
    editor = trim(readline)
    if (len(editor) == 0) editor = default_editor
    open(newunit = lu_config, file = path // config_file, status = 'old', action = 'read', iostat = ios, iomsg = system_msg)
    if (ios /= 0) call terminate(error_code = 1111, sys_msg = system_msg)
    settings: do
       read(unit = lu_config, fmt = '(a)', iostat = ios) readline
       if (ios /= 0) exit
       readline = adjustl(readline)
       if (readline(1:1) == "#" .or. len(trim(readline)) == 0) cycle
       global: if (trim(readline) == "[global]") then
          do
             user_editor = ""
             read(unit = lu_config, fmt = '(a)', iostat = ios) readline
             readline = adjustl(readline)
             if (ios /= 0) exit global
             if (readline(1:1) == "#") cycle
             if (readline(1:1) == "[") then
                backspace(unit = lu_config)
                exit global
             end if
             if (trim(readline) == "") cycle
             position_char = index(string = readline, substring = "=")
             select case (readline(1:position_char-1))
             case ("TITLE")
                blog_title = trim(adjustl(readline(position_char+1:)))
             case ("CHARSET")
                charset = trim(adjustl(readline(position_char+1:)))
             case ("LANG")
                language = trim(adjustl(readline(position_char+1:)))
             case ("DIRECTION")
                direction = adjustl(readline(position_char+1:))
                if (len(trim(direction)) == 0) direction = "ltr"
             case ("MAX_ENTRIES")
                readline = adjustl(readline(position_char+1:))
                read(unit = readline, fmt = '(I2)') entries_per_page
                if (entries_per_page < 1) entries_per_page = 10
             case ("CSS")
                css_file = trim(adjustl(readline(position_char+1:)))
             case ("CSS_PRINT")
                css_print_file = trim(adjustl(readline(position_char+1:)))
             case("EDITOR")
                user_editor = trim(adjustl(readline(position_char+1:)))
                if (len(user_editor) > 0) editor = user_editor
             end select
          end do
       end if global
       locale: if (trim(readline) == "[translation]") then
          do
             read(unit = lu_config, fmt = '(a)', iostat = ios) readline
             readline = adjustl(readline)
             if (ios /= 0) exit locale
             position_char = index(string = readline, substring = "=")
             if (readline(1:1) == "#") cycle
             if (readline(1:1) == "[") then
                backspace(unit = lu_config)
                exit locale
             end if
             if (trim(readline) == "") cycle
             select case (readline(1:position_char-1))
             case ("HOME")
                home = trim(adjustl(readline(position_char+1:)))
                if (len(home) == 0) home = "Home" 
             case ("ARCHIVE")
                archive = trim(adjustl(readline(position_char+1:)))
                if(len(archive) == 0) archive = "Archive"
             case ("PERMALINK")
                permalink = trim(adjustl(readline(position_char+1:)))
                if (len(permalink) == 0) permalink = "permalink"
             case ("PRINTER_FRIENDLY")
                printer_friendly = trim(adjustl(readline(position_char+1:)))
                if (len(printer_friendly) == 0) printer_friendly = "printer-friendly page" 
             case ("JANUARY")
                monthes(1) = adjustl(readline(position_char+1:))
                if (monthes(1) == "") monthes(1) = "jan"
             case ("FEBRUARY")
                monthes(2) = adjustl(readline(position_char+1:))
                if (monthes(2) == "") monthes(2) = "feb"
             case ("MARCH")
                monthes(3) = adjustl(readline(position_char+1:))
                if (monthes(3) == "") monthes(3) = "mar"
             case ("APRIL")
                monthes(4) = adjustl(readline(position_char+1:))
                if (monthes(4) == "") monthes(4) = "apr"
             case ("MAY")
                monthes(5) = adjustl(readline(position_char+1:))
                if (monthes(5) == "") monthes(5) = "may"
             case ("JUNE")
                monthes(6) = adjustl(readline(position_char+1:))
                if (monthes(6) == "") monthes(6) = "jun"
             case ("JULY")
                monthes(7) = adjustl(readline(position_char+1:))
                if (monthes(7) == "") monthes(7) = "jul"
             case ("AUGUST")
                monthes(8) = adjustl(readline(position_char+1:))
                if (monthes(8) == "") monthes(8) = "aug"
             case ("SEPTEMBER")
                monthes(9) = adjustl(readline(position_char+1:))
                if (monthes(9) == "") monthes(9) = "sep"
             case ("OCTOBER")
                monthes(10) = adjustl(readline(position_char+1:))
                if (monthes(10) == "") monthes(10) = "oct"
             case ("NOVEMBER")
                monthes(11) = adjustl(readline(position_char+1:))
                if (monthes(11) == "") monthes(11) = "nov"
             case ("DECEMBER")
                monthes(12) = adjustl(readline(position_char+1:))
                if (monthes(12) == "") monthes(12) = "dec"
             end select
          end do
       end if locale
       menu: if (trim(readline) == "[menu]") then
          menu_html = ""
          do
             url = ""
             read(unit = lu_config, fmt = '(a)', iostat = ios) readline
             readline = adjustl(readline)
             if (ios /= 0) exit menu
             position_char = index(string = readline, substring = "=")
             if (readline(1:1) == "#") cycle
             if (readline(1:1) == "[") then
                backspace(unit = lu_config)
                exit menu
             end if
             if (trim(readline) == "") cycle
             position_char = index(string = readline, substring = "=")
             label = trim(readline(1:position_char-1))
             url = trim(adjustl(readline(position_char+1:)))
             menu_html = menu_html // '[<a href="' // url // '">'// label //'</a>]' // nl
          end do
       end if menu
       template_description: if (trim(readline) == "[description]") then
          description = ""
          do
             read(unit = lu_config, fmt = '(a)', iostat = ios) readline
             readline = adjustl(readline)
             if (ios /= 0) exit template_description
             if (readline(1:1) == "#") cycle
             if (readline(1:1) == "[") then
                backspace(unit = lu_config)
                exit template_description
             end if
             if (trim(readline) == "") cycle
             description = description // trim(readline) //nl
          end do
       end if template_description
       template_footer: if (trim(readline) == "[footer]") then
          footer = ""
          do
             read(unit = lu_config, fmt = '(a)', iostat = ios) readline
             readline = adjustl(readline)
             if (ios /= 0) exit template_footer
             if (readline(1:1) == "#") cycle
             if (readline(1:1) == "[") then
                backspace(unit = lu_config)
                exit template_footer
             end if
             if (trim(readline) == "") cycle
             footer = trim(footer) // trim(readline) //nl
          end do
       end if template_footer
    end do settings
    close(unit = lu_config)
    if (total_entries < 2) then
       menu_html = '[<a href="index.html">' // home //'</a>]' // nl // trim(menu_html)
    else
       menu_html = '[<a href="index.html">' // home //'</a>]' // nl //'[<a href="' // archive // '.html">' // archive // '</a>]'&
         // nl // menu_html
    end if
    if (len(css_file) == 0) then
       css = .false.
    else
       css = .true.
    end if
    if (len(css_print_file) == 0) then
       css_print = .false.
    else
       css_print = .true.
    end if
  end subroutine init_conf
  !
  subroutine init_data()
    !! Collect titles and dates of entries. (This is the second step of two.)
    use, intrinsic :: iso_fortran_env, only: &
         iostat_end
    integer :: command_status = 0, exit_status = 0, file_length, intreadline, intreadline1, i, ios, j, lu_lstmp, lu_post,&
         & lu_wrong
    character(len=256) :: command_msg = "", system_msg
    character(len=512) :: readline, readline1
    !
    if (allocated(data_post)) deallocate(data_post)
    if (allocated(stats)) deallocate(stats)
    call execute_command_line(command = 'ls -r ' // path // data_dir // ' > ' // path // fblog_dir // 'data_list.txt', exitstat =&
         & exit_status, cmdstat = command_status, cmdmsg = command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1121, cmd_msg = command_msg)
    open(newunit = lu_lstmp, file = path // fblog_dir // "data_list.txt", action = 'read', status = 'old', iostat = ios, iomsg =&
         & system_msg)
    if (ios /= 0) call terminate(error_code = 1122, sys_msg    = system_msg)
    do
       read(unit = lu_lstmp, fmt = '(a)', iostat = ios) readline
       if (ios == iostat_end) exit
       if (len(trim(readline)) /= 19) then
          open(newunit = lu_wrong, file = path // data_dir // trim(readline), status = 'old', iostat = ios, iomsg = system_msg)
          if (ios /= 0) call terminate(error_code = 1123, sys_msg = system_msg)
          print *, i18n(is_deleted) // path // data_dir // trim(readline)
          close(unit = lu_wrong, status = 'delete')
          read(unit = lu_lstmp, fmt = '(a)', iostat = ios) readline
          if (ios == iostat_end) exit
       end if
       inquire(file = path // data_dir // trim(readline), size = file_length)
       if (file_length < 2) then
          open(newunit = lu_wrong, file = path // data_dir // trim(readline), status = 'old', iostat = ios, iomsg = system_msg)
          if (ios /= 0) call terminate(error_code = 1124, sys_msg = system_msg)
          print *, i18n(is_deleted) // path // data_dir // trim(readline)
          close(unit = lu_wrong, status = 'delete')
       end if
    end do
    close(unit = lu_lstmp)
    call execute_command_line(command = 'ls -r ' // path // data_dir // ' > ' // path // FBLOG_DIR // 'data_list.txt', exitstat =&
         & exit_status, cmdstat = command_status, cmdmsg = command_msg)
    if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1125, cmd_msg = command_msg)
    open(newunit = lu_lstmp, file = path // fblog_dir // "data_list.txt", action = 'read', status = 'old', iostat = ios, iomsg =&
         & system_msg)
    if (ios /= 0) call terminate(error_code = 1126, sys_msg = system_msg)
    total_entries = 0
    do
       read(unit = lu_lstmp, fmt = '(a)', iostat = ios) readline
       if (ios == iostat_end) exit
       total_entries = total_entries + 1
    end do
    if (total_entries == 0) then
       close(unit = lu_lstmp, status = 'delete')
       return
    end if
    rewind(unit = lu_lstmp)
    allocate(data_post(1:total_entries, 1:7))
    readline = ""
    do i = 1, total_entries
       read(unit = lu_lstmp, fmt = '(a)') readline
       data_post(i,6)%date = trim(readline(1:14))
       open(newunit = lu_post, file = path // data_dir // trim(readline), action = 'read', iostat = ios, iomsg = system_msg)
       if (ios /= 0) call terminate(error_code = 1127, sys_msg = system_msg)
       read(unit = lu_post, fmt = '(a)') readline1
       data_post(i,7)%post_title = trim(readline1)
       read(unit = readline(1:4), fmt = *) intreadline
       data_post(i,1)%year = intreadline
       read(unit = readline(5:6), fmt = *) intreadline
       data_post(i,2)%month = intreadline
       read(unit = readline(7:8), fmt = *) intreadline
       data_post(i,3)%day = intreadline
       read(unit = readline(9:10), fmt = *) intreadline
       data_post(i,4)%hour = intreadline
       read(unit = readline(11:12), fmt = *) intreadline
       data_post(i,5)%minute = intreadline
       read(unit = readline(13:14), fmt = *) intreadline
       data_post(i,6)%seconde = intreadline
       data_post(i,7)%post_title = trim(readline1)
       close(unit = lu_post)
    end do
    year1 = data_post(1,1)%year
    year2 = data_post(total_entries,1)%year
    allocate(stats(year2:year1,0:14))
    stats(year2:year1,0:14) = 0
    do j = year2, year1
       stats(j,0) = j
    end do
    rewind(unit = lu_lstmp)
    do
       read(unit = lu_lstmp, fmt = '(a)', iostat = ios) readline
       if (ios == iostat_end) exit
       read(unit = readline(1:4), fmt = *) intreadline
       read(unit = readline(5:6), fmt = *) intreadline1
       stats(intreadline, intreadline1) = stats(intreadline,intreadline1) + 1
    end do
    close(unit = lu_lstmp, status = 'delete')
  end subroutine init_data
  !
  subroutine update()
    !! Build all pages of the blog. The most important procedure of the project!
    !! This procedure could be called from an other program by including in it module fblog_m.
    logical :: file_exist
    integer :: command_status = 0, count_archive_pages, count_css = 0, count_pages = 0, exit_status = 0
    character(len=256) :: command_msg = ""
    !
    call init_data()
    call init_conf()
    header_1 = '<!DOCTYPE html>'
    if ((len(language) == 0) .and. (direction == "ltr")) header_1 = header_1 // nl // '<html>'
    if ((len(language) == 0) .and. (direction == "rtl")) header_1 = header_1 // nl // '<html dir="rtl">'
    if ((len(language) > 1) .and. (direction == "ltr")) header_1 = header_1 // nl // '<html lang="' // language // '">'
    if ((len(language) > 1) .and. (direction == "rtl")) header_1 = header_1 // nl // '<html lang="' // language // '" dir="rtl">'
    header_1 = header_1 // nl // '<head>' // nl // '<meta charset=' // '"' // charset // '" />' // nl // '<meta name="generator"&
         & content="fBlog ' // VERSION // '" />'
    if (css) header_2 = nl // '<link rel="stylesheet" href="' // css_file // '" type="text/css" media="all" />' // nl // '</head>'&
         & // nl // '<body>' // nl // '<header>' // nl // '<h1><a href="index.html">' // blog_title // '</a></h1>' // nl // '<&
         &/header>'
    if (len(description) > 0) header_2 = header_2 // nl // '<aside>' // nl // description // '</aside>'
    if (len(footer) > 0) footer_1 = '<footer>' // nl // '<hr />' // nl // footer // '</footer>' // nl // '</body>'// nl // '</html&
         &>'
    print '(a)', clock() // i18n(begin_update)
    print '(a)', clock() // i18n(initialisation_reading_data)
    print '(a)', clock() // i18n(initialisation_reading_conf)
    inquire(file = path // export_html_dir // 'index.html', exist = file_exist)
    if (file_exist) then
       print '(6a)', clock(), i18n(make_clear_subdirectory), "./", path, export_html_dir, "'"
       call execute_command_line(command = "rm "// path // export_html_dir // "*", wait=.true., exitstat = exit_status, cmdstat =&
            & command_status, cmdmsg = command_msg)
       if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1211, cmd_msg = command_msg)
       print '(6a)', clock(), i18n(end_clearing_subdirectory), "./", path, export_html_dir, "'"
    end if
    if (total_entries > 1) then
       print '(6a)', clock(), i18n(make_permalink_pages), "./", path, export_html_dir, "ccyymmddhhmmss.htm(l)'"
       call html_permalink()
       print '(a,g0,a)', clock(), 2 * total_entries, " " // i18n(permalink_pages_built)
       call html_month_archive(n_pages = count_archive_pages)
       print '(a,g0,2a)', clock(), count_archive_pages, " ", i18n(monthly_archive_pages_built)
       print '(7a)', clock(), i18n(make_archive_pages), "./", path, export_html_dir, archive, ".html'"
       call html_year_archive()
    end if
    print '(6a)', clock(), i18n(make_home_page), "./", path, export_html_dir, "index.html'"
    call html_home(user_home = home)
    print '(6a)', clock(), i18n(make_pages), "./", path, export_html_dir, "*.html'"
    call html_page(n_pages = count_pages)
    print '(a,g0,2a)', clock(), count_pages, " ", i18n(pages_built)
    if (css) then
       print '(7a)', clock(), i18n(copy_css), "./", path, export_html_dir, css_file, "'"
       call execute_command_line(command = "cp " // path // styles_dir // css_file // " " // path // export_html_dir, exitstat =&
            & exit_status, cmdstat = command_status, cmdmsg = command_msg)
       if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1212, inf_msg = i18n(error_css))
    else
       print'(a)', i18n(warn_about_no_css)
    end if
    if (css_print .and. (total_entries > 1)) then
       print '(7a)', clock(), i18n(copy_css_print), "./", path, export_html_dir, css_print_file, "'"
       call execute_command_line(command = "cp " // path // styles_dir // css_print_file // " " // path // export_html_dir,&
            & exitstat = exit_status, cmdstat = command_status, cmdmsg = command_msg)
       if (exit_status /= 0 .or. command_status /= 0) call terminate(error_code = 1213, inf_msg = i18n(error_css_print))
    else
       if (css_print .and. (total_entries > 1)) print'(a)', i18n(warn_about_no_css_print)
    end if
    print '(2a)', clock(), i18n(update_done)
    if (css) count_css = 1
    if (css_print .and. (total_entries > 1)) count_css = count_css + 1
    if (total_entries < 2) print '(g0,6a)', count_pages + 1 + count_css, " ", i18n(files_generated_inside_the_subdirectory), &
         & "./", path, export_html_dir, "'"
    if (total_entries > 1)print '(g0,6a)', count_archive_pages + (2 * total_entries) + count_pages + 2 + count_css, " ",&
         & i18n(files_generated_inside_the_subdirectory), "./" // path, export_html_dir, "'"
  end subroutine update
  !
  subroutine terminate(error_code, cmd_msg, inf_msg, sys_msg)
    !! Double role: error function and normal terminaison of job. In case of malfunction,
    !! an error code is provided. This error code gives location of the failure in source
    !! code.
    !
    use, intrinsic :: iso_fortran_env, only: error_unit
    !
    integer, intent(in) :: error_code
    character(len=*), intent(in), optional :: cmd_msg
    character(len=*), intent(in), optional :: inf_msg
    character(len=*), intent(in), optional :: sys_msg
    character(len=*), parameter :: move_cursor_left = '[20D'
    !
    if (allocated(archive))          deallocate(archive)
    if (allocated(blog_title))       deallocate(blog_title)
    if (allocated(charset))          deallocate(charset)
    if (allocated(css_file))         deallocate(css_file)
    if (allocated(css_print_file))   deallocate(css_print_file)
    if (allocated(data_post))        deallocate(data_post)
    if (allocated(description))      deallocate(description)
    if (allocated(editor))           deallocate(editor)
    if (allocated(footer))           deallocate(footer)
    if (allocated(footer_1))         deallocate(footer_1)
    if (allocated(header_1))         deallocate(header_1)
    if (allocated(header_2))         deallocate(header_2)
    if (allocated(home))             deallocate(home)
    if (allocated(language))         deallocate(language)
    if (allocated(menu_html))        deallocate(menu_html)
    if (allocated(path))             deallocate(path)
    if (allocated(permalink))        deallocate(permalink)
    if (allocated(printer_friendly)) deallocate(printer_friendly)
    if (allocated(stats))            deallocate(stats)
    !
    !! 1000 -> 1010  subroutine add_post
    !! 1011 -> 1020  subroutine config
    !! 1021 -> 1030  subroutine create_blog
    !! 1031 -> 1040  subroutine delete_css
    !! 1041 -> 1050  subroutine delete_page
    !! 1051 -> 1060  subroutine delete_post
    !! 1061 -> 1070  subroutine edit_css
    !! 1071 -> 1080  subroutine edit_page
    !! 1081 -> 1090  subroutine edit_post
    !! 1091 -> 1100  program main
    !! 1111 -> 1120  subroutine init_conf
    !! 1121 -> 1130  subroutine init_data
    !! 1131 -> 1140  subroutine console
    !! 1141 -> 1150  subroutine menu_post
    !! 1151 -> 1160  subroutine list_css
    !! 1161 -> 1170  subroutine list_pages
    !! 1171 -> 1180  subroutine html_home
    !! 1181 -> 1190  subroutine html_month_archive
    !! 1191 -> 1200  subroutine html_permalink
    !! 1201 -> 1210  subroutine html_year_archive
    !! 1211 -> 1220  subroutine update
    !! 1221 -> 1230  subroutine html_page
    !! 1231 -> 1240  function console_menu
    !! 1241 -> 1250  subroutine html_permalink 
    !
    if (error_code /= 0) then
       if (len(trim(sys_msg)) > 0) write(unit = error_unit, fmt = '(3a)') i18n(system_said), " ", trim(sys_msg)
       if (len(trim(cmd_msg)) > 0) write(unit = error_unit, fmt = '(3a)') i18n(shell_said), " ", trim(cmd_msg)
       if (present(inf_msg)) write(unit = error_unit, fmt = '(a)') trim(inf_msg)
       write(unit = error_unit, fmt = '(t12,g0,a)', advance = 'no') error_code, char(27) // move_cursor_left
       error stop
    else
       if (present(inf_msg)) print'(a)', trim(inf_msg)
       stop
    end if
    error stop 'Internal error in subroutine Terminate'
  end subroutine terminate
  !
end module fblog_m
