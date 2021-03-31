program fblog
  use tot, only: article, header, tot_create, tot_list, tot_append, tot_delete, article, header, free_record  
  implicit none
  character(len=*), parameter :: SOFT = "fblog"
  character(len=*), parameter :: VERSION = "1.0"
  integer :: argc, max_articles
  character(len=256) :: argv="", data_dir, description, editor, index_file, blog_title
  namelist /settings/ blog_title, max_articles, index_file, editor
  namelist /gopher/ description
  argc = command_argument_count()
  if (argc == 0) call menu_init
 call get_command_argument(1,argv)
  select case (argv)
    case ("-h", "--help"); call usage
    case ("-v", "--version") ; print'(a)', VERSION  
    case default; call usage
  end select
contains
  include "menu.f08"
  include "menu_init.f08"
  include "usage.f08"
  include "create_blog.f08"
  include "add_article.f08"
  include "list_articles.f08"
  include "menu_blog_setting.f08"
  include "read_conf.f08"
  include "del_article.f08"
end program fblog
