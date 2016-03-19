   "`fBlog' generates static web pages."  //                                &
   new_line('a') //                                                         &
   new_line('a') //                                                         &
   "Usage:  fblog [path]" // new_line('a') //                               &
   "   or:  fblog [option] [path]" // new_line('a') //                      &
   new_line('a') //                                                         &
   "Interactive mode:" // new_line('a') //                                  &
   "  Type `fblog' without option." //                                      &
   new_line('a') //                                                         &
   new_line('a') //                                                         &
   "Examples:"//new_line('a') //                                            &
   "  -`fblog'          run in interactive mode (in the curent directory)"  &
                                                      //  new_line('a') //  &
   "  -`fblog foo/bar'  run in interactive mode" //                         &
                                                          new_line('a') //  &
   "                    (in the directory `./foo/bar')" //                  &
                                                           new_line('a') // &
                                                           new_line('a') // &
   "Command-line mode:" // new_line('a') //                                 &
   "  Type `fblog' with only one option (and path if any)" //new_line('a')//&
   "Options:"//new_line('a') //                                             &
   "  -a, --add-post            add a post" // new_line('a') //             &
   "  -c, --config              setting of the blog" // new_line('a') //    &
   "      --create              create a new blog" // new_line('a') //      &
   "      --delete-post=NUMBER  delete a post (NUMBER= rank of the post)"   &
                                                       // new_line('a') //  &
   "      --delete-page=FILE    delete a page (FILE= page filename)"        &
                                                        // new_line('a') // &
   "      --delete-css=FILE     delete a style sheet (FILE= CSS filename)"  &
                                                        // new_line('a') // &
   "      --edit-post=NUMBER    edit a post (NUMBER= rank of the post)"     &
                                                        // new_line('a') // &
   "      --edit-css=FILE       edit a style sheet (FILE= CSS filename)"    &
                                                        // new_line('a') // &
   "      --edit-page=FILE      edit a page (FILE= page filename)" //       &
                                                           new_line('a') // &
   "  -h, --help                display this help and exit" // new_line('a')&
                                                                         // &
   "  -L, --list-all            display all posts in reverse" //            &
                                " chronological order" // new_line('a') //  &
   "  -l, --list-last           display last posts in reverse" //           &
                                " chronological order" // new_line('a') //  &
   "      --list-css            display avalaible style sheets" //          &
                                                           new_line('a') // &
   "      --list-pages          display avalaible pages" // new_line('a') //&
   "  -u, --update              update the weblog" // new_line('a') //      &
   "  -v, --version             give version of program"// new_line('a') // &
   "(The rank of a post is given by the options `-l' and `-L'; or, " //     &
                                       "looking at the" // new_line('a') // &
   "weblog, at the bottom of the posted entry, close to the date. " //      &
                                      "The newest post" // new_line('a') // &
   "get the rank `1' and the oldest one holds the amount of total " //      &
                                     "posted entries.)" // new_line('a') // &
   new_line('a') //                                                         &
   "Exit status:" // new_line('a') //                                       &
   "  0: ok, 1: failed" // new_line('a') //                                 &
   new_line('a') //                                                         &
   "Environment:"// new_line('a') //                                        &
   "  -EDITOR  user's prefered text editor (default is 'vi') "//            &
   new_line('a') //                                                         &
   "  -LANG    user's language (default is English)" //                     &
   new_line('a') //                                                         &
   new_line('a') //                                                         &
   "Files:" // new_line('a') //                                             &
   "  -./fBlog/fblog.conf  the configuration file" //                       &
   new_line('a') //                                                         &
   new_line('a') //                                                         &
   "Examples:"//new_line('a') //                                            &
   "  -`fblog --create'              build a new weblog" // new_line('a') //&
   "  -`fblog -a'                    add a new post" // new_line('a') //    &
   "  -`fblog --edit-post=2'         modify the second most recent post"    &
                                                        // new_line('a') // &
   "  -`fblog --edit-page=foo.html'  create (or modify) a static "          &
                                        // "page named `foo.html'" //       &
                                                           new_line('a') // &
   "  -`fblog -u'                    generate the web site into `./fBlog/"  &
                                      // "export_http'" // new_line('a') // &
   "  -`fblog -u foo/bar'            generate the web site into" //         &
                                                           new_line('a') // &
   "                                 `./foo/bar/fBlog/export_http'" //      &
                                                           new_line('a') // &  
   "  -`EDITOR=nano fblog'           use text editor `nano'" //             &
                                                           new_line('a') // &
   new_line('a') //                                                         &
   "Report fBlog bugs to <deber@users.sf.net>"