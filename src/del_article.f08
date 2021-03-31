recursive subroutine del_article
  integer :: iostat, status, u1, u2
  character(len=:), allocatable :: pathname
  character(len=16) :: timestamp
  character(len=256) :: iomsg, msg
  allocate(character :: pathname)
  write(*,'(/,a)', advance='no') "Enter timestamp of article (CCAAMMDDHHMMSS):"
  read(*,'(a)') timestamp
  pathname = trim(data_dir) // SOFT // ".dat"
  call tot_delete(pathname, timestamp, status, msg)
  if (allocated(pathname)) deallocate(pathname)
  print '(a,g0,2a)', "Status=", status, " msg:", msg
  if (status == 0) then
    ! Delete file "*.blog"
    open(newunit=u1, file= timestamp // ".blog", iostat=iostat, iomsg=iomsg)
    if (iostat /= 0) error stop "del_article:" // trim(iomsg)
    close(u1, status='delete')
    ! Delete file "*.g"
    open(newunit=u2, file= timestamp // ".g", iostat=iostat, iomsg=iomsg)
    if (iostat /= 0) error stop "del_article:" // trim(iomsg)
    close(u2, status='delete')    
  end if
  call menu
end subroutine del_article
