subroutine read_conf
  integer :: iostat, u
  character(len=256) :: iomsg
  ! DELIM=QUOTE is needed with IFORT compiler
  open (newunit=u, file=trim(data_dir) // SOFT // '.conf', DELIM='QUOTE', status='old', iostat=iostat, iomsg=iomsg)
  if (iostat /= 0) error stop trim(iomsg)
  read(u, nml=settings, iostat=iostat, iomsg=iomsg)
  if (iostat /= 0) error stop trim(iomsg)
  close(u)
end subroutine read_conf
