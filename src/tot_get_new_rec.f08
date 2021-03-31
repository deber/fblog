function tot_get_new_rec(tot_unit)
  integer, intent(in) :: tot_unit
  integer :: tot_get_new_rec
  integer :: file_size, io_rec_len
  character(len=REC_LEN) :: fake
  type (header) h
  type (free_record) fr
  read(tot_unit, rec=1) h
  if (h%total_free /= 0) then
    tot_get_new_rec = h%last_free
    read(tot_unit, rec=h%last_free) fr
    h%last_free = fr%previous
    h%total_free = h%total_free -1
    write(tot_unit, rec=1) h
  end if
  if (h%total_free == 0) then
    inquire(iolength=io_rec_len) fake  
    inquire(tot_unit, size=file_size) 
    tot_get_new_rec = (file_size / rec_len) + 1
  end if
end function tot_get_new_rec
