subroutine tot_index_year(tot_unit, id)
  integer, intent(in) :: tot_unit
  character(len=16), intent(in) :: id
  integer :: new_rec
  type (header) h
  type (year) current_year, previous_year
  read(tot_unit, rec=1) h
  if (h%next_year == 0) then                       ! Empty data base
print *,"coucou_0"  
    new_rec = tot_get_new_rec(tot_unit)
    current_year%date = id(1:4) // "0000"
    current_year%total_year = 1
    write(tot_unit, rec=new_rec) current_year
    h%next_year = new_rec
    write(tot_unit, rec=1) h
    return
  else
print*,"coucou_1"    
    read(tot_unit, rec=h%next_year) previous_year 
  end if
  if (previous_year%date(1:4) == id(1:4)) then
print*,"coucou_2"  
    current_year = previous_year
    current_year%total_year = current_year%total_year + 1
    write(tot_unit, rec=h%next_year) current_year
    return
  end if
  if (previous_year%date(1:4) < id(1:4)) then
print*,"coucou_3"  
    do
      read(tot_unit, rec=previous_year%next_year) current_year
      if (current_year%date(1:4) < id(1:4)) then
print*,"coucou_4"      
        previous_year = current_year
        cycle
      end if
      if (current_year%date(1:4) == id(1:4)) then
print*,"coucou_5"      
        current_year%total_year = current_year%total_year + 1
        write(tot_unit, rec=previous_year%next_year) current_year
        return      
      end if
    end do
  end if
end subroutine tot_index_year
