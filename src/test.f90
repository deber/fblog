program test
  ! Print how many articles in a period of time
  implicit none
  integer(8) :: years, perday, result
  do
    write(*,'(a)',advance='no')"How many post per days: "
    read(*,*)perday
    if (perday == 0) exit
    write(*,'(a)',advance='no')"During how many years : "
    read(*,*)years
    if (years == 0) exit
    result = perday * 365 * years
      write(*, '(a,2(i0,a),/)') "result = ", result/1000,"K  (",result,")"
  end do

end program test
