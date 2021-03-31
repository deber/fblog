module tot ! TOT means "Time Oriented Table" (soit "Table orientée temps")
  implicit none
  type :: header
    integer :: total
    integer :: first
    integer :: last
    integer :: total_free
    integer :: last_free
    integer :: next_year
  end type header
  type :: free_record
    integer :: previous
  end type free_record
  type :: article
    character(len=16) :: id
    integer(kind=4) :: previous=0
    integer(kind=4) :: next=0
  end type article
  type :: year
    character(len=8) :: date
    integer :: total_year
    integer :: next_year
    integer :: next_month
  end type year
  type :: month
    character(len=8) :: date
    integer :: total_month
    integer :: next_month
    integer :: next_day
  end type month
  type :: day
    character(len=8) :: date
    integer :: total_day
    integer :: next_day
    integer :: next_article
  end type day
  integer,parameter :: REC_LEN = 32
contains
  include "tot_create.f08"
  include "tot_append.f08"
  !include "tot_index.f08"
  include "tot_list.f08"
  include "tot_delete.f08"
  include "tot_get_new_rec.f08"
  include "tot_index_year.f08"
end module tot
