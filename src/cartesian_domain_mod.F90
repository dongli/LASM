module cartesian_domain_mod

  use constants_mod, only: r8
  use geometry_mod
  use domain_mod

  implicit none

  private

  type, extends(domain_type) :: cartesian_domain_type
    type(cartesian_coord_type) left_top_corner
    type(cartesian_coord_type) left_bottom_corner
    type(cartesian_coord_type) right_top_corner
    type(cartesian_coord_type) right_bottom_corner
  contains
    procedure :: outside => cartesian_domain_outside
  end type cartesian_domain_type

  type(cartesian_domain_type), allocatable :: domains(:)

contains

  logical function cartesian_domain_outside(this, coord) result(res)

    class(cartesian_domain_type), intent(in) :: this
    class(coord_type), intent(in) :: coord

    res = .false.
    select type (coord)
    type is (cartesian_coord_type)
      if (coord%x < this%left_top_corner%x    || coord%x > this%right_top_corner%x    || &
          coord%y < this%left_bottom_corner%y || coord%y > this%right_bottom_corner%y) then
        res = .true.
      end if
    class default
      write(*, *) '[Error]: Wrong coordinate type!'
      stop 1
    end select

  end function cartesian_domain_outside

end module cartesian_domain_mod
