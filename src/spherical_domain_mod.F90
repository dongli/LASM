module spherical_domain_mod

  use constants_mod, only: r8
  use geometry_mod
  use domain_mod

  implicit none

  private

  type, extends(domain_type) :: spherical_domain_type
    logical :: north_pole = .false.
    logical :: south_pole = .false.
    type(spherical_coord_type) left_top_corner
    type(spherical_coord_type) left_bottom_corner
    type(spherical_coord_type) right_top_corner
    type(spherical_coord_type) right_bottom_corner
  contains
    procedure :: outside => spherical_domain_outside
  end type spherical_domain_type

  type(spherical_domain_type), allocatable :: domains(:)

contains

  logical function spherical_domain_outside(this, coord) result(res)

    class(spherical_domain_type), intent(in) :: this
    class(coord_type), intent(in) :: coord

  end function spherical_domain_outside

end module spherical_domain_mod
