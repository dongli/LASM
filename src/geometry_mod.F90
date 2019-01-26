module geometry_mod

  use constants_mod, only: r8

  implicit none

  private

  public coord_type
  public spherical_coord_type
  public cartesian_coord_type
  public spherical_velocity_type

  type, abstract :: coord_type
  end type coord_type

  type, extends(coord_type) :: spherical_coord_type
    real(r8) lon
    real(r8) lat
    real(r8) lev
  contains
    procedure :: set => spherical_coord_set
    procedure :: from => spherical_coord_from_cartesian_coord
  end type spherical_coord_type

  type, extends(coord_type) :: cartesian_coord_type
    real(r8) x
    real(r8) y
    real(r8) z
  contains
    procedure :: from => cartesian_coord_from_spherical_coord
  end type cartesian_coord_type

  type spherical_velocity_type
    real(r8) u
    real(r8) v
    real(r8) w
  end type spherical_velocity_type

contains

  subroutine spherical_coord_set(this, lon, lat, lev)

    class(spherical_coord_type), intent(out) :: this
    real(r8), intent(in) :: lon
    real(r8), intent(in) :: lat
    real(r8), intent(in) :: lev

    this%lon = lon
    this%lat = lat
    this%lev = lev

  end subroutine spherical_coord_set

  subroutine spherical_coord_from_cartesian_coord(this, x)

    class(spherical_coord_type), intent(out) :: this
    type(cartesian_coord_type), intent(in) :: x

  end subroutine spherical_coord_from_cartesian_coord

  subroutine cartesian_coord_from_spherical_coord(this, x)

    class(cartesian_coord_type), intent(out) :: this
    type(spherical_coord_type), intent(in) :: x

  end subroutine cartesian_coord_from_spherical_coord

end module geometry_mod
