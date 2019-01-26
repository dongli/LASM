module parcel_mod

  use constants_mod, only: r8
  use geometry_mod

  implicit none

  private

  public parcel_type

  type parcel_type
    integer id
    type(spherical_coord_type) xs
    type(cartesian_coord_type) xc
    type(spherical_velocity_type) vs
    real(r8) H(2,2) ! Horizontal shape matrix
    real(r8) volume
  contains
    procedure :: init => parcel_init
    procedure :: pack => parcel_pack
    final :: parcel_final
  end type parcel_type

  integer, save :: parcel_count = 0

contains

  subroutine parcel_init(this, volume, lon, lat, lev)

    class(parcel_type), intent(out) :: this
    real(r8), intent(in) :: volume
    real(r8), intent(in) :: lon
    real(r8), intent(in) :: lat
    real(r8), intent(in), optional :: lev

    parcel_count = parcel_count + 1

    this%id = parcel_count
    this%volume = volume

    call this%xs%set(lon, lat, lev)

  end subroutine parcel_init

  function parcel_pack(this) result(res)

    class(parcel_type), intent(in) :: this

    real(r8) res

  end function parcel_pack

  subroutine parcel_final(this)

    type(parcel_type), intent(in) :: this

  end subroutine parcel_final

end module parcel_mod
