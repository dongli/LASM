module domain_mod

  use geometry_mod

  implicit none

  private

  public domain_type

  !
  !    left top corner                top edge                right top corner
  !                     |        |        |        |       |
  !                   --|----------------------------------|--
  !                     |                                  |
  !                     |                                  |
  !                     |                                  |
  !          left edge  |                                  |  right edge
  !                     |                                  |
  !                     |                                  |
  !                     |                                  |
  !                   --|----------------------------------|--
  !                     |                                  |
  ! left bottom corner              right edge                right bottom corner

  type, abstract :: domain_type
    integer :: id = 0
    integer, allocatable :: left_edge_ngb_idx(:)
    integer, allocatable :: left_top_corner_ngb_idx(:)
    integer, allocatable :: left_bottom_corner_ngb_idx(:)
    integer, allocatable :: bottom_edge_ngb_idx(:)
    integer, allocatable :: right_bottom_corner_ngb_idx(:)
    integer, allocatable :: right_edge_ngb_idx(:)
    integer, allocatable :: right_top_corner_ngb_idx(:)
    integer, allocatable :: top_edge_ngb_idx(:)
  contains
    procedure(domain_outside), deferred :: outside
  end type domain_type

  interface
    logical function domain_outside(this, coord)
      import domain_type
      import coord_type
      class(domain_type), intent(in) :: this
      class(coord_type), intent(in) :: coord
    end function domain_outside
  end interface

end module domain_mod
