module parallel_mod

  use mpi

  implicit none

  private

  public parallel_init
  public parallel_final

  integer comm_id

  type proc_type
    integer id
  end type proc_type

  type(proc_type), allocatable, target :: procs(:)
  type(proc_type), pointer :: curr_proc => null()

contains

  subroutine parallel_init(comm_id_)

    integer, intent(in), optional :: comm_id_

    integer i, num_proc, ierr

    if (present(comm_id_)) then
      comm_id = comm_id_
    else
      comm_id = MPI_COMM_WORLD
      call MPI_Init(ierr)
    end if

    call MPI_Comm_size(comm_id, num_proc, ierr)
    allocate(procs(num_proc))
    do i = 1, num_proc
      procs(i)%id = i - 1
    end do
    call MPI_Comm_rank(comm_id, i, ierr)
    curr_proc => procs(i + 1)

  end subroutine parallel_init

  subroutine parallel_final()

    integer ierr

    call MPI_Finalize(ierr)

  end subroutine parallel_final

end module parallel_mod
