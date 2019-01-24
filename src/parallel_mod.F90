module parallel_mod

  use mpi

  implicit none

  private

  integer comm_id

contains

  subroutine parallel_init(comm_id_)

    integer, intent(in), optional :: comm_id_

    integer ierr

    if (present(comm_id_)) then
      comm_id = comm_id_
    else
      call MPI_Init(ierr)
    end if

  end subroutine parallel_init

  subroutine parallel_final()

    integer ierr

    call MPI_Final(ierr)

  end subroutine parallel_final

end module parallel_mod
