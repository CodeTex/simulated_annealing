module io

    ! modules
    use constants

    implicit none
    ! static
    ! dynamic
    integer :: ios

    contains ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    subroutine sa_read_data(unit, path, matrix, dim_m, dim_n) ! --------------> SUBR file_read_data
        ! read .dat files for exercise 07
        character(len=*), intent(in) :: path                                    ! path to file
        integer, intent(in) :: unit, dim_m, dim_n
        integer, dimension(dim_m, dim_n), intent(inout) :: matrix
        integer :: i,j,i_dummy,j_dummy

        call open_file_r(unit, path)

        do i = 1, dim_m
            do j = 1, dim_n
                read(unit, fmt='(3i5)') i_dummy, j_dummy, matrix(i,j)
            end do
        end do

        call close_file(unit)

    end subroutine sa_read_data


    subroutine sa_write_data(unit, path, matrix, dim_m, dim_n) ! -------------> SUBR sa_write_data
        ! write .dat files for exercise 07
        character(len=*), intent(in) :: path                                    ! path to file
        integer, intent(in) :: unit, dim_m, dim_n
        integer, dimension(dim_m, dim_n), intent(in) :: matrix
        integer :: i,j

        call open_file_w(unit, path)

        do i = 1, dim_m
            do j = 1, dim_n
                write(unit, fmt='(3i5)') i, j, matrix(i,j)
            end do
            write(unit, *)
        end do

        call close_file(unit)

    end subroutine sa_write_data


    subroutine open_file_r(unit, path) ! -------------------------------------> SUBR open_file_w
        ! generalized file opener for read operations
        character(len=*), intent(in) :: path                                    ! path to file
        integer, intent(in) :: unit
        
        open(                                                               &
            unit=unit, access='sequential', action='read',                  &
            file=path, iostat=ios                                           &
        )

    end subroutine open_file_r
    

    subroutine open_file_w(unit, path) ! -------------------------------------> SUBR open_file_w
        ! generalized file opener for write operations
        character(len=*), intent(in) :: path                                    ! path to file
        integer, intent(in) :: unit
        
        open(                                                               &
            unit=unit, access='sequential', action='write',                 &
            file=path, iostat=ios, status='replace'                         &
        )

    end subroutine open_file_w


    subroutine close_file(unit) ! --------------------------------------------> SUBR close_file()
        ! generalized file closer
        integer, intent(in) :: unit

        close(unit=unit, iostat=ios, status='keep')

    end subroutine close_file


end module io