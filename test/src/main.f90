program test

    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: gmsh_reader_interface

    implicit none

    call run_test('data/t1-bin.msh2')
    call run_test('data/t1-bin.msh4')
    call run_test('data/t1-txt.msh2')
    call run_test('data/t1-txt.msh4')

    print *  ! BLANK_LINE
    print *, 'SUCCESS'
    print *  ! BLANK_LINE

    contains

    subroutine run_test(file_path)

        character(len=*), intent(in) :: file_path
        !! A dummy argument for this SUBROUTINE

        integer :: stat
        !! A local variable for this SUBROUTINE

        character(len=256) :: msg
        !! A local variable for this SUBROUTINE

        type(gmsh_msh_file_t) :: msh_file
        !! A local variable for this SUBROUTINE

        call msh_file%read_file( &!
            file_path = file_path , &!
            stat      = stat      , &!
            msg       = msg         &!
        )

        if (stat .ne. 0) then
            write(ERROR_UNIT, *) stat
            write(ERROR_UNIT, *) msg
            error stop
        end if

        print * , 'file path            : ', trim(file_path)
        print * , '$MeshVersion / Major : ', msh_file%mesh_format%version%get_major()
        print * , '$MeshVersion / Minor : ', msh_file%mesh_format%version%get_minor()

    end subroutine

end program
