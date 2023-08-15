program test

    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: gmsh_reader_interface

    implicit none

!   call run_test('t1-bin.msh2')
!   call run_test('t1-bin.msh4')
    call run_test('t1-txt.msh2')
!   call run_test('t1-txt.msh4')



    contains



    subroutine run_test(file_name)

        character(len=*), parameter :: data_folder_path = 'data/'
        !! A `PARAMETER` for this SUBROUTINE

        character(len=*), parameter :: result_folder_path = 'result/'
        !! A `PARAMETER` for this SUBROUTINE



        character(len=*), intent(in) :: file_name
        !! A dummy argument for this SUBROUTINE



        integer :: index_bin
        !! A local variable for this SUBROUTINE

        integer :: stat
        !! A local variable for this SUBROUTINE

        character(len=256) :: msg
        !! A local variable for this SUBROUTINE

        character(len=len(file_name)) :: file_name_ref
        !! A local variable for this SUBROUTINE

        type(gmsh_msh2_file_t) :: msh_file
        !! A local variable for this SUBROUTINE



        call msh_file%read_file( &!
            file_path = data_folder_path // file_name , &!
            stat      = stat                          , &!
            msg       = msg                             &!
        )

        call verify_stat(stat, msg)



        call msh_file%write_file_ascii( &!
            file_path = result_folder_path // file_name , &!
            iostat    = stat                            , &!
            iomsg     = msg                               &!
        )

        call verify_stat(stat, msg)



        file_name_ref = file_name
        index_bin     = index(string=file_name_ref, substring='bin')

        if (index_bin .gt. 0) then
            file_name_ref( index_bin:(index_bin + 2) ) = 'txt'
        end if



        print *     ! BLANK_LINE
        print '(A)' , file_name



        call execute_command_line( &!
            command =                                             &!
            &   'diff ' //                                        &!
            &   ( result_folder_path // file_name     ) // ' ' // &!
            &   ( data_folder_path   // file_name_ref )           &!
        )

    end subroutine



    subroutine verify_stat(stat, msg)

        integer, intent(in) :: stat
        !! A dummy argument for this SUBROUTINE

        character(len=*) :: msg
        !! A dummy argument for this SUBROUTINE

        if (stat .ne. 0) then
            write(ERROR_UNIT, *) stat
            write(ERROR_UNIT, *) trim(msg)
            error stop
        end if

    end subroutine

end program
