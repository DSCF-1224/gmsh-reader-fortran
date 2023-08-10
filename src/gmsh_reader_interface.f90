module gmsh_reader_interface

    use, intrinsic :: iso_fortran_env


    implicit none


    private
    public  :: gmsh_msh_file_t



    type :: gmsh_msh_file_t

        contains

        procedure, pass, public :: read_file

    end type



    integer, parameter, private :: IOSTAT_OK = 0



    interface ! for `gmsh_msh_file_t

        module subroutine read_file(gmsh_msh_file, file_path, stat, msg)

            class(gmsh_msh_file_t), intent(inout) :: gmsh_msh_file
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(in) :: file_path
            !! A dummy argument for this SUBROUTINE

            integer, intent(out) :: stat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: msg
            !! A dummy argument for this SUBROUTINE

        end subroutine

    end interface

end module



submodule (gmsh_reader_interface) gmsh_msh_file_implementation

    implicit none

    contains



    module procedure read_file

        integer :: read_unit
        !! A local variable for this SUBROUTINE

        open( &!
            newunit = read_unit   , &!
            action  = 'READ'      , &!
            file    = file_path   , &!
            form    = 'FORMATTED' , &!
            iostat  = stat        , &!
            iomsg   = msg         , &!
            status  = 'OLD'         &!
        )

        if (stat .ne. IOSTAT_OK) return



        close( &!
            unit   = read_unit , &!
            iostat = stat      , &!
            iomsg  = msg         &!
        )

    end procedure

end submodule
