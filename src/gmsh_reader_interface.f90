module gmsh_reader_interface

    use, intrinsic :: iso_fortran_env


    implicit none


    private
    public  :: gmsh_msh_file_t



    integer, parameter, private :: IOSTAT_OK = 0

    integer, parameter, private :: LEN_TEXT_LINE = 2048
    !! text line buffer length



    type, abstract :: data_section_t

        contains

        procedure, pass, private :: find_header_ascii

        procedure(is_header_ascii_abstract), nopass, private, deferred :: is_header_ascii

    end type



    type :: gmsh_msh_file_t

        character(LEN_TEXT_LINE), private :: text_line
        !! Read text line buffer

        contains

        procedure, pass, public :: read_file

    end type



    interface ! for `data_section_t`

        module pure elemental function is_header_ascii_abstract(text_line) result(is_header)

            character(len=LEN_TEXT_LINE), intent(in) :: text_line
            !! A dummy argument for this FUNCTION
            !! Read text line buffer

            logical :: is_header
            !! The return value of this FUNCTION

        end function



        module subroutine find_header_ascii(data_section, read_unit, text_line, iostat, iomsg)

            class(data_section_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: read_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            character(len=LEN_TEXT_LINE), intent(inout) :: text_line
            !! A dummy argument for this SUBROUTINE
            !! Read text line buffer

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine

    end interface



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



submodule (gmsh_reader_interface) data_section_implementation

    implicit none

    contains



    module procedure find_header_ascii

        do

            read( &!
                unit   = read_unit , &!
                fmt    = '(A)'     , &!
                iostat = iostat    , &!
                iomsg  = iomsg       &!
            ) &!
            text_line

            if (iostat .ne. IOSTAT_OK) then
                return
            else if ( data_section%is_header_ascii( trim( adjustl(text_line) ) ) ) then
                return
            else
                cycle
            end if

        end do

    end procedure

end submodule



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
