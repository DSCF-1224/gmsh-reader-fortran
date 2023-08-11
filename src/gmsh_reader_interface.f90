module gmsh_reader_interface

    use, intrinsic :: iso_fortran_env


    implicit none


    private
    public  :: gmsh_msh_file_t



    integer(INT32), parameter, private :: DEFAULT_MSH_FILE_MODE = -1

    integer(INT32), parameter, private :: DEFAULT_MAJOR_VERSION = -1
    integer(INT32), parameter, private :: DEFAULT_MINOR_VERSION = -1

    integer, parameter, private :: IOSTAT_OK = 0

    integer, parameter, private :: LEN_TEXT_LINE = 2048
    !! text line buffer length

    integer(INT32), parameter, private :: MSH_FILE_MODE_ASCII = 0
    !! $MeshFormat\file-type
    !! for ASCII mode

    integer(INT32), parameter, private :: MSH_FILE_MODE_BINARY = 1
    !! $MeshFormat\file-type
    !! for binary mode


    type :: mesh_version_t
    !! $MeshFormat\version
    !! written as ASCII `double`

        integer(INT32), private :: major = DEFAULT_MAJOR_VERSION
        integer(INT32), private :: minor = DEFAULT_MINOR_VERSION

        contains

        procedure, pass, public  :: get_major
        procedure, pass, public  :: get_minor
        procedure, pass, private :: reset_mesh_version
        procedure, pass, private :: setup_mesh_version

        generic, private :: reset => reset_mesh_version
        generic, private :: setup => setup_mesh_version

    end type



    type :: msh_file_mode_t
    !! $MeshFormat\file-type
    !! written as ASCII `int`
    !! 0 for ASCII mode
    !! 1 for binary mode

        integer(INT32), private :: value = DEFAULT_MSH_FILE_MODE

        contains
        
        procedure, pass, private :: get_file_mode_as_int32
        procedure, pass, private :: get_file_mode_as_str
        procedure, pass, private :: reset_msh_file_mode
        procedure, pass, private :: setup_msh_file_mode

        generic, private :: reset        => reset_msh_file_mode
        generic, private :: setup        => setup_msh_file_mode
        generic, public  :: get_as_int32 => get_file_mode_as_int32
        generic, public  :: get_as_str   => get_file_mode_as_str

    end type



    type, abstract :: data_section_t

        contains

        procedure, pass, private :: find_header_ascii

        procedure( is_header_ascii_abstract    ), nopass, private, deferred :: is_header_ascii
        procedure( read_section_ascii_abstract ),   pass, private, deferred :: read_section_ascii

    end type



    type, extends(data_section_t) :: mesh_format_t

        type(mesh_version_t), public :: version
        !! $MeshFormat\version
        !! written as ASCII `double`

        type(msh_file_mode_t), public :: file_type
        !! $MeshFormat\file-type
        !! written as ASCII `int`
        !! 0 for ASCII mode
        !! 1 for binary mode

        contains

        procedure, nopass, private :: is_header_ascii    => is_header_ascii_mesh_format
        procedure,   pass, private :: read_section_ascii => read_section_ascii_mesh_format

    end type



    type :: gmsh_msh_file_t

        integer, private :: stat
        !! Receive the `IOSTAT`/`STAT` value

        character(LEN_TEXT_LINE), private :: text_line
        !! Read text line buffer

        type(mesh_format_t), public :: mesh_format
        !! Store the data from `$MeshFormat` section

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



        module subroutine read_section_ascii_abstract(data_section, read_unit, text_line, stat, msg)

            class(data_section_t), intent(inout) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: read_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            character(len=LEN_TEXT_LINE), intent(inout) :: text_line
            !! A dummy argument for this SUBROUTINE
            !! Read text line buffer

            integer, intent(out) :: stat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: msg
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



    interface ! for `mesh_format_t`

        module pure elemental function is_header_ascii_mesh_format(text_line) result(is_header)

            character(len=LEN_TEXT_LINE), intent(in) :: text_line
            !! A dummy argument for this FUNCTION
            !! Read text line buffer

            logical :: is_header
            !! The return value of this FUNCTION

        end function



        module subroutine read_section_ascii_mesh_format(data_section, read_unit, text_line, stat, msg)

            class(mesh_format_t), intent(inout) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: read_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            character(len=LEN_TEXT_LINE), intent(inout) :: text_line
            !! A dummy argument for this SUBROUTINE
            !! Read text line buffer

            integer, intent(out) :: stat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: msg
            !! A dummy argument for this SUBROUTINE

        end subroutine

    end interface



    interface ! for `mesh_version_t`

        module pure elemental function get_major(mesh_version) result(major)

            class(mesh_version_t), intent(in) :: mesh_version
            !! A dummy argument for this FUNCTION

            integer(INT32) :: major
            !! The return value of this FUNCTION

        end function



        module pure elemental function get_minor(mesh_version) result(minor)

            class(mesh_version_t), intent(in) :: mesh_version
            !! A dummy argument for this FUNCTION

            integer(INT32) :: minor
            !! The return value of this FUNCTION

        end function



        module elemental subroutine reset_mesh_version(mesh_version)

            class(mesh_version_t), intent(inout) :: mesh_version
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module elemental subroutine setup_mesh_version(mesh_version, source, iostat, iomsg)

            class(mesh_version_t), intent(inout) :: mesh_version
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(in) :: source
            !! A dummy argument for this SUBROUTINE
            !! Read version data as a string

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine

    end interface



    interface ! for `msh_file_mode_t`

        module pure elemental function get_file_mode_as_int32(msh_file_mode) result(file_type)

            class(msh_file_mode_t), intent(in) :: msh_file_mode
            !! A dummy argument for this FUNCTION

            integer(INT32) :: file_type
            !! The return value of this FUNCTION

        end function



        module pure function get_file_mode_as_str(msh_file_mode) result(file_type)

            class(msh_file_mode_t), intent(in) :: msh_file_mode
            !! A dummy argument for this FUNCTION

            character(len=6), allocatable :: file_type
            !! The return value of this FUNCTION

        end function



        module subroutine reset_msh_file_mode(msh_file_mode)

            class(msh_file_mode_t), intent(inout) :: msh_file_mode
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine setup_msh_file_mode(msh_file_mode, source)

            class(msh_file_mode_t), intent(inout) :: msh_file_mode
            !! A dummy argument for this SUBROUTINE

            integer(INT32), intent(in) :: source
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
            else if ( data_section%is_header_ascii( adjustl(text_line) ) ) then
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



        call gmsh_msh_file%mesh_format%read_section_ascii( &!
            read_unit = read_unit               , &!
            text_line = gmsh_msh_file%text_line , &!
            stat      = stat                    , &!
            msg       = msg                       &!
        )

        if (stat .ne. IOSTAT_OK) return



        close( &!
            unit   = read_unit , &!
            iostat = stat      , &!
            iomsg  = msg         &!
        )

    end procedure

end submodule



submodule (gmsh_reader_interface) mesh_format_implementation

    implicit none

    contains



    module procedure is_header_ascii_mesh_format
        is_header = ( trim(text_line) .eq. '$MeshFormat')
    end procedure



    module procedure read_section_ascii_mesh_format

        integer(INT32) :: file_type
        !! A local variable for this PROCEDURE

        integer :: index_space
        !! A local variable for this PROCEDURE



        associate( mesh_format => data_section )

            call mesh_format%find_header_ascii( &!
                read_unit = read_unit , &!
                text_line = text_line , &!
                iostat    = stat      , &!
                iomsg     = msg         &!
            )

            if (stat .ne. IOSTAT_OK) then
                return
            end if



            read( &!
                unit   = read_unit , &!
                fmt    = '(A)'     , &!
                iostat = stat      , &!
                iomsg  = msg         &!
            ) &!
            text_line

            if (stat .ne. IOSTAT_OK) then
                return
            end if



            index_space = index(string=text_line, substring=' ')



            call mesh_format%version%setup( &!
                source = text_line(:index_space) , &!
                iostat = stat                    , &!
                iomsg  = msg                       &!
            )

            if (stat .ne. IOSTAT_OK) then
                return
            end if



            read( &!
                unit   = text_line(index_space:) , &!
                fmt    = *                       , &!
                iostat = stat                    , &!
                iomsg  = msg                       &!
            ) &!
            file_type

            if (stat .ne. IOSTAT_OK) then
                return
            end if

            call mesh_format%file_type%setup(file_type)

        end associate

    end procedure

end submodule



submodule (gmsh_reader_interface) mesh_version_implementation

    implicit none

    contains



    module procedure get_major
        major = mesh_version%major
    end procedure



    module procedure get_minor
        minor = mesh_version%minor
    end procedure



    module procedure reset_mesh_version
        mesh_version%major = DEFAULT_MAJOR_VERSION
        mesh_version%minor = DEFAULT_MINOR_VERSION
    end procedure



    module procedure setup_mesh_version

        integer :: index_period
        !! A local variable for this PROCEDURE


        call mesh_version%reset()

        index_period = index(string=source, substring='.')

        select case(index_period)

            case(0)

                read( &!
                    unit   = source(:) , &!
                    fmt    = *         , &!
                    iostat = iostat    , &!
                    iomsg  = iomsg       &!
                ) &!
                mesh_version%major

            case default

                read( &!
                    unit   = source( :(index_period - 1) ) , &!
                    fmt    = *                             , &!
                    iostat = iostat                        , &!
                    iomsg  = iomsg                           &!
                ) &!
                mesh_version%major

                if (iostat .ne. IOSTAT_OK) then
                    return
                end if

                read( &!
                    unit   = source( (index_period + 1): ) , &!
                    fmt    = *                             , &!
                    iostat = iostat                        , &!
                    iomsg  = iomsg                           &!
                ) &!
                mesh_version%minor

        end select

    end procedure

end submodule



submodule (gmsh_reader_interface) msh_file_mode_implementation

    implicit none

    contains



    module procedure get_file_mode_as_int32
        file_type = msh_file_mode%value
    end procedure



    module procedure get_file_mode_as_str

        select case ( msh_file_mode%value )
            case ( MSH_FILE_MODE_ASCII  ) ; file_type = 'ASCII '
            case ( MSH_FILE_MODE_BINARY ) ; file_type = 'binary'
            case default                  ; file_type = '#N/A'
        end select

    end procedure



    module procedure reset_msh_file_mode
        msh_file_mode%value = DEFAULT_MSH_FILE_MODE
    end procedure



    module procedure setup_msh_file_mode
        msh_file_mode%value = source
    end procedure

end submodule
