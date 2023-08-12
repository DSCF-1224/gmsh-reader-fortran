module gmsh_reader_interface

    use, intrinsic :: iso_fortran_env


    implicit none


    private
    public  :: gmsh_msh_file_t



    integer(INT32), parameter, private :: DEFAULT_DATA_SIZE = -1
    !! $MeshFormat\data-size
    !! written as ASCII `int`

    integer(INT32), parameter, private :: DEFAULT_MSH_FILE_MODE = -1
    !! $MeshFormat\file-type
    !! written as ASCII `int`
    !! 0 for ASCII mode
    !! 1 for binary mode

    integer(INT32), parameter, private :: DEFAULT_MAJOR_VERSION = -1
    !! $MeshFormat\version

    integer(INT32), parameter, private :: DEFAULT_MINOR_VERSION = -1
    !! $MeshFormat\version

    integer(INT32), parameter, private :: DEFAULT_NUM_PHYSICAL_NAMES = -1
    !! $PhysicalNames\numPhysicalNames

    integer, parameter, private :: IOSTAT_OK = 0
    
    integer, parameter, private :: LEN_PHYSICAL_NAME = 127
    !! $PhysicalNames\numPhysicalNames

    integer, parameter, private :: LEN_TEXT_LINE = 2048
    !! text line buffer length

    integer(INT32), parameter, private :: MSH_FILE_MODE_ASCII = 0
    !! $MeshFormat\file-type
    !! for ASCII mode

    integer(INT32), parameter, private :: MSH_FILE_MODE_BINARY = 1
    !! $MeshFormat\file-type
    !! for binary mode

    integer, parameter, private :: STAT_OK = 0


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



    type, extends(data_section_t), abstract :: allocatable_data_section_t

        contains

        procedure( allocate_field_abstract   ), pass, private, deferred :: allocate_field
        procedure( deallocate_field_abstract ), pass, private, deferred :: deallocate_field

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

        integer(INT32), private :: data_size = DEFAULT_DATA_SIZE
        !! $MeshFormat\data-size
        !! written as ASCII `int`

        contains

        procedure,   pass, public  :: get_data_size
        procedure, nopass, private :: is_header_ascii    => is_header_ascii_mesh_format
        procedure,   pass, private :: read_section_ascii => read_section_ascii_mesh_format

    end type



    type, extends(allocatable_data_section_t) :: physical_names_t

        integer(INT32), private :: num_physical_names = DEFAULT_NUM_PHYSICAL_NAMES
        !! $PhysicalNames\numPhysicalNames
        !! written as ASCII `int`

        integer(INT32), dimension(:), allocatable, private :: physical_dimension
        !! $PhysicalNames\dimension
        !! written as ASCII `int`

        integer(INT32), dimension(:), allocatable, private :: physical_tag
        !! $PhysicalNames\dimension
        !! written as ASCII `int`

        character(len=LEN_PHYSICAL_NAME), dimension(:), allocatable, private :: physical_name
        !! $PhysicalNames\physicalNames
        !! 127 characters max

        contains

        procedure,   pass, private :: allocate_field         => allocate_field_physical_names
        procedure,   pass, private :: deallocate_field       => deallocate_field_physical_names
        procedure,   pass, public  :: get_physical_dimension
        procedure,   pass, public  :: get_num_physical_name
        procedure,   pass, public  :: get_num_physical_names
        procedure,   pass, public  :: get_num_physical_tag
        procedure, nopass, private :: is_header_ascii        => is_header_ascii_physical_names
        procedure,   pass, private :: read_section_ascii     => read_section_ascii_physical_names

    end type



    type :: gmsh_msh_file_t

        integer, private :: stat
        !! Receive the `IOSTAT`/`STAT` value

        character(LEN_TEXT_LINE), private :: text_line
        !! Read text line buffer

        type(mesh_format_t), public :: mesh_format
        !! Store the data from `$MeshFormat` section

        type(physical_names_t), public :: physical_names
        !! Store the data from `$PhysicalNames` section

        contains

        procedure, pass, public :: read_file

    end type



    interface ! for `allocatable_data_section_t`

        module subroutine allocate_field_abstract(data_section, stat, errmsg)

            class(allocatable_data_section_t), intent(inout) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(out) :: stat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: errmsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine deallocate_field_abstract(data_section, stat, errmsg)

            class(allocatable_data_section_t), intent(inout) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(out) :: stat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: errmsg
            !! A dummy argument for this SUBROUTINE

        end subroutine

    end interface



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

        module pure elemental function get_data_size(mesh_format) result(data_size)

            class(mesh_format_t), intent(in) :: mesh_format
            !! A dummy argument for this FUNCTION

            integer(INT32) :: data_size
            !! The return value of this FUNCTION

        end function



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



    interface ! for `physical_names_t`

        module pure elemental function get_physical_dimension(physical_names, index) result(physical_dimension)

            class(physical_names_t), intent(in) :: physical_names
            !! A dummy argument for this FUNCTION

            integer(INT32), intent(in) :: index
            !! A dummy argument for this FUNCTION

            integer(INT32) :: physical_dimension
            !! The return value of this FUNCTION

        end function



        module pure elemental function get_num_physical_name(physical_names, index) result(physical_name)

            class(physical_names_t), intent(in) :: physical_names
            !! A dummy argument for this FUNCTION

            integer(INT32), intent(in) :: index
            !! A dummy argument for this FUNCTION

            character(len=LEN_PHYSICAL_NAME) :: physical_name
            !! The return value of this FUNCTION

        end function



        module pure elemental function get_num_physical_names(physical_names) result(num_physical_names)

            class(physical_names_t), intent(in) :: physical_names
            !! A dummy argument for this FUNCTION

            integer(INT32) :: num_physical_names
            !! The return value of this FUNCTION

        end function



        module pure elemental function get_num_physical_tag(physical_names, index) result(physical_tag)

            class(physical_names_t), intent(in) :: physical_names
            !! A dummy argument for this FUNCTION

            integer(INT32), intent(in) :: index
            !! A dummy argument for this FUNCTION

            integer(INT32) :: physical_tag
            !! The return value of this FUNCTION

        end function



        module pure elemental function is_header_ascii_physical_names(text_line) result(is_header)

            character(len=LEN_TEXT_LINE), intent(in) :: text_line
            !! A dummy argument for this FUNCTION
            !! Read text line buffer

            logical :: is_header
            !! The return value of this FUNCTION

        end function



        module subroutine allocate_field_physical_names(data_section, stat, errmsg)

            class(physical_names_t), intent(inout) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(out) :: stat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: errmsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine deallocate_field_physical_names(data_section, stat, errmsg)

            class(physical_names_t), intent(inout) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(out) :: stat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: errmsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine read_section_ascii_physical_names(data_section, read_unit, text_line, stat, msg)

            class(physical_names_t), intent(inout) :: data_section
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



        call gmsh_msh_file%physical_names%read_section_ascii( &!
            read_unit = read_unit               , &!
            text_line = gmsh_msh_file%text_line , &!
            stat      = stat                    , &!
            msg       = msg                       &!
        )

        select case (stat)
            case (IOSTAT_END) ; ! NOTHING TO DO HERE
            case default      ; return
        end select



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



    module procedure get_data_size
        data_size = mesh_format%data_size
    end procedure



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



            mesh_format%data_size = DEFAULT_DATA_SIZE

            read( &!
                unit   = text_line(index_space:) , &!
                fmt    = *                       , &!
                iostat = stat                    , &!
                iomsg  = msg                       &!
            ) &!
            file_type , &!
            mesh_format%data_size

            if (stat .ne. IOSTAT_OK) then
                return
            end if

            call mesh_format%file_type%reset()
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



submodule (gmsh_reader_interface) physical_names_implementation

    implicit none

    contains



    module procedure allocate_field_physical_names

        associate( physical_names => data_section )

            allocate( &!
                physical_names%physical_dimension(physical_names%num_physical_names), &!
                stat   = stat   , &!
                errmsg = errmsg   &!
            )
            
            if (stat .ne. STAT_OK) return



            allocate( &!
                physical_names%physical_tag(physical_names%num_physical_names), &!
                stat   = stat   , &!
                errmsg = errmsg   &!
            )
            
            if (stat .ne. STAT_OK) return



            allocate( &!
                physical_names%physical_name(physical_names%num_physical_names), &!
                stat   = stat   , &!
                errmsg = errmsg   &!
            )

        end associate

    end procedure



    module procedure deallocate_field_physical_names

        associate( physical_names => data_section )

            stat = STAT_OK



            if ( allocated(physical_names%physical_dimension) ) then

                allocate( &!
                    physical_names%physical_dimension(physical_names%num_physical_names), &!
                    stat   = stat   , &!
                    errmsg = errmsg   &!
                )
                
                if (stat .ne. STAT_OK) return

            end if



            if ( allocated(physical_names%physical_tag) ) then

                allocate( &!
                    physical_names%physical_tag(physical_names%num_physical_names), &!
                    stat   = stat   , &!
                    errmsg = errmsg   &!
                )
                
                if (stat .ne. STAT_OK) return

            end if



            if ( allocated(physical_names%physical_name) ) then

                allocate( &!
                    physical_names%physical_name(physical_names%num_physical_names), &!
                    stat   = stat   , &!
                    errmsg = errmsg   &!
                )

            end if

        end associate

    end procedure



    module procedure get_physical_dimension
        physical_dimension = physical_names%physical_dimension(index)
    end procedure



    module procedure get_num_physical_name
        physical_name = physical_names%physical_name(index)
    end procedure



    module procedure get_num_physical_names
        num_physical_names = physical_names%num_physical_names
    end procedure



    module procedure get_num_physical_tag
        physical_tag = physical_names%physical_tag(index)
    end procedure



    module procedure is_header_ascii_physical_names
        is_header = ( trim(text_line) .eq. '$PhysicalNames')
    end procedure



    module procedure read_section_ascii_physical_names

        integer(INT32) :: iter_item
        !! A local support variable for this PROCEDURE



        associate( physical_names => data_section )

            rewind( &!
                unit   = read_unit , &!
                iostat = stat      , &!
                iomsg  = msg         &!
            )

            if (stat .ne. IOSTAT_OK) then
                return
            end if



            call physical_names%find_header_ascii( &!
                read_unit = read_unit , &!
                text_line = text_line , &!
                iostat    = stat      , &!
                iomsg     = msg         &!
            )

            if (stat .ne. IOSTAT_OK) then

                return

            else if (stat .eq. IOSTAT_END) then

                physical_names%num_physical_names = 0

                call physical_names%deallocate_field(stat, msg)

                return

            end if



            call physical_names%deallocate_field(stat, msg)



            read( &!
                unit   = read_unit , &!
                fmt    = *         , &!
                iostat = stat      , &!
                iomsg  = msg         &!
            ) &!
            physical_names%num_physical_names

            if (stat .ne. IOSTAT_OK) then
                return
            end if



            call physical_names%allocate_field(stat, msg)



            do iter_item = 1, physical_names%get_num_physical_names()

                read( &!
                    unit   = read_unit , &!
                    fmt    = *         , &!
                    iostat = stat      , &!
                    iomsg  = msg         &!
                ) &!
                physical_names%physical_dimension (iter_item) , &!
                physical_names%physical_tag       (iter_item) , &!
                physical_names%physical_name      (iter_item)

                if (stat .ne. IOSTAT_OK) then
                    return
                end if

            end do

        end associate

    end procedure

end submodule
