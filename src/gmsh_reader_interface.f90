module gmsh_reader_interface

    use, intrinsic :: iso_fortran_env


    implicit none


    private
    public  :: gmsh_msh2_file_t



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

    integer(INT32), parameter, private :: DEFAULT_NODE_NUMBER = -1
    !! (version 2) $Nodes\node-number
    !! `node-number` is the number (index) of the n-th node in the mesh;
    !! `node-number` must be a positive (non-zero) integer.

    integer(INT32), parameter, private :: DEFAULT_NUM_NODES = -1
    !! (version 2)
    !! $Nodes\number-of-nodes
    !! the number of nodes in the mesh
    !! 
    !! (version 4)
    !! $Nodes\numNodes

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



    real(REAL64), parameter, private :: QUIET_NAN = transfer(source=-1_REAL64, mold=0.0_REAL64)



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
        
        procedure, nopass, private :: get_ascii_file_mode_as_int32
        procedure, nopass, private :: get_binary_file_mode_as_int32
        procedure,   pass, private :: get_file_mode_as_int32
        procedure,   pass, private :: get_file_mode_as_str
        procedure,   pass, private :: reset_msh_file_mode
        procedure,   pass, private :: setup_msh_file_mode

        generic, private :: reset                    => reset_msh_file_mode
        generic, private :: setup                    => setup_msh_file_mode
        generic, public  :: get_as_int32             => get_file_mode_as_int32
        generic, private :: get_ascii_mode_as_int32  => get_ascii_file_mode_as_int32
        generic, private :: get_binary_mode_as_int32 => get_binary_file_mode_as_int32
        generic, public  :: get_as_str               => get_file_mode_as_str

    end type



    type, abstract :: data_section_t

        contains

        procedure,   pass, private :: find_header_ascii
        procedure, nopass, private :: write_section_header_ascii_core
        procedure,   pass, private :: write_section_ascii

        procedure( is_header_ascii_abstract            ), nopass, deferred, private :: is_header_ascii
        procedure( read_section_ascii_abstract         ),   pass, deferred, private :: read_section_ascii
        procedure( write_section_footer_ascii_abstract ),   pass, deferred, private :: write_section_footer_ascii
        procedure( write_section_header_ascii_abstract ),   pass, deferred, private :: write_section_header_ascii
        procedure( write_section_main_ascii_abstract   ),   pass, deferred, private :: write_section_main_ascii

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
        procedure, nopass, private :: is_header_ascii            => is_header_ascii_mesh_format
        procedure,   pass, private :: read_section_ascii         => read_section_ascii_mesh_format
        procedure,   pass, private :: write_section_footer_ascii => write_section_footer_ascii_mesh_format
        procedure,   pass, private :: write_section_header_ascii => write_section_header_ascii_mesh_format
        procedure,   pass, private :: write_section_main_ascii   => write_section_main_ascii_mesh_format

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

        procedure,   pass, private :: allocate_field             => allocate_field_physical_names
        procedure,   pass, private :: deallocate_field           => deallocate_field_physical_names
        procedure,   pass, public  :: get_physical_dimension
        procedure,   pass, public  :: get_num_physical_name
        procedure,   pass, public  :: get_num_physical_names
        procedure,   pass, public  :: get_num_physical_tag
        procedure, nopass, private :: is_header_ascii            => is_header_ascii_physical_names
        procedure,   pass, private :: read_section_ascii         => read_section_ascii_physical_names
        procedure,   pass, private :: write_section_footer_ascii => write_section_footer_ascii_physical_names
        procedure,   pass, private :: write_section_header_ascii => write_section_header_ascii_physical_names
        procedure,   pass, private :: write_section_main_ascii   => write_section_main_ascii_physical_names

    end type



    type, abstract :: gmsh_msh_file_t

        integer, private :: stat
        !! Receive the `IOSTAT`/`STAT` value

        character(LEN_TEXT_LINE), private :: text_line
        !! Read text line buffer

        type(mesh_format_t), public :: mesh_format
        !! Store the data from `$MeshFormat` section

        type(physical_names_t), public :: physical_names
        !! Store the data from `$PhysicalNames` section

        contains

        procedure, pass, public  :: read_file
        procedure, pass, private :: read_file_fore
        procedure, pass, public  :: write_file_ascii
        procedure, pass, private :: write_file_fore_ascii

        procedure( read_file_rear_ascii_abstract  ), pass, deferred, private :: read_file_rear_ascii
        procedure( write_file_rear_ascii_abstract ), pass, deferred, private :: write_file_rear_ascii

    end type



    type :: coordinate_t

        real(REAL64), private :: x = QUIET_NAN
        !! (version 2) $Nodes\x-coord
        !! (version 4) $Nodes\x

        real(REAL64), private :: y = QUIET_NAN
        !! (version 2) $Nodes\y-coord
        !! (version 4) $Nodes\y

        real(REAL64), private :: z = QUIET_NAN
        !! (version 2) $Nodes\z-coord
        !! (version 4) $Nodes\z

        contains

        procedure, pass, private :: get_coordinate_x
        procedure, pass, private :: get_coordinate_y
        procedure, pass, private :: get_coordinate_z
        procedure, pass, private :: reset_coordinate

        generic, public :: get_x => get_coordinate_x
        generic, public :: get_y => get_coordinate_y
        generic, public :: get_z => get_coordinate_z
        generic, public :: reset => reset_coordinate

    end type



    type(coordinate_t), parameter, private :: DEFAULT_COORDINATE = coordinate_t(QUIET_NAN, QUIET_NAN, QUIET_NAN)



    type :: node_version2_t

        integer(INT32), private :: node_number = DEFAULT_NODE_NUMBER
        !! (version 2) $Nodes\node-number
        !! `node-number` is the number (index) of the n-th node in the mesh;
        !! `node-number` must be a positive (non-zero) integer.
        !! Note that the node-numbers do not necessarily have to form a dense nor an ordered sequence.

        type(coordinate_t), public :: coord = DEFAULT_COORDINATE
        !! (version 2) $Nodes\x-coord
        !! (version 2) $Nodes\y-coord
        !! (version 2) $Nodes\z-coord
        !! the floating point values giving the X, Y and Z coordinates of the n-th node

        contains

        procedure, pass, public  :: get_node_number
        procedure, pass, private :: reset_node_version2

        generic, private :: reset => reset_node_version2

    end type



    type, extends(allocatable_data_section_t), abstract :: nodes_abstract_t

        integer(INT32), private :: num_nodes
        !! (version 2)
        !! $Nodes\number-of-nodes
        !! the number of nodes in the mesh
        !! 
        !! (version 4)
        !! $Nodes\numNodes

        contains

        procedure,   pass, private :: check_num_nodes_nodes_abstract
        procedure,   pass, private :: get_num_nodes_nodes_abstract
        procedure, nopass, private :: is_header_ascii                => is_header_ascii_nodes_abstract
        procedure,   pass, private :: write_section_footer_ascii     => write_section_footer_ascii_nodes_abstract
        procedure,   pass, private :: write_section_header_ascii     => write_section_header_ascii_nodes_abstract

        generic, public :: check_num_nodes => check_num_nodes_nodes_abstract
        generic, public :: get_num_nodes   => get_num_nodes_nodes_abstract

    end type



    type, extends(nodes_abstract_t) :: nodes_version2_t

        type(node_version2_t), dimension(:), allocatable :: node
        !! (version 2) $Nodes\node-number
        !! (version 2) $Nodes\x-coord
        !! (version 2) $Nodes\y-coord
        !! (version 2) $Nodes\z-coord

        contains

        procedure, pass, private :: allocate_field           => allocate_field_nodes_version2
        procedure, pass, private :: deallocate_field         => deallocate_field_nodes_version2
        procedure, pass, private :: read_section_ascii       => read_section_ascii_nodes_version2
        procedure, pass, private :: write_section_main_ascii => write_section_main_ascii_nodes_version2

    end type



    type, extends(gmsh_msh_file_t) :: gmsh_msh2_file_t

        type(nodes_version2_t), public :: nodes
        !! Store the data from `$Nodes` section

        contains

        procedure, pass, private :: read_file_rear_ascii  => read_msh2_file_rear_ascii
        procedure, pass, private :: write_file_rear_ascii => write_msh2_file_rear_ascii

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



    interface ! for `coordinate_t`

        module pure elemental function get_coordinate_x(coordinate) result(x)

            class(coordinate_t), intent(in) :: coordinate
            !! A dummy argument for this FUNCTION

            real(REAL64) :: x
            !! The return value of this FUNCTION

        end function



        module pure elemental function get_coordinate_y(coordinate) result(y)

            class(coordinate_t), intent(in) :: coordinate
            !! A dummy argument for this FUNCTION

            real(REAL64) :: y
            !! The return value of this FUNCTION

        end function



        module pure elemental function get_coordinate_z(coordinate) result(z)

            class(coordinate_t), intent(in) :: coordinate
            !! A dummy argument for this FUNCTION

            real(REAL64) :: z
            !! The return value of this FUNCTION

        end function



        module elemental subroutine reset_coordinate(coordinate)

            class(coordinate_t), intent(inout) :: coordinate
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



        module subroutine write_section_ascii(data_section, write_unit, iostat, iomsg)

            class(data_section_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine write_section_footer_ascii_abstract(data_section, write_unit, iostat, iomsg)

            class(data_section_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine write_section_header_ascii_abstract(data_section, write_unit, iostat, iomsg)

            class(data_section_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine write_section_header_ascii_core(write_unit, header, iostat, iomsg)

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            character(len=*), intent(in) :: header
            !! A dummy argument for this SUBROUTINE
            !! The header (string) to write

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine write_section_main_ascii_abstract(data_section, write_unit, iostat, iomsg)

            class(data_section_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

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



        module subroutine read_file_fore(gmsh_msh_file, file_path, read_unit, stat, msg)

            class(gmsh_msh_file_t), intent(inout) :: gmsh_msh_file
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(in) :: file_path
            !! A dummy argument for this SUBROUTINE

            integer, intent(out) :: read_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: stat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: msg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine read_file_rear_ascii_abstract(gmsh_msh_file, read_unit, stat, msg)

            class(gmsh_msh_file_t), intent(inout) :: gmsh_msh_file
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: read_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: stat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: msg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine write_file_ascii(gmsh_msh_file, file_path, iostat, iomsg)

            class(gmsh_msh_file_t), intent(in) :: gmsh_msh_file
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(in) :: file_path
            !! A dummy argument for this SUBROUTINE
            !! The path of the target file to write

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine write_file_fore_ascii(gmsh_msh_file, write_unit, iostat, iomsg)

            class(gmsh_msh_file_t), intent(in) :: gmsh_msh_file
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to write the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine write_file_rear_ascii_abstract(gmsh_msh_file, write_unit, iostat, iomsg)

            class(gmsh_msh_file_t), intent(in) :: gmsh_msh_file
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to write the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine

    end interface



    interface ! for `gmsh_msh2_file_t

        module subroutine read_msh2_file_rear_ascii(gmsh_msh_file, read_unit, stat, msg)

            class(gmsh_msh2_file_t), intent(inout) :: gmsh_msh_file
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: read_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: stat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: msg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine write_msh2_file_rear_ascii(gmsh_msh_file, write_unit, iostat, iomsg)

            class(gmsh_msh2_file_t), intent(in) :: gmsh_msh_file
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to write the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
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



        module subroutine write_section_footer_ascii_mesh_format(data_section, write_unit, iostat, iomsg)

            class(mesh_format_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine write_section_header_ascii_mesh_format(data_section, write_unit, iostat, iomsg)

            class(mesh_format_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine write_section_main_ascii_mesh_format(data_section, write_unit, iostat, iomsg)

            class(mesh_format_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
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

        module pure elemental function get_ascii_file_mode_as_int32() result(file_type)

            integer(INT32) :: file_type
            !! The return value of this FUNCTION

        end function



        module pure elemental function get_binary_file_mode_as_int32() result(file_type)

            integer(INT32) :: file_type
            !! The return value of this FUNCTION

        end function



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



    interface ! `for `node_version2_t`

        module pure elemental function get_node_number(node) result(node_number)

            class(node_version2_t), intent(in) :: node
            !! A dummy argument for this FUNCTION

            integer(INT32) :: node_number
            !! The return value of this FUNCTION

        end function


        module elemental subroutine reset_node_version2(node)

            class(node_version2_t), intent(inout) :: node
            !! A dummy argument for this SUBROUTINE

        end subroutine

    end interface



    interface ! `for `nodes_abstract_t`

        module elemental subroutine check_num_nodes_nodes_abstract(nodes, flag_termination)

            class(nodes_abstract_t), intent(in) :: nodes
            !! A dummy argument for this SUBROUTINE

            logical, intent(out) :: flag_termination
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module pure elemental function get_num_nodes_nodes_abstract(nodes) result(num_nodes)

            class(nodes_abstract_t), intent(in) :: nodes
            !! A dummy argument for this FUNCTION

            integer(INT32) :: num_nodes
            !! The return value of this FUNCTION

        end function



        module pure elemental function is_header_ascii_nodes_abstract(text_line) result(is_header)

            character(len=LEN_TEXT_LINE), intent(in) :: text_line
            !! A dummy argument for this FUNCTION
            !! Read text line buffer

            logical :: is_header
            !! The return value of this FUNCTION

        end function



        module subroutine write_section_header_ascii_nodes_abstract(data_section, write_unit, iostat, iomsg)

            class(nodes_abstract_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine write_section_footer_ascii_nodes_abstract(data_section, write_unit, iostat, iomsg)

            class(nodes_abstract_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine write_section_main_ascii_nodes_abstract(data_section, write_unit, iostat, iomsg)

            class(nodes_abstract_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine

    end interface



    interface ! `for `nodes_version2_t`

        module subroutine allocate_field_nodes_version2(data_section, stat, errmsg)

            class(nodes_version2_t), intent(inout) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(out) :: stat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: errmsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine deallocate_field_nodes_version2(data_section, stat, errmsg)

            class(nodes_version2_t), intent(inout) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(out) :: stat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: errmsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine read_section_ascii_nodes_version2(data_section, read_unit, text_line, stat, msg)

            class(nodes_version2_t), intent(inout) :: data_section
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



        module subroutine write_section_main_ascii_nodes_version2(data_section, write_unit, iostat, iomsg)

            class(nodes_version2_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
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



        module subroutine write_section_header_ascii_physical_names(data_section, write_unit, iostat, iomsg)

            class(physical_names_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine write_section_footer_ascii_physical_names(data_section, write_unit, iostat, iomsg)

            class(physical_names_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine



        module subroutine write_section_main_ascii_physical_names(data_section, write_unit, iostat, iomsg)

            class(physical_names_t), intent(in) :: data_section
            !! A dummy argument for this SUBROUTINE

            integer, intent(in) :: write_unit
            !! A dummy argument for this SUBROUTINE
            !! The device number to read the target file

            integer, intent(out) :: iostat
            !! A dummy argument for this SUBROUTINE

            character(len=*), intent(inout) :: iomsg
            !! A dummy argument for this SUBROUTINE

        end subroutine

    end interface

end module



submodule (gmsh_reader_interface) coordinate_implementation

    implicit none

    contains



    module procedure get_coordinate_x; x = coordinate%x; end procedure
    module procedure get_coordinate_y; y = coordinate%y; end procedure
    module procedure get_coordinate_z; z = coordinate%z; end procedure



    module procedure reset_coordinate
        coordinate%x = QUIET_NAN
        coordinate%y = QUIET_NAN
        coordinate%z = QUIET_NAN
    end procedure

end submodule



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



    module procedure write_section_header_ascii_core

        write( &!
            unit   = write_unit , &!
            fmt    = '(A)'      , &!
            iostat = iostat     , &!
            iomsg  = iomsg        &!
        ) &!
        header

    end procedure



    module procedure write_section_ascii

        call data_section%write_section_header_ascii( &!
            write_unit = write_unit , &!
            iostat     = iostat     , &!
            iomsg      = iomsg        &!
        )

        if (iostat .ne. IOSTAT_OK) return



        call data_section%write_section_main_ascii( &!
            write_unit = write_unit , &!
            iostat     = iostat     , &!
            iomsg      = iomsg        &!
        )

        if (iostat .ne. IOSTAT_OK) return



        call data_section%write_section_footer_ascii( &!
            write_unit = write_unit , &!
            iostat     = iostat     , &!
            iomsg      = iomsg        &!
        )

    end procedure

end submodule



submodule (gmsh_reader_interface) gmsh_msh_file_implementation

    implicit none

    contains



    module procedure read_file

        integer :: read_unit
        !! A local variable for this SUBROUTINE
        !! The device number to read the target file



        call gmsh_msh_file%read_file_fore( &!
            file_path = file_path , &!
            read_unit = read_unit , &!
            stat      = stat      , &!
            msg       = msg         &!
        )

        select case (stat)
            case ( IOSTAT_OK  ) ; ! NOTHING TO DO HERE
            case ( IOSTAT_END ) ; ! NOTHING TO DO HERE
            case default        ; return
        end select



        call gmsh_msh_file%read_file_rear_ascii( &!
            read_unit = read_unit , &!
            stat      = stat      , &!
            msg       = msg         &!
        )

        select case (stat)
            case (IOSTAT_OK) ; ! NOTHING TO DO HERE
            case default     ; return
        end select



        close( &!
            unit   = read_unit , &!
            iostat = stat      , &!
            iomsg  = msg         &!
        )

    end procedure



    module procedure read_file_fore

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

    end procedure



    module procedure write_file_ascii

        integer :: write_unit
        !! A local variable for this SUBROUTINE
        !! The device number to write the target file



        open( &!
            newunit = write_unit  , &!
            action  = 'WRITE'     , &!
            file    = file_path   , &!
            form    = 'FORMATTED' , &!
            iostat  = iostat      , &!
            iomsg   = iomsg       , &!
            status  = 'UNKNOWN'     &!
        )

        if (iostat .ne. IOSTAT_OK) return



        call gmsh_msh_file%write_file_fore_ascii( &!
            write_unit = write_unit , &!
            iostat     = iostat     , &!
            iomsg      = iomsg        &!
        )

        if (iostat .ne. IOSTAT_OK) return



        call gmsh_msh_file%write_file_rear_ascii( &!
            write_unit = write_unit , &!
            iostat     = iostat     , &!
            iomsg      = iomsg        &!
        )

        if (iostat .ne. IOSTAT_OK) return



        close( &!
            unit   = write_unit , &!
            iostat = iostat     , &!
            iomsg  = iomsg        &!
        )

    end procedure



    module procedure write_file_fore_ascii

        call gmsh_msh_file%mesh_format%write_section_ascii( &!
            write_unit = write_unit , &!
            iostat     = iostat     , &!
            iomsg      = iomsg        &!
        )

        if     (iostat .ne. IOSTAT_OK) then ; return
        else                                ; flush(write_unit)
        end if



        call gmsh_msh_file%physical_names%write_section_ascii( &!
            write_unit = write_unit , &!
            iostat     = iostat     , &!
            iomsg      = iomsg        &!
        )

    end procedure

end submodule



submodule (gmsh_reader_interface) gmsh_msh2_file_implementation

    implicit none

    contains



    module procedure read_msh2_file_rear_ascii

        call gmsh_msh_file%nodes%read_section_ascii( &!
            read_unit = read_unit               , &!
            text_line = gmsh_msh_file%text_line , &!
            stat      = stat                    , &!
            msg       = msg                       &!
        )

    end procedure



    module procedure write_msh2_file_rear_ascii

        call gmsh_msh_file%nodes%write_section_ascii( &!
            write_unit = write_unit , &!
            iostat     = iostat     , &!
            iomsg      = iomsg        &!
        )

    end procedure

end submodule



submodule (gmsh_reader_interface) mesh_format_implementation

    implicit none

    character(len=*), parameter :: STR_FOOTER = '$EndMeshFormat'
    character(len=*), parameter :: STR_HEADER = '$MeshFormat'

    contains



    module procedure get_data_size
        data_size = mesh_format%data_size
    end procedure



    module procedure is_header_ascii_mesh_format
        is_header = ( trim(text_line) .eq. STR_HEADER )
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



    module procedure write_section_footer_ascii_mesh_format

        call data_section%write_section_header_ascii_core( &!
            write_unit = write_unit , &!
            header     = STR_FOOTER , &!
            iostat     = iostat     , &!
            iomsg      = iomsg        &!
        )

    end procedure



    module procedure write_section_header_ascii_mesh_format

        call data_section%write_section_header_ascii_core( &!
            write_unit = write_unit , &!
            header     = STR_HEADER , &!
            iostat     = iostat     , &!
            iomsg      = iomsg        &!
        )

    end procedure



    module procedure write_section_main_ascii_mesh_format

        associate( mesh_format => data_section )

            write( &!
                unit   = write_unit             , &!
                fmt    = '(I0,".",I0,2(1X,I0))' , &!
                iostat = iostat                 , &!
                iomsg  = iomsg                    &!
            ) &!
            mesh_format%version%get_major()                 , &!
            mesh_format%version%get_minor()                 , &!
            mesh_format%file_type%get_ascii_mode_as_int32() , &!
            mesh_format%get_data_size()

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



    module procedure get_ascii_file_mode_as_int32
        file_type = MSH_FILE_MODE_ASCII
    end procedure



    module procedure get_binary_file_mode_as_int32
        file_type = MSH_FILE_MODE_BINARY
    end procedure



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



submodule (gmsh_reader_interface) node_version2_implementation

    implicit none

    contains



    module procedure get_node_number
        node_number = node%node_number
    end procedure



    module procedure reset_node_version2
        node%node_number = DEFAULT_NODE_NUMBER
        call node%coord%reset()
    end procedure

end submodule



submodule (gmsh_reader_interface) nodes_abstract_implementation

    implicit none

    character(len=*), parameter :: STR_FOOTER = '$EndNodes'
    character(len=*), parameter :: STR_HEADER = '$Nodes'

    contains



    module procedure check_num_nodes_nodes_abstract

        flag_termination =(nodes%get_num_nodes() .lt. 1_INT32)

    end procedure



    module procedure get_num_nodes_nodes_abstract
        num_nodes = nodes%num_nodes
    end procedure



    module procedure is_header_ascii_nodes_abstract
        is_header = ( trim(text_line) .eq. STR_HEADER )
    end procedure



    module procedure write_section_footer_ascii_nodes_abstract

        logical :: flag_termination
        !! A local variable for this SUBROUTINE



        associate( nodes => data_section )

            call nodes%check_num_nodes(flag_termination)
            if (flag_termination) return



            call nodes%write_section_header_ascii_core( &!
                write_unit = write_unit , &!
                header     = STR_FOOTER , &!
                iostat     = iostat     , &!
                iomsg      = iomsg        &!
            )

        end associate

    end procedure



    module procedure write_section_header_ascii_nodes_abstract

        logical :: flag_termination
        !! A local variable for this SUBROUTINE



        associate( nodes => data_section )

            call nodes%check_num_nodes(flag_termination)
            if (flag_termination) return



            call nodes%write_section_header_ascii_core( &!
                write_unit = write_unit , &!
                header     = STR_HEADER , &!
                iostat     = iostat     , &!
                iomsg      = iomsg        &!
            )

        end associate

    end procedure

end submodule



submodule (gmsh_reader_interface) nodes_version2_implementation

    implicit none

    contains



    module procedure allocate_field_nodes_version2

        associate( nodes => data_section )

            allocate( &!
                nodes%node( nodes%get_num_nodes() ) , &!
                stat   = stat   , &!
                errmsg = errmsg , &!
                mold   = node_version2_t(DEFAULT_NUM_NODES, DEFAULT_COORDINATE) &!
            )

        end associate

    end procedure



    module procedure deallocate_field_nodes_version2

        associate( nodes => data_section )

            stat = STAT_OK



            if ( allocated(nodes%node) ) then

                deallocate( &!
                    nodes%node      , &!
                    stat   = stat   , &!
                    errmsg = errmsg   &!
                )
                
                if (stat .ne. STAT_OK) return

            end if

        end associate

    end procedure



    module procedure read_section_ascii_nodes_version2

        integer(INT32) :: iter_item
        !! A local support variable for this PROCEDURE



        associate( nodes => data_section )

            rewind( &!
                unit   = read_unit , &!
                iostat = stat      , &!
                iomsg  = msg         &!
            )

            if (stat .ne. IOSTAT_OK) then
                return
            end if



            call nodes%find_header_ascii( &!
                read_unit = read_unit , &!
                text_line = text_line , &!
                iostat    = stat      , &!
                iomsg     = msg         &!
            )

            if (stat .ne. IOSTAT_OK) then

                return

            else if (stat .eq. IOSTAT_END) then

                nodes%num_nodes = 0

                call nodes%deallocate_field(stat, msg)

                return

            end if



            call nodes%deallocate_field(stat, msg)



            read( &!
                unit   = read_unit , &!
                fmt    = *         , &!
                iostat = stat      , &!
                iomsg  = msg         &!
            ) &!
            nodes%num_nodes

            if (stat .ne. IOSTAT_OK) then
                return
            end if



            call nodes%allocate_field(stat, msg)



            do iter_item = 1, nodes%get_num_nodes()

                associate( target_node => nodes%node(iter_item) )

                    read( &!
                        unit   = read_unit , &!
                        fmt    = *         , &!
                        iostat = stat      , &!
                        iomsg  = msg         &!
                    ) &!
                    target_node%node_number , &!
                    target_node%coord%x     , &!
                    target_node%coord%y     , &!
                    target_node%coord%z

                    if (stat .ne. IOSTAT_OK) then
                        return
                    end if

                end associate

            end do

        end associate

    end procedure



    module procedure write_section_main_ascii_nodes_version2

        logical :: flag_termination
        !! A local variable for this SUBROUTINE

        integer(INT32) :: iter_item
        !! A local support variable for this PROCEDURE



        associate( nodes => data_section )

            call nodes%check_num_nodes(flag_termination)
            if (flag_termination) return



            write( &!
                unit   = write_unit , &!
                fmt    = '(I0)'     , &!
                iostat = iostat     , &!
                iomsg  = iomsg        &!
            ) &!
            nodes%get_num_nodes()

            if (iostat .ne. IOSTAT_OK) return


            do iter_item = 1, nodes%get_num_nodes()

                associate( target_node => nodes%node(iter_item) )

                    !!@todo reproduces the output of the C `printf' function with the format specifier "%g"
                    write( &!
                        unit   = write_unit   , &!
                        fmt    = '(I0,3(1X,G0))' , &!
                        iostat = iostat     , &!
                        iomsg  = iomsg        &!
                    ) &!
                    target_node%get_node_number() , &!
                    target_node%coord%get_x()     , &!
                    target_node%coord%get_y()     , &!
                    target_node%coord%get_z()

                    if (iostat .ne. IOSTAT_OK) return

                end associate

            end do

        end associate

    end procedure

end submodule



submodule (gmsh_reader_interface) physical_names_implementation

    implicit none

    character(len=*), parameter :: STR_FOOTER = '$EndPhysicalNames'
    character(len=*), parameter :: STR_HEADER = '$PhysicalNames'

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

                deallocate( &!
                    physical_names%physical_dimension , &!
                    stat   = stat                     , &!
                    errmsg = errmsg                     &!
                )
                
                if (stat .ne. STAT_OK) return

            end if



            if ( allocated(physical_names%physical_tag) ) then

                deallocate( &!
                    physical_names%physical_tag , &!
                    stat   = stat               , &!
                    errmsg = errmsg               &!
                )
                
                if (stat .ne. STAT_OK) return

            end if



            if ( allocated(physical_names%physical_name) ) then

                deallocate( &!
                    physical_names%physical_name , &!
                    stat   = stat                , &!
                    errmsg = errmsg                &!
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
        is_header = ( trim(text_line) .eq. STR_HEADER )
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



    module procedure write_section_footer_ascii_physical_names

        logical :: flag_termination
        !! A local variable for this SUBROUTINE



        associate( physical_names => data_section )

            call check_num_physical_names( &!
                num_physical_names = physical_names%get_num_physical_names() , &!
                flag_termination   = flag_termination                          &!
            )

            if (flag_termination) return



            call physical_names%write_section_header_ascii_core( &!
                write_unit = write_unit , &!
                header     = STR_FOOTER , &!
                iostat     = iostat     , &!
                iomsg      = iomsg        &!
            )

        end associate

    end procedure



    module procedure write_section_header_ascii_physical_names

        logical :: flag_termination
        !! A local variable for this SUBROUTINE



        associate( physical_names => data_section )

            call check_num_physical_names( &!
                num_physical_names = physical_names%get_num_physical_names() , &!
                flag_termination   = flag_termination                          &!
            )

            if (flag_termination) return



            call physical_names%write_section_header_ascii_core( &!
                write_unit = write_unit , &!
                header     = STR_HEADER , &!
                iostat     = iostat     , &!
                iomsg      = iomsg        &!
            )

        end associate

    end procedure



    module procedure write_section_main_ascii_physical_names

        logical :: flag_termination
        !! A local variable for this SUBROUTINE

        integer(INT32) :: iter_item
        !! A local support variable for this PROCEDURE



        associate( physical_names => data_section )

            call check_num_physical_names( &!
                num_physical_names = physical_names%get_num_physical_names() , &!
                flag_termination   = flag_termination                          &!
            )

            if (flag_termination) return



            write( &!
                unit   = write_unit , &!
                fmt    = '(I0)'     , &!
                iostat = iostat     , &!
                iomsg  = iomsg        &!
            ) &!
            physical_names%get_num_physical_names()

            if (iostat .ne. IOSTAT_OK) return



            do iter_item = 1, physical_names%get_num_physical_names()

                write( &!
                    unit   = write_unit               , &!
                    fmt    = '(2(I0,1X),"""",A,"""")' , &!
                    iostat = iostat                   , &!
                    iomsg  = iomsg                      &!
                ) &!
                &     physical_names%get_physical_dimension (iter_item)   , &!
                &     physical_names%get_num_physical_tag   (iter_item)   , &!
                trim( physical_names%get_num_physical_name  (iter_item) )

                if (iostat .ne. IOSTAT_OK) return

            end do

        end associate

    end procedure



    subroutine check_num_physical_names(num_physical_names, flag_termination)

        integer(INT32), intent(in) :: num_physical_names
        !! A dummy argument for this SUBROUTINE

        logical, intent(out) :: flag_termination
        !! A dummy argument for this SUBROUTINE



        flag_termination =(num_physical_names .lt. 1_INT32)

    end subroutine

end submodule
