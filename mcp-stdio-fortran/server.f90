module mcpc_interface
    use iso_c_binding
    implicit none

    ! Constants
    integer(c_int), parameter :: MCPC_U8STR = 9

    ! Callback type
    abstract interface
        subroutine mcpc_tcallcb_t(tool, ucbr) bind(c)
            import :: c_ptr
            type(c_ptr), value :: tool
            type(c_ptr), value :: ucbr
        end subroutine mcpc_tcallcb_t
    end interface

    interface
        ! C Helpers
        function get_stdin() bind(c, name="get_stdin")
            import :: c_ptr
            type(c_ptr) :: get_stdin
        end function get_stdin

        function get_stdout() bind(c, name="get_stdout")
            import :: c_ptr
            type(c_ptr) :: get_stdout
        end function get_stdout

        subroutine set_stdout_unbuffered() bind(c, name="set_stdout_unbuffered")
        end subroutine set_stdout_unbuffered
        
        subroutine helper_add_text_result(ucbr, text) bind(c, name="helper_add_text_result")
            import :: c_ptr, c_char
            type(c_ptr), value :: ucbr
            character(kind=c_char), dimension(*), intent(in) :: text
        end subroutine helper_add_text_result

        subroutine log_info_c(msg) bind(c, name="log_info_c")
            import :: c_char
            character(kind=c_char), dimension(*), intent(in) :: msg
        end subroutine log_info_c

        ! MCPC Functions
        function mcpc_server_new_iostrm(strm_in, strm_out) bind(c, name="mcpc_server_new_iostrm")
            import :: c_ptr
            type(c_ptr), value :: strm_in, strm_out
            type(c_ptr) :: mcpc_server_new_iostrm
        end function mcpc_server_new_iostrm
        
        function mcpc_server_set_nament(sv, nament) bind(c, name="mcpc_server_set_nament")
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: sv
            character(kind=c_char), dimension(*), intent(in) :: nament
            integer(c_int) :: mcpc_server_set_nament
        end function mcpc_server_set_nament

        function mcpc_server_capa_enable_tool(sv) bind(c, name="mcpc_server_capa_enable_tool")
            import :: c_ptr, c_int
            type(c_ptr), value :: sv
            integer(c_int) :: mcpc_server_capa_enable_tool
        end function mcpc_server_capa_enable_tool

        function mcpc_tool_new2(nament, descnt) bind(c, name="mcpc_tool_new2")
            import :: c_ptr, c_char
            character(kind=c_char), dimension(*), intent(in) :: nament, descnt
            type(c_ptr) :: mcpc_tool_new2
        end function mcpc_tool_new2

        function mcpc_toolprop_new2(nament, descnt, typ) bind(c, name="mcpc_toolprop_new2")
            import :: c_ptr, c_char, c_int
            character(kind=c_char), dimension(*), intent(in) :: nament, descnt
            integer(c_int), value :: typ
            type(c_ptr) :: mcpc_toolprop_new2
        end function mcpc_toolprop_new2

        subroutine mcpc_tool_addfre_toolprop(tool, toolprop) bind(c, name="mcpc_tool_addfre_toolprop")
            import :: c_ptr
            type(c_ptr), value :: tool, toolprop
        end subroutine mcpc_tool_addfre_toolprop

        function mcpc_tool_set_call_cb(tool, cb) bind(c, name="mcpc_tool_set_call_cb")
            import :: c_ptr, c_funptr, c_int
            type(c_ptr), value :: tool
            type(c_funptr), value :: cb
            integer(c_int) :: mcpc_tool_set_call_cb
        end function mcpc_tool_set_call_cb

        function mcpc_server_add_tool(sv, tool) bind(c, name="mcpc_server_add_tool")
            import :: c_ptr, c_int
            type(c_ptr), value :: sv, tool
            integer(c_int) :: mcpc_server_add_tool
        end function mcpc_server_add_tool

        function mcpc_server_start(sv) bind(c, name="mcpc_server_start")
            import :: c_ptr, c_int
            type(c_ptr), value :: sv
            integer(c_int) :: mcpc_server_start
        end function mcpc_server_start

        function mcpc_server_close(sv) bind(c, name="mcpc_server_close")
            import :: c_ptr, c_int
            type(c_ptr), value :: sv
            integer(c_int) :: mcpc_server_close
        end function mcpc_server_close
        
        function mcpc_tool_get_tpropval_u8str(tool, tprop_nament, ret, ret_cap, ret_len) &
                bind(c, name="mcpc_tool_get_tpropval_u8str")
            import :: c_ptr, c_char, c_size_t, c_int
            type(c_ptr), value :: tool
            character(kind=c_char), dimension(*), intent(in) :: tprop_nament
            type(c_ptr), value :: ret
            integer(c_size_t), value :: ret_cap
            type(c_ptr), value :: ret_len
            integer(c_int) :: mcpc_tool_get_tpropval_u8str
        end function mcpc_tool_get_tpropval_u8str
        
        subroutine mcpc_ucbr_toolcall_add_errmsg_printf8(ucbr, fmt) &
                bind(c, name="mcpc_ucbr_toolcall_add_errmsg_printf8")
             import :: c_ptr, c_char
             type(c_ptr), value :: ucbr
             character(kind=c_char), dimension(*), intent(in) :: fmt
        end subroutine mcpc_ucbr_toolcall_add_errmsg_printf8

    end interface
end module mcpc_interface

program server
    use iso_c_binding
    use iso_fortran_env, only: error_unit
    use mcpc_interface
    implicit none
    
    type(c_ptr) :: server_ptr
    integer(c_int) :: ret
    integer :: setup_ret

    ! Ensure stdout is unbuffered
    call set_stdout_unbuffered()

    ! Initialize Server
    server_ptr = mcpc_server_new_iostrm(get_stdin(), get_stdout())
    if (.not. c_associated(server_ptr)) then
        write(error_unit,*) "Failed to create server"
        error stop 1
    end if

    ! Set server name
    ret = mcpc_server_set_nament(server_ptr, c_char_"mcp-stdio-fortran" // c_null_char)
    if (ret /= 0) then
        write(error_unit,*) 'Failed to set server name'
        error stop 1
    end if

    ! Enable Tool Capabilities
    ret = mcpc_server_capa_enable_tool(server_ptr)

    ! Setup Tools
    setup_ret = setup_tools(server_ptr)
    if (setup_ret /= 0) then
        ret = mcpc_server_close(server_ptr)
        error stop 1
    end if

    ! Start Server Loop
    ret = mcpc_server_start(server_ptr)

    ! Cleanup
    ret = mcpc_server_close(server_ptr)

contains

    function setup_tools(server) result(res)
        type(c_ptr), intent(in) :: server
        integer :: res
        type(c_ptr) :: greet_tool, param_prop
        integer(c_int) :: ret_c

        ! Define "greet" tool
        greet_tool = mcpc_tool_new2(c_char_"greet" // c_null_char, &
            c_char_"Get a greeting from a local stdio server." // c_null_char)
        
        if (.not. c_associated(greet_tool)) then
            write(error_unit,*) "Failed to create tool"
            res = -1
            return
        end if

        ! Define "param" argument
        param_prop = mcpc_toolprop_new2(c_char_"param" // c_null_char, &
            c_char_"Greeting parameter" // c_null_char, MCPC_U8STR)
        
        if (.not. c_associated(param_prop)) then
            write(error_unit,*) "Failed to create tool property"
            res = -1
            return
        end if

        ! Add property to tool (takes ownership)
        call mcpc_tool_addfre_toolprop(greet_tool, param_prop)

        ! Set callback
        ret_c = mcpc_tool_set_call_cb(greet_tool, c_funloc(greet_cb))

        ! Add tool to server
        ret_c = mcpc_server_add_tool(server, greet_tool)

        res = 0
    end function setup_tools

    subroutine greet_cb(tool, ucbr) bind(c)
        type(c_ptr), value :: tool
        type(c_ptr), value :: ucbr
        
        character(kind=c_char), target :: param(4096)
        integer(c_size_t), target :: len
        integer(c_int) :: err

        ! Log execution
        call log_info_c(c_char_"Executed greet tool" // c_null_char)

        err = mcpc_tool_get_tpropval_u8str(tool, c_char_"param" // c_null_char, &
            c_loc(param), 4096_c_size_t, c_loc(len))
            
        if (err /= 0) then
            call mcpc_ucbr_toolcall_add_errmsg_printf8(ucbr, c_char_"Error retrieving 'param'" // c_null_char)
            return
        end if

        ! Ensure null termination
        if (len < 4096) then
            param(len+1) = c_null_char
        else
            param(4096) = c_null_char
        end if

        ! Return param
        call helper_add_text_result(ucbr, param)

    end subroutine greet_cb

end program server