       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  server-ptr          USAGE POINTER.
       01  tool-ptr            USAGE POINTER.
       01  prop-ptr            USAGE POINTER.
       01  ret-val             BINARY-LONG.
       01  cb-ptr              USAGE PROGRAM-POINTER.
       
       01  server-name         PIC X(20) VALUE Z"mcp-https-cobol".
       01  tool-name-greet     PIC X(10) VALUE Z"greet".
       01  tool-desc-greet     PIC X(50) VALUE Z"Get a greeting from a local http server.".
       01  prop-name-param     PIC X(10) VALUE Z"param".
       01  prop-desc-param     PIC X(30) VALUE Z"Greeting parameter".
       
       01  err-srv-new         PIC X(30) VALUE Z"Failed to create server".
       01  err-srv-name        PIC X(30) VALUE Z"Failed to set server name".
       01  err-tool-new        PIC X(30) VALUE Z"Failed to create tool: greet".
       01  err-prop-new        PIC X(40) VALUE Z"Failed to create property: param".
       01  err-srv-add         PIC X(30) VALUE Z"Failed to add tool to server".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           CALL "mcpc_server_new_tcp" RETURNING server-ptr.
                                       
           IF server-ptr = NULL THEN
               CALL "log_info_c" USING BY REFERENCE err-srv-new
               STOP RUN RETURNING 1
           END-IF.

           CALL "mcpc_server_set_nament" USING BY VALUE server-ptr 
                                               BY REFERENCE server-name
                                       RETURNING ret-val.
           IF ret-val NOT = 0 THEN
               CALL "log_info_c" USING BY REFERENCE err-srv-name
           END-IF.

           CALL "mcpc_server_capa_enable_tool" USING BY VALUE server-ptr.

           PERFORM SETUP-TOOLS.

           CALL "mcpc_server_start" USING BY VALUE server-ptr.
           CALL "mcpc_server_close" USING BY VALUE server-ptr.
           
           STOP RUN.

       SETUP-TOOLS.
           *> Tool: greet
           CALL "mcpc_tool_new2" USING BY REFERENCE tool-name-greet
                                       BY REFERENCE tool-desc-greet
                                 RETURNING tool-ptr.
           
           IF tool-ptr = NULL THEN
               CALL "log_info_c" USING BY REFERENCE err-tool-new
               STOP RUN RETURNING 1
           END-IF.

           CALL "mcpc_toolprop_new2" USING BY REFERENCE prop-name-param
                                           BY REFERENCE prop-desc-param
                                           BY VALUE 9 
                                     RETURNING prop-ptr.
                                     
           IF prop-ptr = NULL THEN
               CALL "log_info_c" USING BY REFERENCE err-prop-new
               STOP RUN RETURNING 1
           END-IF.

           CALL "mcpc_tool_addfre_toolprop" USING BY VALUE tool-ptr
                                                  BY VALUE prop-ptr.

           *> Note: We register 'greet_cb_wrapper' (from cob_helpers.c) as the callback.
           *> This C function then calls our COBOL program 'greet_cb_impl'.
           SET cb-ptr TO ENTRY "greet_cb_wrapper".
           CALL "mcpc_tool_set_call_cb" USING BY VALUE tool-ptr
                                              BY VALUE cb-ptr.
           
           CALL "mcpc_server_add_tool" USING BY VALUE server-ptr
                                             BY VALUE tool-ptr
                                       RETURNING ret-val.
           IF ret-val NOT = 0 THEN
               CALL "log_info_c" USING BY REFERENCE err-srv-add
           END-IF.
           
           EXIT.

       END PROGRAM server.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. greet_cb_impl.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  log-msg             PIC X(30) VALUE Z"Executed greet tool".
       01  err-tool-null       PIC X(30) VALUE Z"Error: tool-ptr is NULL".
       01  err-msg             PIC X(30) VALUE Z"Error retrieving param".
       
       LOCAL-STORAGE SECTION.
       78  BUFFER-SIZE         VALUE 4096.
       78  BUFFER-CAP          VALUE 4095.
       01  prop-name           PIC X(6) VALUE Z"param".
       01  param-buffer        PIC X(BUFFER-SIZE).
       01  param-len           USAGE BINARY-LONG-LONG SYNC VALUE 0.
       01  call-res            BINARY-LONG.
       
       LINKAGE SECTION.
       01  tool-ptr            USAGE POINTER.
       01  ucbr-ptr            USAGE POINTER.
       
       PROCEDURE DIVISION USING tool-ptr ucbr-ptr.
           CALL "log_info_c" USING BY REFERENCE log-msg.
           
           MOVE 0 TO param-len.
           MOVE LOW-VALUES TO param-buffer.
           
           IF tool-ptr = NULL THEN
                CALL "log_info_c" USING BY REFERENCE err-tool-null
                EXIT PROGRAM
           END-IF.

           CALL "mcpc_tool_get_tpropval_u8str" USING BY VALUE tool-ptr
                                                     BY REFERENCE prop-name
                                                     BY REFERENCE param-buffer
                                                     BY VALUE BUFFER-CAP
                                                     BY REFERENCE param-len
                                               RETURNING call-res.

           IF call-res NOT = 0 THEN
               CALL "mcpc_ucbr_toolcall_add_errmsg_printf8" USING BY VALUE ucbr-ptr
                                                                  BY REFERENCE err-msg
               EXIT PROGRAM
           END-IF.
           
           *> Buffer is already pre-filled with LOW-VALUES (nulls),
           *> so it is safely null-terminated regardless of param-len.
           
           CALL "helper_add_text_result" USING BY VALUE ucbr-ptr
                                               BY REFERENCE param-buffer.
           
           MOVE 0 TO RETURN-CODE.
           EXIT PROGRAM.
       END PROGRAM greet_cb_impl.
