#include <stdio.h>
#include <libcob.h>
#include "mcpc/mcpc.h"

void greet_cb_wrapper(void *tool, void *ucbr);

/* Wrapper for COBOL callback to handle argument passing safely */
void greet_cb_wrapper(void *tool, void *ucbr) {
    void *args[2];
    args[0] = &tool;
    args[1] = &ucbr;
    cob_call("greet_cb_impl", 2, args);
}
