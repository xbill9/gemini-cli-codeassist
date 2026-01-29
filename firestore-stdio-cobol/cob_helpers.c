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

void check_db_cb_wrapper(void *tool, void *ucbr) {
    void *args[2];
    args[0] = &tool;
    args[1] = &ucbr;
    cob_call("check_db_cb_impl", 2, args);
}

void get_root_cb_wrapper(void *tool, void *ucbr) {
    void *args[2];
    args[0] = &tool;
    args[1] = &ucbr;
    cob_call("get_root_cb_impl", 2, args);
}

void get_products_cb_wrapper(void *tool, void *ucbr) {
    void *args[2];
    args[0] = &tool;
    args[1] = &ucbr;
    cob_call("get_products_cb_impl", 2, args);
}

void get_product_by_id_cb_wrapper(void *tool, void *ucbr) {
    void *args[2];
    args[0] = &tool;
    args[1] = &ucbr;
    cob_call("get_product_by_id_cb_impl", 2, args);
}

void seed_cb_wrapper(void *tool, void *ucbr) {
    void *args[2];
    args[0] = &tool;
    args[1] = &ucbr;
    cob_call("seed_cb_impl", 2, args);
}

void reset_cb_wrapper(void *tool, void *ucbr) {
    void *args[2];
    args[0] = &tool;
    args[1] = &ucbr;
    cob_call("reset_cb_impl", 2, args);
}
