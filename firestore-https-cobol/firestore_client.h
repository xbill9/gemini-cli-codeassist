#ifndef FIRESTORE_CLIENT_H
#define FIRESTORE_CLIENT_H

#include <stdio.h>
#include "mcpc/mcpc.h"

/* c_helpers.c */
FILE* get_stdin(void);
FILE* get_stdout(void);
void set_stdout_unbuffered(void);
void helper_add_text_result(mcpc_ucbr_t *ucbr, const char *text);
void log_info_c(const char *msg);
void log_error_c(const char *msg);

/* firestore_client.c */
void impl_get_root(void *ucbr);
void impl_check_db(void *ucbr);
void impl_get_products(void *ucbr);
void impl_get_product_by_id(void *ucbr, const char *id);
void impl_seed(void *ucbr);
void impl_reset(void *ucbr);

#endif
