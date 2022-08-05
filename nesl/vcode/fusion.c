/*
 * These are the definitions of the externally-generated, fused functions.
 *
 * TODO:
 * - A function that loads them from a shared library, based on a
 * commandline passed in to the VCODE interpreter.
 */

#include "config.h"
#include "vcode.h"
#include <cvl.h>
#include "y.tab.h"
#include <dlfcn.h>
#include <stdio.h>

vopdes_t *vopdes_fusion_table = NULL;
cvl_triple_t *cvl_fused_fun_list = NULL;

void load_fused_library() {
    if (fused_library_name[0] == '\0') {
        fprintf(stderr, "FUSED calls found in VCODE, but no fused library was provided with -l.\n");
        vinterp_exit (1);
    }

    void *handle = dlopen(fused_library_name, RTLD_NOW);
    if (handle == NULL) {
        fprintf(stderr, "Unable to dynamically load library %s, error %s.\n", fused_library_name, dlerror());
        vinterp_exit (1);
    }

    vopdes_fusion_table = dlsym(handle, "vops");
    if (vopdes_fusion_table == NULL) {
        fprintf(stderr, "Symbol vopdes_fusion_table missing from dynamically load library %s.\n", fused_library_name);
        vinterp_exit (1);
    }

    cvl_fused_fun_list = dlsym(handle, "cvl_funs");
    if (cvl_fused_fun_list == NULL) {
        fprintf(stderr, "Symbol cvl_fused_fun_list missing from dynamically load library %s.\n", fused_library_name);
        vinterp_exit (1);
    }
}
