#define QUOTED(x) #x
#define onefun(n, a, b, c) QUOTED(n),
#define twofun(n, a, b, c) QUOTED(n),
#define selfun(n, a, b) QUOTED(n),

#define make_fpm(n, a) QUOTED(n),
#define make_seg_fpm(n, a, b) QUOTED(n),
#define make_bfp(n, a) QUOTED(n),
#define make_seg_bfp(n, a, b) QUOTED(n),
#define make_seg_dpe(n, a, b) QUOTED(dpe_pe1##n),QUOTED(dpe_pe2##n),

// These two are implemented in terms of other vector ops
#define make_dfp(n) 
#define make_seg_dfp(n)

// defined directly
#define make_extract(n, a, b)

#define simpscan(n, a, b,c) QUOTED(n),
#define simpsegscan(n, a, b,c,d) QUOTED(n),
#define reduce(n, a, b,c) QUOTED(n),
#define segreduce(n, a, b,c,d) QUOTED(n),
#define make_seg_ext(n, a) QUOTED(n),
#define make_replace(n, a,b,c) QUOTED(n),
#define make_seg_replace(n, a) QUOTED(n),
#define make_distribute(n, a, c) QUOTED(n),
#define make_seg_distribute(n, a, b) QUOTED(n),
#define make_smpper(n, a) QUOTED(n),
#define make_seg_smpper(n, a, b) QUOTED(n),
#define make_bckper(n, a) QUOTED(n),
#define make_seg_bckper(n, a, b) QUOTED(n),

#define make_rk(n, a, b) QUOTED(n),
#define make_seg(n, a, b) QUOTED(n),

char const *KernelNames[] = {

#include "elementwise.h"
#include "vecops.h"
#include "vprims.h"
    "ind_lez",
    "pk1_luv",
    "pk1_lev",
    "pk2_luz",
    "pk2_lub",
    "pk2_lud",
    "pk2_lez",
    "pk2_leb",
    "pk2_led",
    #include "rank.h"
};

int KernelCount = sizeof(KernelNames) / sizeof(char*);
