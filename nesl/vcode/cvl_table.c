/*
 * Copyright 1992, 1994, 1994, 1995 Carnegie Mellon University
 */

#include "config.h"
#include "vcode.h"
#include <cvl.h>

cvl_triple_t cvl_fun_list[] = 
{
  // PLUS
    { { (void (*)())add_wuz, (int (*)())add_wuz_scratch, (unsigned (*)())add_wuz_inplace },
      { (void (*)())add_wud, (int (*)())add_wud_scratch, (unsigned (*)())add_wud_inplace },
    },
  // MINUS
    { { (void (*)())sub_wuz, (int (*)())sub_wuz_scratch, (unsigned (*)())sub_wuz_inplace },
      { (void (*)())sub_wud, (int (*)())sub_wud_scratch, (unsigned (*)())sub_wud_inplace },
    },
  // TIMES
    { { (void (*)())mul_wuz, (int (*)())mul_wuz_scratch, (unsigned (*)())mul_wuz_inplace },
      { (void (*)())mul_wud, (int (*)())mul_wud_scratch, (unsigned (*)())mul_wud_inplace },
    },
  // DIV
    { { (void (*)())div_wuz, (int (*)())div_wuz_scratch, (unsigned (*)())div_wuz_inplace },
      { (void (*)())div_wud, (int (*)())div_wud_scratch, (unsigned (*)())div_wud_inplace },
    },
  // MOD
    { { (void (*)())mod_wuz, (int (*)())mod_wuz_scratch, (unsigned (*)())mod_wuz_inplace },
    },
  // LT
    { { (void (*)())les_wuz, (int (*)())les_wuz_scratch, (unsigned (*)())les_wuz_inplace },
      { (void (*)())les_wud, (int (*)())les_wud_scratch, (unsigned (*)())les_wud_inplace },
    },
  // LEQ
    { { (void (*)())leq_wuz, (int (*)())leq_wuz_scratch, (unsigned (*)())leq_wuz_inplace },
      { (void (*)())leq_wud, (int (*)())leq_wud_scratch, (unsigned (*)())leq_wud_inplace },
    },
  // GT
    { { (void (*)())grt_wuz, (int (*)())grt_wuz_scratch, (unsigned (*)())grt_wuz_inplace },
      { (void (*)())grt_wud, (int (*)())grt_wud_scratch, (unsigned (*)())grt_wud_inplace },
    },
  // GEQ
    { { (void (*)())geq_wuz, (int (*)())geq_wuz_scratch, (unsigned (*)())geq_wuz_inplace },
      { (void (*)())geq_wud, (int (*)())geq_wud_scratch, (unsigned (*)())geq_wud_inplace },
    },
  // EQ
    { { (void (*)())eql_wuz, (int (*)())eql_wuz_scratch, (unsigned (*)())eql_wuz_inplace },
      { (void (*)())eql_wud, (int (*)())eql_wud_scratch, (unsigned (*)())eql_wud_inplace },
      { (void (*)())eql_wub, (int (*)())eql_wub_scratch, (unsigned (*)())eql_wub_inplace },
    },
  // NEQ
    { { (void (*)())neq_wuz, (int (*)())neq_wuz_scratch, (unsigned (*)())neq_wuz_inplace },
      { (void (*)())neq_wud, (int (*)())neq_wud_scratch, (unsigned (*)())neq_wud_inplace },
      { (void (*)())neq_wub, (int (*)())neq_wub_scratch, (unsigned (*)())neq_wub_inplace },
    },
  // LSHIFT
    { { (void (*)())lsh_wuz, (int (*)())lsh_wuz_scratch, (unsigned (*)())lsh_wuz_inplace },
    },
  // RSHIFT
    { { (void (*)())rsh_wuz, (int (*)())rsh_wuz_scratch, (unsigned (*)())rsh_wuz_inplace },
    },
  // NOT
    { { (void (*)())not_wuz, (int (*)())not_wuz_scratch, (unsigned (*)())not_wuz_inplace },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())not_wub, (int (*)())not_wub_scratch, (unsigned (*)())not_wub_inplace },
    },
  // AND
    { { (void (*)())and_wuz, (int (*)())and_wuz_scratch, (unsigned (*)())and_wuz_inplace },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())and_wub, (int (*)())and_wub_scratch, (unsigned (*)())and_wub_inplace },
    },
  // OR
    { { (void (*)())ior_wuz, (int (*)())ior_wuz_scratch, (unsigned (*)())ior_wuz_inplace },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())ior_wub, (int (*)())ior_wub_scratch, (unsigned (*)())ior_wub_inplace },
    },
  // XOR
    { { (void (*)())xor_wuz, (int (*)())xor_wuz_scratch, (unsigned (*)())xor_wuz_inplace },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())xor_wub, (int (*)())xor_wub_scratch, (unsigned (*)())xor_wub_inplace },
    },
  // SELECT
    { { (void (*)())sel_wuz, (int (*)())sel_wuz_scratch, (unsigned (*)())sel_wuz_inplace },
      { (void (*)())sel_wud, (int (*)())sel_wud_scratch, (unsigned (*)())sel_wud_inplace },
      { (void (*)())sel_wub, (int (*)())sel_wub_scratch, (unsigned (*)())sel_wub_inplace },
    },
  // RAND
    { { (void (*)())rnd_wuz, (int (*)())rnd_wuz_scratch, (unsigned (*)())rnd_wuz_inplace },
    },
  // FLOOR
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())flr_wud, (int (*)())flr_wud_scratch, (unsigned (*)())flr_wud_inplace },
    },
  // CEIL
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())cei_wud, (int (*)())cei_wud_scratch, (unsigned (*)())cei_wud_inplace },
    },
  // TRUNC
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())trn_wud, (int (*)())trn_wud_scratch, (unsigned (*)())trn_wud_inplace },
    },
   // ROUND
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())rou_wud, (int (*)())rou_wud_scratch, (unsigned (*)())rou_wud_inplace },
    },
  // LOG
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())log_wud, (int (*)())log_wud_scratch, (unsigned (*)())log_wud_inplace },
    },
  // SQRT
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())sqt_wud, (int (*)())sqt_wud_scratch, (unsigned (*)())sqt_wud_inplace },
    },
  // EXP
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())exp_wud, (int (*)())exp_wud_scratch, (unsigned (*)())exp_wud_inplace },
    },
  // SIN
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())sin_wud, (int (*)())sin_wud_scratch, (unsigned (*)())sin_wud_inplace },
    },
  // COS
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())cos_wud, (int (*)())cos_wud_scratch, (unsigned (*)())cos_wud_inplace },
    },
  // TAN
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())tan_wud, (int (*)())tan_wud_scratch, (unsigned (*)())tan_wud_inplace },
    },
  // ASIN
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())asn_wud, (int (*)())asn_wud_scratch, (unsigned (*)())asn_wud_inplace },
    },
  // ACOS
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())acs_wud, (int (*)())acs_wud_scratch, (unsigned (*)())acs_wud_inplace },
    },
  // ATAN
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())atn_wud, (int (*)())atn_wud_scratch, (unsigned (*)())atn_wud_inplace },
    },
  // SINH
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())snh_wud, (int (*)())snh_wud_scratch, (unsigned (*)())snh_wud_inplace },
    },
  // COSH
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())csh_wud, (int (*)())csh_wud_scratch, (unsigned (*)())csh_wud_inplace },
    },
  // TANH
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())tnh_wud, (int (*)())tnh_wud_scratch, (unsigned (*)())tnh_wud_inplace },
    },
  // I_TO_F
    { { (void (*)())dbl_wuz, (int (*)())dbl_wuz_scratch, (unsigned (*)())dbl_wuz_inplace },
    },
  // I_TO_B
    { { (void (*)())boo_wuz, (int (*)())boo_wuz_scratch, (unsigned (*)())boo_wuz_inplace },
    },
  // B_TO_I
    { { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())int_wub, (int (*)())int_wub_scratch, (unsigned (*)())int_wub_inplace },
    },
  // PLUS_SCAN
    { { (void (*)())add_sez, (int (*)())add_sez_scratch, (unsigned (*)())add_sez_inplace },
      { (void (*)())add_sed, (int (*)())add_sed_scratch, (unsigned (*)())add_sed_inplace },
    },
  // MULT_SCAN
    { { (void (*)())mul_sez, (int (*)())mul_sez_scratch, (unsigned (*)())mul_sez_inplace },
      { (void (*)())mul_sed, (int (*)())mul_sed_scratch, (unsigned (*)())mul_sed_inplace },
    },
  // MAX_SCAN
    { { (void (*)())max_sez, (int (*)())max_sez_scratch, (unsigned (*)())max_sez_inplace },
      { (void (*)())max_sed, (int (*)())max_sed_scratch, (unsigned (*)())max_sed_inplace },
    },
  // MIN_SCAN
    { { (void (*)())min_sez, (int (*)())min_sez_scratch, (unsigned (*)())min_sez_inplace },
      { (void (*)())min_sed, (int (*)())min_sed_scratch, (unsigned (*)())min_sed_inplace },
    },
  // AND_SCAN
    { { (void (*)())and_sez, (int (*)())and_sez_scratch, (unsigned (*)())and_sez_inplace },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())and_seb, (int (*)())and_seb_scratch, (unsigned (*)())and_seb_inplace },
    }, 
  // OR_SCAN
    { { (void (*)())ior_sez, (int (*)())ior_sez_scratch, (unsigned (*)())ior_sez_inplace },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())ior_seb, (int (*)())ior_seb_scratch, (unsigned (*)())ior_seb_inplace },
    }, 
  // XOR_SCAN
    { { (void (*)())xor_sez, (int (*)())xor_sez_scratch, (unsigned (*)())xor_sez_inplace },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())xor_seb, (int (*)())xor_seb_scratch, (unsigned (*)())xor_seb_inplace },
    },
  // PLUS_REDUCE
    { { (void (*)())add_rez, (int (*)())add_rez_scratch, (unsigned (*)())add_rez_inplace },
      { (void (*)())add_red, (int (*)())add_red_scratch, (unsigned (*)())add_red_inplace },
    },
  // MULT_REDUCE
    { { (void (*)())mul_rez, (int (*)())mul_rez_scratch, (unsigned (*)())mul_rez_inplace },
      { (void (*)())mul_red, (int (*)())mul_red_scratch, (unsigned (*)())mul_red_inplace },
    },
  // MAX_REDUCE
    { { (void (*)())max_rez, (int (*)())max_rez_scratch, (unsigned (*)())max_rez_inplace },
      { (void (*)())max_red, (int (*)())max_red_scratch, (unsigned (*)())max_red_inplace },
    },
  // MIN_REDUCE
    { { (void (*)())min_rez, (int (*)())min_rez_scratch, (unsigned (*)())min_rez_inplace },
      { (void (*)())min_red, (int (*)())min_red_scratch, (unsigned (*)())min_red_inplace },
    },
  // AND_REDUCE
    { { (void (*)())and_rez, (int (*)())and_rez_scratch, (unsigned (*)())and_rez_inplace },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())and_reb, (int (*)())and_reb_scratch, (unsigned (*)())and_reb_inplace },
    }, 
  // OR_REDUCE
    { { (void (*)())ior_rez, (int (*)())ior_rez_scratch, (unsigned (*)())ior_rez_inplace },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())ior_reb, (int (*)())ior_reb_scratch, (unsigned (*)())ior_reb_inplace },
    }, 
  // XOR_REDUCE
    { { (void (*)())xor_rez, (int (*)())xor_rez_scratch, (unsigned (*)())xor_rez_inplace },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())xor_reb, (int (*)())xor_reb_scratch, (unsigned (*)())xor_reb_inplace },
    }, 
  // PERMUTE
    { { (void (*)())smp_pez, (int (*)())smp_pez_scratch, (unsigned (*)())smp_pez_inplace },
      { (void (*)())smp_ped, (int (*)())smp_ped_scratch, (unsigned (*)())smp_ped_inplace },
      { (void (*)())smp_peb, (int (*)())smp_peb_scratch, (unsigned (*)())smp_peb_inplace },
    },
  // DPERMUTE
    { { (void (*)())dpe_pez, (int (*)())dpe_pez_scratch, (unsigned (*)())dpe_pez_inplace },
      { (void (*)())dpe_ped, (int (*)())dpe_ped_scratch, (unsigned (*)())dpe_ped_inplace },
      { (void (*)())dpe_peb, (int (*)())dpe_peb_scratch, (unsigned (*)())dpe_peb_inplace },
    },
  // FPERMUTE
    { { (void (*)())fpm_pez, (int (*)())fpm_pez_scratch, (unsigned (*)())fpm_pez_inplace },
      { (void (*)())fpm_ped, (int (*)())fpm_ped_scratch, (unsigned (*)())fpm_ped_inplace },
      { (void (*)())fpm_peb, (int (*)())fpm_peb_scratch, (unsigned (*)())fpm_peb_inplace },
    },
  // BPERMUTE
    { { (void (*)())bck_pez, (int (*)())bck_pez_scratch, (unsigned (*)())bck_pez_inplace },
      { (void (*)())bck_ped, (int (*)())bck_ped_scratch, (unsigned (*)())bck_ped_inplace },
      { (void (*)())bck_peb, (int (*)())bck_peb_scratch, (unsigned (*)())bck_peb_inplace },
    },
  // BFPERMUTE
    { { (void (*)())bfp_pez, (int (*)())bfp_pez_scratch, (unsigned (*)())bfp_pez_inplace },
      { (void (*)())bfp_ped, (int (*)())bfp_ped_scratch, (unsigned (*)())bfp_ped_inplace },
      { (void (*)())bfp_peb, (int (*)())bfp_peb_scratch, (unsigned (*)())bfp_peb_inplace },
    },
  // DFPERMUTE
    { { (void (*)())dfp_pez, (int (*)())dfp_pez_scratch, (unsigned (*)())dfp_pez_inplace },
      { (void (*)())dfp_ped, (int (*)())dfp_ped_scratch, (unsigned (*)())dfp_ped_inplace },
      { (void (*)())dfp_peb, (int (*)())dfp_peb_scratch, (unsigned (*)())dfp_peb_inplace },
    },
  // EXTRACT
    { { (void (*)())ext_vez, (int (*)())ext_vez_scratch, (unsigned (*)())ext_vez_inplace },
      { (void (*)())ext_ved, (int (*)())ext_ved_scratch, (unsigned (*)())ext_ved_inplace },
      { (void (*)())ext_veb, (int (*)())ext_veb_scratch, (unsigned (*)())ext_veb_inplace },
    },
  // REPLACE
    { { (void (*)())rep_vez, (int (*)())rep_vez_scratch, (unsigned (*)())rep_vez_inplace },
      { (void (*)())rep_ved, (int (*)())rep_ved_scratch, (unsigned (*)())rep_ved_inplace },
      { (void (*)())rep_veb, (int (*)())rep_veb_scratch, (unsigned (*)())rep_veb_inplace },
    },
  // DIST
    { { (void (*)())dis_vez, (int (*)())dis_vez_scratch, (unsigned (*)())dis_vez_inplace },
      { (void (*)())dis_ved, (int (*)())dis_ved_scratch, (unsigned (*)())dis_ved_inplace },
      { (void (*)())dis_veb, (int (*)())dis_veb_scratch, (unsigned (*)())dis_veb_inplace },
    },
  // INDEX
    { { (void (*)())ind_lez, (int (*)())ind_lez_scratch, (unsigned (*)())ind_lez_inplace },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
    },
  // RANK_UP
    { { (void (*)())rku_lez, (int (*)())rku_lez_scratch, (unsigned (*)())rku_lez_inplace },
      { (void (*)())rku_led, (int (*)())rku_led_scratch, (unsigned (*)())rku_led_inplace },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
    },
  // RANK_DOWN
    { { (void (*)())rkd_lez, (int (*)())rkd_lez_scratch, (unsigned (*)())rkd_lez_inplace },
      { (void (*)())rkd_led, (int (*)())rkd_led_scratch, (unsigned (*)())rkd_led_inplace },
      { (void (*)())0, (int (*)())0, (unsigned (*)())0 },
    },
  // PACK
    { { (void (*)())pk2_lez, (int (*)())pk2_lez_scratch, (unsigned (*)())pk2_lez_inplace },
      { (void (*)())pk2_led, (int (*)())pk2_led_scratch, (unsigned (*)())pk2_led_inplace },
      { (void (*)())pk2_leb, (int (*)())pk2_leb_scratch, (unsigned (*)())pk2_leb_inplace },
    },
};
