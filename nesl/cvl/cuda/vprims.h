simpscan(add_suz, cpp_plus_int, int, 0)		/* add scans */
simpscan(add_sud, cpp_plus_float, float, (float) 0.0)

simpscan(mul_suz, cpp_times_int, int, 1)	/* multiply scans */
simpscan(mul_sud, cpp_times_float, float, (float) 1.0)

simpscan(min_suz, cpp_mmin_int, int, MAX_INT)	/* min scans */
simpscan(min_sud, cpp_mmin_float, float, MAX_FLOAT)

simpscan(max_suz, cpp_mmax_int, int, MIN_INT)	/* max scans */
simpscan(max_sud, cpp_mmax_float, float, MIN_FLOAT)

simpscan(and_sub, cpp_aand_int, int, 1)	/* logical and scan */
simpscan(and_suz, cpp_band_int, int, ~0)	/* bitwise and scan */

simpscan(ior_sub, cpp_oor_int, int, 0)	/* logical or scan */
simpscan(ior_suz, cpp_bor_int, int, 0)		/* bitwise or scan */

simpscan(xor_sub, cpp_lxor_int, int, 0)	/* logical or scan */
simpscan(xor_suz, cpp_xxor_int, int, 0)		/* bitwise xor scan */

simpsegscan(add_sez, cpp_plus_int, int, 0, add_suz)		/* add scans */
simpsegscan(add_sed, cpp_plus_float, float, (float) 0.0, add_sud)

simpsegscan(mul_sez, cpp_times_int, int, 1, mul_suz)		/* multiply scans */
simpsegscan(mul_sed, cpp_times_float, float, (float) 1.0, mul_sud)

simpsegscan(min_sez, cpp_mmin_int, int, MAX_INT, min_suz)	/* min scans */
simpsegscan(min_sed, cpp_mmin_float, float, MAX_FLOAT, min_sud)

simpsegscan(max_sez, cpp_mmax_int, int, MIN_INT, max_suz)	/* max scans */
simpsegscan(max_sed, cpp_mmax_float, float, MIN_FLOAT, max_sud)

simpsegscan(and_seb, cpp_aand_int, int, 1, and_sub)		/* logical and scan */
simpsegscan(and_sez, cpp_band_int, int, ~0, and_suz)		/* bitwise and scan */

simpsegscan(ior_seb, cpp_oor_int, int, 0, ior_sub)		/* logical or scan */
simpsegscan(ior_sez, cpp_bor_int, int, 0, ior_suz)		/* bitwise or scan */

simpsegscan(xor_seb, cpp_lxor_int, int, 0, xor_sub)	/* logical or scan */
simpsegscan(xor_sez, cpp_xxor_int, int, 0, xor_suz)		/* bitwise xor scan */

reduce(add_ruz, cpp_plus_int, int, int, 0)			/* add reduces */
reduce(add_rud, cpp_plus_float, float, double, (double) 0.0)

reduce(mul_ruz, cpp_times_int, int, int, 1)			/* multiply reduce */
reduce(mul_rud, cpp_times_float, float, double, (double) 1.0)

reduce(min_ruz, cpp_mmin_int, int, int, MAX_INT)		/* min reduces */
reduce(min_rud, cpp_mmin_float, int, double, MAX_DOUBLE)

reduce(max_ruz, cpp_mmax_int, int, int, MIN_INT)		/* max reduces */
reduce(max_rud, cpp_mmax_float, float, double, MIN_DOUBLE)

reduce(and_rub, cpp_aand_int, int, int, TRUE)		/* logical and reduce */
reduce(and_ruz, cpp_band_int, int, int, (~0))		/* bitwise and scan */

reduce(ior_rub, cpp_oor_int, int, int, FALSE)		/* logical or reduce */
reduce(ior_ruz, cpp_bor_int, int, int, 0)			/* bitwise or reduce */

reduce(xor_rub, cpp_lxor_int, int, int, 0)	/* logical or reduce */
reduce(xor_ruz, cpp_xxor_int, int, int, 0)		/* bitwise xor reduce */

segreduce(add_rez, cpp_plus_int, int, 0, add_ruz)		/* add reduces */
segreduce(add_red, cpp_plus_float, float, (float) 0.0, add_rud)

segreduce(mul_rez, cpp_times_int, int, 1, mul_ruz)		/* multiply scans */
segreduce(mul_red, cpp_times_float, float, (float) 1.0, mul_rud)

segreduce(min_rez, cpp_mmin_int, int, MAX_INT, min_ruz)		/* min reduces */
segreduce(min_red, cpp_mmin_float, float, MAX_FLOAT, min_rud)

segreduce(max_rez, cpp_mmax_int, int, MIN_INT, max_ruz)		/* max reduces */
segreduce(max_red, cpp_mmax_float, float, MIN_FLOAT, max_rud)

segreduce(and_reb, cpp_aand_int, int, TRUE, and_rub)	/* logical and reduce */
segreduce(and_rez, cpp_band_int, int, ~0, and_ruz)		/* bitwise and reduce */

segreduce(ior_reb, cpp_oor_int, int, FALSE, ior_rub)	/* logical or reduce */
segreduce(ior_rez, cpp_bor_int, int, 0, ior_ruz)		/* bitwise or reduce */

segreduce(xor_reb, cpp_lxor_int, int, 0, xor_rub)		/* logical xor scan */
segreduce(xor_rez, cpp_xxor_int, int, 0, xor_ruz)		/* bitwise xor scan */


make_extract(ext_vuz, int, int)
make_extract(ext_vub, int, int)
make_extract(ext_vud, double, float)

make_seg_ext(ext_vez, int)
make_seg_ext(ext_veb, int)
make_seg_ext(ext_ved, double)

make_replace(rep_vuz, int, int, ident)
make_replace(rep_vub, int, int, notnot)
make_replace(rep_vud, double, float, ident)

make_seg_replace(rep_vez, int)
make_seg_replace(rep_veb, int)
make_seg_replace(rep_ved, double)

make_distribute(dis_vuz, int, int)
make_distribute(dis_vub, int, int)
make_distribute(dis_vud, double, float)

make_seg_distribute(dis_vez, int, dis_vuz)
make_seg_distribute(dis_veb, int, dis_vub)
make_seg_distribute(dis_ved, double, dis_vud)


make_smpper(smp_puz, int)
make_smpper(smp_pub, int)
make_smpper(smp_pud, double)


make_seg_smpper(smp_pez, int, smp_puz)
make_seg_smpper(smp_peb, int, smp_pub)
make_seg_smpper(smp_ped, double, smp_pud)


make_bckper(bck_puz, int)
make_bckper(bck_pub, int)
make_bckper(bck_pud, double)

make_seg_bckper(bck_pez, int, bck_puz)
make_seg_bckper(bck_peb, int, bck_pub)
make_seg_bckper(bck_ped, double, bck_pud)
