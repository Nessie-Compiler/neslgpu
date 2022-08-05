simpscan(add_suz, plus, int, 0)		/* add scans */
simpscan(add_sud, plus, double, (double) 0.0)

simpscan(mul_suz, times, int, 1)	/* multiply scans */
simpscan(mul_sud, times, double, (double) 1.0)

simpscan(min_suz, mmin, int, MAX_INT)	/* min scans */
simpscan(min_sud, mmin, double, MAX_DOUBLE)

simpscan(max_suz, mmax, int, MIN_INT)	/* max scans */
simpscan(max_sud, mmax, double, MIN_DOUBLE)

simpscan(and_sub, aand, cvl_bool, 1)	/* logical and scan */
simpscan(and_suz, band, int, ~0)	/* bitwise and scan */

simpscan(ior_sub, oor, cvl_bool, 0)	/* logical or scan */
simpscan(ior_suz, bor, int, 0)		/* bitwise or scan */

simpscan(xor_sub, lxor, cvl_bool, 0)	/* logical or scan */
simpscan(xor_suz, xxor, int, 0)		/* bitwise xor scan */

simpsegscan(add_sez, plus, int, 0, add_suz)		/* add scans */
simpsegscan(add_sed, plus, double, (double) 0.0, add_sud)

simpsegscan(mul_sez, times, int, 1, mul_suz)		/* multiply scans */
simpsegscan(mul_sed, times, double, (double) 1.0, mul_sud)

simpsegscan(min_sez, mmin, int, MAX_INT, min_suz)	/* min scans */
simpsegscan(min_sed, mmin, double, MAX_DOUBLE, min_sud)

simpsegscan(max_sez, mmax, int, MIN_INT, max_suz)	/* max scans */
simpsegscan(max_sed, mmax, double, MIN_DOUBLE, max_sud)

simpsegscan(and_seb, aand, cvl_bool, 1, and_sub)		/* logical and scan */
simpsegscan(and_sez, band, int, ~0, and_suz)		/* bitwise and scan */

simpsegscan(ior_seb, oor, cvl_bool, 0, ior_sub)		/* logical or scan */
simpsegscan(ior_sez, bor, int, 0, ior_suz)		/* bitwise or scan */

simpsegscan(xor_seb, lxor, cvl_bool, 0, xor_sub)	/* logical or scan */
simpsegscan(xor_sez, xxor, int, 0, xor_suz)		/* bitwise xor scan */
reduce(add_ruz, plus, int, 0)			/* add reduces */
reduce(add_rud, plus, double, (double) 0.0)

reduce(mul_ruz, times, int, 1)			/* multiply reduce */
reduce(mul_rud, times, double, (double) 1.0)

reduce(min_ruz, mmin, int, MAX_INT)		/* min reduces */
reduce(min_rud, mmin, double, MAX_DOUBLE)

reduce(max_ruz, mmax, int, MIN_INT)		/* max reduces */
reduce(max_rud, mmax, double, MIN_DOUBLE)

reduce(and_rub, aand, cvl_bool, TRUE)		/* logical and reduce */
reduce(and_ruz, band, int, (~0))		/* bitwise and scan */

reduce(ior_rub, oor, cvl_bool, FALSE)		/* logical or reduce */
reduce(ior_ruz, bor, int, 0)			/* bitwise or reduce */

reduce(xor_rub, lxor, cvl_bool, 0)	/* logical or reduce */
reduce(xor_ruz, xxor, int, 0)		/* bitwise xor reduce */

segreduce(add_rez, plus, int, 0, add_ruz)		/* add reduces */
segreduce(add_red, plus, double, (double) 0.0, add_rud)

segreduce(mul_rez, times, int, 1, mul_ruz)		/* multiply scans */
segreduce(mul_red, times, double, (double) 1.0, mul_rud)

segreduce(min_rez, mmin, int, MAX_INT, min_ruz)		/* min reduces */
segreduce(min_red, mmin, double, MAX_DOUBLE, min_rud)

segreduce(max_rez, mmax, int, MIN_INT, max_ruz)		/* max reduces */
segreduce(max_red, mmax, double, MIN_DOUBLE, max_rud)

segreduce(and_reb, aand, cvl_bool, TRUE, and_rub)	/* logical and reduce */
segreduce(and_rez, band, int, ~0, and_ruz)		/* bitwise and reduce */

segreduce(ior_reb, oor, cvl_bool, FALSE, ior_rub)	/* logical or reduce */
segreduce(ior_rez, bor, int, 0, ior_ruz)		/* bitwise or reduce */

segreduce(xor_reb, lxor, cvl_bool, 0, xor_rub)		/* logical xor scan */
segreduce(xor_rez, xxor, int, 0, xor_ruz)		/* bitwise xor scan */


make_extract(ext_vuz, int, cl_int)
make_extract(ext_vub, cvl_bool, cl_int)
make_extract(ext_vud, double, cl_float)

make_seg_ext(ext_vez, int)
make_seg_ext(ext_veb, cvl_bool)
make_seg_ext(ext_ved, double)

make_replace(rep_vuz, int, cl_int, ident)
make_replace(rep_vub, cvl_bool, cl_bool, notnot)
make_replace(rep_vud, double, cl_float, ident)

make_seg_replace(rep_vez, int)
make_seg_replace(rep_veb, cvl_bool)
make_seg_replace(rep_ved, double)

make_distribute(dis_vuz, int, cl_int)
make_distribute(dis_vub, cvl_bool, cl_bool)
make_distribute(dis_vud, double, cl_float)

make_seg_distribute(dis_vez, int, dis_vuz)
make_seg_distribute(dis_veb, cvl_bool, dis_vub)
make_seg_distribute(dis_ved, double, dis_vud)


make_smpper(smp_puz, int)
make_smpper(smp_pub, cvl_bool)
make_smpper(smp_pud, double)


make_seg_smpper(smp_pez, int, smp_puz)
make_seg_smpper(smp_peb, cvl_bool, smp_pub)
make_seg_smpper(smp_ped, double, smp_pud)


make_bckper(bck_puz, int)
make_bckper(bck_pub, cvl_bool)
make_bckper(bck_pud, double)

make_seg_bckper(bck_pez, int, bck_puz)
make_seg_bckper(bck_peb, cvl_bool, bck_pub)
make_seg_bckper(bck_ped, double, bck_pud)
