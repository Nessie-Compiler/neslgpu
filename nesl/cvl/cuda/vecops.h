make_fpm(fpm_puz, int)
make_fpm(fpm_pub, cvl_bool)
make_fpm(fpm_pud, float)

make_seg_fpm(fpm_pez, int, 0, fpm_puz)
make_seg_fpm(fpm_peb, cvl_bool, 0, fpm_pub)
make_seg_fpm(fpm_ped, float, 0.0, fpm_pud)

make_bfp(bfp_puz, int)
make_bfp(bfp_pub, cvl_bool)
make_bfp(bfp_pud, float)

make_seg_bfp(bfp_pez, int, bfp_puz)
make_seg_bfp(bfp_peb, cvl_bool, bfp_pub)
make_seg_bfp(bfp_ped, float, bfp_pud)

make_seg_dpe(z,int,dpe_puz)
make_seg_dpe(b,cvl_bool,dpe_pub)
make_seg_dpe(d,float,dpe_pud)

make_dfp(z)
make_dfp(b)
make_dfp(d)

make_seg_dfp(z)
make_seg_dfp(b)
make_seg_dfp(d)
