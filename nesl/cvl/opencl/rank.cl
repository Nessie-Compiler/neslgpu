//typedef union maxaligned { int i; float f;} MAXALIGN;
//typedef int cvl_bool;

/* This file contains the rank functions:
 * o upward and downward,
 * o integer and double,
 * o segmented and unsegmented.
 * The result of these functions is a permutation (suitable for use with
 * the smp_p** functions) that gives the sorted order of the elements
 * of the source vector.  The rank functions are stable, and use a radix
 * rank to get linear performance.
 */


#define make_rk(_name, _first, _second)            \
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int d, \
    int s, \
    int len, \
    int scratch,\
    int first,\
    int second)                          \
{\
    /* TODO */ \
    return; \
}

make_rk(rku_luz, 0, 1)
make_rk(rkd_luz, 0, 0)


#define make_seg(_name, _first, _second)            \
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int d, \
    int s, \
    int segd,\
    int vec_len, \
    int seg_count, \
    int scratch,\
    int first,\
    int second)                          \
{\
    /* TODO */ \
    return; \
}

make_seg(rku_lez, 1, 1)
make_seg(rkd_lez, 1, 0)

/* ----------------------- double rank -----------------------*/

/* This function does all the double ranks: up, down, segmented,
 * unsegmented.  Algorithm is: 
 * 1. xor the elements of the data to handle signed numbers correctly:
 *   if upward rank, then flip the sign bit
 *   if down rank, then flip all the bits except the sign bit
 * 2. Use field_rank to do the rank
 * 3. Restore the flipped bits from 1
 * 4. If the sort is segemented, then use the segment number to do
 *    an additional rank.
 * 5. Get the final rank out of tmp
 */


make_rk(rku_lud, 0, 1)
make_rk(rkd_lud, 0, 0)

make_seg(rku_led, 1, 1)
make_seg(rkd_led, 1, 0)
