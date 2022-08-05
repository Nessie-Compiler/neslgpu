#
#  Copyright 2008-2010 NVIDIA Corporation
#  Copyright 2009-2010 University of California
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#

from copperhead import *
import plac
import time

@cu
def radix_sort_iteration(A, lsb):
    def delta(flag, ones_before, zeros_after):
        if flag==0:  return -ones_before
        else:        return +zeros_after

    
    flags = map(lambda x: (x>>lsb)&1, A)
    ones  = scan(op_add, flags)
    zeros = rscan(op_add, [f^1 for f in flags])
    
    offsets = map(delta, flags, ones, zeros)
        
    return permute(A, map(op_add, indices(A), offsets))

def radix_sort(A, bits, lsb):
    """
    Sort A using radix sort.
    
    Each element of A is assumed to be an integer.  The key used in
    sorting will be bits [lsb, lsb+bits).  For the general case, use
    bits=32 and lsb=0 to sort on all 32 bits.

    For sequences of length n with b-bit keys, this performs O(b*n) work.
    """
    for bit in xrange(lsb, bits):
        A = radix_sort_iteration(A, bit)
    return A

def radix_sort8(A):   return radix_sort(A, 8, 0)
def radix_sort32(A):  return radix_sort(A, 32, 0)

@plac.annotations(n="Length of array to test sort with, defaults to 1000000")
def main(n=500000):
    """Tests Copperhead radix sort in Python interpreter and on GPU."""
    def random_numbers(n, bits=8):
        import random
        return [int(random.getrandbits(bits)) for i in xrange(n)]

    def test_sort(S, n, trials=1, bits=8):
        npass, nfail = 0,0
        name = S.__name__

        for i in xrange(trials):
            data_in  = random_numbers(n, bits)
            gold     = sorted(data_in)
            t1 = time.time()
            data_out = S(data_in)
            t2 = time.time()

            if gold==data_out:
                npass = npass+1
            else:
                nfail = nfail+1

            print '%0.3f ms' % ((t2-t1)*1000.0)
        print

    print
    
    print "---- Checking Python implementations (n) ----"
    # with places.here:
    #     test_sort(radix_sort8,    n)
   

    print "---- Checking GPU results (n) ----"
    with places.gpu0:
        test_sort(radix_sort8,    n)

if __name__ == '__main__':
    plac.call(main)
