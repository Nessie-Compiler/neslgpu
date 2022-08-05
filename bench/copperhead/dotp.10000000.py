from copperhead import *
import plac
import time

@cu
def dot_product(x, y):
    return sum(map(lambda xi,yi: xi*yi, x, y))

@plac.annotations(n="Length of array to test dotp with, defaults to 1000000")
def main(n=10000000):
    """Tests Copperhead dotp on GPU."""
    def random_numbers(n, bits=8):
        import random
        return CuArray([np.float32(random.getrandbits(bits)) for i in xrange(n)])

    def test_dotp(S, n, trials=1, bits=8):
        npass, nfail = 0,0
        name = S.__name__

        for i in xrange(trials):
            xs  = random_numbers(n, bits)
            ys  = random_numbers(n, bits)
            t1 = time.time()
            data_out = S(xs, ys)
            t2 = time.time()

            print '%0.3f ms' % ((t2-t1)*1000.0)
        print

    print
    
    # print "---- Checking python results (n=10000000) ----"
    # with places.here:
    #     test_dotp(dot_product,    n)

    print "---- Checking GPU results (n=10000000) ----"
    with places.gpu0:
        test_dotp(dot_product,    n)

if __name__ == '__main__':
    plac.call(main)
