/*
 *
 * Copyright (c) 2011 - 2015
 *   University of Houston System and UT-Battelle, LLC.
 * Copyright (c) 2009 - 2015
 *   Silicon Graphics International Corp.  SHMEM is copyrighted
 *   by Silicon Graphics International Corp. (SGI) The OpenSHMEM API
 *   (shmem) is released by Open Source Software Solutions, Inc., under an
 *   agreement with Silicon Graphics International Corp. (SGI).
 * Copyright (c) 2016 Intel Corporation
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * o Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 *
 * o Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * o Neither the name of the University of Houston System, UT-Battelle, LLC
 *   nor the names of its contributors may be used to endorse or promote
 *   products derived from this software without specific prior written
 *   permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */


/*
 * Tests shmem_broadcast32 shmem_broadcast64 calls
 * PE 0 broadcasts to all other PEs
 * source and destination arrays are shmem_malloc-ed
 * Strided tests use active sets of all even PEs
 * PE 0 is the root,
 * and require a minimum of 3 PEs to test.
*/

#include <stdio.h>
#include <stdlib.h>

/* #include <mpp/shmem.h> */
#include <shmem.h>

long pSync[_SHMEM_BCAST_SYNC_SIZE];
int success32_root;
int success64_root;
int success32, success64;

int
main (void)
{
    int *targ;
    int *src;
    long *dest;
    long *source;
    int i, me, npes;

    int fail_count = 0;

    shmem_init ();
    me = shmem_my_pe ();
    npes = shmem_n_pes ();
    success32 = -9;
    success64 = -9;
    success32_root = -9;
    success64_root = -9;
    for (i = 0; i < _SHMEM_BCAST_SYNC_SIZE; i += 1) {
        pSync[i] = _SHMEM_SYNC_VALUE;
    }
    src = (int *) shmem_malloc (npes * sizeof (*src));
    targ = (int *) shmem_malloc (npes * sizeof (*targ));
    source = (long *) shmem_malloc (npes * sizeof (*source));
    dest = (long *) shmem_malloc (npes * sizeof (*dest));
    for (i = 0; i < npes; i += 1) {
        src[i] = i + 1;
        targ[i] = -999;
        source[i] = i + 1;
        dest[i] = -999;
    }

    shmem_barrier_all ();

    if (npes > 1) {

        /* Test shmem_broadcast32 */

        shmem_broadcast32 (targ, src, npes, 0, 0, 0, npes, pSync);

        if (me == 0) {
            for (i = 0; i < npes; i++) {
                if (targ[i] == -999) {
                    success32 = 0;
                }
            }
        }

        if (me == npes - 1) {
            for (i = 0; i < npes; i++) {
                if (targ[i] == (i + 1)) {
                    success32 = 0;
                }
            }
        }

        shmem_barrier_all ();
        if (me == 0) {
            shmem_int_get (&success32_root, &success32, 1, npes - 1);
            if (success32 == 0 && success32_root == 0) {
                printf ("Test shmem_broadcast32: Passed\n");
            }
            else {
                printf ("Test shmem_broadcast32: Failed\n");
                fail_count++;
            }
        }


        shmem_barrier_all ();

        /* Test shmem_broadcast64 */

        shmem_broadcast64 (dest, source, npes, 0, 0, 0, npes, pSync);

        if (me == 0) {
            for (i = 0; i < npes; i++) {
                if (dest[i] == -999) {
                    success64 = 0;
                }
            }
        }

        if (me == npes - 1) {
            for (i = 0; i < npes; i++) {
                if (dest[i] == (i + 1)) {
                    success64 = 0;
                }
            }
        }

        shmem_barrier_all ();

        if (me == 0) {
            shmem_int_get (&success64_root, &success64, 1, npes - 1 );
            if (success64 == 0 && success64_root == 0) {
                printf ("Test shmem_broadcast64: Passed\n");
            }
            else {
                printf ("Test shmem_broadcast64: Failed\n");
                fail_count++;
            }
        }

    }
    else {
        printf ("Number of PEs must be > 1 to test broadcast, test skipped\n");
    }

    shmem_barrier_all ();

    success32 = -9;
    success64 = -9;
    success32_root = -9;
    success64_root = -9;

    if (npes > 2) {

    for (i = 0; i < npes; i += 1) {
            targ[i] = -999;
            dest[i] = -999;
        }

        shmem_barrier_all ();
        if ((me % 2) == 0) {    /* Active set of all even PEs */
            if ((npes % 2) == 0) {
                shmem_broadcast32 (targ, src, npes, 0, 0, 1, npes / 2,
                                   pSync);
            }
            else {
                shmem_broadcast32 (targ, src, npes, 0, 0, 1, (npes + 1) / 2,
                                   pSync);
            }
        }

        if (me == 0) {
            for (i = 0; i < npes; i++) {
                if (targ[i] == -999) {
                    success32 = 0;
                }
            }
        }

        if (me == 2) {
            for (i = 0; i < npes; i++) {
                if (targ[i] == (i + 1)) {
                    success32 = 0;
                }
            }
        }

        shmem_barrier_all ();
        if (me == 0) {
            shmem_int_get (&success32_root, &success32, 1, 2);
            if (success32 == 0 && success32_root == 0) {
                printf ("Test strided shmem_broadcast32: Passed\n");
            }
            else {
                printf ("Test strided shmem_broadcast32: Failed\n");
                fail_count++;
            }
        }

        shmem_barrier_all ();

        /* Test strided shmem_broadcast64 */

        if ((me % 2) == 0) {
            if ((npes % 2) == 0) {
                shmem_broadcast64 (dest, source, npes, 0, 0, 1,
                                   npes / 2, pSync);
            }
            else {
                shmem_broadcast64 (dest, source, npes, 0, 0, 1,
                                   (npes + 1) / 2, pSync);
            }
        }

        if (me == 0) {
            for (i = 0; i < npes; i++) {
                if (dest[i] == -999) {
                    success64 = 0;
                }
            }
        }

        if (me == 2) {
            for (i = 0; i < npes; i++) {
                if (dest[i] == (i + 1)) {
                    success64 = 0;
                }
            }
        }

        shmem_barrier_all ();

        if (me == 0) {
            shmem_int_get (&success64_root, &success64, 1, 2);
            if (success64 == 0 && success64_root == 0) {
                printf ("Test strided shmem_broadcast64: Passed\n");
            }
            else {
                printf ("Test strided shmem_broadcast64: Failed\n");
                fail_count++;
            }
        }

        shmem_free (targ);
        shmem_free (src);
        shmem_free (dest);
        shmem_free (source);

    }
    else {
        if (me == 0) {
            printf ("Number of PEs must be > 2 to test strided broadcast, test skipped\n");
        }
    }

    if (me == 0) {
      if (fail_count == 0)
        printf("All Tests Passed\n");
      else
        printf("%d Tests Failed\n", fail_count);
    }

    shmem_finalize ();

    return 0;
}
