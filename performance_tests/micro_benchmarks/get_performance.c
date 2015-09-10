/*
 *
 * Copyright (c) 2011 - 2015
 *   University of Houston System and UT-Battelle, LLC.
 * Copyright (c) 2009 - 2015
 *   Silicon Graphics International Corp.  SHMEM is copyrighted
 *   by Silicon Graphics International Corp. (SGI) The OpenSHMEM API
 *   (shmem) is released by Open Source Software Solutions, Inc., under an
 *   agreement with Silicon Graphics International Corp. (SGI).
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
 * Performance test for shmem_XX_get (latency and bandwidth)
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <shmem.h>

long double time_taken;

long pSync[_SHMEM_REDUCE_SYNC_SIZE];
long double pWrk[_SHMEM_REDUCE_MIN_WRKDATA_SIZE];

int
main (void)
{
    int j;
    long int i;
    int *target;
    int *source;
    int me, npes;
    int nxtpe;
    struct timeval start, end;
    long double start_time, end_time;

    const int N_ELEMENTS = (4194304 * 2) / sizeof (int);

    shmem_init ();
    me = shmem_my_pe ();
    npes = shmem_n_pes ();

    for (i = 0; i < _SHMEM_BCAST_SYNC_SIZE; i += 1) {
        pSync[i] = _SHMEM_SYNC_VALUE;
    }
    nxtpe = (me + 1) % npes;
    source = (int *) shmem_malloc (N_ELEMENTS * sizeof (*source));
    target = (int *) shmem_malloc (N_ELEMENTS * sizeof (*target));

    if (me == 0) {
        printf
            ("Get Performance test results:\nSize (Bytes)\t\tTime (Microseconds)\t\tBandwidth (Bytes/Second)\n");
    }

    for (i = 0; i < N_ELEMENTS; i += 1) {
        source[i] = i + 1;
        target[i] = -90;
    }
    shmem_barrier_all ();

    /* For int put we take average of all the times realized by a pair of PEs,
       thus reducing effects of physical location of PEs */
    for (i = 1; i <= N_ELEMENTS; i = i * 2) {
        time_taken = 0;

        for (j = 0; j < 10000; j++) {
            gettimeofday (&start, NULL);

            start_time = (start.tv_sec * 1000000.0) + start.tv_usec;

            shmem_int_get (target, source, i, nxtpe);

            gettimeofday (&end, NULL);

            end_time = (end.tv_sec * 1000000.0) + end.tv_usec;

            time_taken = time_taken + (end_time - start_time);

        }
        shmem_longdouble_sum_to_all (&time_taken, &time_taken, 1, 0, 0, npes,
                                     pWrk, pSync);
        shmem_barrier_all ();

        if (me == 0) {
            time_taken = time_taken / (npes * 10000);   /* Average time across
                                                           all PEs for one put */
            if (i * sizeof (i) < 1048576) {
                printf ("%ld \t\t\t\t %Lf\t\t\t\t %Lf\n", i * sizeof (i),
                        time_taken,
                        (i * sizeof (i)) / (time_taken * 1000000.0));
            }
            else {
                printf ("%ld \t\t\t %Lf\t\t\t\t %Lf\n", i * sizeof (i),
                        time_taken,
                        (i * sizeof (i)) / (time_taken * 1000000.0));
            }
        }

    }
    shmem_barrier_all ();

    shmem_free (target);
    shmem_free (source);

    shmem_finalize ();

    return 0;
}
