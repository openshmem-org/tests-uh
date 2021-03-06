/*
 *
 * Copyright (c) 2011 - 2015
 *   University of Houston System and UT-Battelle, LLC.
 * Copyright (c) 2009 - 2015
 *   Silicon Graphics International Corp.  SHMEM is copyrighted
 *   by Silicon Graphics International Corp. (SGI) The OpenSHMEM API
 *   (shmem) is released by Open Source Software Solutions, Inc., under an
 *   agreement with Silicon Graphics International Corp. (SGI).
 * Copyright (c) 2017
 *    Los Alamos National Security, LLC. All rights reserved.
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
 * Performance test for shmem_fcollect32
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <shmem.h>

static long pSyncA[_SHMEM_COLLECT_SYNC_SIZE];
static long pSyncB[_SHMEM_COLLECT_SYNC_SIZE];

#define N_ELEMENTS 4

int
main (void)
{
    int i;
    int *target;
    int *source;
    int me, npes;
    struct timeval start, end;
    long time_taken, start_time, end_time;

    shmem_init ();
    me = shmem_my_pe ();
    npes = shmem_n_pes ();

    source = (int *) shmem_malloc (N_ELEMENTS * sizeof (*source));

    time_taken = 0;

    for (i = 0; i < N_ELEMENTS; i += 1) {
        source[i] = (i + 1) * 10 + me;
    }
    target = (int *) shmem_malloc (N_ELEMENTS * sizeof (*target) * npes);
    for (i = 0; i < N_ELEMENTS * npes; i += 1) {
        target[i] = -90;
    }
    for (i = 0; i < _SHMEM_COLLECT_SYNC_SIZE; i += 1) {
        pSyncA[i] = _SHMEM_SYNC_VALUE;
        pSyncB[i] = _SHMEM_SYNC_VALUE;
    }
    shmem_barrier_all ();

    for (i = 0; i < 10000; i++) {
        gettimeofday (&start, NULL);

        start_time = (start.tv_sec * 1000000.0) + start.tv_usec;

        /* alternate between 2 pSync arrays to synchronize consequent
           collectives of even and odd iterations */
        if (i % 2) {
            shmem_fcollect32 (target, source, N_ELEMENTS, 0, 0, npes, pSyncA);
        }
        else {
            shmem_fcollect32 (target, source, N_ELEMENTS, 0, 0, npes, pSyncB);
        }

        gettimeofday (&end, NULL);

        end_time = (end.tv_sec * 1000000.0) + end.tv_usec;
        if (me == 0) {
            time_taken = time_taken + (end_time - start_time);
        }

    }
    if (me == 0) {
        printf
            ("Time required to collect %d bytes of data, with %d PEs is %ld microseconds\n",
             (4 * N_ELEMENTS * npes), npes, time_taken / 10000);
    }

    shmem_barrier_all ();
    shmem_free (target);
    shmem_free (source);

    shmem_finalize ();

    return 0;
}
