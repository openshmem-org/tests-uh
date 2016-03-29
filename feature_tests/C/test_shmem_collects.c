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


/* Tests shmem_fcollect32 call
 * Each PE contributes 4 elements
 *
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <shmem.h>

static long pSync[_SHMEM_COLLECT_SYNC_SIZE];

static int src1[4] = { 11, 12, 13, 14 };
static long src2[4] = { 101, 102, 103, 104 };
static int src3[4] = { 1, 2, 3, 4 };
static long src4[4] = { 11, 12, 13, 14 };


int npes;
int me;

volatile int x = 1;

int
main (void)
{
    int i, j, k, success;
    int dst_len, x, y;
    int *dst1, *dst3;
    long *dst2, *dst4;
    int *compare_dst1, *compare_dst3;
    long *compare_dst2, *compare_dst4;

    int fail_count = 0;

    shmem_init ();
    npes = shmem_n_pes ();
    me = shmem_my_pe ();

    for (i = 0; i < _SHMEM_BCAST_SYNC_SIZE; i += 1) {
        pSync[i] = _SHMEM_SYNC_VALUE;
    }

    if (npes > 1) {

        dst1 = (int *) shmem_malloc (npes * 4 * sizeof (*dst1));
        dst2 = (long *) shmem_malloc (npes * 4 * sizeof (*dst2));
        dst3 = (int *) shmem_malloc (npes * 4 * sizeof (*dst3));
        dst4 = (long *) shmem_malloc (npes * 4 * sizeof (*dst4));

        compare_dst1 = (int *) malloc (npes * 4 * sizeof (*compare_dst1));
        compare_dst2 = (long *) malloc (npes * 4 * sizeof (*compare_dst2));
        compare_dst3 = (int *) malloc (npes * 4 * sizeof (*compare_dst3));
        compare_dst4 = (long *) malloc (npes * 4 * sizeof (*compare_dst4));

        /* Test shmem_fcollect32 */
        /* Create the output of fcollect32 and save in compare_dst array */
        success = 0;
        j = 0;
        k = 0;
        for (i = 0; i < npes; i++) {
            for (j = 0; j < 4; j++) {
                compare_dst1[k] = src1[j];
                k++;
            }
        }

        for (i = 0; i < (npes * 4); i++) {
            dst1[i] = -1;
        }

        shmem_barrier_all ();

        shmem_fcollect32 (dst1, src1, 4, 0, 0, npes, pSync);


        if (me == 0) {
            for (i = 0; i < npes * 4; i += 1) {
                if (dst1[i] != compare_dst1[i]) {
                    success = 1;
                }
            }
            if (success == 1) {
                printf ("Test shmem_fcollect32: Failed\n");
                fail_count++;
            }
            else {
                printf ("Test shmem_fcollect32: Passed\n");
            }
        }

        shmem_barrier_all ();


        /* Create the output of fcollect64 and save in compare_dest array */
        success = 0;
        j = 0;
        k = 0;
        for (i = 0; i < npes; i++) {
            for (j = 0; j < 4; j++) {
                compare_dst2[k] = src2[j];
                k++;
            }
        }

        for (i = 0; i < (npes * 4); i++) {
            dst2[i] = -1;
        }

        shmem_barrier_all ();

        shmem_fcollect64 (dst2, src2, 4, 0, 0, npes, pSync);


        if (me == 0) {
            for (i = 0; i < npes * 4; i += 1) {
                if (dst2[i] != compare_dst2[i]) {
                    success = 1;
                }
            }
            if (success == 1) {
                printf ("Test shmem_fcollect64: Failed\n");
                fail_count++;
            }
            else {
                printf ("Test shmem_fcollect64: Passed\n");
            }
        }

        /* Test collect32 */
        success = 0;
        /* Decide the length of the destination array */
        x = npes / 4;
        y = npes % 4;

        switch (y) {
        case 1:
            dst_len = x * 10 + 1;
            break;
        case 2:
            dst_len = x * 10 + 3;
            break;
        case 3:
            dst_len = x * 10 + 6;
            break;
        default:
            dst_len = x * 10;
            break;
        }

        /* Create the output of collect32 and save in compare_dst array */

        j = 0;
        k = 0;
        for (i = 0; i < npes; i++) {
            for (j = 0; j < (i % 4 + 1); j++) {
                compare_dst3[k] = i * 10 + src3[j];
                /* printf("compare_dst[%d] = %d \n",k,compare_dst[k]); */
                k++;
            }
        }

        for (i = 0; i < dst_len; i++) {
            dst3[i] = -1;
        }

        for (i = 0; i < 4; i++) {
            src3[i] = me * 10 + src3[i];
        }

        shmem_barrier_all ();

        shmem_collect32 (dst3, src3, (me % 4 + 1), 0, 0, npes, pSync);
        shmem_barrier_all ();

        if (me == 0) {
            for (i = 0; i < dst_len; i += 1) {
                if (dst3[i] != compare_dst3[i]) {
                    success = 1;
                }
            }
            if (success == 1) {
                printf ("Test shmem_collect32: Failed\n");
                fail_count++;
            }
            else {
                printf ("Test shmem_collect32: Passed\n");
            }
        }

        shmem_barrier_all ();

        /* Test shmem_collect64 */
        success = 0;
        if (y == 1)
            dst_len = x * 10 + 1;
        else if (y == 2)
            dst_len = x * 10 + 3;
        else if (y == 3)
            dst_len = x * 10 + 6;
        else
            dst_len = x * 10;

        /* Create the output of collect64 and save in compare_dst array */

        j = 0;
        k = 0;
        for (i = 0; i < npes; i++) {
            for (j = 0; j < (i % 4 + 1); j++) {
                compare_dst4[k] = src4[j];
                /* printf("compare_dst[%d] = %d \n",k,compare_dst[k]); */
                k++;
            }
        }


        for (i = 0; i < dst_len; i++) {
            dst4[i] = -1;
        }

        shmem_barrier_all ();

        shmem_collect64 (dst4, src4, (me % 4 + 1), 0, 0, npes, pSync);
        shmem_barrier_all ();

        if (me == 0) {
            for (i = 0; i < dst_len; i += 1) {
                if (dst4[i] != compare_dst4[i]) {
                    success = 1;
                }
            }
            if (success == 1) {
                printf ("Test shmem_collect64: Failed\n");
                fail_count++;
            }
            else {
                printf ("Test shmem_collect64: Passed\n");
            }
        }

        if (me == 0) {
            if (fail_count == 0)
                printf("All Tests Passed\n");
            else
                printf("%d Tests Failed\n", fail_count);
        }

        free (compare_dst4);
        free (compare_dst3);
        free (compare_dst2);
        free (compare_dst1);

        shmem_free (dst4);
        shmem_free (dst3);
        shmem_free (dst2);
        shmem_free (dst1);

    }
    else {
        printf ("Number of PEs must be > 1 to test collects, test skipped\n");
    }

    shmem_finalize ();

    return 0;
}
