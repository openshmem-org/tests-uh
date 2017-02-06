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
 * Tests
 * shmem_int_and_to_all, shmem_long_and_to_all, shmem_longlong_and_to_all, shmem_short_and_to_all,
 * shmem_double_max_to_all, shmem_float_max_to_all, shmem_int_max_to_all, shmem_long_max_to_all,
 * shmem_longdouble_max_to_all, shmem_longlong_max_to_all, shmem_short_max_to_all,
 * shmem_double_min_to_all, shmem_float_min_to_all, shmem_int_min_to_all, shmem_long_min_to_all,
 * shmem_longdouble_min_to_all, shmem_longlong_min_to_all, shmem_short_min_to_all,
 * shmem_double_sum_to_all, shmem_float_sum_to_all, shmem_int_sum_to_all, shmem_long_sum_to_all,
 * shmem_longdouble_sum_to_all, shmem_longlong_sum_to_all, shmem_short_sum_to_all,
 * shmem_double_prod_to_all, shmem_float_prod_to_all, shmem_int_prod_to_all, shmem_long_prod_to_all,
 * shmem_longdouble_prod_to_all, shmem_longlong_prod_to_all, shmem_short_prod_to_all,
 * shmem_int_or_to_all, shmem_long_or_to_all, shmem_longlong_or_to_all, shmem_short_or_to_all,
 * shmem_int_xor_to_all, shmem_long_xor_to_all, shmem_longlong_xor_to_all, shmem_short_xor_to_all
 */

#include <stdio.h>
#include <string.h>
#include <limits.h>

#include <shmem.h>

long pSync[SHMEM_REDUCE_SYNC_SIZE];
long pSync1[SHMEM_REDUCE_SYNC_SIZE];

#define N 3

short src0[N], dst0[N];
int src1[N], dst1[N];
long src2[N], dst2[N];
float src3[N], dst3[N];
double src4[N], dst4[N];
long double src5[N], dst5[N];
long long src6[N], dst6[N];

short expected_result0;
int expected_result1;
long expected_result2;
float expected_result3;
double expected_result4;
long double expected_result5;
long long expected_result6;

#define MAX(x,y) (x) > (y) ? (x) : (y)
#define WRKDATA_SIZE MAX(N/2+1, SHMEM_REDUCE_MIN_WRKDATA_SIZE)

short pWrk0[WRKDATA_SIZE];
int pWrk1[WRKDATA_SIZE];
long pWrk2[WRKDATA_SIZE];
float pWrk3[WRKDATA_SIZE];
double pWrk4[WRKDATA_SIZE];
long double pWrk5[WRKDATA_SIZE];
long long pWrk6[WRKDATA_SIZE];

int
main ()
{
    int i, j;
    int me, npes;
    int success0, success1, success2, success3, success4, success5, success6;

    int fail_count = 0;
    int pe_bound=1;

    shmem_init ();
    me = shmem_my_pe ();
    npes = shmem_n_pes ();

    success0 = success1 = success2 = success3 = success4 = success5 =
        success6 = 0;

    i = 2;

    while (i < SHRT_MAX) {
      i = 2 * i;
      pe_bound++;
    }

    pe_bound--;

    if (npes < pe_bound)
        pe_bound = npes;

    for (i = 0; i < SHMEM_REDUCE_SYNC_SIZE; i += 1) {
        pSync[i] = SHMEM_SYNC_VALUE;
        pSync1[i] = SHMEM_SYNC_VALUE;
    }

    for (i = 0; i < N; i+= 1) {
          src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] =
            src6[i] = me + i;
    }

    /* Test MAX: shmem_double_max_to_all, shmem_float_max_to_all,
       shmem_int_max_to_all, shmem_long_max_to_all,
       shmem_longdouble_max_to_all, shmem_longlong_max_to_all,
       shmem_short_max_to_all */
    shmem_barrier_all ();

    shmem_short_max_to_all (dst0, src0, N, 0, 0, npes, pWrk0, pSync);
    shmem_int_max_to_all (dst1, src1, N, 0, 0, npes, pWrk1, pSync1);
    shmem_long_max_to_all (dst2, src2, N, 0, 0, npes, pWrk2, pSync);
    shmem_float_max_to_all (dst3, src3, N, 0, 0, npes, pWrk3, pSync1);
    shmem_double_max_to_all (dst4, src4, N, 0, 0, npes, pWrk4, pSync);
    shmem_longdouble_max_to_all (dst5, src5, N, 0, 0, npes, pWrk5, pSync1);
    shmem_longlong_max_to_all (dst6, src6, N, 0, 0, npes, pWrk6, pSync);


    if (me == 0) {
        for (i = 0, j = -1; i < N; i++, j++) {
            if (dst0[i] != npes + j)
                success0 = 1;
            if (dst1[i] != npes + j)
                success1 = 1;
            if (dst2[i] != npes + j)
                success2 = 1;
            if (dst3[i] != npes + j)
                success3 = 1;
            if (dst4[i] != npes + j)
                success4 = 1;
            if (dst5[i] != npes + j)
                success5 = 1;
            if (dst6[i] != npes + j)
                success6 = 1;
        }
        if (success0 == 1) {
            printf ("Reduction operation shmem_short_max_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_short_max_to_all: Passed\n");
        }
        if (success1 == 1) {
            printf ("Reduction operation shmem_int_max_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_int_max_to_all: Passed\n");
        }
        if (success2 == 1) {
            printf ("Reduction operation shmem_long_max_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_long_max_to_all: Passed\n");
        }
        if (success3 == 1) {
            printf ("Reduction operation shmem_float_max_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_float_max_to_all: Passed\n");
        }
        if (success4 == 1) {
            printf ("Reduction operation shmem_double_max_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_double_max_to_all: Passed\n");
        }
        if (success5 == 1) {
            printf
                ("Reduction operation shmem_longdouble_max_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf
                ("Reduction operation shmem_longdouble_max_to_all: Passed\n");
        }
        if (success6 == 1) {
            printf ("Reduction operation shmem_longlong_max_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_longlong_max_to_all: Passed\n");
        }

    }

    /* Test MIN: shmem_double_min_to_all, shmem_float_min_to_all,
       shmem_int_min_to_all, shmem_long_min_to_all,
       shmem_longdouble_min_to_all, shmem_longlong_min_to_all,
       shmem_short_min_to_all */
    success0 = success1 = success2 = success3 = success4 = success5 =
        success6 = 0;

    for (i = 0; i < N; i += 1) {
        src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] =
            src6[i] = me + i;
    }

    for (i = 0; i < N; i += 1) {
        dst0[i] = -9;
        dst1[i] = -9;
        dst2[i] = -9;
        dst3[i] = -9;
        dst4[i] = -9;
        dst5[i] = -9;
        dst6[i] = -9;
    }

    shmem_barrier_all ();

    shmem_short_min_to_all (dst0, src0, N, 0, 0, npes, pWrk0, pSync);
    shmem_int_min_to_all (dst1, src1, N, 0, 0, npes, pWrk1, pSync1);
    shmem_long_min_to_all (dst2, src2, N, 0, 0, npes, pWrk2, pSync);
    shmem_float_min_to_all (dst3, src3, N, 0, 0, npes, pWrk3, pSync1);
    shmem_double_min_to_all (dst4, src4, N, 0, 0, npes, pWrk4, pSync);
    shmem_longdouble_min_to_all (dst5, src5, N, 0, 0, npes, pWrk5, pSync1);
    shmem_longlong_min_to_all (dst6, src6, N, 0, 0, npes, pWrk6, pSync);


    if (me == 0) {
        for (i = 0; i < N; i++) {
            if (dst0[i] != i)
                success0 = 1;
            if (dst1[i] != i)
                success1 = 1;
            if (dst2[i] != i)
                success2 = 1;
            if (dst3[i] != i)
                success3 = 1;
            if (dst4[i] != i)
                success4 = 1;
            if (dst5[i] != i)
                success5 = 1;
            if (dst6[i] != i)
                success6 = 1;
        }
        if (success0 == 1) {
            printf ("Reduction operation shmem_short_min_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_short_min_to_all: Passed\n");
        }
        if (success1 == 1) {
            printf ("Reduction operation shmem_int_min_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_int_min_to_all: Passed\n");
        }
        if (success2 == 1) {
            printf ("Reduction operation shmem_long_min_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_long_min_to_all: Passed\n");
        }
        if (success3 == 1) {
            printf ("Reduction operation shmem_float_min_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_float_min_to_all: Passed\n");
        }
        if (success4 == 1) {
            printf ("Reduction operation shmem_double_min_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_double_min_to_all: Passed\n");
        }
        if (success5 == 1) {
            printf
                ("Reduction operation shmem_longdouble_min_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf
                ("Reduction operation shmem_longdouble_min_to_all: Passed\n");
        }
        if (success6 == 1) {
            printf ("Reduction operation shmem_longlong_min_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_longlong_min_to_all: Passed\n");
        }

    }

    /* Test SUM: shmem_double_sum_to_all, shmem_float_sum_to_all,
       shmem_int_sum_to_all, shmem_long_sum_to_all,
       shmem_longdouble_sum_to_all, shmem_longlong_sum_to_all,
       shmem_short_sum_to_all */
    success0 = success1 = success2 = success3 = success4 = success5 =
        success6 = 0;
    for (i = 0; i < N; i += 1) {
        src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] =
            src6[i] = me;
    }
    for (i = 0; i < N; i += 1) {
        dst0[i] = -9;
        dst1[i] = -9;
        dst2[i] = -9;
        dst3[i] = -9;
        dst4[i] = -9;
        dst5[i] = -9;
        dst6[i] = -9;
    }
    shmem_barrier_all ();

    shmem_short_sum_to_all (dst0, src0, N, 0, 0, npes, pWrk0, pSync);
    shmem_int_sum_to_all (dst1, src1, N, 0, 0, npes, pWrk1, pSync1);
    shmem_long_sum_to_all (dst2, src2, N, 0, 0, npes, pWrk2, pSync);
    shmem_float_sum_to_all (dst3, src3, N, 0, 0, npes, pWrk3, pSync1);
    shmem_double_sum_to_all (dst4, src4, N, 0, 0, npes, pWrk4, pSync);
    shmem_longdouble_sum_to_all (dst5, src5, N, 0, 0, npes, pWrk5, pSync1);
    shmem_longlong_sum_to_all (dst6, src6, N, 0, 0, npes, pWrk6, pSync);


    if (me == 0) {
        for (i = 0; i < N; i++) {
            if (dst0[i] != (npes * (npes - 1) / 2))
                success0 = 1;
            if (dst1[i] != (npes * (npes - 1) / 2))
                success1 = 1;
            if (dst2[i] != (npes * (npes - 1) / 2))
                success2 = 1;
            if (dst3[i] != (npes * (npes - 1) / 2))
                success3 = 1;
            if (dst4[i] != (npes * (npes - 1) / 2))
                success4 = 1;
            if (dst5[i] != (npes * (npes - 1) / 2))
                success5 = 1;
            if (dst6[i] != (npes * (npes - 1) / 2))
                success6 = 1;
        }
        if (success0 == 1) {
            printf ("Reduction operation shmem_short_sum_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_short_sum_to_all: Passed\n");
        }
        if (success1 == 1) {
            printf ("Reduction operation shmem_int_sum_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_int_sum_to_all: Passed\n");
        }
        if (success2 == 1) {
            printf ("Reduction operation shmem_long_sum_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_long_sum_to_all: Passed\n");
        }
        if (success3 == 1) {
            printf ("Reduction operation shmem_float_sum_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_float_sum_to_all: Passed\n");
        }
        if (success4 == 1) {
            printf ("Reduction operation shmem_double_sum_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_double_sum_to_all: Passed\n");
        }
        if (success5 == 1) {
            printf
                ("Reduction operation shmem_longdouble_sum_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf
                ("Reduction operation shmem_longdouble_sum_to_all: Passed\n");
        }
        if (success6 == 1) {
            printf ("Reduction operation shmem_longlong_sum_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_longlong_sum_to_all: Passed\n");
        }

    }

    /* Test AND: shmem_int_and_to_all, shmem_long_and_to_all,
       shmem_longlong_and_to_all, shmem_short_and_to_all, */
    success0 = success1 = success2 = success6 = 0;
    for (i = 0; i < N; i += 1) {
        src0[i] = src1[i] = src2[i] = src6[i] = me;
    }
    for (i = 0; i < N; i += 1) {
        dst0[i] = -9;
        dst1[i] = -9;
        dst2[i] = -9;
        dst6[i] = -9;
    }

    shmem_barrier_all ();

    shmem_short_and_to_all (dst0, src0, N, 0, 0, npes, pWrk0, pSync);
    shmem_int_and_to_all (dst1, src1, N, 0, 0, npes, pWrk1, pSync1);
    shmem_long_and_to_all (dst2, src2, N, 0, 0, npes, pWrk2, pSync);
    shmem_longlong_and_to_all (dst6, src6, N, 0, 0, npes, pWrk6, pSync1);


    if (me == 0) {
        for (i = 0; i < N; i++) {
            if (dst0[i] != 0)
                success0 = 1;
            if (dst1[i] != 0)
                success1 = 1;
            if (dst2[i] != 0)
                success2 = 1;
            if (dst6[i] != 0)
                success6 = 1;
        }
        if (success0 == 1) {
            printf ("Reduction operation shmem_short_and_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_short_and_to_all: Passed\n");
        }
        if (success1 == 1) {
            printf ("Reduction operation shmem_int_and_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_int_and_to_all: Passed\n");
        }
        if (success2 == 1) {
            printf ("Reduction operation shmem_long_and_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_long_and_to_all: Passed\n");
        }
        if (success6 == 1) {
            printf ("Reduction operation shmem_longlong_and_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_longlong_and_to_all: Passed\n");
        }

    }

    /* Test PROD: shmem_double_prod_to_all, shmem_float_prod_to_all,
       shmem_int_prod_to_all, shmem_long_prod_to_all,
       shmem_longdouble_prod_to_all, shmem_longlong_prod_to_all,
       shmem_short_prod_to_all, */

    success0 = success1 = success2 = success3 = success4 = success5 =
        success6 = 0;
    for (i = 0; i < N; i += 1) {
        if (me < pe_bound) {
            src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] =
              src6[i] = 2;
        }
        else {
            src0[i] = src1[i] = src2[i] = src3[i] = src4[i] = src5[i] =
              src6[i] = 1;
        }
    }
    for (i = 0; i < N; i += 1) {
        dst0[i] = -9;
        dst1[i] = -9;
        dst2[i] = -9;
        dst3[i] = -9;
        dst4[i] = -9;
        dst5[i] = -9;
        dst6[i] = -9;
    }

    expected_result0 = 1 << pe_bound;
    expected_result1 = 1 << pe_bound;
    expected_result2 = 1 << pe_bound;
    expected_result3 = 1 << pe_bound;
    expected_result4 = 1 << pe_bound;
    expected_result5 = 1 << pe_bound;
    expected_result6 = 1 << pe_bound;

    shmem_barrier_all ();

    shmem_short_prod_to_all (dst0, src0, N, 0, 0, npes, pWrk0, pSync);
    shmem_int_prod_to_all (dst1, src1, N, 0, 0, npes, pWrk1, pSync1);
    shmem_long_prod_to_all (dst2, src2, N, 0, 0, npes, pWrk2, pSync);
    shmem_float_prod_to_all (dst3, src3, N, 0, 0, npes, pWrk3, pSync1);
    shmem_double_prod_to_all (dst4, src4, N, 0, 0, npes, pWrk4, pSync);
    shmem_longdouble_prod_to_all (dst5, src5, N, 0, 0, npes, pWrk5, pSync1);
    shmem_longlong_prod_to_all (dst6, src6, N, 0, 0, npes, pWrk6, pSync);

    if (me == 0) {
        for (i = 0; i < N; i++) {
            if (dst0[i] != expected_result0)
                success0 = 1;

            if (dst1[i] != expected_result1)
                success1 = 1;

            if (dst2[i] != expected_result2)
                success2 = 1;

            if (dst3[i] != expected_result3)
                success3 = 1;

            if (dst4[i] != expected_result4)
                success4 = 1;

            if (dst5[i] != expected_result5)
                success5 = 1;

            if (dst6[i] != expected_result6)
                success6 = 1;
        }
        if (success0 == 1) {
            printf ("Reduction operation shmem_short_prod_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_short_prod_to_all: Passed\n");
        }
        if (success1 == 1) {
            printf ("Reduction operation shmem_int_prod_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_int_prod_to_all: Passed\n");
        }
        if (success2 == 1) {
            printf ("Reduction operation shmem_long_prod_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_long_prod_to_all: Passed\n");
        }
        if (success3 == 1) {
            printf ("Reduction operation shmem_float_prod_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_float_prod_to_all: Passed\n");
        }
        if (success4 == 1) {
            printf ("Reduction operation shmem_double_prod_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_double_prod_to_all: Passed\n");
        }
        if (success5 == 1) {
            printf
                ("Reduction operation shmem_longdouble_prod_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf
                ("Reduction operation shmem_longdouble_prod_to_all: Passed\n");
        }
        if (success6 == 1) {
            printf ("Reduction operation shmem_longlong_prod_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_longlong_prod_to_all: Passed\n");
        }

    }

    /* Test OR: shmem_int_or_to_all, shmem_long_or_to_all,
       shmem_longlong_or_to_all, shmem_short_or_to_all, */

    success0 = success1 = success2 = success6 = 0;
    for (i = 0; i < N; i += 1) {
        src0[i] = src1[i] = src2[i] = src6[i] = (me + 1) % 4;
    }
    for (i = 0; i < N; i += 1) {
        dst0[i] = -9;
        dst1[i] = -9;
        dst2[i] = -9;
        dst6[i] = -9;
    }

    shmem_barrier_all ();

    shmem_short_or_to_all (dst0, src0, N, 0, 0, npes, pWrk0, pSync);
    shmem_int_or_to_all (dst1, src1, N, 0, 0, npes, pWrk1, pSync1);
    shmem_long_or_to_all (dst2, src2, N, 0, 0, npes, pWrk2, pSync);
    shmem_longlong_or_to_all (dst6, src6, N, 0, 0, npes, pWrk6, pSync1);


    if (me == 0) {
        for (i = 0; i < N; i++) {
            if (dst0[i] != 3)
                success0 = 1;
            if (dst1[i] != 3)
                success1 = 1;
            if (dst2[i] != 3)
                success2 = 1;
            if (dst6[i] != 3)
                success6 = 1;
        }
        if (success0 == 1) {
            printf ("Reduction operation shmem_short_or_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_short_or_to_all: Passed\n");
        }
        if (success1 == 1) {
            printf ("Reduction operation shmem_int_or_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_int_or_to_all: Passed\n");
        }
        if (success2 == 1) {
            printf ("Reduction operation shmem_long_or_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_long_or_to_all: Passed\n");
        }
        if (success6 == 1) {
            printf ("Reduction operation shmem_longlong_or_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_longlong_or_to_all: Passed\n");
        }

    }

    /* Test XOR: shmem_int_xor_to_all, shmem_long_xor_to_all,
       shmem_longlong_xor_to_all, shmem_short_xor_to_all */

    success0 = success1 = success2 = success6 = 0;
    for (i = 0; i < N; i += 1) {
        src0[i] = src1[i] = src2[i] = src6[i] = me % 2;
    }
    for (i = 0; i < N; i += 1) {
        dst0[i] = -9;
        dst1[i] = -9;
        dst2[i] = -9;
        dst6[i] = -9;
    }
    int expected_result = ((int) (npes / 2) % 2);


    shmem_barrier_all ();

    shmem_short_xor_to_all (dst0, src0, N, 0, 0, npes, pWrk0, pSync);
    shmem_int_xor_to_all (dst1, src1, N, 0, 0, npes, pWrk1, pSync1);
    shmem_long_xor_to_all (dst2, src2, N, 0, 0, npes, pWrk2, pSync);
    shmem_longlong_xor_to_all (dst6, src6, N, 0, 0, npes, pWrk6, pSync1);

    if (me == 0) {
        for (i = 0; i < N; i++) {
            if (dst0[i] != expected_result)
                success0 = 1;
            if (dst1[i] != expected_result)
                success1 = 1;
            if (dst2[i] != expected_result)
                success2 = 1;
            if (dst6[i] != expected_result)
                success6 = 1;
        }
        if (success0 == 1) {
            printf ("Reduction operation shmem_short_xor_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_short_xor_to_all: Passed\n");
        }
        if (success1 == 1) {
            printf ("Reduction operation shmem_int_xor_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_int_xor_to_all: Passed\n");
        }
        if (success2 == 1) {
            printf ("Reduction operation shmem_long_xor_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_long_xor_to_all: Passed\n");
        }
        if (success6 == 1) {
            printf ("Reduction operation shmem_longlong_xor_to_all: Failed\n");
            fail_count++;
        }
        else {
            printf ("Reduction operation shmem_longlong_xor_to_all: Passed\n");
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
