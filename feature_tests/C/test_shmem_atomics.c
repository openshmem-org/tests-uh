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



/* Tests all atomics
 * shmem_int_swap, shmem_float_swap, shmem_long_swap, shmem_double_swap, shmem_longlong_swap,
 * shmem_longlong_cswap, shmem_long_cswap, shmem_int_cswap,
 * shmem_long_fadd, shmem_int_fadd,  shmem_longlong_fadd,
 * shmem_long_finc, shmem_int_finc, shmem_longlong_finc,
 */

#include <stdio.h>

#include <shmem.h>

int success1_p2;
int success2_p2;
int success3_p2;
int success4_p2;
int success5_p2;

int
main ()
{
    int me, npes;

    int *dest1;
    float *dest2;
    long *dest3;
    double *dest4;
    long long *dest5;

    int swapped_val1, new_val1;
    float swapped_val2, new_val2;
    long swapped_val3, new_val3;
    double swapped_val4, new_val4;
    long long swapped_val5, new_val5;

    int success = 1;
    int success1_p1;
    int success2_p1;
    int success3_p1;
    int success4_p1;
    int success5_p1;

    int *fail_count;
    int fail_count_remote;

    shmem_init ();
    me = shmem_my_pe ();
    npes = shmem_n_pes ();

    shmem_barrier_all ();

    /* Checks if there are atleast 2 executing PEs */

    if (npes > 1) {

        dest1 = (int *) shmem_malloc (sizeof (*dest1));
        dest2 = (float *) shmem_malloc (sizeof (*dest2));
        dest3 = (long *) shmem_malloc (sizeof (*dest3));
        dest4 = (double *) shmem_malloc (sizeof (*dest4));
        dest5 = (long long *) shmem_malloc (sizeof (*dest5));

        fail_count = (int *) shmem_malloc (sizeof (int));
        *fail_count = 0;

        *dest1 = *dest2 = *dest3 = *dest4 = *dest5 = me;
        new_val1 = new_val2 = new_val3 = new_val4 = new_val5 = me;
        success1_p1 = success1_p2 = success2_p1 = success2_p2 = success3_p1 =
            success3_p2 = success4_p1 = success4_p2 = success5_p1 =
            success5_p2 = -1;

        shmem_barrier_all ();

        swapped_val1 = shmem_int_swap (dest1, new_val1, (me + 1) % npes);
        swapped_val2 = shmem_float_swap (dest2, new_val2, (me + 1) % npes);
        swapped_val3 = shmem_long_swap (dest3, new_val3, (me + 1) % npes);
        swapped_val4 = shmem_double_swap (dest4, new_val4, (me + 1) % npes);
        swapped_val5 = shmem_longlong_swap (dest5, new_val5, (me + 1) % npes);


        /* To validate the working of swap we need to check the value received
           at the PE that initiated the swap as well as the dest PE */

        if (me == 0) {
            if (swapped_val1 == 1) {
                success1_p1 = 1;
            }
            if (swapped_val2 == 1) {
                success2_p1 = 1;
            }
            if (swapped_val3 == 1) {
                success3_p1 = 1;
            }
            if (swapped_val4 == 1) {
                success4_p1 = 1;
            }
            if (swapped_val5 == 1) {
                success5_p1 = 1;
            }
        }

        if (me == 1) {
            if (*dest1 == 0) {
                shmem_int_put (&success1_p2, &success, 1, 0);
            }
            if (*dest2 == 0) {
                shmem_int_put (&success2_p2, &success, 1, 0);
            }
            if (*dest3 == 0) {
                shmem_int_put (&success3_p2, &success, 1, 0);
            }
            if (*dest4 == 0) {
                shmem_int_put (&success4_p2, &success, 1, 0);
            }
            if (*dest5 == 0) {
                shmem_int_put (&success5_p2, &success, 1, 0);
            }
        }

        shmem_barrier_all ();

        if (me == 0) {
            if (success1_p1 && success1_p2) {
                printf ("Test shmem_int_swap: Passed\n");
            }
            else {
                printf ("Test shmem_int_swap: Failed\n");
                (*fail_count)++;
            }

            if (success2_p1 && success2_p2) {
                printf ("Test shmem_float_swap: Passed\n");
            }
            else {
                printf ("Test shmem_float_swap: Failed\n");
                (*fail_count)++;
            }

            if (success3_p1 && success3_p2) {
                printf ("Test shmem_long_swap: Passed\n");
            }
            else {
                printf ("Test shmem_long_swap: Failed\n");
                (*fail_count)++;
            }

            if (success4_p1 && success4_p2) {
                printf ("Test shmem_double_swap: Passed\n");
            }
            else {
                printf ("Test shmem_double_swap: Failed\n");
                (*fail_count)++;
            }

            if (success5_p1 && success5_p2) {
                printf ("Test shmem_longlong_swap: Passed\n");
            }
            else {
                printf ("Test shmem_longlong_swap: Failed\n");
                (*fail_count)++;
            }

        }
        shmem_barrier_all ();



        /* Test conditional swaps shmem_longlong_cswap, shmem_long_cswap,
           shmem_int_cswap, */

        *dest1 = *dest3 = *dest5 = me;
        new_val1 = new_val3 = new_val5 = me;
        success1_p1 = success1_p2 = success3_p1 = success3_p2 = success5_p1 =
            success5_p2 = -1;

        shmem_barrier_all ();

        swapped_val1 = shmem_int_cswap (dest1, me + 1, (long) me, 1);
        swapped_val3 = shmem_long_cswap (dest3, me + 1, (long) me, 1);
        swapped_val5 = shmem_longlong_cswap (dest5, me + 1, (long) me, 1);


        /* To validate the working of conditionalswap we need to check the
           value received at the PE that initiated the conditional swap as
           well as the dest PE */

        if (me == 0) {
            if (swapped_val1 == 1) {
                success1_p1 = 1;
            }

            if (swapped_val3 == 1) {
                success3_p1 = 1;
            }

            if (swapped_val5 == 1) {
                success5_p1 = 1;
            }
        }

        if (me == 1) {
            if (*dest1 == 0) {
                shmem_int_put (&success1_p2, &success, 1, 0);
            }

            if (*dest3 == 0) {
                shmem_int_put (&success3_p2, &success, 1, 0);
            }

            if (*dest5 == 0) {
                shmem_int_put (&success5_p2, &success, 1, 0);
            }
        }

        shmem_barrier_all ();

        if (me == 0) {
            if (success1_p1 && success1_p2) {
                printf ("Test shmem_int_cswap: Passed\n");
            }
            else {
                printf ("Test shmem_int_cswap: Failed\n");
                (*fail_count)++;
            }

            if (success3_p1 && success3_p2) {
                printf ("Test shmem_long_cswap: Passed\n");
            }
            else {
                printf ("Test shmem_long_cswap: Failed\n");
                (*fail_count)++;
            }

            if (success5_p1 && success5_p2) {
                printf ("Test shmem_longlong_cswap: Passed\n");
            }
            else {
                printf ("Test shmem_longlong_cswap: Failed\n");
                (*fail_count)++;
            }

        }
        shmem_barrier_all ();

        /* Test shmem_long_fadd, shmem_int_fadd, shmem_longlong_fadd */

        *dest1 = *dest3 = *dest5 = me;
        new_val1 = new_val3 = new_val5 = me;
        success1_p1 = success1_p2 = success3_p1 = success3_p2 = success5_p1 =
            success5_p2 = -1;

        shmem_barrier_all ();

        swapped_val1 = shmem_int_fadd (dest1, 1, 0);
        swapped_val3 = shmem_long_fadd (dest3, 1, 0);
        swapped_val5 = shmem_longlong_fadd (dest5, 1, 0);


        /* To validate the working of fetch and add we need to check the old
           value received at the PE that initiated the fetch and increment as
           well as the new value on the dest PE */

        if (me != 0) {
            if (swapped_val1 == 0) {
                success1_p1 = 1;
            }

            if (swapped_val3 == 0) {
                success3_p1 = 1;
            }

            if (swapped_val5 == 0) {
                success5_p1 = 1;
            }
        }

        if (me == 0) {
            if (*dest1 == npes - 1) {
                shmem_int_put (&success1_p2, &success, 1, npes - 1);
            }

            if (*dest3 == npes - 1) {
                shmem_int_put (&success3_p2, &success, 1, npes - 1);
            }

            if (*dest5 == npes - 1) {
                shmem_int_put (&success5_p2, &success, 1, npes - 1);
            }
        }

        shmem_barrier_all ();

        if (me == npes - 1) {
            if (success1_p1 && success1_p2) {
                printf ("Test shmem_int_fadd: Passed\n");
            }
            else {
                printf ("Test shmem_int_fadd: Failed\n");
                (*fail_count)++;
            }

            if (success3_p1 && success3_p2) {
                printf ("Test shmem_long_fadd: Passed\n");
            }
            else {
                printf ("Test shmem_long_fadd: Failed\n");
                (*fail_count)++;
            }

            if (success5_p1 && success5_p2) {
                printf ("Test shmem_longlong_fadd: Passed\n");
            }
            else {
                printf ("Test shmem_longlong_fadd: Failed\n");
                (*fail_count)++;
            }

        }
        shmem_barrier_all ();

        /* Test shmem_long_finc, shmem_int_finc, shmem_longlong_finc */

        *dest1 = *dest3 = *dest5 = me;
        new_val1 = new_val3 = new_val5 = me;
        success1_p1 = success1_p2 = success3_p1 = success3_p2 = success5_p1 =
            success5_p2 = -1;

        shmem_barrier_all ();

        swapped_val1 = shmem_int_finc (dest1, 0);
        swapped_val3 = shmem_long_finc (dest3, 0);
        swapped_val5 = shmem_longlong_finc (dest5, 0);


        /* To validate the working of fetch and increment we need to check the
           old value received at the PE that initiated the fetch and increment
           as well as the new value on the dest PE */

        if (me != 0) {
            if (swapped_val1 == 0) {
                success1_p1 = 1;
            }

            if (swapped_val3 == 0) {
                success3_p1 = 1;
            }

            if (swapped_val5 == 0) {
                success5_p1 = 1;
            }
        }

        if (me == 0) {
            if (*dest1 == npes - 1) {
                shmem_int_put (&success1_p2, &success, 1, npes - 1);
            }

            if (*dest3 == npes - 1) {
                shmem_int_put (&success3_p2, &success, 1, npes - 1);
            }

            if (*dest5 == npes - 1) {
                shmem_int_put (&success5_p2, &success, 1, npes - 1);
            }
        }

        shmem_barrier_all ();

        if (me == npes - 1) {
            if (success1_p1 && success1_p2) {
                printf ("Test shmem_int_finc: Passed\n");
            }
            else {
                printf ("Test shmem_int_finc: Failed\n");
                (*fail_count)++;
            }

            if (success3_p1 && success3_p2) {
                printf ("Test shmem_long_finc: Passed\n");
            }
            else {
                printf ("Test shmem_long_finc: Failed\n");
                (*fail_count)++;
            }

            if (success5_p1 && success5_p2) {
                printf ("Test shmem_longlong_finc: Passed\n");
            }
            else {
                printf ("Test shmem_longlong_finc: Failed\n");
                (*fail_count)++;
            }
        }

        shmem_barrier_all ();

        if (me == 0) {
            fail_count_remote = shmem_int_g(fail_count, npes - 1);

            *fail_count += fail_count_remote;

            if (*fail_count == 0)
                printf("All Tests Passed\n");
            else
                printf("%d Tests Failed\n", *fail_count);
        }

        shmem_barrier_all();

        shmem_free (dest1);
        shmem_free (dest2);
        shmem_free (dest3);
        shmem_free (dest4);
        shmem_free (dest5);
        shmem_free (fail_count);

    }
    else {
        printf
            ("Number of PEs must be > 1 to test shmem atomics, test skipped\n");
    }

    shmem_finalize ();

    return 0;
}
