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



/* Test whether various types of variables are accessible
 * Test if all PEs are accessible
 */

#include <stdio.h>
#include <shmem.h>

static int
check_it (void *addr)
{
    return shmem_addr_accessible (addr, 1);
}

long global_dest;
static int static_dest;

int
main (int argc, char *argv[])
{
    long local_dest;
    int *shm_dest;
    int me, npes, i;
    int pe_acc_success = 0;
    int fail_count = 0;

    shmem_init ();
    me = shmem_my_pe ();
    npes = shmem_n_pes ();

    shm_dest = (int *) shmem_malloc (sizeof (int));

    shmem_barrier_all ();

    if (me == 0) {

        if (!check_it (&global_dest)) {   /* long global: yes */
            printf ("Test Global Address Accessible: Failed\n");
            fail_count++;
        }
        else {
            printf ("Test Global Address Accessible: Passed\n");
        }
        if (!check_it (&static_dest)) {   /* static int global: yes */
            printf ("Test Static Global Address Accessible: Failed\n");
            fail_count++;
        }
        else {
            printf ("Test Static Global Address Accessible: Passed\n");
        }
        if (check_it (&local_dest)) { /* main() stack: no */
            printf ("Test Stack Address Accessible: Failed\n");
            fail_count++;
        }
        else {
            printf ("Test Stack Address Accessible: Passed\n");
        }
        if (!check_it (shm_dest)) {   /* shmem_malloc: yes */
            printf ("Test Shmalloc-ed Address Accessible: Failed\n");
            fail_count++;
        }
        else {
            printf ("Test Shmalloc-ed Address Accessible: Passed\n");
        }


        for (i = 1; i < npes; i++) {

            if (shmem_pe_accessible (i) != 1) {
                pe_acc_success = 1;
            }

        }
        if (pe_acc_success == 1) {
            printf ("Test shmem_pe_accessible: Failed\n");
            fail_count++;
        }
        else {
            printf ("Test shmem_pe_accessible: Passed\n");
        }

        if (fail_count == 0)
            printf("All Tests Passed\n");
        else
            printf("%d Tests Failed\n", fail_count);
    }

    shmem_free (shm_dest);

    shmem_finalize ();

    return 0;
}
