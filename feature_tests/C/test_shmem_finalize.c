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
 * Calls tested
 * shmem_finalize
 *
 * All PEs put a 64-bit value to its right neighbor and expect the
 * transfer to complete after shmem_finalize
 */

#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <shmem.h>
#include <stdint.h>

uint64_t dest = -9;

int
main (int argc, char **argv)
{
    int nextpe;
    int me, npes;
    uint64_t src;

    int fail_count = 0;

    shmem_init ();
    me = shmem_my_pe ();
    npes = shmem_n_pes ();

    if (npes > 1) {

        src = (uint64_t)((me+1)%npes);
        nextpe = (me+1)%npes;
        shmem_put64 (&dest, &src, 1, nextpe);

        shmem_finalize ();

        if (me == 0) {
          /* Check for completion of all communication */
          if ((int)dest == me)
              printf ("Test shmem_finalize: Passed\n");
          else {
              printf ("Test shmem_finalize: Failed\n");
              fail_count++;
          }
        }

        if (me == 0) {
            if (fail_count == 0)
                printf("All Tests Passed\n");
            else
                printf("%d Tests Failed\n", fail_count);
        }
    }
    else {
        printf ("Number of PEs must be > 1 to test shmem finalize, test skipped\n");

        shmem_finalize ();
    }

    return 0;
}
