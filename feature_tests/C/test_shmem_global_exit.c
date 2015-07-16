/*
 *
 * Copyright (c) 2011 - 2015 
 *   University of Houston System and UT-Battelle, LLC.
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
 * o Neither the name of the University of Houston System, Oak Ridge
 *   National Laboratory nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
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
 * shmem_global_exit
 *
 * All PEs sleep for a finite number of seconds while PE0 
 * calls shmem_global_exit.
 */

#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <shmem.h>
#include <unistd.h>
int
main (int argv, char **argc)
{
    int nextpe;
    int me, npes;
    int status=99;

    shmem_init ();
    me = shmem_my_pe ();
    npes = shmem_n_pes ();
    
    if(me==0) {
        shmem_global_exit(status);
    }
    else {
        sleep(me*3);
    }
    shmem_barrier_all();

    /* PE-0 reach this point if shmem_global_exit is a no-op */
    if(me==0) {
      printf ("Test shmem_global_exit: Failed\n");
    }

    shmem_finalize ();

    return 0;
}
