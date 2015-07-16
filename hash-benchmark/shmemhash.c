/*
Copyright 2014 The University of Edinburgh

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/


/*
 *
 * Copyright (c) 2011 - 2014
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

#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <mpp/shmem.h>

/* global vars */

int npes, mype;
int hashlen, collisions, localhashlen, total_collisions;
int *hashtab, *hashcount;
long *pe_lock;

FILE *outfile;

int p_wrk[_SHMEM_REDUCE_MIN_WRKDATA_SIZE];
long p_sync[_SHMEM_REDUCE_SYNC_SIZE];

double get_Wtime()
{
    double t;
    struct timeval tv;

    gettimeofday(&tv, NULL);

    t = (tv.tv_sec*1000000LL + tv.tv_usec)/1000000.0;

    return t;
}


void inithash()
{
    npes = _num_pes();
    mype = _my_pe();

    hashlen = 2*NHASH*npes + 1;
    localhashlen = 2*NHASH + 1;
    collisions = 0;
    total_collisions = 0;

    hashtab   = shmalloc( localhashlen * sizeof *hashtab );
    hashcount = shmalloc( localhashlen * sizeof *hashcount );

    pe_lock   = shmalloc( npes * sizeof *pe_lock );

    memset( pe_lock, 0, npes * sizeof *pe_lock);
}

void finhash()
{
    shfree(hashtab);
    shfree(hashcount);
    shfree(pe_lock);
}

void hashlookup_with_amo(long o, long v)
{
    long lv;
    int dest_pe, dest_pos, local_count, local_hash;
    lv = v;

    while (1) {
        dest_pe = v / localhashlen;

        if (dest_pe*localhashlen < v) {
            dest_pe += 1;
        }
        dest_pe -= 1;

        dest_pos = v - (dest_pe)*localhashlen;

        local_hash = shmem_int_cswap(&hashtab[dest_pos-1], 0, o, dest_pe);

        if (local_hash == 0 || local_hash == o) {
            /* update successful, so increment count and return */
            shmem_int_inc(&hashcount[dest_pos-1], dest_pe);

            return;
        } else {
            /* its a collision */
            collisions += 1;
            v += 1;
            if (v > hashlen) {
                v = 1;
            }
        }
    }
}

void hashlookup(long o, long v)
{
    long lv;
    int dest_pe, dest_pos, local_count, local_hash;
    lv = v;

    while (1) {
        dest_pe = v / localhashlen;

        if (dest_pe*localhashlen < v) {
            dest_pe += 1;
        }
        dest_pe -= 1;

        dest_pos = v - (dest_pe)*localhashlen;

        /* lock the data */
        shmem_set_lock( &pe_lock[dest_pe] );

        shmem_int_get(&local_hash, &hashtab[dest_pos-1], 1, dest_pe);

        if (local_hash == 0) {
            /* insert the entry */
            int one = 1;
            shmem_int_put(&hashtab[dest_pos-1], (int *)&o, 1, dest_pe);
            shmem_int_put(&hashcount[dest_pos-1], &one, 1, dest_pe);

            /* unlock before return */
            shmem_clear_lock( &pe_lock[dest_pe] );
#ifdef _DEBUG
            fprintf(outfile, "%d writing %d to pe %d at position %d\n", mype, o, dest_pe, dest_pos);
#endif
            return;
        } else {
            /* check to see if it is a collision */
            if (local_hash == o) {
                /* its a repetition */
                shmem_int_inc(&hashcount[dest_pos-1], dest_pe);
                shmem_clear_lock( &pe_lock[dest_pe] );
#ifdef _DEBUG
                fprintf(outfile, "%d writing %d to pe %d at position %d\n", mype, o, dest_pe, dest_pos);
#endif
                return;
            } else {
                /* its a collision */
#ifdef _DEBUG
                fprintf(outfile, "%d writing %d to pe %d at position %d ***\n", mype, o, dest_pe, dest_pos);
                printf("%d:%d collision!\n", mype);
#endif
                collisions += 1;
                v += 1;
                if (v > hashlen) {
                    v = 1;
                }
                shmem_clear_lock( &pe_lock[dest_pe] );
            }
        }

    }
}


int main()
{
    int i,j;
    long pb,b,v;
    int M;
    double t1, t2;
    int rank_id;

#ifdef _DEBUG
    char filename[20];
#endif

    start_pes(0);

    for (i = 0; i < _SHMEM_REDUCE_SYNC_SIZE; i++) {
      p_sync[i] = _SHMEM_SYNC_VALUE;
    }

    inithash();
    shmem_barrier_all();

#ifdef _DEBUG
    sprintf(filename, "output.%d", mype);
    outfile = fopen(filename, "w+");
#endif

    t1 = get_Wtime();

    M = NHASH / 100;
    if (M == 0) M = 1;

    rank_id = mype + 1;
    for (j=1;j<=2;j++) {
        fresetvalue_(&npes, &rank_id);

#ifdef PROGRESS_BAR
        if (mype == 0) {
          printf("Pass %d: ", j);
        }
#endif

        for (i = 1; i <= NHASH; i++) {
            long n = npes;
            fnew_(&pb, &b, &v, &n);
#ifndef USE_AMO
            hashlookup(b,v);
#else
            hashlookup_with_amo(b,v);
#endif

#ifdef PROGRESS_BAR
            if (mype == 0 && i%M == 1) {
              printf(".");
              fflush(stdout);
            }
#endif

        }
#ifdef PROGRESS_BAR
            if (mype == 0) {
              printf(" DONE!\n");
            }
#endif
        shmem_barrier_all();
    }

    shmem_barrier_all();

    t2 = get_Wtime();

    if (mype == 0 && NHASH < 1000) {
        for (j = 0; j < npes; j++) {
            for (i = 0; i < localhashlen; i++) {
                int count;
                shmem_int_get(&count, &hashcount[i], 1, j);
                if (count > 0) {
                    int tab;
                    shmem_int_get(&tab, &hashtab[i], 1, j);
                    printf("%d %d %d %d\n", j+1, i+1, tab, count);
                }
            }
        }
    }

    shmem_int_sum_to_all(&total_collisions, &collisions, 1, 0, 0, npes,
                         p_wrk, p_sync);

    if (mype == 0) {
        printf("Avg # collisions: %12.2f\n", total_collisions/(1.0*npes));
        printf("Total time is %8.3f sec\n", (t2-t1) );
    }

#ifdef _DEBUG
    fclose(outfile);
#endif
    finhash();

    return 0;
}
