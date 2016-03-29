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
 * Test for the info/query interfaces
 * Compare the values with the supported OpenSHMEM constants:
 * _SHMEM_MAJOR_VERSION, _SHMEM_MINOR_VERSION, and
 * _SHMEM_MAX_NAME_LEN
 *
 */

#include <stdio.h>
#include <shmem.h>
#include <string.h>

#define PRINT_VER(arg,result) \
    printf("Test shmem_info_get_version (%s):%s\n",\
            arg,result);

#define PRINT_NAME(arg) \
    printf("Test shmem_info_get_name: %s\n",arg);

int
main ()
{
    char name[_SHMEM_MAX_NAME_LEN];
    int major_ver, minor_ver;
    int me;

    int fail_count = 0;

    shmem_init ();
    me = shmem_my_pe ();

    if (me == 0) {

      shmem_info_get_version (&major_ver, &minor_ver);
      shmem_info_get_name (name);

      if (major_ver == _SHMEM_MAJOR_VERSION) {
          PRINT_VER("Major","Passed");
      }
      else {
          PRINT_VER("Major","Failed");
          fail_count++;
      }

      if (minor_ver == _SHMEM_MINOR_VERSION) {
          PRINT_VER("Minor","Passed");
      }
      else {
          PRINT_VER("Minor","Failed");
          fail_count++;
      }

      if (strlen (name) < _SHMEM_MAX_NAME_LEN) {
          PRINT_NAME("Passed");
      }
      else {
          PRINT_NAME("Undefined");
          fail_count++;
      }

      if (fail_count == 0)
            printf("All Tests Passed\n");
      else
            printf("%d Tests Failed\n", fail_count);
    }

    shmem_finalize ();

    return 0;
}
