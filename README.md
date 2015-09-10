/*
 *
 * Copyright (c) 2011 - 2015
 *  University of Houston System and UT-Battelle.
 * Copyright (c) 2009 - 2015
 *  Silicon Graphics International Corp.  SHMEM is copyrighted
 *  by Silicon Graphics International Corp. (SGI) The OpenSHMEM API
 *  (shmem) is released by Open Source Software Solutions, Inc., under an
 *  agreement with Silicon Graphics International Corp. (SGI).
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
 * o Neither the name of the University of Houston System, and UT-Battelle
 *   nor the names of its contributors may be used to endorse or promote 
 *   producs derived from this software without specific prior written 
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

1.  This is the README for the tests-uh suite.

2.  Tests

The tests are divided into feature, and performance programs in
the top-level directory.  

2.1 Feature Tests

The following lists of tests can be found in the folder
'feature_tests' for both C and Fortran;

i.  test_shmem_put_shmem_malloc , test_shmem_put_globals

This tests 
a.  elemental put calls

C/C++ only:
shmem_double_p, shmem_float_p, shmem_int_p, shmem_long_p, shmem_short_p 

b.  block put calls

C/C++ and Fortran:
shmem_put32, shmem_put64, shmem_put128

C/C++ only:
shmem_double_put, shmem_float_put, shmem_int_put, shmem_real_put 

c.  strided put calls 

C/C++ and Fortran:
shmem_iput32, shmem_iput64, shmem_iput128, 

C/C++ only:
shmem_double_iput, shmem_float_iput, shmem_int_iput, shmem_long_iput,
shmem_short_iput 

Fortran only:
shmem_complex_iput, shmem_integer_iput, shmem_logical_iput, shmem_real_iput 

d.  byte-granularity block put calls

C/C++ and Fortran:
shmem_putmem

Additional information:

Test test_shmem_put_shmem_malloc uses shmem_malloc-ed variables that
are allocated and managed by the OpenSHMEM library while the
test_shmem_put_globals checks that the same calls work with global
variables.

ii. test_shmem_get_shmem_malloc, test_shmem_get_globals 

This tests 
a.  elemental get calls

C/C++ only:
shmem_double_p, shmem_float_p, shmem_int_p, shmem_long_p, shmem_short_p 

b.  block get calls

C/C++ and Fortran:
shmem_get32, shmem_get64, shmem_get128

C/C++ only:
shmem_double_get, shmem_float_get, shmem_int_get, shmem_real_get 

c.  strided get calls 

C/C++ and Fortran:
shmem_iget32, shmem_iget64, shmem_iget128, 

C/C++ only:
shmem_double_iget, shmem_float_iget, shmem_int_iget, shmem_long_iget,
shmem_short_iget 

Fortran only:
shmem_complex_iget, shmem_integer_iget, shmem_logical_iget, shmem_real_iget 

d.  byte-granularity block get calls

C/C++ and Fortran:
shmem_getmem

Additional information:

Test test_shmem_get_shmem_malloc uses shmem_malloc-ed variables that
are allocated and managed by the OpenSHMEM library while the
test_shmem_get_globals checks that the same calls work with global
variables.

iii.  test_shmem_broadcast  

This tests shmem_broadcast32, shmem_broadcast64 calls available for
C/C++ and Fortran.

iv.  test_shmem_reductions  

This  tests;

Fortran only:
test_shmem_max_01_int8, test_shmem_min_01_real4, test_shmem_prod_01_int8, 
test_shmem_sum_01_real4, test_shmem_max_01_real4, test_shmem_min_01_real8, 
test_shmem_prod_01_real4, test_shmem_sum_01_real80, test_shmem_and_01_int4, 
test_shmem_max_01_real8, test_shmem_or_01_int4, test_shmem_prod_01_real8, 
test_shmem_xor_01_int4, test_shmem_and_01_int8, test_shmem_min_01_int4,
test_shmem_or_01_int8, test_shmem_sum_01_int4, test_shmem_xor_01_int8,
test_shmem_max_01_int4  test_shmem_min_01_int8, test_shmem_prod_01_int4, test_shmem_sum_01_int8,


v. test_shmem_atomic  

This tests;

Fortran only:
test_shmem_cswap_01_int4, test_shmem_finc_01_int8, test_shmem_inc_02_int4, test_shmem_swap_02_int8,
test_shmem_cswap_01_int8, test_shmem_finc_02_int4, test_shmem_inc_02_int8,  test_shmem_swap_03_int4,
test_shmem_cswap_02_int4, test_shmem_finc_02_int8, test_shmem_inc_03_int4,  test_shmem_swap_03_int8,
test_shmem_add_01_int4, test_shmem_cswap_02_int8, test_shmem_finc_03_int4, test_shmem_inc_03_int8, test_shmem_swap_04_int4,
test_shmem_add_01_int8, test_shmem_cswap_03_int4, test_shmem_finc_03_int8, test_shmem_inc_04_int4, test_shmem_swap_04_int8,
test_shmem_add_02_int4, test_shmem_cswap_03_int8, test_shmem_finc_04_int4, test_shmem_inc_04_int8, test_shmem_swap_05_int4,
test_shmem_add_02_int8, test_shmem_cswap_04_int4, test_shmem_finc_04_int8, test_shmem_inc_05_int4, test_shmem_swap_05_int8,
test_shmem_add_03_int4, test_shmem_cswap_04_int8, test_shmem_finc_05_int4, test_shmem_inc_05_int8,
test_shmem_add_03_int8, test_shmem_fadd_01_int4, test_shmem_finc_05_int8, test_shmem_swap_01_int4,
test_shmem_add_04_int4, test_shmem_fadd_01_int8, test_shmem_inc_01_int4, test_shmem_swap_01_int8,
test_shmem_add_04_int8, test_shmem_finc_01_int4, test_shmem_inc_01_int8, test_shmem_swap_02_int4,


vi.  test_shmem_synchronization 

This tests;

C/C++ only:
shmem_int_wait, shmem_int_wait_until, shmem_long_wait, shmem_long_wait_until,
shmem_longlong_wait, shmem_longlong_wait_until, shmem_short_wait,
shmem_short_wait_until

Fortran only:
shmem_int4_wait, shmem_int4_wait_until, shmem_int8_wait, shmem_int8_wait_until

Additional information:

Correct output for this test also depends on a reliable implementation
of the shmem_long_put and shmem_long_wait OpenSHMEM calls.

vii. test_shmem_accessible
This tests;
Fortran:
test_shmem_acc_mem_01_real8, test_shmem_acc_mem_03_int4, test_shmem_acc_mem_04_real4,
test_shmem_acc_mem_02_char, test_shmem_acc_mem_03_int8, test_shmem_acc_mem_04_real8,
test_shmem_acc_01, test_shmem_acc_mem_02_double,test_shmem_acc_mem_03_logical, test_shmem_acc_mem_05_char,
test_shmem_acc_02, test_shmem_acc_mem_02_int4, test_shmem_acc_mem_03_real4, test_shmem_acc_mem_05_double,
test_shmem_acc_mem_01_char, test_shmem_acc_mem_02_int8, test_shmem_acc_mem_03_real8, test_shmem_acc_mem_05_int4,
test_shmem_acc_mem_01_double, test_shmem_acc_mem_02_logical, test_shmem_acc_mem_04_char, test_shmem_acc_mem_05_int8,
test_shmem_acc_mem_01_int4, test_shmem_acc_mem_02_real4, test_shmem_acc_mem_04_double, test_shmem_acc_mem_05_logical,
test_shmem_acc_mem_01_int8, test_shmem_acc_mem_02_real8, test_shmem_acc_mem_04_int4, test_shmem_acc_mem_05_real4,
test_shmem_acc_mem_01_logical, test_shmem_acc_mem_03_char, test_shmem_acc_mem_04_int8, test_shmem_acc_mem_05_real8,
test_shmem_acc_mem_01_real4, test_shmem_acc_mem_03_double, test_shmem_acc_mem_04_logical,


viii. test_shmem_collects  
This tests;
Fortran only:
test_shmem_collect_02_int8, test_shmem_fcollect_01_int4, test_shmem_fcollect_03_int8,
test_shmem_collect_03_int4, test_shmem_fcollect_01_int8, test_shmem_fcollect_04_int4,
test_shmem_collect_01_int4, test_shmem_collect_03_int8, test_shmem_fcollect_02_int4, 
test_shmem_fcollect_04_int8,
test_shmem_collect_01_int8, test_shmem_collect_04_int4, test_shmem_fcollect_02_int8,
test_shmem_collect_02_int4, test_shmem_collect_04_int8, test_shmem_fcollect_03_int4,
	  

ix.  test_shmem_lock

This tests;

C/C++ and Fortran:
shmem_clear_lock, shmem_set_lock, shmem_test_lock 

Additional information:

Correct output for this test also depends on a reliable implementation
of the shmem_quiet OpenSHMEM calls.


2.2 Performance Tests

2.2.1 Micro-benchmarks

The Micro-benchmark directory contains programs to measure latency of
data transfer calls and collective calls in OpenSHMEM.  Performance
tests measure the time taken for a OpenSHMEM call by finding the
average over 10000 calls.

i.  put_performance
ii. get_performance
iii.  broadcast_performance  
iv. barrier_performance  
v.  collects_performance  


3.  Running Tests

Edit the Makefile or export values, such that they use the appropriate
compiler, (SHMEM_FLAGS), run command (RUNCMD), run options (RUNOPT),
command line options to control execution environment (NPROCOPT),
NPROC (this parameter decides the number of PEs, default value is 2).
To compile use 'make all' and to execute use 'make run'.

4.  Expected Results and their interpretation

Example: Feature test for atomic operations
Execute: make run
Expected Result:
(test_001) Running test_shmem_swap_01_int4.x: shmem_swap with SAVE variables... OK
(test_002) Running test_shmem_swap_01_int8.x: shmem_swap with SAVE variables... OK
(test_003) Running test_shmem_swap_02_int4.x: shmem_swap with COMMON variables... OK
(test_004) Running test_shmem_swap_02_int8.x: shmem_swap with COMMON variables... OK
(test_005) Running test_shmem_swap_03_int4.x: shmem_swap with shpalloc'ed variables... OK
(test_006) Running test_shmem_swap_03_int8.x: shmem_swap with shpalloc'ed variables... OK
(test_007) Running test_shmem_swap_04_int4.x: shmem_swap with non remotely accessible variables... OK
(test_008) Running test_shmem_swap_04_int8.x: shmem_swap with non remotely accessible variables... OK
(test_009) Running test_shmem_swap_05_int4.x: shmem_swap with non remotely accessible variables (ALLOCATABLE)... OK
(test_010) Running test_shmem_swap_05_int8.x: shmem_swap with non remotely accessible variables (ALLOCATABLE)... OK
(test_011) Running test_shmem_cswap_01_int4.x: shmem_cswap with SAVE variables... OK
(test_012) Running test_shmem_cswap_01_int8.x: shmem_cswap with SAVE variables... OK
(test_013) Running test_shmem_cswap_02_int4.x: shmem_cswap with COMMON variables... OK
(test_014) Running test_shmem_cswap_02_int8.x: shmem_cswap with COMMON variables... OK
(test_015) Running test_shmem_cswap_03_int4.x: shmem_cswap with shpalloc'ed variables... OK
(test_016) Running test_shmem_cswap_03_int8.x: shmem_cswap with shpalloc'ed variables... OK
(test_017) Running test_shmem_cswap_04_int4.x: shmem_cswap with non remotely accessible variables... OK
(test_018) Running test_shmem_cswap_04_int8.x: shmem_cswap with non remotely accessible variables... OK
(test_019) Running test_shmem_add_01_int4.x: shmem_add with SAVE variables... OK
(test_020) Running test_shmem_add_01_int8.x: shmem_add with SAVE variables... OK
(test_021) Running test_shmem_add_02_int4.x: shmem_add with COMMON variables... OK
(test_022) Running test_shmem_add_02_int8.x: shmem_add with COMMON variables... OK
(test_023) Running test_shmem_add_03_int4.x: shmem_add with shpalloc'ed variables... OK
(test_024) Running test_shmem_add_03_int8.x: shmem_add with shpalloc'ed variables... OK
(test_025) Running test_shmem_add_04_int4.x: shmem_add with non remotely accessible variables... OK
(test_026) Running test_shmem_add_04_int8.x: shmem_add with non remotely accessible variables... OK
(test_027) Running test_shmem_fadd_01_int4.x: shmem_fadd with SAVE variables... OK
(test_028) Running test_shmem_fadd_01_int8.x: shmem_fadd with SAVE variables... OK
(test_029) Running test_shmem_inc_01_int4.x: shmem_inc with SAVE variables... OK
(test_030) Running test_shmem_inc_01_int8.x: shmem_inc with SAVE variables... OK
(test_031) Running test_shmem_inc_02_int4.x: shmem_inc with COMMON variables... OK
(test_032) Running test_shmem_inc_02_int8.x: shmem_inc with COMMON variables... OK
(test_033) Running test_shmem_inc_03_int4.x: shmem_inc with shpalloc'ed variables... OK
(test_034) Running test_shmem_inc_03_int8.x: shmem_inc with shpalloc'ed variables... OK
(test_035) Running test_shmem_inc_04_int4.x: shmem_inc with non remotely accessible variables... OK
(test_036) Running test_shmem_inc_04_int8.x: shmem_inc with non remotely accessible variables... OK
(test_037) Running test_shmem_inc_05_int4.x: shmem_inc with non remotely accessible variables (ALLOCATABLE)... OK
(test_038) Running test_shmem_inc_05_int8.x: shmem_inc with non remotely accessible variables (ALLOCATABLE)... OK
(test_039) Running test_shmem_finc_01_int4.x: shmem_finc with SAVE variables... OK
(test_040) Running test_shmem_finc_01_int8.x: shmem_finc with SAVE variables... OK
(test_041) Running test_shmem_finc_02_int4.x: shmem_finc with COMMON variables... OK
(test_042) Running test_shmem_finc_02_int8.x: shmem_finc with COMMON variables... OK
(test_043) Running test_shmem_finc_02_int4.x: shmem_finc with COMMON variables... OK
(test_044) Running test_shmem_finc_02_int8.x: shmem_finc with COMMON variables... OK
(test_045) Running test_shmem_finc_03_int4.x: shmem_finc with shpalloc'ed variables... OK
(test_046) Running test_shmem_finc_03_int8.x: shmem_finc with shpalloc'ed variables... OK
(test_047) Running test_shmem_finc_04_int4.x: shmem_finc with non remotely accessible variables... OK
(test_048) Running test_shmem_finc_04_int8.x: shmem_finc with non remotely accessible variables... OK
(test_049) Running test_shmem_finc_05_int4.x: shmem_finc with non remotely accessible variables (ALLOCATABLE)... OK
(test_050) Running test_shmem_finc_05_int8.x: shmem_finc with non remotely accessible variables (ALLOCATABLE)... OK

Done testing.
Summary:
- 50/50 Passed.
- 0/50 Failed.

If the test says 'Passed' then the routines that are being tested
behave in accordance with OpenSHMEM Specification 1.2 and the result
produced (if applicable) is correct.

