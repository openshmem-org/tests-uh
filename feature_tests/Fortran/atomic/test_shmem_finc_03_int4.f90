!
!
! Copyright (c) 2011 - 2015
!   University of Houston System and UT-Battelle, LLC.
! Copyright (c) 2009 - 2015
!   Silicon Graphics International Corp.  SHMEM is copyrighted
!   by Silicon Graphics International Corp. (SGI) The OpenSHMEM API
!   (shmem) is released by Open Source Software Solutions, Inc., under an
!   agreement with Silicon Graphics International Corp. (SGI).
! Copyright (c) 2015 Intel Corporation
!
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions
! are met:
!
! o Redistributions of source code must retain the above copyright notice,
!   this list of conditions and the following disclaimer.
!
! o Redistributions in binary form must reproduce the above copyright
!   notice, this list of conditions and the following disclaimer in the
!   documentation and/or other materials provided with the distribution.
!
! o Neither the name of the University of Houston System, UT-Battelle, LLC
!   nor the names of its contributors may be used to endorse or promote
!   products derived from this software without specific prior written
!   permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
! HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
! TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
! PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
! LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
! NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!

program test_shmem_atomics
  implicit none
  include 'shmem.fh'

  logical, parameter        :: true_val = .TRUE.
  logical, save             :: success1
  logical, save             :: success2

  integer*4                 :: dest(1)
  pointer                  (dest_ptr, dest)

  integer*4                 :: swapped_val

  integer                   :: errcode
  integer, parameter        :: abort = 0
  integer                   :: me, npes

  call shmem_init()
  me = shmem_my_pe()
  npes = shmem_n_pes()

  call shmem_barrier_all()

  ! Make sure this job is running with at least 2 PEs.

  if (npes .gt. 1) then
    success1 = .FALSE.
    success2 = .FALSE.

    call shpalloc(dest_ptr, 1, errcode, abort)

    dest(1) = 51234

    call shmem_barrier_all()

    if(me .eq. 0) then
      swapped_val = shmem_int4_finc(dest, npes - 1)
    end if

    call shmem_barrier_all()

    ! To validate the working of swap we need to check the value received at the PE that initiated the swap
    !  as well as the dest PE

    if(me .eq. 0) then
      if(swapped_val .eq. 51234) then
        success1 = .TRUE.
      end if
    end if

    if(me .eq. npes - 1) then
      if(dest(1) .eq. 51234 + 1) then
        call shmem_logical_put(success2, true_val, 1, 0)
      end if
    end if

    call shmem_barrier_all()

    if(me .eq. 0) then
      if(success1 .eqv. .TRUE. .and. success2 .eqv. .TRUE.) then
        write (*,*) "Test 03 shmem_int4_finc: Passed"
      else
        write (*,*) "Test 03 shmem_int4_finc: Failed"
      end if
    end if

    call shmem_barrier_all()

    call shpdeallc(dest_ptr, errcode, abort)

  else
    write (*,*) "Number of PEs must be > 1 to test shmem atomics, test skipped"
  end if

  call shmem_finalize()

end program test_shmem_atomics
