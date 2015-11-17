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

program test_shmem_iget
  implicit none
  include 'shmem.fh'

  integer, parameter :: N = 10

  integer            ::  i

  integer            ::  me, npes
  logical            ::  success

  double precision           :: dest(N)

  double precision           :: src(1)
  pointer            (src_ptr, src)

  integer                 :: errcode
  integer, parameter  :: abort = 0

  call shmem_init()

  me   = shmem_my_pe();
  npes = shmem_n_pes();

  if(npes .gt. 1) then

    success = .TRUE.

    call shpalloc(src_ptr,2*N, errcode, abort)

    dest(:) = -9

    do i = 1, N, 1
      src(i) = 54321.67 + DBLE(i)
    end do

    call shmem_barrier_all()

    if(me .eq. 0) then
      call shmem_double_iget(dest, src, 1, 2, N/2, npes - 1)
    end if

    call shmem_barrier_all()

    if(me .eq. 0) then
      do i = 1, N/2, 1
        if(dest(i) .ne. src((i-1)*2 + 1)) then
          success = .FALSE.
        end if
      end do

      if(success .eqv. .TRUE.) then
        write(*,*) "Test shmem_double_iget: Passed"
      else
        write(*,*) "Test shmem_double_iget: Failed"
      end if
    end if

    call shmem_barrier_all()

    call shpdeallc(src_ptr, errcode, abort)

  else
    write(*,*) "Number of PEs must be > 1 to test shmem get, test skipped"
  end if

  call shmem_finalize()

end program
