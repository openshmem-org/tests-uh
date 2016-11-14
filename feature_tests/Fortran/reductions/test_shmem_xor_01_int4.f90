!
!
! Copyright (c) 2011 - 2015
!   University of Houston System and UT-Battelle, LLC.
! Copyright (c) 2009 - 2015
!   Silicon Graphics International Corp.  SHMEM is copyrighted
!   by Silicon Graphics International Corp. (SGI) The OpenSHMEM API
!   (shmem) is released by Open Source Software Solutions, Inc., under an
!   agreement with Silicon Graphics International Corp. (SGI).
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

program test_shmem_reduction
  implicit none
  include 'shmem.fh'

  integer,   parameter :: min_npes = 3
  integer,   parameter :: nelems = 10

  integer*4, save      :: src(nelems)
  integer*4, save      :: dest(nelems)
  integer*4  , save    :: dest_expected(nelems)

  integer,   save      :: pSync(SHMEM_REDUCE_SYNC_SIZE)
  integer*4, save      :: pWrk(MAX(nelems/2 + 1, SHMEM_REDUCE_MIN_WRKDATA_SIZE))

  integer              :: me, npes, i, pe
  logical              :: success

  character*(*), parameter :: TEST_NAME = 'shmem_xor'

  ! Function definitions
  integer                   :: shmem_my_pe, shmem_n_pes

  success = .TRUE.

  call shmem_init()

  me   = shmem_my_pe()
  npes = shmem_n_pes()

  if (npes .ge. min_npes) then

    pSync(:) = SHMEM_SYNC_VALUE

    do i = 1, nelems, 1
      dest(i) = 0
      src(i) = me + i
      dest_expected(i) = i
    end do

    do pe = 1, npes - 1, 1
      do i = 1, nelems, 1
        dest_expected(i) = IEOR(dest_expected(i), pe + i)
      end do
    end do

    call shmem_barrier_all()

    call shmem_int4_xor_to_all(dest, src, nelems, 0, 0, npes, pWrk, pSync)

    do i = 1, nelems, 1
      if(dest(i) .ne. dest_expected(i)) then
        success = .FALSE.
      end if
    end do

    if(me .eq. 0) then
      if(success .eqv. .TRUE.) then
        write(*,*) TEST_NAME, ': Passed'
      else
        write(*,*) TEST_NAME, ': Failed'
      end if
    end if

  else
    if(me .eq. 0) then
      write(*,*) 'This test requires ', min_npes, ' or more PEs.'
    end if
  end if

  call shmem_finalize()

end program test_shmem_reduction
