!
!
! Copyright (c) 2011 - 2015
!   University of Houston System and UT-Battelle, LLC.
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
!    nor the names of its contributors may be used to
!   endorse or promote products derived from this software without specific
!   prior written permission.
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

program test_shmem_barrier
  implicit none

  include 'shmem.fh'

  integer*4               :: dest
  integer*8               :: dest_ptr
  pointer                    (dest_ptr, dest)
  integer*4               :: src
  integer, save           :: flag
  integer                 :: me, npes, i, errcode, abort

! Function definitions
  integer                   :: shmem_my_pe, shmem_n_pes

  flag = 10101
  
  call shmem_init();

  me   = shmem_my_pe();
  npes = shmem_n_pes();

  dest = 0

  call shpalloc(dest_ptr, 1, errcode, abort)

  if (npes .gt. 1) then
    
    if(me .eq. npes - 1) then
      call shmem_barrier_all()
    end if

! All PEs should call shmem_barrier_all before leaving the barrier
! Atomic increment of destination on 0

    call shmem_int4_inc(dest, 0)

    if(me .eq. 0) then
      if(dest .eq. npes - 1) then
        write (*,*) 'Test shmem_barrier_all: Passed'
      else
        write (*,*) 'Test shmem_barrier_all: Failed'
      end if
    end if

    if(me .ne. npes - 1) then
      call shmem_barrier_all()
    end if

  else
    write (*,*) 'Number of PEs must be > 1 to test barrier, test skipped'
  end if  

  call shmem_finalize()

end program test_shmem_barrier
