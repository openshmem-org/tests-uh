#
#
# Copyright (c) 2011 - 2015 
#   University of Houston System and UT-Battelle, LLC.
# Copyright (c) 2009 - 2015
#   Silicon Graphics International Corp.  SHMEM is copyrighted
#   by Silicon Graphics International Corp. (SGI) The OpenSHMEM API
#   (shmem) is released by Open Source Software Solutions, Inc., under an
#   agreement with Silicon Graphics International Corp. (SGI).
# 
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 
# o Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# 
# o Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# 
# o Neither the name of the University of Houston System, UT-Battelle, LLC
#   nor the names of its contributors may be used to endorse or promote
#   products derived from this software without specific prior written
#   permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
# TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#


SHELL = /bin/sh

help: 
	@echo
	@echo "Please use one of the following targets:"
	@echo
	@echo "  * For C       : make C_feature_tests, make C_feature_tests-run "
	@echo "  * For Fortran : make F_feature_tests, make F_feature_tests-run"
	@echo "  * all         : compile both C and Fortran tests"
	@echo "  * run         : compile both C and Fortran tests, then run them
	@echo "  * clean       : removes object and executable files"
	@echo

FEATURE_TESTS = C_feature_tests F_feature_tests
FEATURE_TESTS_RUN = $(addsuffix -run, $(FEATURE_TESTS))


.PHONY: $(FEATURE_TESTS) \
	all clean

all: $(FEATURE_TESTS)

run: $(FEATURE_TESTS_RUN)

C_feature_tests: 
	cd ./C; $(MAKE) all

C_feature_tests-run:	C_feature_tests
	cd ./C; $(MAKE) run

F_feature_tests:
	cd ./Fortran; $(MAKE) all

F_feature_tests-run: F_feature_tests
	cd ./Fortran; $(MAKE) run

clean:
	cd ./C; $(MAKE) $@
	cd ./Fortran; $(MAKE) $@

