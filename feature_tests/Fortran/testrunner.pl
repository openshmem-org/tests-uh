#!/usr/bin/perl
# Copyright (c) 2011, University of Houston System and Oak Ridge National
# Laboratory.
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
# o Neither the name of the University of Houston System, Oak Ridge
#   National Laboratory nor the names of its contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written permission.
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
 
# TestRunner
#   Runs a set of tests and prints out results in a format that is somehow nice to humans :D
# This script assumes that executables are launched with mpirun, but the script can be easily
# modified to use other methods for launching executables.
# TestRunner reads the file "test_parameters.conf" in the current directory and launches the tests
# defined in that file one by one. If the test expects to PASS (the program prints "Passed") the test
# assumes the test passed. If the program terminates because of some unexpected error or the program
# printed "Failed", TestRunner assumes the test failed.

# TODO: support user configurable launchers
# TODO: support test timeout functionality
# TODO: improve test Pass/Fail detection ?
# TODO: logging of individual tests ? to find out more about a failed test

use strict;
use Env;
use Env qw(PATH HOME TERM);
use Env qw($SHELL @LD_LIBRARY_PATH);
use Data::Dumper;

our $EXIT_ERROR = -1;
our $EXIT_OK = 0;
our $TEST_FAILED = 1;
our $TEST_OK = 2;
our $TEST_NOTFOUND =3;
our $RUN_CMD="mpirun";
our $SHELL_OPT="2>&1";
our $TEST_CONFIG_FILE="test_parameters.conf";

our $ht_stats = {
  'total'    => '0',
  'notfound' => '0',
  'pass'     => '0',
  'fail'      => '0'
}; 

sub read_test_config($);
sub clean_string($);
sub test_runner($);
sub run_test($$);
sub read_dt_conf($);

my $ht_configuration;

$ht_configuration = read_test_config $TEST_CONFIG_FILE;

test_runner $ht_configuration;

exit(0);

### routines

sub print_usage(){
  # TODO: ?
}

# Runs all the tests in the configuration
sub test_runner($){
 my ($ht_configuration) = @_;
 
 foreach my $test_id (sort keys %$ht_configuration){
   my $test_parameters = $ht_configuration->{$test_id}; 
   run_test $test_id, $test_parameters; 
   $ht_stats->{'total'}++; 
 }

 print "\nDone testing.\n";

 print "Summary:\n";
 print "- $ht_stats->{'pass'}/$ht_stats->{'total'} Passed.\n";
 print "- $ht_stats->{'fail'}/$ht_stats->{'total'} Failed.\n";

 if($ht_stats->{'notfound'} > 0){
   print "- $ht_stats->{'notfound'}/$ht_stats->{'total'} tests were not found. The tests build may be incomplete :/\n";
 }

 print "\n\n";
}

# Runs a single test
sub run_test($$) {
  my ($test_id, $test_config) = @_;
  my @output;
  my $test_result = "Fail";
  my $executable = $test_config->{'executable'};

  if(! -e $executable) {
    print "Warning: The test file \"$executable\" could not be found, skipping test.\n";
    $ht_stats->{'notfound'}++;
  }
  print "($test_id) Running $executable: $test_config->{'title'}... ";
  @output = `($RUN_CMD -np $test_config->{'npes'} ./$executable ) 2>&1`;   

  if($output[0] =~ /Passed/){
    $test_result = "Pass";
  }

  if($test_config->{'expect'} eq $test_result){
    print "OK\n";
    $ht_stats->{'pass'}++;
  }else{
    print "Failed\n";
    $ht_stats->{'fail'}++;
  }
}


# Reads the configuration, returns a hash table with all the settings
# hash table structure:
# ht_configuration => {
#   test_1 => { 
#     test_name,
#     title,
#     parameters,
#     timeout,
#     expect,
#     types,
#   }
#   ...
#   test_N => { 
#     test_name,
#     title,
#     parameters,
#     timeout,
#     expect,
#     types
#   }
# }
# test_name  => name of the test (used to generate the executable name) 
# types      => data types to be tested (used to generate the executable name)
# parameters => parameters to be passed with the executable
# timeout    => how long before assuming that the test Failed (0 = wait forever). Time in seconds.
# expect     => Expected result. Possible values: Pass, Fail. Fail includes cases where the program terminates with exception or unexpected
#               errors.
sub read_test_config($){
  my ($filename) = @_;
  my $ht_config = {};
  my $line_num = 1;

  open CONFIGFILE, $filename or die "Failed to open configuration file: $filename\n";

  # read the configuration file line by line
  while(<CONFIGFILE>){
   
     my $line = $_;
     my @elems = split /,/, $line; # read comma-separated elements 
     my $test_name;
     my $tmp_value;

     my $ht_tmp = {};

     if($line =~ /^\#/) {
       next;
     }

     $test_name = clean_string($elems[0]);      
     $ht_tmp->{'executable'} = $test_name;

     $tmp_value = clean_string($elems[1]);
     if(!$tmp_value =~ /^[0-9]+$/){
       print "Error: Configuration file, line $line_num >> Invalid value for number of PEs: $tmp_value \n";
       exit;
     }
     $ht_tmp->{'npes'} = $tmp_value;

     $tmp_value = clean_string($elems[2]);
     if($tmp_value eq ""){
       print "Error: Configuration file, line $line_num >> Test does not have a title :(\n";
       exit;
     }
     $ht_tmp->{'title'} = $tmp_value;

     $tmp_value = clean_string($elems[3]);
     $ht_tmp->{'parameters'} = $tmp_value;
      
     $tmp_value = clean_string($elems[4]);
     if(! ($tmp_value =~ /^[0-9]+$/)){
       print "Error: Configuration file, line $line_num >> Test has invalid timeout value: $tmp_value\n";
       exit;
     }
     $ht_tmp->{'timeout'} = $tmp_value; 
  
     $tmp_value = clean_string($elems[5]);
     if($tmp_value ne "Pass" and $tmp_value ne "Fail"){
       print "Error: Configuration file, line $line_num >> Invalid value for 'Expected' field: $tmp_value \n";
       exit;
     }
     $ht_tmp->{'expect'} = $tmp_value; 

     # Finally, add the parameters to the hash table.
     # The key for this set of parameters is "test_NNN" where NNN is a 3 digit number padded on the left with 0s.
     $ht_config->{sprintf("test_%03d", $line_num)} = $ht_tmp;

     $line_num++; # next line
  }

  close CONFIGFILE;

  return $ht_config;
}

sub clean_string($){
  my ($word) = @_;

  $word =~ s/^\s+//;
  $word =~ s/\s+$//;
  
  return $word;
}
