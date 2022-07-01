#!/usr/bin/perl

#use strict;
#use warnings;
use Path::Class;

my $arg = $ARGV[0] || die "usage: $0 TeXfile";
my ($path, $stem) = get_path_stem($arg);

open(my $IN, "<", $path);
foreach(<$IN>){
  if(/^%#job: *(\w+)$/){
    print(`lualatex -jobname=$1 $path`);
  }
}
close($IN);
exit(0);

sub get_path_stem {
  my $path = file(shift);
  my $name = $path->basename;
  if ($path =~ /\.tex$/ && -f $path) {
    $name =~ s/\.tex$//;
  }elsif(-f "$path.tex"){
    $path .= ".tex";
  }else{
    die "File not found: $path(.tex)";
  }
  return ($path, $name);
}

