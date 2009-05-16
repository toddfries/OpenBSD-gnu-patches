#!/usr/bin/perl -w
# $Id: fork.t,v 1.2 2009/05/16 21:42:57 simon Exp $

BEGIN {
    if( $ENV{PERL_CORE} ) {
        chdir 't';
        @INC = '../lib';
    }
}

use Test::More;
use Config;

my $Can_Fork = $Config{d_fork} ||
               (($^O eq 'MSWin32' || $^O eq 'NetWare') and
                $Config{useithreads} and 
                $Config{ccflags} =~ /-DPERL_IMPLICIT_SYS/
               );

if( !$Can_Fork ) {
    plan skip_all => "This system cannot fork";
}
else {
    plan tests => 1;
}

if( fork ) { # parent
    pass("Only the parent should process the ending, not the child");
}
else {
    exit;   # child
}

