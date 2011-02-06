package Archive::Extract;

use strict;

use Cwd                         qw[cwd chdir];
use Carp                        qw[carp];
use IPC::Cmd                    qw[run can_run];
use FileHandle;
use File::Path                  qw[mkpath];
use File::Spec;
use File::Basename              qw[dirname basename];
use Params::Check               qw[check];
use Module::Load::Conditional   qw[can_load check_install];
use Locale::Maketext::Simple    Style => 'gettext';

### solaris has silly /bin/tar output ###
use constant ON_SOLARIS     => $^O eq 'solaris' ? 1 : 0;
use constant FILE_EXISTS    => sub { -e $_[0] ? 1 : 0 };

### VMS may require quoting upper case command options
use constant ON_VMS         => $^O eq 'VMS' ? 1 : 0;

### Windows needs special treatment of Tar options
use constant ON_WIN32       => $^O eq 'MSWin32' ? 1 : 0;

### we can't use this extraction method, because of missing
### modules/binaries:
use constant METHOD_NA      => []; 

### If these are changed, update @TYPES and the new() POD
use constant TGZ            => 'tgz';
use constant TAR            => 'tar';
use constant GZ             => 'gz';
use constant ZIP            => 'zip';
use constant BZ2            => 'bz2';
use constant TBZ            => 'tbz';
use constant Z              => 'Z';
use constant LZMA           => 'lzma';

use vars qw[$VERSION $PREFER_BIN $PROGRAMS $WARN $DEBUG 
            $_ALLOW_BIN $_ALLOW_PURE_PERL $_ALLOW_TAR_ITER
         ];

$VERSION            = '0.38';
$PREFER_BIN         = 0;
$WARN               = 1;
$DEBUG              = 0;
$_ALLOW_PURE_PERL   = 1;    # allow pure perl extractors
$_ALLOW_BIN         = 1;    # allow binary extractors
$_ALLOW_TAR_ITER        = 1;    # try to use Archive::Tar->iter if available

# same as all constants
my @Types           = ( TGZ, TAR, GZ, ZIP, BZ2, TBZ, Z, LZMA ); 

local $Params::Check::VERBOSE = $Params::Check::VERBOSE = 1;

=pod

=head1 NAME

Archive::Extract - A generic archive extracting mechanism

=head1 SYNOPSIS

    use Archive::Extract;

    ### build an Archive::Extract object ###
    my $ae = Archive::Extract->new( archive => 'foo.tgz' );

    ### extract to cwd() ###
    my $ok = $ae->extract;

    ### extract to /tmp ###
    my $ok = $ae->extract( to => '/tmp' );

    ### what if something went wrong?
    my $ok = $ae->extract or die $ae->error;

    ### files from the archive ###
    my $files   = $ae->files;

    ### dir that was extracted to ###
    my $outdir  = $ae->extract_path;


    ### quick check methods ###
    $ae->is_tar     # is it a .tar file?
    $ae->is_tgz     # is it a .tar.gz or .tgz file?
    $ae->is_gz;     # is it a .gz file?
    $ae->is_zip;    # is it a .zip file?
    $ae->is_bz2;    # is it a .bz2 file?
    $ae->is_tbz;    # is it a .tar.bz2 or .tbz file?
    $ae->is_lzma;   # is it a .lzma file?

    ### absolute path to the archive you provided ###
    $ae->archive;

    ### commandline tools, if found ###
    $ae->bin_tar     # path to /bin/tar, if found
    $ae->bin_gzip    # path to /bin/gzip, if found
    $ae->bin_unzip   # path to /bin/unzip, if found
    $ae->bin_bunzip2 # path to /bin/bunzip2 if found
    $ae->bin_unlzma  # path to /bin/unlzma if found

=head1 DESCRIPTION

Archive::Extract is a generic archive extraction mechanism.

It allows you to extract any archive file of the type .tar, .tar.gz,
.gz, .Z, tar.bz2, .tbz, .bz2, .zip or .lzma without having to worry how it 
does so, or use different interfaces for each type by using either 
perl modules, or commandline tools on your system.

See the C<HOW IT WORKS> section further down for details.

=cut


### see what /bin/programs are available ###
$PROGRAMS = {};
for my $pgm (qw[tar unzip gzip bunzip2 uncompress unlzma]) {
    $PROGRAMS->{$pgm} = can_run($pgm);
}

### mapping from types to extractor methods ###
my $Mapping = {  # binary program           # pure perl module
    is_tgz  => { bin => '_untar_bin',       pp => '_untar_at'   },
    is_tar  => { bin => '_untar_bin',       pp => '_untar_at'   },
    is_gz   => { bin => '_gunzip_bin',      pp => '_gunzip_cz'  },
    is_zip  => { bin => '_unzip_bin',       pp => '_unzip_az'   },
    is_tbz  => { bin => '_untar_bin',       pp => '_untar_at'   },
    is_bz2  => { bin => '_bunzip2_bin',     pp => '_bunzip2_bz2'},
    is_Z    => { bin => '_uncompress_bin',  pp => '_gunzip_cz'  },
    is_lzma => { bin => '_unlzma_bin',      pp => '_unlzma_cz'  },
};

{   ### use subs so we re-generate array refs etc for the no-overide flags
    ### if we don't, then we reuse the same arrayref, meaning objects store
    ### previous errors
    my $tmpl = {
        archive         => sub { { required => 1, allow => FILE_EXISTS }    },
        type            => sub { { default => '', allow => [ @Types ] }     },
        _error_msg      => sub { { no_override => 1, default => [] }        },
        _error_msg_long => sub { { no_override => 1, default => [] }        },
    };

    ### build accesssors ###
    for my $method( keys %$tmpl, 
                    qw[_extractor _gunzip_to files extract_path],
    ) {
        no strict 'refs';
        *$method = sub {
                        my $self = shift;
                        $self->{$method} = $_[0] if @_;
                        return $self->{$method};
                    }
    }

=head1 METHODS

=head2 $ae = Archive::Extract->new(archive => '/path/to/archive',[type => TYPE])

Creates a new C<Archive::Extract> object based on the archive file you
passed it. Automatically determines the type of archive based on the
extension, but you can override that by explicitly providing the
C<type> argument.

Valid values for C<type> are:

=over 4

=item tar

Standard tar files, as produced by, for example, C</bin/tar>.
Corresponds to a C<.tar> suffix.

=item tgz

Gzip compressed tar files, as produced by, for example C</bin/tar -z>.
Corresponds to a C<.tgz> or C<.tar.gz> suffix.

=item gz

Gzip compressed file, as produced by, for example C</bin/gzip>.
Corresponds to a C<.gz> suffix.

=item Z

Lempel-Ziv compressed file, as produced by, for example C</bin/compress>.
Corresponds to a C<.Z> suffix.

=item zip

Zip compressed file, as produced by, for example C</bin/zip>.
Corresponds to a C<.zip>, C<.jar> or C<.par> suffix.

=item bz2

Bzip2 compressed file, as produced by, for example, C</bin/bzip2>.
Corresponds to a C<.bz2> suffix.

=item tbz

Bzip2 compressed tar file, as produced by, for exmample C</bin/tar -j>.
Corresponds to a C<.tbz> or C<.tar.bz2> suffix.

=item lzma

Lzma compressed file, as produced by C</bin/lzma>.
Corresponds to a C<.lzma> suffix.

=back

Returns a C<Archive::Extract> object on success, or false on failure.

=cut

    ### constructor ###
    sub new {
        my $class   = shift;
        my %hash    = @_;
        
        ### see above why we use subs here and generate the template;
        ### it's basically to not re-use arrayrefs
        my %utmpl   = map { $_ => $tmpl->{$_}->() } keys %$tmpl;

        my $parsed = check( \%utmpl, \%hash ) or return;

        ### make sure we have an absolute path ###
        my $ar = $parsed->{archive} = File::Spec->rel2abs( $parsed->{archive} );

        ### figure out the type, if it wasn't already specified ###
        unless ( $parsed->{type} ) {
            $parsed->{type} =
                $ar =~ /.+?\.(?:tar\.gz|tgz)$/i     ? TGZ   :
                $ar =~ /.+?\.gz$/i                  ? GZ    :
                $ar =~ /.+?\.tar$/i                 ? TAR   :
                $ar =~ /.+?\.(zip|jar|par)$/i       ? ZIP   :
                $ar =~ /.+?\.(?:tbz2?|tar\.bz2?)$/i ? TBZ   :
                $ar =~ /.+?\.bz2$/i                 ? BZ2   :
                $ar =~ /.+?\.Z$/                    ? Z     :
                $ar =~ /.+?\.lzma$/                 ? LZMA  :
                '';

        }

        bless $parsed, $class;

        ### don't know what type of file it is 
        ### XXX this *has* to be an object call, not a package call
        return $parsed->_error(loc("Cannot determine file type for '%1'",
                                $parsed->{archive} )) unless $parsed->{type};
        return $parsed;
    }
}

=head2 $ae->extract( [to => '/output/path'] )

Extracts the archive represented by the C<Archive::Extract> object to
the path of your choice as specified by the C<to> argument. Defaults to
C<cwd()>.

Since C<.gz> files never hold a directory, but only a single file; if 
the C<to> argument is an existing directory, the file is extracted 
there, with its C<.gz> suffix stripped. 
If the C<to> argument is not an existing directory, the C<to> argument 
is understood to be a filename, if the archive type is C<gz>. 
In the case that you did not specify a C<to> argument, the output
file will be the name of the archive file, stripped from its C<.gz>
suffix, in the current working directory.

C<extract> will try a pure perl solution first, and then fall back to
commandline tools if they are available. See the C<GLOBAL VARIABLES>
section below on how to alter this behaviour.

It will return true on success, and false on failure.

On success, it will also set the follow attributes in the object:

=over 4

=item $ae->extract_path

This is the directory that the files where extracted to.

=item $ae->files

This is an array ref with the paths of all the files in the archive,
relative to the C<to> argument you specified.
To get the full path to an extracted file, you would use:

    File::Spec->catfile( $to, $ae->files->[0] );

Note that all files from a tar archive will be in unix format, as per
the tar specification.

=back

=cut

sub extract {
    my $self = shift;
    my %hash = @_;

    ### reset error messages
    $self->_error_msg( [] );
    $self->_error_msg_long( [] );

    my $to;
    my $tmpl = {
        to  => { default => '.', store => \$to }
    };

    check( $tmpl, \%hash ) or return;

    ### so 'to' could be a file or a dir, depending on whether it's a .gz 
    ### file, or basically anything else.
    ### so, check that, then act accordingly.
    ### set an accessor specifically so _gunzip can know what file to extract
    ### to.
    my $dir;
    {   ### a foo.gz file
        if( $self->is_gz or $self->is_bz2 or $self->is_Z or $self->is_lzma ) {
    
            my $cp = $self->archive; $cp =~ s/\.(?:gz|bz2?|Z|lzma)$//i;
        
            ### to is a dir?
            if ( -d $to ) {
                $dir = $to; 
                $self->_gunzip_to( basename($cp) );

            ### then it's a filename
            } else {
                $dir = dirname($to);
                $self->_gunzip_to( basename($to) );
            }

        ### not a foo.gz file
        } else {
            $dir = $to;
        }
    }

    ### make the dir if it doesn't exist ###
    unless( -d $dir ) {
        eval { mkpath( $dir ) };

        return $self->_error(loc("Could not create path '%1': %2", $dir, $@))
            if $@;
    }

    ### get the current dir, to restore later ###
    my $cwd = cwd();

    my $ok = 1;
    EXTRACT: {

        ### chdir to the target dir ###
        unless( chdir $dir ) {
            $self->_error(loc("Could not chdir to '%1': %2", $dir, $!));
            $ok = 0; last EXTRACT;
        }

        ### set files to an empty array ref, so there's always an array
        ### ref IN the accessor, to avoid errors like:
        ### Can't use an undefined value as an ARRAY reference at
        ### ../lib/Archive/Extract.pm line 742. (rt #19815)
        $self->files( [] );

        ### find out the dispatch methods needed for this type of 
        ### archive. Do a $self->is_XXX to figure out the type, then
        ### get the hashref with bin + pure perl dispatchers.
        my ($map) = map { $Mapping->{$_} } grep { $self->$_ } keys %$Mapping;

        ### add pure perl extractor if allowed & add bin extractor if allowed
        my @methods;
        push @methods, $map->{'pp'}  if $_ALLOW_PURE_PERL;
        push @methods, $map->{'bin'} if $_ALLOW_BIN;
        
        ### reverse it if we prefer bin extractors
        @methods = reverse @methods if $PREFER_BIN;

        my($na, $fail);
        for my $method (@methods) {
            print "# Extracting with ->$method\n" if $DEBUG;
        
            my $rv = $self->$method;
            
            ### a positive extraction
            if( $rv and $rv ne METHOD_NA ) {
                print "# Extraction succeeded\n" if $DEBUG;
                $self->_extractor($method);
                last;
            
            ### method is not available
            } elsif ( $rv and $rv eq METHOD_NA ) {               
                print "# Extraction method not available\n" if $DEBUG;
                $na++;                
            } else {
                print "# Extraction method failed\n" if $DEBUG;
                $fail++;
            }                
        }

        ### warn something went wrong if we didn't get an extractor
        unless( $self->_extractor ) {
            my $diag = $fail ? loc("Extract failed due to errors") :
                       $na   ? loc("Extract failed; no extractors available") :
                       '';
                       
            $self->_error($diag);
            $ok = 0;
        }                   
    }

    ### and chdir back ###
    unless( chdir $cwd ) {
        $self->_error(loc("Could not chdir back to start dir '%1': %2'",
                            $cwd, $!));
    }

    return $ok;
}

=pod

=head1 ACCESSORS

=head2 $ae->error([BOOL])

Returns the last encountered error as string.
Pass it a true value to get the C<Carp::longmess()> output instead.

=head2 $ae->extract_path

This is the directory the archive got extracted to.
See C<extract()> for details.

=head2 $ae->files

This is an array ref holding all the paths from the archive.
See C<extract()> for details.

=head2 $ae->archive

This is the full path to the archive file represented by this
C<Archive::Extract> object.

=head2 $ae->type

This is the type of archive represented by this C<Archive::Extract>
object. See accessors below for an easier way to use this.
See the C<new()> method for details.

=head2 $ae->types

Returns a list of all known C<types> for C<Archive::Extract>'s
C<new> method.

=cut

sub types { return @Types }

=head2 $ae->is_tgz

Returns true if the file is of type C<.tar.gz>.
See the C<new()> method for details.

=head2 $ae->is_tar

Returns true if the file is of type C<.tar>.
See the C<new()> method for details.

=head2 $ae->is_gz

Returns true if the file is of type C<.gz>.
See the C<new()> method for details.

=head2 $ae->is_Z

Returns true if the file is of type C<.Z>.
See the C<new()> method for details.

=head2 $ae->is_zip

Returns true if the file is of type C<.zip>.
See the C<new()> method for details.

=head2 $ae->is_lzma

Returns true if the file is of type C<.lzma>.
See the C<new()> method for details.

=cut

### quick check methods ###
sub is_tgz  { return $_[0]->type eq TGZ }
sub is_tar  { return $_[0]->type eq TAR }
sub is_gz   { return $_[0]->type eq GZ  }
sub is_zip  { return $_[0]->type eq ZIP }
sub is_tbz  { return $_[0]->type eq TBZ }
sub is_bz2  { return $_[0]->type eq BZ2 }
sub is_Z    { return $_[0]->type eq Z   }
sub is_lzma { return $_[0]->type eq LZMA }

=pod

=head2 $ae->bin_tar

Returns the full path to your tar binary, if found.

=head2 $ae->bin_gzip

Returns the full path to your gzip binary, if found

=head2 $ae->bin_unzip

Returns the full path to your unzip binary, if found

=head2 $ae->bin_unlzma

Returns the full path to your unlzma binary, if found

=cut

### paths to commandline tools ###
sub bin_gzip        { return $PROGRAMS->{'gzip'}    if $PROGRAMS->{'gzip'}  }
sub bin_unzip       { return $PROGRAMS->{'unzip'}   if $PROGRAMS->{'unzip'} }
sub bin_tar         { return $PROGRAMS->{'tar'}     if $PROGRAMS->{'tar'}   }
sub bin_bunzip2     { return $PROGRAMS->{'bunzip2'} if $PROGRAMS->{'bunzip2'} }
sub bin_uncompress  { return $PROGRAMS->{'uncompress'} 
                                                 if $PROGRAMS->{'uncompress'} }
sub bin_unlzma      { return $PROGRAMS->{'unlzma'}  if $PROGRAMS->{'unlzma'} }

=head2 $bool = $ae->have_old_bunzip2

Older versions of C</bin/bunzip2>, from before the C<bunzip2 1.0> release,
require all archive names to end in C<.bz2> or it will not extract
them. This method checks if you have a recent version of C<bunzip