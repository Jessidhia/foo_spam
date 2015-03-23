#! /usr/bin/env perl
#
# foo_spam - Prints the currently playing song from foobar2000.
#
# Copyright (c) 2009-2010, Diogo Franco (Kovensky) <diogomfranco@gmail.com>
# Contributions by Kulag <g.kulag@gmail.com>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

use v5.10.1;
use utf8;
use strict;
use warnings;
use version 0.77;

our $VERSION = qv('v0.9.2');

my %info = (
	author      => 'Kovensky, pink_mist',
	contact     => '#shameimaru@irc.rizon.net',
	url         => 'http://repo.or.cz/w/foo_spam.git',
	name        => 'foo_spam',
	description => 'Prints the currently playing song from foobar2000, Banshee or an MPRIS compliant player',
	license     => 'ISC'
);

# ChangeLog:
# 0.9.2 - Fixed compatibility with perl 5.12, remove dependency on common::sense. Allow it to run on perl v5.10.0.
# 0.9.1 - Reverted 0.9.0, it broke everything.
# 0.8.3 - Better %filesize_natural% calculation
# 0.8.2 - Added support for several file information tags. The Fields option on foobar2000 was changed, support for the old one will be dropped after two major bumps.
# 0.8.1 - Added --command and /foo_control. Added %foo_spam_version% tag.
# 0.8   - Added MPRIS support, patch by Kulag.
# 0.7   - Added Banshee support. Parses command line options using Getopt::Long.
# 0.6.1 - Added weechat support.
# 0.6   - Backwards incompatible version. Changes the format syntax, documents functions, implement some others.
# 0.5.2 - Added discnumber and totaldiscs tags. Changed default format. Silences a warning when a function ends on ",)". Fixed two warnings in the $if family.
# 0.5.1 - Fixed $if, $if2, $and, $or and $xor behavior on certain strings.
# 0.5   - Support subfunctions and tags with underlines. Changed some other details.
# 0.4   - Fixed UTF-8 corruption issues. Allow the user to specify a comment when using /aud by giving it as an argument. Document build_output.
# 0.3.2 - Change the method used to read foobar2000's response. The previous method would hang once in a while.
# 0.3.1 - Change default settings to avoid breakage if a track has | on one of the tags. Update documentation.
# 0.3   - Allow customization of the format string. Changed method of desync handling.
# 0.2.2 - Fix desync issues if foobar takes "too long" to respond. Added codec and bitrate to the output.
# 0.2.1 - Forgot to handle one error case on the telnet connection.
# 0.2   - Changed the recommended string and output. Fixed my wrong XChat API usage. Changed the way the telnet connection is handled.
# 0.1   - First version

# Known Bugs:
# Doesn't support tags that are equal to "?" (foo_controlserver limitation).
# MPRIS (or amarok) is dumb and doesn't export album artist information.

use Encode;
use Getopt::Long;
use File::Path;
use Time::HiRes qw(usleep);
use List::Util;

BEGIN {
	*HAVE_XCHAT = Xchat->can('register')     ? sub {1} : sub {0};
	*HAVE_IRSSI = Irssi->can('command_bind') ? sub {1} : sub {0};
	*HAVE_WEECH = weechat->can('register')   ? sub {1} : sub {0};
}

if (HAVE_IRSSI) {
	our %IRSSI   = %info;
}

Xchat::register( $info{name}, $VERSION->stringify, $info{description}, \&close_telnet )
    if HAVE_XCHAT;
weechat::register( $info{name}, $info{author}, $VERSION->stringify, $info{license},
	$info{description}, 'close_telnet', 'UTF-8' )
    if HAVE_WEECH;

our $player         = "foobar2000";
our $telnet_open    = 0;
our $telnet         = undef;
our $default_format = <<'EOF';
%player%[ (%version%)]:
 [%album artist% ]'['[%date% ][%album%][ #[%discnumber%.]%tracknumber%[/[%totaldiscs%.]%totaltracks%]]']'
 [%track artist% - ]%title% '['%playback_time%[/%length%]']'[ %bitrate%kbps][ %filesize_natural%][ %codec%[ %codec_profile%]][ <-- %foo_spam_comment%]
EOF
$default_format =~ s/\R//g;
our $format = $default_format;
our $hostname = "127.0.0.1";
our $hostport = 3333;
our %heap;

our $settings_file = undef;    # Only used by Xchat

sub open_telnet {
	eval { require Net::Telnet; 1 } or ( warn "Can't find Net::Telnet" and return undef );
	$telnet
	    = new Net::Telnet( Timeout => 10, Errmode => 'return' )
	    if not defined($telnet);
	$telnet_open = $telnet->open( Host => $hostname, Port => $hostport );
	unless ($telnet_open) {
		irc_print(
			"Error connecting to foobar2000! Make sure fb2k is running.");
		irc_print("Also check if foo_controlserver is properly configured.");
	}
	return $telnet_open;
}

our ($bus, $bplayer, $mpris_player);

sub close_telnet {
	if ($telnet_open) {
		$telnet_open = 0;
		$telnet->put("exit\n");
		$telnet->close;
	}
}

sub send_command {
	return unless open_telnet();

	my $line;
	my $command = shift;
	$command && $command =~ s/^previous$/prev/;
	if( !$command || !grep { $_ eq $command } ("play", "pause", "next", "prev", "stop") ) {
		warn "Invalid command. Must be one of 'play', 'pause', 'next', 'prev', 'previous' or 'stop'.";
		return;
	}
	my $state;
	while( ($line = $telnet->getline(Errmode => "return", Timeout => 5)) ) {
		if( $line =~ /^11(\d)/ ) {
			$state = $1;
			last;
		}
	}
	return unless defined $state;
	if( $command eq "pause" and $state eq "2" ) {
		$telnet->print("play");
	} else {
		$telnet->print($command);
	}
	while( ($line = $telnet->getline(Errmode => "return", Timeout => 5)) && $line !~ /^11/ )
	    {}
	close_telnet;
}

sub info_clean {
	my $info = shift;

	$info->{'foo_spam_version'} = $VERSION->stringify;

	if ($info->{'isplaying'}) {
		if ($info->{'ispaused'}) {
			$info->{'state'} = "paused";
		} else {
			$info->{'state'} = "playing";
		}
	} else {
		$info->{'state'} = "stopped";
	}

	if (defined $info->{'artist'}) {
		$info->{'album artist'} = $info->{'artist'} unless defined $info->{'album artist'};
		$info->{'track artist'} = $info->{'artist'} if $info->{'album artist'} ne $info->{'artist'};
	}

	if ( defined $info->{'length'} && !defined $info->{'length_seconds'} ) {
		my ( $h, $m, $s ) = split( /\:/, $info->{'length'} );
		if ( defined $s ) {
			$info->{'length_seconds'} = $s + $m*60 + $h*3600;
		} else {
			$info->{'length_seconds'} = $m + $h*60;
		}
	}

	if (!$info->{'tracknumber'} && $info->{'track number'}) {
		$info->{'tracknumber'} = sprintf("%02d", $info->{'track number'});
	}
	if ($info->{'tracknumber'} || $info->{'track number'}) {
		$info->{'track number'} = sprintf("%0". length(int($info->{'totaltracks'} ? $info->{'totaltracks'} : 0)) ."d", $info->{'tracknumber'} ? $info->{'tracknumber'} : $info->{'track number'});
	}

	if ( $info->{'length_seconds'} and $info->{'playback_time_seconds'} ) {
		$info->{'playback_time_remaining_seconds'}
		    = $info->{'length_seconds'} - $info->{'playback_time_seconds'};
	}

	for ( ( 'length', 'playback_time', 'playback_time_remaining' ) ) {
		unless ( defined( $info->{$_} ) ) {
			if ( my $t = $info->{"${_}_seconds"} ) {
				my @u = ( 0, 0 );
				for ( my $i = 1; $i >= 0; $i-- ) {
					$u[$i] = $t % 60;
					$t = int( $t/60 );
				}
				$info->{$_}
				    = sprintf( '%s%02d:%02d', $t > 0 ? "$t:" : "", @u[ 0, 1 ] );
			}
		}
	}

	if ( $info->{'filesize'} && !$info->{'filesize_natural'} ) {
		my @mult = ('', 'k', 'M', 'G', 'T');
		my $fs = $info->{'filesize'};
		my $i = 0;
		while ($fs > 1024) {
			$fs /= 1024;
			++$i;
		}
		my $fs_n = sprintf('%.2f', $fs);
		$fs_n =~ s/\.(.*?)0+$/@{[$1 ? ".$1" : ""]}/;
		$fs_n .= "$mult[$i]B";
		$info->{'filesize_natural'} = $fs_n;
	}

	for (keys %$info) {
		delete $info->{$_} unless defined $info->{$_} and $info->{$_} ne '';
	}

	for (keys %$info) {
		$info->{$_} = decode("UTF-8", $info->{$_}) unless utf8::is_utf8($info->{$_});
	}
	return $info;
}

sub get_track_info_fb2k {
	return undef unless open_telnet();

	my $line = undef;

	unless ( defined( $telnet->print("trackinfo") ) ) {
		close_telnet();
		return undef unless open_telnet();
	}

	my @result = $telnet->waitfor(
		Match   => '/11[123]\|+.+?\|+.+?\|+(?!0\.[0-5][0-9]).*/',
		Timeout => 5
	);

	$line = $result[1] if @result;

	close_telnet();

	unless ($line) {
		irc_print("Error retrieving status from foobar2000!");
		return undef;
	}

	unless ( eval { $line = decode( "UTF-8", $line, Encode::FB_CROAK ) } ) {
		irc_print(
			"Error: line is not valid UTF-8. Check foo_controlserver's settings."
		);
		return undef;
	}

	%heap = ();

	my @fields;

	if ( $line =~ /^11.\|\|\|/ and $line =~ /\|\|\|(.*?)\|\|\|$/ )
	{    # proper setting
		@fields = split( /\|\|\|/, $line );
	} else {    # the luser didn't configure it correctly
		$line =~ s/\|\|\|/\|/g;    # fix possible half-configuration
		@fields = split( /\|/, $line );
	}

	# Standard settings
	my $info = {
		state                 => $fields[0],
		playback_time_seconds => $fields[3],
		codec                 => $fields[4],
		bitrate               => $fields[5],
		'album artist'        => $fields[6],
		album                 => $fields[7],
		date                  => $fields[8],
		genre                 => $fields[9],
		tracknumber           => $fields[10],
	    title                 => $fields[11] };
	if ( $fields[19] ) {
		$info->{'artist'}        = $fields[12];
		$info->{'totaltracks'}   = $fields[13];
		if ( $fields[20] ) { # New 0.8.2 format
			$info->{'length_seconds'}      = $fields[14];
			$info->{'_foobar2000_version'} = $fields[15];

			$info->{'codec_profile'} = $fields[16];

			$info->{'discnumber'} = $fields[17];
			$info->{'totaldiscs'} = $fields[18];

			$info->{'filesize'}  = $fields[19];
			$info->{'_path_raw'} = $fields[20];
			$info->{'_path_raw'} =~ s!\\!/!g;
		} else {
			$info->{'playback_time'} = $fields[14];
			$info->{'length'}        = $fields[15];

			$info->{'_foobar2000_version'} = $fields[16];

			$info->{'codec_profile'} = $fields[17];

			$info->{'discnumber'} = $fields[18];
			$info->{'totaldiscs'} = $fields[19];
		}
	}

	if (defined($info->{'_foobar2000_version'})) {
		my @ver = split (/ /, $info->{'_foobar2000_version'}, 2);
		$info->{'player'} = $ver[0];
		$info->{'version'} = $ver[1];
	} else {
		$info->{'player'} = $player;
	}

	$info->{'isplaying'} = 1;
	$info->{'ispaused'}  = 0;
	if ( $info->{'state'} eq "113" ) {
		$info->{'ispaused'} = 1;
	} elsif ( $info->{'state'} eq "112" ) {
		$info->{'isplaying'} = 0;
	}

	if ($info->{'_path_raw'}) {
		my @path = split /\/+/, $info->{'_path_raw'};
		$info->{'filename_ext'}  = $path[$#path];
		$info->{'directoryname'} = $path[$#path-1];

		$info->{'filename'} = $info->{'filename_ext'};
		$info->{'filename'} =~ s/(.*)\..*/$1/;
		if ($path[0] eq 'file:') {
			$info->{'path'} = join '/', @path[1 .. $#path];
		}
	}

	for ( keys %$info ) {
		delete $info->{$_}
		    if ( defined( $info->{$_} ) and $info->{$_} eq '?' );
	}

	return info_clean($info);
}

sub get_track_info_banshee {
	eval { require Net::DBus; 1 } or ( warn "Can't find Net::DBus" and return undef );

	if(!$bplayer) {
		if (!$bus) {
			$bus = Net::DBus->session or return undef;
		}
		my $banshee = $bus->get_service("org.bansheeproject.Banshee") or return undef;
		$bplayer = $banshee->get_object("/org/bansheeproject/Banshee/PlayerEngine",
		                                "org.bansheeproject.Banshee.PlayerEngine") or return undef;
	}

	my $btagsref = $bplayer->GetCurrentTrack or return undef;
	my %btags = %$btagsref;

	my %info;

	foreach (keys %btags) {
		if    ($_ eq "album-artist") { $info{'album artist'} = $btags{$_} }
		elsif ($_ eq "track-number") { $info{'track number'} = int($btags{$_}) }
		elsif ($_ eq "track-count")  { $info{totaltracks}    = $btags{$_} }
		elsif ($_ eq "disc-number")  { $info{discnumber}     = $btags{$_} }
		elsif ($_ eq "disc-count")   { $info{totaldiscs}     = $btags{$_} }
		elsif ($_ eq "name")         { $info{title}          = $btags{$_} }
		# NOTE: does not update in real time
		elsif ($_ eq "bit-rate")     { $info{bitrate}        = $btags{$_} }
		elsif ($_ eq "genre")        { $info{genre}          = $btags{$_} }
		elsif ($_ eq "album")        { $info{album}          = $btags{$_} }
		elsif ($_ eq "artist")       { $info{artist}         = $btags{$_} }
		# NOTE: does not return string dates, only integer (known bug)
		elsif ($_ eq "year")         { $info{date}           = $btags{$_} }
		# NOTE: not equivalent to foobar2000's, but close enough
		elsif ($_ eq "mime-type")    { $info{codec}          = $btags{$_} }
		elsif ($_ eq "bpm")          { $info{bpm}            = $btags{$_} if $btags{$_} }
		# TODO: add the same to foobar2000
		elsif ($_ eq "score")        { $info{rating}         = $btags{$_} }
	}

	$info{'state'} = $bplayer->GetCurrentState;

	$info{'player'} = "Banshee";

	my $posmili = $bplayer->GetPosition;
	my $lenmili = $bplayer->GetLength;

	$info{'playback_time_seconds'} = $posmili / 1000;
	$info{'length_seconds'}        = $lenmili / 1000;
	$info{'playback_time_remaining_seconds'} = ($lenmili - $posmili) / 1000;

	# NOTE: there's no "stopped" state in banshee
	$info{isplaying} = 1;
	$info{ispaused} = $info{state} eq "paused" ? 1 : 0;

	return info_clean(\%info);
}

sub get_track_info_mpris {
	eval { require Net::DBus; 1 } or ( warn "Can't find Net::DBus" and return undef );

	if(!$mpris_player->{$player}) {
		if (!$bus) {
			$bus = Net::DBus->session or return undef;
		}
		my $p = $bus->get_service("org.mpris.$player") or return undef;
		$mpris_player->{$player} = $p->get_object("/Player",
		                                "org.freedesktop.MediaPlayer") or return undef;
	}

	my $metadata = $mpris_player->{$player}->GetMetadata or return undef;
	my %info;

	foreach (keys %$metadata) {
		if    ($_ eq 'location')         { $info{url}            = $metadata->{$_} }
		elsif ($_ eq 'title')            { $info{title}          = $metadata->{$_} }
		elsif ($_ eq 'artist')           { $info{artist}         = $metadata->{$_}; $info{'album artist'} = $info{artist} }
		elsif ($_ eq 'album')            { $info{album}          = $metadata->{$_} }
		elsif ($_ eq 'tracknumber')      { $info{'track number'} = $metadata->{$_} }
		elsif ($_ eq 'time')             { $info{length_seconds} = $metadata->{$_} }
		elsif ($_ eq 'genre')            { $info{genre}          = $metadata->{$_} }
		elsif ($_ eq 'comment')          { $info{file_comment}   = $metadata->{$_} }
		elsif ($_ eq 'rating')           { $info{rating}         = $metadata->{$_} }
		elsif ($_ eq 'date')             { $info{date}           = $metadata->{$_} }
		elsif ($_ eq 'arturl')           { $info{arturl}         = $metadata->{$_} }
		elsif ($_ eq 'audio-bitrate')    { $info{bitrate}        = $metadata->{$_} }
		elsif ($_ eq 'audio-samplerate') { $info{samplerate}     = $metadata->{$_} }
	}

	$info{player} = $player;

	my $state = $mpris_player->{$player}->GetStatus;
	$info{isplaying} = 1;
	$info{ispaused} = $state->[0] == 1 ? 1 : 0;
	$info{isplaying} = 0 if($state->[0] == 2);
	return if !$info{isplaying};

	my $posmili = $mpris_player->{$player}->PositionGet;
	my $lenmili = $metadata->{mtime};
	$info{playback_time_seconds} = $posmili / 1000;
	$info{playback_time_remaining_seconds} = ($lenmili - $posmili) / 1000;


	return info_clean(\%info);
}

sub get_track_info {
	if    ($player eq "foobar2000") { return get_track_info_fb2k(@_); }
	elsif ($player eq "banshee")    { return get_track_info_banshee(@_); }
	else                            { return get_track_info_mpris(@_); }
}

sub parse_format {
	my ( $format, $info, $sublevel ) = @_;
	$sublevel = 0 if not defined $sublevel;

	my $output = "";

	$format =~ s/\R//g;    # ignore line breaks
	my @chars = split( //, $format );

	# Language Definition

	# lowercasestring      <== should be parsed as a tag name, makes the expression fail if such tag is not defined
	# []                   <== brackets allow the parsing inside them to fail
	# $func(arg1,arg2,...) <== function call (see parse_subfunction for details)
	# ''                   <== string literal (ignores all parsing)
	# \(character)         <== literal character

	# Bracket Nesting

	# A bracket returns a defined value only if it has at least one tag or at least one of its embedded brackets return true.

	my @tokens   = ();
	my $tagcount = 0;
	my $fail     = 0;

	my $literal = 0;
	my $sub     = 0;
	my $func    = 0;
	my $tagmode = 0;
	my $str     = "";
	my $ignore  = 0;

	for ( my $i = 0; $i < @chars; $i++ )
	{    # 1st Pass (Lexical analysis, push into @tokens)
		if ($literal) {    # If on literal mode
			$str .= $chars[$i]
			    ; # Simply copy everything as-is until an unescaped ' is found
			if ( $chars[$i] eq "'" ) {
				push @tokens, $str;
				$str     = "";
				$literal = 0;
			} elsif ( not defined( $chars[ $i + 1 ] ) )
			{ # This means we ended the string with an uneven number of unescaped 's
				warn "Malformed: mismatched ': $str";
				return undef;
			}
		} elsif ($sub) {    # If on subexpression mode
			$str .= $chars[$i]
			    ;    # Copy everything as-is until an unescaped ] is found
			if ( $chars[$i] eq "'" ) {
				$ignore = !$ignore;
			} elsif ( $chars[$i] eq "[" )
			{ # We must copy any sub-subexpressions inside this sub-expression for recursive evaluation
				++$sub unless $ignore;
			} elsif ( $chars[$i] eq "]" and !$ignore and --$sub == 0 ) {
				push @tokens, $str;
				$str = "";
			} elsif ( not defined( $chars[ $i + 1 ] ) )
			{    # This means we ended the string without $sub being 0
				warn "Malformed: mismatched [: $str";
				return undef;
			}
		} elsif ($tagmode) {    # If on tag mode
			$str .= $chars[$i]
			    ;    # Copy tags as-is until any % character is found
			if ( $chars[$i] eq '%' ) {
				push @tokens, $str;
				$str     = "";
				$tagmode = 0;
			} elsif ( not defined( $chars[ $i + 1 ] ) ) {
				warn "Malformed: mismatched %: $str";
				return undef;
			}
		} elsif ($func) {    # If on function mode
			$str
			    .= $chars[$i]; # Copy everything until an unescaped ) is found
			if ( $chars[$i] eq "'" ) {
				$ignore = !$ignore;
			} elsif ( $chars[$i] eq "(" ) {
				$func++ unless $ignore;
			} elsif ( $chars[$i] eq ")" and !$ignore and --$func <= 1 ) {
				push @tokens, $str;
				$str  = "";
				$func = 0;
			} elsif ( not defined( $chars[ $i + 1 ] ) ) {
				warn "Malformed: mismatched (: $str";
				return undef;
			}
		} else {
			if ( $chars[$i] eq "'" ) {
				push @tokens, "$str" if $str ne "";   # Found an opening quote
				$str     = $chars[$i];
				$literal = 1;                         # Enter literal mode
			} elsif ( $chars[$i] eq "[" ) {
				push @tokens, "$str"
				    if $str ne "";    # Found a subexpression opener
				$str = $chars[$i];
				$sub = 1;             # Enter subexpression mode
			} elsif ( $chars[$i] eq "\$" ) {
				push @tokens, "$str" if $str ne "";
				$str  = $chars[$i];
				$func = 1;            # Enter subfunction mode
			} elsif ( $chars[$i] eq "%" ) {
				push @tokens, "$str" if $str ne "";    # Found a tag name
				$str     = $chars[$i];
				$tagmode = 1;                          # Enter tag mode
			} else {
				$str .= $chars[$i];                    # Copy as a literal
			}
		}
	}

	push @tokens, "$str"
	    if $str ne ""
	;    # Make sure whatever is left from parsing is added as a literal

	foreach my $token (@tokens) {    # 2nd Pass, execute tokens
		if ( $token =~ /^'(.*)'$/ or $token =~ /^([^['%\$].*)$/ )
		{                            # If the token is a literal, then
			$output
			    .= $token eq "''"
			    ? "'"
			    : $1;    # '' means a literal ', otherwise literal contents
		} elsif ( $token =~ /^%(.*)%$/ ) {    # If this is a tag
			$token = $1;
			return undef unless defined( $info->{$token} );
			$output .= $info->{$token};       # Copy value to output
		} elsif ( $token =~ /^\[(.*)\]$/ ) { # If the token is a subexpression
			$token = $1;
			my $recurse
			    = parse_format( $token, $info, $sublevel + 1 );    # Recurse
			$output .= $recurse if defined($recurse);
		} elsif ( $token =~ /^\$/ ) {    # If the token is a subfunction
			my $res = parse_subfunction( $token, $info, $sublevel );
			return undef unless defined($res);
			$output .= $res;
		} else {
			warn "Parsing error: $token";
			return undef;
		}
	}

	return $output;
}

sub build_output {
	my ( $format, $info, $sublevel ) = @_;
	$sublevel = 0 if not defined $sublevel;

	return parse_format( $format, $info, $sublevel );
}

sub parse_subfunction {
	my ( $function, $info, $sublevel ) = @_;

	$function =~ /^\$(.*?)\((.*)\)$/;

	my $func = $1;

	my @rawargs = split( //, $2 );
	my @args = ();

	my $ignore = 0;
	my $str    = "";
	for ( my $i = 0; $i < @rawargs; $i++ ) {
		if ( $rawargs[$i] eq "'" ) {
			$ignore = !$ignore;
		} elsif ( $rawargs[$i] eq "," ) {
			unless ($ignore) {
				push @args, $str;
				$str = "";
				++$i;
			}
		}
		$str .= $rawargs[$i] if defined( $rawargs[$i] );
	}
	push @args, $str;

	for ( my $i = 0; $i < @args; $i++ ) {
		$args[$i] = parse_format( $args[$i], $info, $sublevel + 1 );
	}

	if ( $func eq "len" ) {
		return defined $args[0] ? length( $args[0] ) : undef;
	} elsif ( $func eq "repeat" ) {
		return ( defined $args[0] and defined $args[1] )
		    ? ( $args[0] x $args[1] )
		    : undef;
	} elsif ( $func eq "trim" ) {
		my ($str) = @args;
		return undef unless defined $str;
		$str =~ /^\s*+(.*?)\s*+$/;
		return $1;
	} elsif ( $func eq "put" or $func eq "puts" ) {
		my ( $var, $val ) = @args;
		return undef unless ( defined $var and defined $val );
		$heap{$var} = $val;
		return ( $func eq "put" ) ? $val : "";
	} elsif ( $func eq "get" ) {
		my ($var) = @args;
		return undef unless defined $var;
		return exists $heap{$var} ? $heap{$var} : "";
	} elsif ( $func eq "pad"
		or $func eq "pad_right"
		or $func eq "left"
		or $func eq "cut"
		or $func eq "padcut"
		or $func eq "padcut_right" ) {
		my ( $str, $maxlen, $char ) = @args;
		return undef unless ( defined $str and $maxlen );

		my $pad = (
			       $func eq "pad"
			    or $func eq "pad_right"
			    or $func eq "padcut"
			    or $func eq "padcut_right"
		);
		my $cut = (
			       $func eq "left"
			    or $func eq "cut"
			    or $func eq "padcut"
			    or $func eq "padcut_right"
		);

		if ($cut) {
			$str = substr( $str, 0, $maxlen );
		}
		if ($pad) {
			$char = " " unless defined $char and $char ne "";
			$char = substr( $char, 0, 1 );
			$str .= ( $char x ( $maxlen - length($str) ) );
		}
		return $str;
	} elsif ( $func eq "right" ) {
		my ( $str, $maxlen ) = @args;
		return undef unless ( defined $str and defined $maxlen );
		return substr( $str, -$maxlen );
	} elsif ( $func eq "insert" or $func eq "replace" ) {
		my ( $haystack, $needle, $pos ) = @args;
		return undef
		    unless ( defined($haystack)
			and defined($needle)
			and defined($pos) );
		if ( $func eq "insert" ) {
			return
			      substr( $haystack, 0, $pos )
			    . $needle
			    . substr( $haystack, $pos );
		}
		$needle = quotemeta($needle);
		$haystack =~ s/$needle/$pos/g;
		return $haystack;
	} elsif ( $func eq "if" or $func eq "if2" ) {
		my ( $test, $iftrue, $iffalse );
		if ( $func eq "if" ) {
			( $test, $iftrue, $iffalse ) = @args;
		} else {
			( $test, $iffalse ) = @args;
			$iftrue = $test;
		}

		if ($test) {
			return $iftrue;
		} else {
			return $iffalse;
		}
	} elsif ( $func eq "if3" ) {
		foreach (@args) {
			return $_ if $_;
		}
		return undef;
	} elsif ( $func eq "greater" ) {
		my ( $arg1, $arg2 ) = @args;
		return undef unless ( defined($arg1) or defined($arg2) );
		return $arg1 unless defined $arg2;
		return $arg2 unless defined $arg1;
		return $arg1 if $arg1 >= $arg2;
		return $arg2;
	} elsif ( $func eq "longer" ) {
		my ( $arg1, $arg2 ) = @args;
		return undef unless ( defined($arg1) or defined($arg2) );
		return $arg1 unless defined $arg2;
		return $arg2 unless defined $arg1;
		return $arg1 if length($arg1) >= length($arg2);
		return $arg2;
	} elsif ( $func eq "longest" ) {
		return undef unless scalar(@args);
		my $longest = $_[0];
		foreach (@args) {
			next unless defined;
			$longest = $_ if length($_) > length($longest);
		}
		return $longest;
	} elsif ( $func eq "ifgreater"
		or $func eq "ifequal"
		or $func eq "iflonger" ) {
		my ( $arg1, $arg2, $iftrue, $iffalse ) = @args;

		unless ( defined($arg2) ) {
			return $iftrue if ( defined($arg1) );
			return $iffalse;
		}
		return $iffalse unless ( defined($arg1) );

		if ( $func eq "iflonger" ) {
			return defined($arg1) ? $iftrue : $iffalse
			    unless ( defined($arg1) and defined($arg2) );
			return $iftrue if ( length($arg1) > length( " " x $arg2 ) );
		} elsif ( $func eq "ifequal" ) {

			# Any of the args may not be comparable, return false in that case
			return $iftrue if ( defined($arg1) and defined($arg2) );
			return $iffalse unless ( defined($arg1) and defined($arg2) );
			eval { return $iftrue if $arg1 == $arg2 };
		} else {    # ifgreater
			return defined($arg1) ? $iftrue : $iffalse
			    unless ( defined($arg1) and defined($arg2) );
			eval { return $iftrue if $arg1 > $arg2 };
		}
		return $iffalse;
	} elsif ( $func eq "abbr" ) {
		my ( $arg1, $arg2 ) = ( 0, 0 );
		$arg1 = $args[0];
		$arg2 = $args[1] if ( defined( $args[1] ) );
		return undef unless ( defined $arg1 and $arg2 >= 0 );

		if ( length($arg1) > $arg2 ) {
			my $abbr = "";
			my @tokens = split( /\s+/, $arg1 );
			foreach my $token (@tokens) {
				my @chars = split( //, $token );
				$abbr .= $chars[0];
			}
			return $abbr;
		}
		return $arg1;
	} elsif ( $func eq "num" ) {
		my ( $arg1, $arg2 ) = @args;
		return undef unless ( defined($arg1) and $arg2 > 0 );
		return sprintf( "%0${arg2}d", $arg1 );
	} elsif ( $func =~ /^(add|sub|mul|div|mod|max|min)$/ ) {
		my ( $arg1, $arg2 ) = @args;
		return undef unless ( defined($arg1) and defined($arg2) );

		# Make sure both are numbers. Better way to do this?
		return undef unless eval { $arg1 != $arg2 or $arg1 == $arg2 };
		return $arg1 + $arg2 if ( $func eq "add" );
		return $arg1 - $arg2 if ( $func eq "sub" );
		return $arg1*$arg2   if ( $func eq "mul" );
		return $arg1/$arg2   if ( $func eq "div" );
		return $arg1 % $arg2 if ( $func eq "mod" );
		return ( $arg1 >= $arg2 ? $arg1 : $arg2 ) if ( $func eq "max" );
		return ( $arg1 < $arg2  ? $arg1 : $arg2 ) if ( $func eq "min" );
	} elsif ( $func =~ /^(and|or|xor|not)$/ ) {
		my ( $arg1, $arg2 ) = @args;
		$arg1 = 0 unless defined $arg1;
		$arg2 = 0 unless defined $arg2;

		# Need to give explicit returns to avoid eating on parse_format

		return ( $arg1 ? 0 : 1 ) if ( $func eq "not" );
		return ( ( $arg1 && $arg2 ) ? 1 : 0 ) if ( $func eq "and" );
		return ( ( $arg1 || $arg2 ) ? 1 : 0 ) if ( $func eq "or" );
		return ( ( $arg1 && !$arg2 ) ? 1 : ( ( !$arg1 && $arg2 ) ? 1 : 0 ) )
		    if ( $func eq "xor" );
	} elsif ( $func eq "strcmp" or $func eq "stricmp" ) {
		my ( $arg1, $arg2 ) = @args;
		return undef unless ( defined($arg1) and defined($arg2) );
		return ( ( lc($arg1) eq lc($arg2) ) ? 1 : 0 )
		    if ( $func eq "stricmp" );
		return ( ( $arg1 eq $arg2 ) ? 1 : 0 );
	} elsif ( $func eq "caps" ) {
		my ($arg1) = @args;
		return undef unless defined $arg1;
		$arg1 =~ s/\b(\S)(\S*)\b/@{[uc($1)]}@{[lc($2)]}/g;
		return $arg1;
	} elsif ( $func eq "caps2" ) {
		my ($arg1) = @args;
		return undef unless defined $arg1;
		$arg1 =~ s/\b(\S)/@{[uc($1)]}/g;
		return $arg1;
	} elsif ( $func eq "lower" or $func eq "upper" ) {
		my ($arg1) = @args;
		return undef unless defined $arg1;
		return lc($arg1) if $func eq "lower";
		return uc($arg1);
	} elsif ( $func eq "fix_eol" ) {
		my ( $meta, $repl ) = @args;
		$repl = " (...)" unless $repl;
		return undef unless defined($meta);
		$meta =~ s/\010?\013.*//;
		return $meta;
	}

	warn "Unknown or unimplemented function: $function";
	return undef;
}

sub get_np_string {
	my $info = get_track_info();
	$info->{foo_spam_comment} = $_[0] if $_[0];
	if ( defined($info) ) {
		return build_output( $format, $info );
	}
	return undef;
}

sub get_help_string {
	my $help = <<EOF
Required Plugin: foo_controlserver
URL: http://www.hydrogenaudio.org/forums/index.php?showtopic=38114
Required settings: Control Server tab:
* Server Port: $hostport
* UTF-8 output/input: checked
* Base delimiter: |||
Recommended setting:
* Number of Clients: Some big number like 700
* Fields: %codec%|||%bitrate%|||%album artist%|||%album%|||%date%|||%genre%|||%tracknumber%|||%title%|||%artist%|||%totaltracks%|||%length_seconds%|||%_foobar2000_version%|||%codec_profile%|||%discnumber%|||%totaldiscs%|||%filesize%|||%_path_raw%

NOTE: the script only works with either the default or this custom Fields line.

This script can also work via SSH tunneling, by using -R 3333:localhost:3333.
EOF
	    ;
	return $help;
}

sub get_intro_string {
	my $intro = <<EOF
\002----------------------------------------------------------------------------------------------------
\002foo_spam - prints the currently playing track from foobar2000, Banshee or an MPRIS compliant player
\002Version @{[$VERSION->stringify]} - Created by Kovensky \(irc.rizon.net #shameimaru\)
This script requires Banshee or a properly configured foobar2000.
Note that the script only works remotely with foobar2000.
Run /foo_help for help setting foobar2000 up.
\002----------------------------------------------------------------------------------------------------
Usage:
/aud         - prints the playing song as an ACTION
/np          - alias to /aud
/foo_control - sends a command to your player. One of 'play', 'pause', 'prev', 'next', 'stop'.
/foo_help    - explains how to set up foobar2000
/foo_format  - explains how to set up the output format
\002----------------------------------------------------------------------------------------------------
\002To choose which player will be used (default is foobar2000), do:
 * Irssi: /set foo_player <player>
 * X-Chat: /set_foo_player <player>
 * WeeChat: /set plugins.var.perl.foo_spam.player <player>
For now, /foo_control only supports foobar2000.
foobar2000 and banshee have specific implementations. Other players will use the MPRIS interface.
\002----------------------------------------------------------------------------------------------------
EOF
	    ;
	return $intro;
}

sub get_foo_format_help_string {
	my $help = <<EOF
Format Definition
Example: %artist% - [%album% - ]%title%

foo_spam now uses the same syntax as foobar2000 (title format), however only
a subset of it is currently implemented. To see the list of supported
tags, use /foo_tags. To see the list of supported functions, use
/foo_funcs.

Not all tags available on foobar2000 are available on banshee, but almost all
of them are. Some tags return different values, like %codec_profile%.

To change the format, you can use:
 * Irssi: /set foo_format <new format> (use /set -default to reset)
 * X-Chat: /set_foo_format <new format> (use /set_foo_format default to reset)
 * WeeChat: /set plugins.var.perl.foo_spam.format <new format> (use /unset to reset)
You can also edit the script and change the value of \$default_format, in case
you use an unsupported client.

Default: $default_format
EOF
	    ;
	return $help;
}

sub get_taglist_string {
	my $list = <<EOF
List of available tags (refer to foobar2000's documentation for their meanings):
 - %isplaying%, %ispaused%, %_foobar2000_version%
 - %playback_time%, %playback_time_remaining%, %length% (plus the _seconds variants)
 - %artist%, %album artist%, %track artist%, %album%, %title%, %genre%
 - %date%, %discnumber%, %totaldiscs%, %tracknumber%, %track number%,  %totaltracks%
 - %codec%, %bitrate%, %codec_profile%
 - %filename%, %filename_ext%, %directoryname%, %path%, %_path_raw%
 - %filesize%, %filesize_natural%
foo_spam sets %foo_spam_version% with its own version.
foo_spam also sets %player% and %version%, which refer to the used player and its version.
The %foo_spam_comment% tag contains all arguments that the user gives to /aud in a single string.
EOF
	    ;
	return $list;
}

sub get_funclist_string {
	my $list = <<'EOF'
List of available functions (refer to foobar2000's documentation for their meanings):
 - $if(X,Y,Z), $if2(X,Y), $if3(X,Y,Z,...), $ifgreater(A,B,C,D), $iflonger(A,B,C,D), $ifequal(A,B,C,D)
 - $and(X,Y), $or(X,Y), $xor(X,Y), $not(X)
 - $strcmp(X,Y), $stricmp(X,Y), $len(X), $num(X,Y)
 - $greater(X,Y), $longer(X,Y), $longest(A,B,C,...)
 - $caps(X), $caps2(X), $lower(X), $upper(X)
 - $trim(A), $pad(X,Y), $pad_right(X,Y), $pad(X,Y,Z), $pad_right(X,Y,Z), $left(X,Y), $cut(X,Y), $padcut(X,Y), $padcut_right(X,Y), $right(X,Y)
 - $insert(A,B,N), $replace(A,B,C), $repeat(X,N)
 - $abbr(X), $abbr(X,Y)
 - $add(X,Y), $sub(X,Y), $mul(X,Y), $div(X,Y), $mod(X,Y), $min(X,Y), $max(X,Y)
 - $put(name,text), $puts(name,text), $get(name)
EOF
	    ;
	return $list;
}

if (HAVE_IRSSI) {
	*print_now_playing = sub {
		my ( $data, $server, $witem ) = @_;

		$format = Irssi::settings_get_str("foo_format");
		$player = lc(Irssi::settings_get_str("foo_player"));
		$hostname = Irssi::settings_get_str("foo_host");
		$hostport = Irssi::settings_get_str("foo_port");

		my $str = get_np_string( decode( "UTF-8", $data ) );
		if ((defined($str)) && ($witem) &&
				(any { $witem->{'type'} eq $_ } qw{CHANNEL QUERY})) {
			$witem->command( encode_utf8("me $str") );
		}
	};

	*print_foo_help = sub {
		my $help = get_help_string();
		$help =~ s/%/%%/g;
		Irssi::print($help);
	};

	*print_foo_format_help = sub {
		my $help = get_foo_format_help_string();
		$help =~ s/%/%%/g;
		Irssi::print($help);
	};

	*irc_print = sub {
		Irssi::print( $_[0] );
	};

	*print_foo_tags = sub {
		my $help = get_foo_taglist_string();
		$help =~ s/%/%%/g;
		Irssi::print($help);
	};

	*print_foo_funcs = sub {
		Irssi::print( get_funclist_string() );
	};

	Irssi::settings_add_str( "foo_spam", "foo_format", $format );
	Irssi::settings_add_str( "foo_spam", "foo_player", $player );
	Irssi::settings_add_str( "foo_spam", "foo_host", $hostname );
	Irssi::settings_add_str( "foo_spam", "foo_port", $hostport );
	Irssi::command_bind( 'aud',         'print_now_playing' );
	Irssi::command_bind( 'np',          'print_now_playing' );
	Irssi::command_bind( 'foo_control', sub { send_command(shift) } );
	Irssi::command_bind( 'foo_help',    'print_foo_help' );
	Irssi::command_bind( 'foo_format',  'print_foo_format_help' );
	Irssi::command_bind( 'foo_tags',    'print_foo_tags' );
	Irssi::command_bind( 'foo_funcs',   'print_foo_funcs' );
} elsif (HAVE_XCHAT) {
	*print_now_playing = sub {
		my $str = get_np_string( $_[0][1] ? $_[1][1] : undef );
		if ( defined($str) ) {
			Xchat::command( encode_utf8("me $str") );
		}
		return Xchat::EAT_ALL();
	};

	*print_foo_help = sub {
		Xchat::print( get_help_string() );
		return Xchat::EAT_ALL();
	};

	*irc_print = sub {
		Xchat::print(@_);
	};

	*set_foo_settings = sub {
		my $setting = shift;
		my $value = shift;
		my $ref;
		if    ($setting eq 'player')   { $ref = \$player }
		elsif ($setting eq 'format')   { $ref = \$format }
		elsif ($setting eq 'hostname') { $ref = \$hostname }
		elsif ($setting eq 'port')     { $ref = \$hostport }

		if (not defined $ref) {
			Xchat::print("Unknown setting: $setting.\n");
			return;
		}
		if (not defined $value) {
			Xchat::print("Current $setting: $$ref\n");
			return;
		}
		if (($setting eq 'format') && ($value eq 'default')) { $format = $default_format; }
		else { $$ref = $value; }
		Xchat::print("Changed $setting to $$ref\n");
		if (open( $settings_file, ">", Xchat::get_info('xchatdir') . "/foo_spam.conf" )) {
			print $settings_file "player=$player\n";
			print $settings_file "format=$format\n";
			print $settings_file "hostname=$hostname\n";
			print $settings_file "port=$hostport\n";
			close($settings_file);
		}
		else {
			Xchat::print("Failed to save settings! Error: $!\n");
		}
	};

	*set_foo_format = sub {
		set_foo_settings('format', $_[1][1]);
		return Xchat::EAT_ALL();
	};
	if ( defined(*set_foo_format) ) { }    # Silence a warning

	*set_foo_player = sub {
		set_foo_settings('player', $_[0][1]);
		return Xchat::EAT_ALL();
	};
	if ( defined(*set_foo_player) ) { }    # Silence a warning

	*set_foo_port = sub {
		set_foo_settings('port', $_[0][1]);
		return Xchat::EAT_ALL();
	};
	if ( defined(*set_foo_port) ) { }      # Silence a warning

	*set_foo_hostname = sub {
		set_foo_settings('hostname', $_[0][1]);
		return Xchat::EAT_ALL();
	};
	if ( defined(*set_foo_hostname) ) { }  # Silence a warning

	*print_foo_format_help = sub {
		Xchat::print( get_foo_format_help_string() );
		return Xchat::EAT_ALL();
	};

	*print_foo_tags = sub {
		Xchat::print( get_taglist_string() );
		return Xchat::EAT_ALL();
	};

	*print_foo_funcs = sub {
		Xchat::print( get_funclist_string() );
		return Xchat::EAT_ALL();
	};

	if ( open($settings_file, "<", Xchat::get_info('xchatdir') . "/foo_spam.conf") ) {
		for (<$settings_file>) {
			chomp;
			if    (/^format=(.*)/)   { $format   = $1 }
			elsif (/^player=(.*)/)   { $player   = lc($1) }
			elsif (/^hostname=(.*)/) { $hostname = $1 }
			elsif (/^port=(.*)/)     { $hostport = $1 }
			elsif ($_)               { $format   = $_ }
		}
		close($settings_file);
	}

	Xchat::hook_command( "np", "print_now_playing",
		{ help => "alias to /aud" } );
	Xchat::hook_command(
		"aud",
		"print_now_playing", {
			help =>
			    "prints your currently playing song on your selected player on an ACTION"
	    } );
	Xchat::hook_command( "foo_help", "print_foo_help",
		{ help => "explains how to set up foobar2000" } );
	Xchat::hook_command( "set_foo_format", "set_foo_format",
		{ help => "displays or changes the current format string" } );
	Xchat::hook_command( "set_foo_player", "set_foo_player",
		{ help => "displays or changes the used player" } );
	Xchat::hook_command( "set_foo_port", "set_foo_port",
		{ help => "displays or changes the port to connect to" } );
	Xchat::hook_command( "set_foo_hostname", "set_foo_hostname",
		{ help => "displays or changes the hostname to connect to" } );
	Xchat::hook_command( "foo_control", sub { send_command($_[0][1]); return Xchat::EAT_ALL(); },
		{ help => "sends a command to foobar2000. One of play, pause, stop, next or prev." } );
	Xchat::hook_command( "foo_format", "print_foo_format_help",
		{ help => "explains how to configure the format string" } );
	Xchat::hook_command( "foo_tags", "print_foo_tags",
		{ help => "lists all available tags" } );
	Xchat::hook_command( 'foo_funcs', 'print_foo_funcs',
		{ help => "lists all available functions" } );
} elsif (HAVE_WEECH) {
	*print_now_playing = sub {
		my ( $data, $buffer, @args ) = @_;
		$format   = weechat::config_get_plugin("format");
		$player   = lc(weechat::config_get_plugin("player"));
		$hostport = weechat::config_get_plugin("port");
		$hostname = weechat::config_get_plugin("hostname");
		my $str = get_np_string(
			$args[0] ? decode( "UTF-8", join( ' ', @args ) ) : undef );
		if ( defined($str) ) {
			weechat::command( $buffer, encode_utf8("/me $str") );
		}
		return weechat::WEECHAT_RC_OK_EAT();
	};

	*irc_print = sub {
		weechat::print( '', shift );
	};

	*print_foo_help = sub {
		irc_print( get_help_string() );
		return weechat::WEECHAT_RC_OK_EAT();
	};

	*print_foo_format_help = sub {
		irc_print( get_foo_format_help_string() );
		return weechat::WEECHAT_RC_OK_EAT();
	};

	*print_foo_tags = sub {
		irc_print( get_taglist_string() );
		return weechat::WEECHAT_RC_OK_EAT();
	};

	*print_foo_funcs = sub {
		irc_print( get_funclist_string() );
		return weechat::WEECHAT_RC_OK_EAT();
	};

	unless ( weechat::config_is_set_plugin("format") ) {
		weechat::config_set_plugin( "format", $default_format );
	}
	unless ( weechat::config_is_set_plugin("player") ) {
		weechat::config_set_plugin( "player", "foobar2000" );
	}
	unless ( weechat::config_is_set_plugin("port") ) {
		weechat::config_set_plugin( "port", $hostport );
	}
	unless ( weechat::config_is_set_plugin("hostname") ) {
		weechat::config_set_plugin( "hostname", $hostname );
	}

	weechat::hook_command( 'np', 'alias to /aud',
		'', '', '%(nicks)', 'print_now_playing', '' );
	weechat::hook_command( 'aud',
		'prints your currently playing song on your selected player on an ACTION',
		'', '', '%(nicks)', 'print_now_playing', '' );
	weechat::hook_command( 'foo_control', "sends a command to foobar2000. One of play, pause, stop, next or prev.",
	    '', '', '', sub { send_command($_[2][0]); return weechat::WEECHAT_RC_OK_EAT(); }, '' );
	weechat::hook_command( 'foo_help', 'explains how to set up foobar2000',
		'', '', '', 'print_foo_help', '' );
	weechat::hook_command( 'foo_format',
		'explains how to configure the format string',
		'', '', '', 'print_foo_format_help', '' );
	weechat::hook_command( 'foo_tags', 'lists all available tags',
		'', '', '', 'print_foo_tags', '' );
	weechat::hook_command( 'foo_funcs', 'lists all available functions',
		'', '', '', 'print_foo_funcs', '' );
} else {
	$| = 1;
	binmode( STDERR, ":encoding(utf-8)" );
	binmode( STDOUT, ":encoding(utf-8)" );

	my $comment = undef;
	my $command = undef;

	GetOptions( 'player=s' => sub {
	            	if    ($_[1] eq "foobar2000") { $player = "foobar2000" }
	            	elsif ($_[1] eq "banshee")    { $player = "banshee" }
	            	else                          { $player = lc($_[1]) }
	            },
	            'comment=s' => \$comment,
	            'format=s' => \$format,
	            'command=s' => \$command,
	            'host=s' => \$hostname,
	            'port=s' => \$hostport,
	            'help' => sub {
	            	$_ = <<EOF;
foo_spam $VERSION - prints the currently playing track from foobar2000, Banshee or an MPRIS compliant player
Supports command line, X-Chat, irssi and weechat.
Options:
    --player=PLAYER    Any of the supported players
    --format=FORMAT    The string template in foobar2000's title format syntax.
    --comment=COMMENT  The value of \%foo_spam_comment%
    --command=COMMAND  A command to send to foobar2000. One of 'play', 'pause', 'prev', 'next', 'stop'. Foobar2000 only.
    --host=ADDRESS     The address of the computer running foobar2000. Default is localhost. Foobar2000 only.
    --port=PORT        The port number foobar2000 is listening at. Default is 3333. Foobar2000 only.
EOF
	            	print $_;
	            	exit;
	            });

	*irc_print = sub {
		print( STDERR "@_\n" ) if @_;
	};
	$format = join( " ", @ARGV ) if $ARGV[0];
	send_command($command) if ($command);
	my $np = get_np_string($comment);
	print "$np\n" if $np;
}

if ( HAVE_XCHAT or HAVE_IRSSI or HAVE_WEECH ) {
	irc_print( get_intro_string() );
}

