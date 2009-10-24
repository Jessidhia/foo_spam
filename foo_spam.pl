#! /usr/bin/env perl

use warnings;
use strict;
#use utf8;
use Encode;

use Net::Telnet;
use File::Path;
use Time::HiRes qw(usleep);

BEGIN { *HAVE_XCHAT = Xchat->can('register') ? sub {1} : sub {0}; *HAVE_IRSSI = Irssi->can('command_bind') ? sub{1} : sub{0}; }

Xchat::register("foo_spam","0.5.1", "Prints the current playing song from foobar2000.", \&close_telnet) if (HAVE_XCHAT);
# ChangeLog:
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
# The progress bar produced by $bar() is imprecise.
# Missing documentation for $functions().

# TODO:
# Replace the current format syntax by foobar2000's title format

our $telnet_open    = 0;
our $telnet         = undef;
our $default_format = 'player[ (version)]: [albumartist ]\[[date ][album$ifgreater(totaldiscs,1,[\' - Disc \'discnumber\/totaldiscs],)][ #track[/totaltracks]]\] [trackartist - ]title \[position[/length]\][ bitrate\'kbps\'][ codec[ codec_profile]][ <-- comment]';
our $format         = $default_format;

our $setting_file   = undef; # Only used by Xchat

sub open_telnet {
	$telnet = new Net::Telnet(Port => 3333, Timeout => 10, Errmode => 'return') if not defined($telnet);
	$telnet_open = $telnet->open("localhost");
	if($telnet_open) {
		irc_print("Successfully connected to foobar2000.");
	} else {
		irc_print("Error opening telnet connection! Make sure fb2k is running.");
		irc_print("Also check if foo_controlserver is properly configured.");
	}
	return $telnet_open;
}

sub close_telnet {
	if($telnet_open) {
		$telnet_open = 0;
		$telnet->put("exit\n");
		$telnet->close;
		irc_print("Connection to foobar2000 closed.");
	}
}

sub get_track_info {
	if (!$telnet_open) {
		return undef if not open_telnet();
	}

	$telnet->buffer_empty();
	$telnet->getlines(All => 0, Timeout => 0); # Discard old lines

	if(not defined($telnet->print("trackinfo"))) {
		close_telnet();
		if (! open_telnet()) {
			return undef;
		}
	}

	my $line = undef;

	my @result = $telnet->waitfor(Match => '/11[123]\|+.+?\|+.+?\|+(?!0\.[0-5][0-9]).*/', Timeout => 5);

	$line = $result[1] if @result;

	$line = decode_utf8($line);

	if(not defined($line)) {
		irc_print("Error retrieving status from foobar2000!");
		close_telnet();
		return undef;
	}

	$telnet->buffer_empty();
	$telnet->getlines(All => 0, Timeout => 0); # Discard old lines

	my @fields;

	if($line =~ /^11.\|\|\|/ and $line =~ /\|\|\|(.*?)\|\|\|$/) { # proper setting
		@fields = split(/\|\|\|/, $line);
	} else {                    # the luser didn't configure it correctly
		$line =~ s/\|\|\|/\|/g; # fix possible half-configuration
		@fields = split(/\|/, $line);
	}

	# Standard settings
	my $info = {player         => 'foobar2000',
	            state          => $fields[0],
	            playlistindex  => $fields[1],
	            trackindex     => $fields[2],
	            pos_sec        => $fields[3],
	            codec          => $fields[4],
	            bitrate        => $fields[5],
	            albumartist    => $fields[6],
	            album          => $fields[7],
	            date           => $fields[8],
	            genre          => $fields[9],
	            track          => $fields[10],
	            title          => $fields[11]};
	if ($fields[15]) { # Compatibility with 0.1
		$info->{'artist'}        = $fields[12];
		$info->{'totaltracks'}   = $fields[13];
		$info->{'position'}      = $fields[14];
		$info->{'length'}        = $fields[15];

		if ($fields[16]) {
			$info->{'version'}   = $fields[16];
			$info->{'version'}   =~ s/^foobar2000 //;

			# New field in 0.5
			$info->{'codec_profile'} = $fields[17] if ($fields[17]);

			if ($fields[19]) {
				# New fields in 0.5.2
				$info->{'discnumber'} = $fields[18];
				$info->{'totaldiscs'} = $fields[19];
			}
		}
	}

	$info->{'state'} = "playing" if ($info->{'state'} eq "111");
	$info->{'state'} = "paused" if ($info->{'state'} eq "113");
	$info->{'state'} = "stopped" if ($info->{'state'} eq "112");

	for (keys %$info) {
		delete $info->{$_} if (defined($info->{$_}) and $info->{$_} eq '?');
	}

	$info->{'albumartist'} = $info->{'artist'} unless defined($info->{'albumartist'});
	$info->{'trackartist'} = $info->{'artist'} if (defined($info->{'artist'}) and $info->{'albumartist'} ne $info->{'artist'});

	if (defined($info->{'length'})) {
		$info->{'len'} = $info->{'length'}; # Compatibility with 0.4

		my ($h, $m, $s) = split(/\:/, $info->{'length'});
		if (defined $s) {
			$info->{'len_sec'} = $s + $m * 60 + $h * 3600;
		} else {
			$info->{'len_sec'} = $m + $h * 60;
		}
	}

	if (not defined($info->{'position'})) {
		my ($s, $m, $h) = ($info->{'pos_sec'}, 0, 0);

		$h = $s / 3600;
		$s %= 3600;

		$m = $s / 60;
		$s %= 60;

		if ($h > 0) {
			$info->{'position'} = sprintf("%d:%02d:%02d", $h, $m, $s);
		} else {
			$info->{'position'} = sprintf("%d:%02d", $m, $s);
		}
	}

	return $info;
}

sub parse_format {
	my ($format, $info, $sublevel, $parsed) = @_;
	$sublevel = 0 if not defined $sublevel;

	my $output = "";

	my @chars = split(//,$format);

	# Language Definition

	# lowercasestring      <== should be parsed as a tag name, makes the expression fail if such tag is not defined
	# []                   <== brackets allow the parsing inside them to fail
	# $func(arg1,arg2,...) <== function call (see parse_subfunction for details)
	# ''                   <== string literal (ignores all parsing)
	# \(character)         <== literal character

	# Bracket Nesting

	# A bracket returns a defined value only if it has at least one tag or at least one of its embedded brackets return true.

	my @tokens = ();
	my $tagcount = 0;
	my $fail = 0;

	my $literal = 0;
	my $sub = 0;
	my $func = 0;
	my $tagmode = 0;
	my $str = "";

	for(my $i = 0; $i < @chars; $i++ ) { # 1st Pass (Lexical analysis, push into @tokens)
		if ($chars[$i] eq "\\") { # Escaped character
			$str .= "\\$chars[++$i]";                   # Copy the next character without parsing
			++$sub if ($chars[$i] eq "[" and $sub > 0); # However, we still need to count [ and ]
			--$sub if ($chars[$i] eq "]" and $sub > 0); # or the subexpression parser will give wrong results
		} elsif($literal) { # If on literal mode
			$str .= $chars[$i]; # Simply copy everything as-is until an unescaped ' is found
			if ($chars[$i] eq "'" and ($chars[$i-1] ne "\\" or (defined($chars[$i-2]) and $chars[$i-2] ne "\\"))) {
				push @tokens, $str;
				$str = "";
				$literal = 0;
			} elsif (not defined($chars[$i+1])) { # This means we ended the string with an uneven number of unescaped 's
				return "Malformed: mismatched ': $str";
			}
		} elsif ($sub) { # If on subexpression mode
			$str .= $chars[$i]; # Copy everything as-is until an unescaped ] is found
			if ($chars[$i] eq "[") { # We must copy any sub-subexpressions inside this sub-expression for recursive evaluation
				++$sub;
			} elsif ($chars[$i] eq "]" and $chars[$i-1] ne "\\" and --$sub == 0) {
				push @tokens, $str;
				$str = "";
			} elsif (not defined($chars[$i+1])) { # This means we ended the string without $sub being 0
				return "Malformed: mismatched [: $str";
			}
		} elsif ($tagmode) { # If on tag mode
			$str .= $chars[$i]; # Copy tags as-is until any non-lowercase-alpha character is found
			if (not defined($chars[$i+1]) or $chars[$i+1] !~ /[a-z_]/) {
				push @tokens, $str;
				$str = "";
				$tagmode = 0;
			}
		} elsif ($func) { # If on function mode
			$str .= $chars[$i]; # Copy everything until an unescaped ) is found
			if ($chars[$i] eq "(" and $chars[$i-1] ne "\\") {
				$func++;
			} elsif ($chars[$i] eq ")" and $chars[$i-1] ne "\\" and --$func <= 1) {
				push @tokens, $str;
				$str = "";
				$func = 0;
			} elsif (not defined($chars[$i+1])) {
				return "Malformed: mismatched (: $str";
			}
		} else {
			if ($chars[$i] eq "'" and (not defined($chars[$i-1]) or $chars[$i-1] ne "\\")) {
				push @tokens, "'$str'" if $str ne ""; # Found an opening quote
				$str = $chars[$i];
				$literal = 1; # Enter literal mode
			} elsif ($chars[$i] eq "[" and (not defined($chars[$i-1]) or $chars[$i-1] ne "\\")) {
				push @tokens, "'$str'" if $str ne ""; # Found a subexpression opener
				$str = $chars[$i];
				$sub = 1; # Enter subexpression mode
			} elsif ($chars[$i] eq "\$" and (not defined($chars[$i-1]) or $chars[$i-1] ne "\\")) {
				push @tokens, "'$str'" if $str ne "";
				$str = $chars[$i];
				$func = 1; # Enter subfunction mode
			} elsif ($chars[$i] =~ /[a-z]/) {
				push @tokens, "'$str'" if $str ne ""; # Found a tag name
				$str = $chars[$i];
				$tagmode = 1; # Enter tag mode
			} else {
				$str .= $chars[$i]; # Copy as a literal
			}
		}
	}

	push @tokens, "'$str'" if $str ne ""; # Make sure whatever is left from parsing is added as a literal

	$tagcount = 0;
	$sub = 0;
	$func = 0;

	foreach my $token (@tokens) { # 2nd Pass, execute tokens
		if ($token =~ /^'/) { # If the token is a literal, then
			$token =~ s/^'//; # Strip the opening quote
			$token =~ s/'$//; # And the closing one
			$token =~ s/\\(.)/$1/g; # Remove the escape from all escaped characters
			$output .= $token; # Copy to output
		} elsif ($token =~ /^\[/) { # If the token is a subexpression
			$token =~ s/^\[//; # Strip the opening [
			$token =~ s/\]$//; # And the closing ]
			my $recurse = parse_format($token, $info, $sublevel+1); # Recurse
			if (defined($recurse) and $recurse ne "") { # If the subexpression is true
				$output .= $recurse; # Copy result to output
				$sub++; # Count this as a valid subexpression
			}
		} elsif ($token =~ /^\$/) { # If the token is a subfunction
			my $res = parse_subfunction($token, $info, $sublevel);
			if (defined($res) and $res ne "") {
				$output .= $res;
				$func++;
		}
		} else { # If this is a tag
			if (!defined($info->{$token})) { # If this tag is not defined
				return undef; # Fail immediatly
			}
			$tagcount++; # Count this as a valid tag
			$output .= $info->{$token}; # Copy value to output
		}
	}

	$$parsed = ($tagcount or $sub or $func) if defined($parsed);

	return $output;
}

sub build_output {
	my ($format, $info, $sublevel) = @_;
	$sublevel = 0 if not defined $sublevel;

	my $sub;
	my $output = parse_format($format, $info, $sublevel, \$sub);

	return undef unless ($sub); # Fail if there are no tags and all subexpressions are false

	return $output;
}

sub parse_subfunction {
	my ($function, $info, $sublevel) = @_;

	$function =~ /^\$(.*?)\((.*)\)$/;

	my $func = $1;

	my @rawargs = split(//, $2);
	my @args = ();
	my @failargs = ();

	my $parens = 0;
	my $str = "";
	for(my $i = 0; $i < @rawargs; $i++) {
		if ($i > 0 and $rawargs[$i-1] eq "\\") {
			# Ignore everything else
		} elsif($parens) {
			--$parens if ($rawargs[$i] eq ")");
		} elsif ($rawargs[$i] eq "(") {
			++$parens;
		} elsif ($rawargs[$i] eq ",") {
			push @args, $str;
			$str = "";
			++$i;
		}
		$str .= $rawargs[$i] if defined($rawargs[$i]);
	}
	push @args, $str;

	for (my $i = 0; $i < @args; $i++) {
		$args[$i] = parse_format($args[$i], $info, $sublevel+1, \$failargs[$i]);
	}

	if ($func eq "bar") {
		my $length = $info->{len_sec};
		my $pos = $info->{pos_sec};

		my ($len, $fill, $space, $filled) = @args;
		$filled = 0 unless defined($filled);

		my $fillpos = int($pos * $len / $length + 0.5);

		my $bar = "";

		if ($filled) {
			for (my $i = 0; $i < $len; $i++) {
				if ($i < $fillpos) {
					$bar .= $fill;
				} else {
					$bar .= $space;
				}
			}
		} else {
			for (my $i = 0; $i < $len; $i++) {
				if ($i == $fillpos) {
					$bar .= $fill;
				} else {
					$bar .= $space;
				}
			}
		}

		return "$bar";
	} elsif ($func eq "if" or $func eq "if2") {
		my ($test, $iftrue, $iffalse);
		if ($func eq "if") {
			($test, $iftrue, $iffalse) = @args;
		} else {
			($test, $iffalse) = @args;
			$iftrue = $test;
		}

		if (not $failargs[0] and $test) {
			return $iftrue;
		} else {
			return $iffalse;
		}
	} elsif ($func eq "ifgreater" or $func eq "ifequal" or $func eq "iflonger") {
		my ($arg1, $arg2, $iftrue, $iffalse) = @args;

		if (defined($arg1)) {
			# Remove possible literal markers
			$arg1 =~ s/\'//g;
			# Ignore spacing
			$arg1 =~ s/\s+//g;
		}
		if (defined($arg2)) {
			$arg2 =~ s/\'//g;
			$arg2 =~ s/\s+//g;
		} else {
			return $iftrue if (defined($arg1));
			return $iffalse;
		}
		return $iffalse unless (defined($arg1));

		if ($func eq "iflonger") {
			return $iftrue if (length($arg1) > length($arg2));
		} elsif ($func eq "ifequal") {
			return $iftrue if $arg1 == $arg2;
		} else { # ifgreater
			return $iftrue if $arg1 > $arg2;
		}
		return $iffalse;
	} elsif ($func eq "abbr") {
		my ($arg1, $arg2) = (0,0);
		$arg1 = $args[0];
		$arg2 = $args[1] if ($args[1]);

		if (length($arg1) > $arg2) {
			my $abbr = "";
			my @tokens = split(/\s+/, $arg1);
			foreach my $token (@tokens) {
				my @chars = split(//, $token);
				$abbr .= $chars[0];
			}
			return $abbr;
		}
		return $arg1;
	} elsif ($func eq "num") {
		my ($arg1, $arg2) = @args;
		return undef unless $arg1;
		return sprintf("%0${arg2}d", $arg1);
	} elsif ($func =~ /^(add|sub|mul|div|mod|max|min)$/) {
		my ($arg1, $arg2) = @args;
		return undef unless (defined($arg1) and defined($arg2));
		return $arg1 + $arg2 if ($func eq "add");
		return $arg1 - $arg2 if ($func eq "sub");
		return $arg1 * $arg2 if ($func eq "mul");
		return $arg1 / $arg2 if ($func eq "div");
		return $arg1 % $arg2 if ($func eq "mod");
		return ($arg1 >= $arg2 ? $arg1 : $arg2) if ($func eq "max");
		return ($arg1 < $arg2 ? $arg1 : $arg2) if ($func eq "min");
	} elsif ($func =~ /^(and|or|xor|not)$/) {
		my ($arg1, $arg2) = @args;
		$arg1 = 0 unless defined $arg1;
		$arg2 = 0 unless defined $arg2;

		# Need to give explicit returns to avoid eating on parse_format

		return ($arg1 ? 0 : 1) if ($func eq "not");
		return (($arg1 && $arg2) ? 1 : 0) if ($func eq "and");
		return (($arg1 || $arg2) ? 1 : 0) if ($func eq "or");
		return (($arg1 && !$arg2) ? 1 : ((!$arg1 && $arg2) ? 1 : 0)) if ($func eq "xor");
	} elsif ($func eq "strcmp") {
		my ($arg1, $arg2) = @args;
		return undef unless (defined($arg1) and defined($arg2));
		return (($arg1 eq $arg2) ? 1 : 0);
	} else {
		return undef;
	}
}

sub get_np_string {
	my $info = get_track_info();
	$info->{comment} = $_[0] if $_[0];
	if (defined($info)) {
		return build_output($format, $info);
	}
	return undef;
}

sub get_help_string {
	my $fields;
	if (HAVE_IRSSI) {
		$fields = '%%codec%%|||%%bitrate%%|||%%album artist%%|||%%album%%|||%%date%%|||%%genre%%|||%%tracknumber%%|||%%title%%|||%%artist%%|||%%totaltracks%%|||%%playback_time%%|||%%length%%|||%%_foobar2000_version%%|||%%codec_profile%%|||%%discnumber%%|||%%totaldiscs%%';
	} else {
		$fields = '%codec%|||%bitrate%|||%album artist%|||%album%|||%date%|||%genre%|||%tracknumber%|||%title%|||%artist%|||%totaltracks%|||%playback_time%|||%length%|||%_foobar2000_version%|||%codec_profile%|||%discnumber%|||%totaldiscs%';
	}
	my $help = <<EOF
Required Plugin: foo_controlserver
URL: http://www.hydrogenaudio.org/forums/index.php?showtopic=38114
Required settings: Control Server tab:
* Server Port: 3333
* UTF-8 output/input: checked
* Base delimiter: |||
Recommended setting:
* Number of Clients: Some big number like 700
* Fields: $fields

NOTE: the script only works with either the default or this custom Fields line.

This script can also work via SSH tunneling, by using -R 3333:localhost:3333.

EOF
;
	return $help;
}

sub get_intro_string {
	my $intro = <<EOF
\002-----------------------------------------------------------------
\002foo_spam - prints the currently playing track from foobar2000
\002Created by Kovensky \(irc.rizon.net #shameimaru\)
This script requires a properly configured foobar2000.
Run /foo_help for help setting foobar2000 up.
\002-----------------------------------------------------------------
Usage:
/aud        - prints the playing song as an ACTION
/np         - alias to /aud
/foo_help   - explains how to set up foobar2000
/foo_format - explains how to set up the output format
\002-----------------------------------------------------------------

EOF
;
	return $intro;
}

sub get_foo_format_help_string {
	my $help = <<EOF
Format Definition
Example: artist - [album - ]title

* lowercasestring <= Is parsed as a tag name. To see a list of tags, use /foo_tags.
* [expression]    <= Evaluate expression as a regular format.
If there are no missing tags or at least one subexpression is true,
then the result of the expression will be included on the output.
* 'literal'       <= Everything inside the quotes is copied as-is.
* \\'             <= Inserts a literal '.

To change the format, you can use:
* Irssi: /set foo_format <new format>\n * X-Chat: /set_foo_format <default or new format>

Default: $default_format

EOF
;
	return $help;
}

sub get_taglist_string {
	my $list = <<EOF
List of available tags:
* player <= contains the player name (in this case, foobar2000).
* state <= one of \"paused\", \"playing\" or \"paused\".
* playlistindex <= the index of the current playlist, 0-based.
* trackindex <= the index of the current track on the playlist, 1-based.
* pos_sec <= the current position on the track, in seconds.
* codec <= the codec used on the currently playing track.
* bitrate <= the current bitrate in kbps, varies with VBR.
* albumartist <= the artist of the album. May or may not be the same as artist.
* album <= the current album title.
* date <= the album's / track's release date.
* genre <= the song's genre.
* track <= the track number.
* title <= the track's title.
* position <= the current position on the track, in m:ss format.
The following tags are only available if using the recommended settings:
* artist <= the artist of the current track.
* totaltracks <= the number of tracks in the album.
* len <= the track's duration, in m:ss format.
* version <= the player's version (e.g. \"v0.9.6.8\").
The following tag is set by foo_spam itself:
* comment <= all arguments that the user gives to /aud in a single string.

EOF
;
	return $list;
}

if (HAVE_IRSSI) {
	*print_now_playing = sub {
		my ($data, $server, $witem) = @_;
		my $str = get_np_string($data);
		if (defined($str)) {
			if ($witem && ($witem->{type} eq "CHANNEL" ||
				$witem->{type} eq "QUERY")) {
				$witem->command(encode_utf8("me $str"));
			}
		}
	};

	*print_foo_help = sub{
		Irssi::print(get_help_string());
	};

	*print_foo_format_help = sub {
		Irssi::print(get_foo_format_help_string());
	};

	*irc_print = sub {
		Irssi::print($_[0]);
	};

	*print_foo_tags = sub {
		Irssi::print(get_taglist_string());
	};

	Irssi::settings_add_str("foo_spam", "foo_format", $format);
	$format = Irssi::settings_get_str("foo_format");

	Irssi::command_bind('aud', 'print_now_playing');
	Irssi::command_bind('np', 'print_now_playing');
	Irssi::command_bind('foo_help', 'print_foo_help');
	Irssi::command_bind('foo_format','print_foo_format_help');
	Irssi::command_bind('foo_tags','print_foo_tags');

} elsif (HAVE_XCHAT) {
	*print_now_playing = sub {
		my $str = get_np_string($_[0][1] ? $_[1][1] : undef);
		if (defined($str)) {
			Xchat::command(encode_utf8("me $str"));
		}
		return Xchat::EAT_ALL();
	};

	*print_foo_help = sub {
		Xchat::print(get_help_string());
		return Xchat::EAT_ALL();
	};

	*irc_print = sub {
		Xchat::print(@_);
	};

	*set_foo_format = sub {
		if (defined($_[0][1])) {
			open($setting_file, ">", Xchat::get_info('xchatdir') . "/foo_spam.conf");
			if ($_[0][1] eq "default") {
				$format = $default_format;
			} else {
				$format = $_[1][1];
			}
			Xchat::print("Changed format to $format\n");
			if (defined($setting_file)) {
				print $setting_file $format;
				close($setting_file);
			} else {
				Xchat::print("Failed to save settings! Error: $!");
			}
		} else {
			Xchat::print("Current format: $format\n");
		}
		return Xchat::EAT_ALL();
	};
	if (defined(*set_foo_format)) {} # Silence a warning

	*print_foo_format_help = sub {
		Xchat::print(get_foo_format_help_string());
		return Xchat::EAT_ALL();
	};

	*print_foo_tags = sub {
		Xchat::print(get_taglist_string());
		return Xchat::EAT_ALL();
	};

	if (open($setting_file, "<", Xchat::get_info('xchatdir') . "/foo_spam.conf")) {
		my $line = <$setting_file>;
		chomp $line;
		$format = $line if (defined($line) and $line ne "");
		close($setting_file);
	}

	Xchat::hook_command("np","print_now_playing", {help => "alias to /aud"});
	Xchat::hook_command("aud","print_now_playing", {help => "prints your current playing song on foobar2000 on an ACTION"});
	Xchat::hook_command("foo_help","print_foo_help", {help => "explains how to set up foobar2000"});
	Xchat::hook_command("set_foo_format","set_foo_format", {help => "displays or changes the current format string"});
	Xchat::hook_command("foo_format","print_foo_format_help", {help => "explains how to configure the format string"});
	Xchat::hook_command("foo_tags","print_foo_tags", {help => "lists all available tags"});
} else {
	$| = 1;
	*irc_print = sub {
		print (STDERR encode_utf8("@_\n")) if @_;
	};
	$format = join(" ", @ARGV) if $ARGV[0];
	print encode_utf8(get_np_string() . "\n");
}

if (HAVE_XCHAT or HAVE_IRSSI) {
	irc_print(get_intro_string());
	open_telnet();
} else {
	close_telnet();
}