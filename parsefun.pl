#! /usr/bin/env perl
use strict;
use warnings;
use feature ':5.10';
use Data::Dumper;
$Data::Dumper::Indent = 1;

sub unquoted_fixan { $_ = pop @_; s/''/'/g; s/%([^%]+)%/\$foo_variable($1)/ig; return $_; } # ' to make pastebin happy
sub quoted_fixan { $_ = pop @_; s/''/'/g; return $_ } # ' again

sub prepare_tree {
	my ( $level, $tp, @q, @lv ) = ( 0, @_ );
	while ( $tp =~ /\$([A-Za-z0-9_-]+)\(|(\))|([^)\$]*)/ig ) {
		if ($1) {
			push @lv, [ $1, $level++ ];
		} elsif ($2) {
			push @lv,
			    [
				    ( $level > 0 ? "function end" : $2 ),
				    ( $level > 0 ? --$level       : -2 )
				   ];
		} elsif ($3) {
			my $a = $3;
			if ( $a =~ /\x00/ ) {
				{
					$a =~ s/^([^\x00]*)//;
					$1 && push @lv, [ $1, -1 ];
					if ( $a =~ /^\x00/ ) {
						push @lv, [ shift(@q), -2 ];
					}
					( $a =~ s/\x00// ) && redo
				}
			} else {
				push @lv, [ $a, -1 ];
			}
		}
	}
	( $level > 0 ) && die "Error, missing closing parenthesis";
	return \@lv;
}

sub create_tree {
	my ( $f, $tree ) = (@_);
	my @results = ($f);
	{
		$_ = shift @{$tree};
		if ( $_->[1] < 0 ) {
			if ( defined( $_->[0] ) ) {
				if ( $_->[1] eq -1 ) {
					push @results, $_->[0];
				} else {
					push @results, [ "foo_quote", $_->[0] ];
				}
			}
		} elsif ( $_ > 0 ) {
			my @subtree;
			my $last;
			my $level = $_->[1];
			my $name  = $_->[0];
			{
				my $s = shift @{$tree};
				if ( $s->[1] eq $level ) {
					$last = 1;
				}
				if ( !$last ) {
					push @subtree, $s;
					@{$tree} && redo || die "Internal Error, unmatched tree";
				}
			}
			push @results, create_tree( $name, \@subtree );
		}
		@{$tree} && redo;
	}
	return \@results;
}

sub finalize_tree {
	my ($tree)     = (@_);
	my @bt         = @{$tree};
	my $base       = shift @bt;
	my @returntree = ($base);
	my @tt;
	while ( $_ = shift @bt ) {
		if ( $base eq "foo_base" ) {
			if ( ref($_) eq "ARRAY" ) {
				push @returntree, finalize_tree($_);
			} else {
				push @returntree, $_;
			}
		} else {
			if ( ref($_) eq "ARRAY" ) {
				push @tt, finalize_tree($_);
			} else {
				if ( $_ =~ /,/ ) {
					if ( $_ =~ /^,/ ) {
						push @returntree, @tt;
						@tt = ();
					}
					while (( ( @returntree eq 1 ) && ( $_ =~ s/^([^,]+)// ) )
					           || ( $_ =~ s/^,([^,]*)// ) ) {
						my $data = (
							$1 ? $1
							    : ($1 eq "0"?"0":(
								    substr( $_, 0, 1 ) eq "," ? ""
								        :   ( @bt ? undef : "" ) ))
							   );
						if ( defined($data) ) {
							if (@tt) {
								push @returntree, @tt;
								@tt = ();
								push @tt, $data;
							} else {
								push @tt, $data;
							}
						}
					}
				} else {
					push @tt, $_;
				}
			}
		}
	}
	(@tt) && push @returntree, @tt;
	return \@returntree;
}

sub apply_tree {
	my ( $info, $tr, @ar ) = (@_);

	state $funcs;
	state $apply;

	unless ($funcs) {
		my $all_var_ok;
		my @has_var;
		my @var_ok;
		my $lex_lvl = 0; # Wastes position 0 on $test but prevents error on foo_variable if outside a $test
		my %heap;
		
		my $exec = sub {
			$_ = shift;
			if ( ref $_ eq "ARRAY" ) {
				my $fn = shift @{$_};
				unless ($fn) {
					warn "Unknown function '$fn'";
					return "";
				}
				return $funcs->{$fn}->( @{$_} );
			} else {
				return $_;
			}
		};
		$apply = sub {
			my @res;
			$all_var_ok = 1;
			for (@_) {
				push @res, $exec->($_);
			}
			return @res;
		};

		my $checkargs = sub {
			my ($name, $minc, $maxc, @ar) = @_;
			my $ok = 1;
			$ok = $ok && @ar >= $minc if $minc >= 0;
			unless ($ok) {
				warn "Too few arguments for \$$name (expected at least $minc, found ". scalar(@ar) .")";
				return undef;
			}
			$ok = $ok && @ar <= $maxc if $maxc >= 0;
			unless ($ok) {
				warn "Too many arguments for \$$name (expected at most $maxc, found ". scalar(@ar) .")";
				return undef;
			}
			return 1;
		};
		my $test = sub {
			++$lex_lvl;
			($var_ok[$lex_lvl], $has_var[$lex_lvl]) = (0, 0);
			my $res = $exec->(shift @_);
			my $var_chk = $has_var[$lex_lvl] && $var_ok[$lex_lvl] || !$has_var[$lex_lvl];
			delete $has_var[$lex_lvl--];
			return $res if (defined $res && $var_chk);
			return undef;
		};
		my $asint = sub {
			$_=shift;
			return undef unless defined;
			return int if (/^-?[0-9]+$/);
			return undef;
		};
		my $nonempty = sub {
			$_=shift;
			return undef unless defined && $_ ne '';
			return $_;
		};
		my $getargs = sub {
			my ($name, $minc, $maxc, @ar) = @_;
			return (undef,undef) unless $checkargs->($name, $minc, $maxc, @ar);
			return (1, map { $exec->($_) } @ar);
		};
		my $defcheck = sub {
			for (@_) {
				return undef unless defined;
			}
			return 1;
		};
		
		$funcs = {
			"foo_base"  => sub { return join "", map { $_='?' unless defined; $_ } $apply->(@_) },
			"foo_quote" => sub { return join "", @_ },
			"foo_variable" => sub {
				$_ = shift;
				$has_var[$lex_lvl] = 1;
				if ( defined $info->{$_} ) {
					$var_ok[$lex_lvl] = 1;
					return $info->{$_};
				}
				$all_var_ok = 0;
				return undef;
			},
			"foo_conditional" => sub {
				my @res = $apply->(@_);
				return join "", @res if $all_var_ok;
				$all_var_ok = 1;
				return "";
			},
			"meta_test" => sub {
				my ($ok, @tags) = ($checkargs->('meta_test',1,-1,@_),@_);
				for (@tags) {
					return undef unless $defcheck->($_) && exists $info->{$_};
				}
				return 1;
			},
			"meta_num" => sub {
				my ($ok, $tag) = ($checkargs->('meta_num',1,1,@_),@_);
				return undef unless $defcheck->($tag) && exists $info->{$tag};
				$tag = $info->{$tag};
				return scalar @$tag if (ref $tag eq "ARRAY");
				return 1;
			},
			"meta" => sub {
				my ($ok, $tag, $n) = ($checkargs->('meta',1,2,@_),@_);
				return undef unless $defcheck->($tag) && exists $info->{$tag};
				return $funcs->{'meta_sep'}->($tag,', ') unless $defcheck->($n);
				$tag = $info->{$tag};
				if (ref $tag eq "ARRAY" && $n < scalar @$tag) {
					return $tag->[$n];
				}
				return $tag if $n == 0;
				return undef;
			},
			"meta_sep" => sub {
				my ($ok, $tag, $sep, $lastsep) = ($checkargs->('meta_sep',2,3,@_),@_);
				return undef unless $defcheck->($tag,$sep) && exists $info->{$tag};
				$lastsep = $sep unless $defcheck->($lastsep);
				$tag = $info->{$tag};
				if (ref $tag eq "ARRAY") {
					my $n = scalar(@$tag);
					my $ret = $tag->[0];
					my $last = $tag->[$n-1];
					for (@$tag[1..$n-2]) {
						$ret .= "$sep$_";
					}
					$ret .= "$lastsep$last" if $last;
					return $ret;
				}
				return $tag;
			},
			
			"put" => sub {
				my ($ok, $key, $val) = $getargs->('put',2,2,@_);
				return undef unless $ok && $nonempty->($key);
				if (defined $key) {
					return $heap{$key} = $val;
				}
				delete $heap{$key};
				return undef;
			},
			"puts" => sub {
				my ($ok, @ar) = $getargs->('puts',2,2,@_);
				return undef unless $ok;
				return defined $funcs->{"put"}->(@ar) ? "" : undef;
			},
			"get" => sub {
				my ($ok, $key) = $getargs->('get',1,1,@_);
				return undef unless $ok && $nonempty->($key);
				return exists $heap{$key} ? $heap{$key} : undef;
			},
			
			"not" => sub {
				my ($ok, $a) = $getargs->('not',1,1,@_);
				$defcheck->($a) && return undef || return "";
			},
			"and" => sub {
				return undef unless $checkargs->('and',2,-1,@_);
				for (@_) {
					defined $test->($_) || return undef;
				}
				return "";
			},
			"or" => sub {
				return undef unless $checkargs->('or',2,-1,@_);
				for (@_) {
					defined $test->($_) && return "";
				}
				return undef;
			},
			"xor" => sub {
				my ($ok, $a, $b) = $getargs->('xor',2,2,@_);
				($a, $b) = (defined $a, defined $b);
				return "" if $ok && ($a && !$b) || (!$a && $b);
				return undef;
			},
			"strcmp" => sub {
				my ($ok, $a, $b) = $getargs->('strcmp',2,2,@_);
				return "" if $ok && $defcheck->($a, $b) && $a eq $b;
				return undef;
			},
			"stricmp" => sub {
				my ($ok, $a, $b) = $getargs->('stricmp',2,2,@_);
				return "" if $ok && $defcheck->($a, $b) && lc($a) eq lc($b);
				return undef;
			},
			
			
			"if" => sub {
				return undef unless $checkargs->('if',2,3,@_);
				defined $test->(shift @_) && return $exec->(shift @_);
				return $exec->($_[1]) if defined $_[1];
				return undef;
			},
			"if2" => sub {
				return undef unless $checkargs->('if2',2,2,@_);
				my $cond = shift @_;
				if (defined (my $res = $test->($cond))) {
					return $res;
				}
				return $exec->(shift @_);
			},
			"if3" => sub {
				return undef unless $checkargs->('if3',1,-1,@_);
				for (@_) {
					if (defined (my $res = $test->($_))) {
						return $res;
					}
				}
				return undef;
			},
			"ifgreater" => sub {
				my ($ok, $a, $b, $then, $else) = ($checkargs->('ifgreater',3,4,@_), @_);
				map { $_ = $exec->($_) } ($a, $b);
				$a = -1 unless $defcheck->($a = $asint->($a));
				$b = -1 unless $defcheck->($b = $asint->($b));
				return $exec->($then) if ($a > $b);
				return $exec->($else);
			},
			"iflonger" => sub {
				my ($ok, $str, $len, $then, $else) = ($checkargs->('ifgreater',3,4,@_), @_);
				map { $_ = $exec->($_) } ($str, $len);
				return $exec->($else) unless $defcheck->($str);
				$len = -1 unless $defcheck->($len = $asint->($len));
				return $exec->($then) if (length($str) > $len);
				return $exec->($else);
			},
			"ifequal" => sub {
				my ($ok, $a, $b, $then, $else) = ($checkargs->('ifequal',3,4,@_), @_);
				map { $_ = $exec->($_) } ($a, $b);
				$a = 0 unless $defcheck->($a);
				$b = 0 unless $defcheck->($b);
				my ($ia, $ib) = ($asint->($a), $asint->($b));
				if ($defcheck->($ia, $ib)) {
					return $exec->($then) if $ia == $ib;
					return $exec->($else);
				}
				return $exec->($then) if $a eq $b;
				return $exec->($else);
			},
			
			"greater" => sub {
				my ($ok, $a, $b) = $getargs->('greater',2,2,@_);
				return undef unless $defcheck->($ok, $a = $asint->($a), $b = $asint->($b));
				return $a >= $b ? $a : $b;
			},
			
			"len" => sub {
				(my($ok), $_) = $getargs->('len',1,1,@_);
				return undef unless $defcheck->($ok, $_);
				return length;
			},
			"longer" => sub {
				my ($ok, $a, $b) = $getargs->('longer',2,2,@_);
				return undef unless $defcheck->($ok, $nonempty->($a), $nonempty->($b));
				return length($a) >= length($b) ? $a : $b;
			},
			"longest" => sub {
				my ($ok, @ar) = $getargs->('longest',2,-1,@_);
				return undef unless $ok;
				my $longest = undef;
				for (@ar) {
					$longest = $_ if (!defined $longest || (defined && length > length $longest));
				}
				return $longest;
			},
			
			"repeat" => sub {
				my ($ok, $str, $c) = $getargs->('repeat',2,2,@_);
				return undef unless $defcheck->($ok, $str, $c = $asint->($c));
				return $c ? ($str x $c) : "";
			},
			"trim" => sub {
				my ($ok, $str) = $getargs->('trim',1,1,@_);
				return undef unless $defcheck->($ok, $str);
				$str =~ /^\s*+(.*?)\s*+$/;
				return $1;
			},
			
			"cut" => sub {
				my ($ok, $str, $len) = $getargs->('cut',2,2,@_);
				return undef unless $defcheck->($ok, $str, $len = $asint->($len));
				return substr $str, 0, $len;
			},
			"right" => sub {
				my ($ok, $str, $len) = $getargs->('right',2,2,@_);
				return undef unless $defcheck->($ok, $str, $len = $asint->($len));
				return substr $str, -$len;
			},
			"pad" => sub {
				my ($ok, $str, $len, $c) = $getargs->('pad',2,3,@_);
				return undef unless $defcheck->($ok, $str, $len = $asint->($len));
				$c = $nonempty->($c) ? substr $c, 0, 1 : " ";
				return ($c x ($len - length($str))) . $str;
			},
			"pad_right" => sub {
				my ($ok, $str, $len, $c) = $getargs->('pad_right',2,3,@_);
				return undef unless $defcheck->($ok, $str, $len = $asint->($len));
				$c = $nonempty->($c) ? substr $c, 0, 1 : " ";
				return $_[0] . ($c x ($len - length($_[0])));
			},
			"padcut" => sub {
				my ($ok, $str, $len) = $getargs->('padcut',2,2,@_);
				return undef unless $defcheck->($ok, $str, $len = $asint->($len));
				return $funcs->{'cut'}->($funcs->{'pad'}->($_[0],$len),$len);
			},
			"padcut_right" => sub {
				my ($ok, $str, $len) = $getargs->('padcut_right',2,2,@_);
				return undef unless $defcheck->($ok, $str, $len = $asint->($len));
				return $funcs->{'cut'}->($funcs->{'pad_right'}->($_[0],$len),$len);
			},
			
			"insert" => sub {
				my ($ok, $haystack, $needle, $pos) = $getargs->('insert',3,3,@_);
				return undef unless $defcheck->($ok, $haystack, $needle, $pos = $asint->($pos));
				return substr($haystack, 0, $pos) . $needle . substr($_[0], $haystack);
			},
			"replace" => sub {
				my ($ok, $haystack, $needle, $newneedle) = $getargs->('insert',3,3,@_);
				return undef unless $defcheck->($ok, $haystack);
				return $haystack unless $nonempty->($needle);
				return undef unless defined $newneedle;
				$needle = quotemeta($needle);
				return $haystack =~ s/$needle/$$newneedle/;
			},
			
			"abbr" => sub {
				my ($ok, $str, $maxlen) = $getargs->('abbr',2,2,@_);
				return undef unless $defcheck->($str);
				return $str unless $defcheck->($maxlen = $asint->($maxlen)) && length($str) > $maxlen;
				my @tok = split /\s+/, $funcs->{'trim'}->($str);
				my $abbr = '';
				for (@tok) {
					my @chars = split //;
					$abbr .= $chars[0];
				}
				return $abbr;
			},
			"num" => sub {
				my ($ok, $num, $len) = $getargs->('num',2,2,@_);
				return undef unless $defcheck->($num = $asint->($num), $len = $asint->($len));
				return undef if $len < 0;
				return "" if $len == 0;
				return sprintf("%0${len}d", $num);
			},
			
			"add" => sub {
				my ($ok, @ar) = $getargs->('add',2,-1,@_);
				return undef if grep { !defined } map { $asint->($_) } @ar;
				my $ax = shift @ar;
				map { $ax += $_ } @ar;
				return $ax;
			},
			"sub" => sub {
				my ($ok, @ar) = $getargs->('sub',2,-1,@_);
				return undef if grep { !defined } map { $asint->($_) } @ar;
				my $ax = shift @ar;
				map { $ax -= $_ } @ar;
				return $ax;
			},
			"mul" => sub {
				my ($ok, @ar) = $getargs->('mul',2,-1,@_);
				return undef if grep { !defined } map { $asint->($_) } @ar;
				my $ax = shift @ar;
				map { $ax *= $_ } @ar;
				return $ax;
			},
			"div" => sub {
				my ($ok, @ar) = $getargs->('div',2,-1,@_);
				return undef if grep { !defined } map { $asint->($_) } @ar;
				my $ax = shift @ar;
				map { $ax /= $_ } @ar;
				return $ax;
			},
			"mod" => sub {
				my ($ok, @ar) = $getargs->('mod',2,-1,@_);
				return undef if grep { !defined } map { $asint->($_) } @ar;
				my $ax = shift @ar;
				map { $ax %= $_ } @ar;
				return $ax;
			},
			"max" => sub {
				my ($ok, @ar) = $getargs->('max',2,-1,@_);
				map { $_ = $asint->($_) } @ar;
				my $ax = shift @ar;
				for (@ar) {
					$ax = $_ if defined $ax && $_ > $ax;
				}
				return $ax;
			},
			"min" => sub {
				my ($ok, @ar) = $getargs->('min',2,-1,@_);
				map { $_ = $asint->($_) } @ar;
				my $ax = shift @ar;
				for (@ar) {
					$ax = $_ if defined $ax && $_ < $ax;
				}
				return $ax;
			},
			
			"caps" => sub {
				my ($ok, $str) = $getargs->('caps',1,1,@_);
				return undef unless $defcheck->($ok, $str);
				$str =~ s/\b(\S)(\S*)\b/@{[uc($1)]}@{[lc($2)]}/g;
				return $str;
			},
			"caps2" => sub {
				my ($ok, $str) = $getargs->('caps2',1,1,@_);
				return undef unless $defcheck->($ok, $str);
				$str =~ s/\b(\S)/@{[uc($1)]}/g;
				return $str;
			},
			"lower" => sub {
				my ($ok, $str) = $getargs->('lower',1,1,@_);
				return lc($str) if $defcheck->($ok, $str);
				return undef;
			},
			"upper" => sub {
				my ($ok, $str) = $getargs->('upper',1,1,@_);
				return uc($str) if $defcheck->($ok, $str);
				return undef;
			},
			"fix_eol" => sub {
				my ($ok, $meta, $repl) = $getargs->('fix_eol',1,2,@_);
				return undef unless $defcheck->($ok, $meta);
				$repl = " (...)" unless $defcheck->($repl);
				$meta =~ s/\010?\013.*//;
				return "$meta$repl";
			},
		};
	}
	
	return $apply->([@{$tr}]);
}

sub parse_format {
	my ($string) = (@_);

	# First we separate quoted parts for some quick and dirty work over [] and '', but that's a secret because the goat is around
	my ( $k, @uq, @q ) = (1);
	while ( $string =~ /((?:''|^[^']*|[^']+)+)/ig ) {
		( $k ) && ( $' =~ /^'\s*$/ ) && die "Error parsing at the end: missing quote";
		( $k = !$k )
		    && ( ( substr( $`, -1, 1 ) . substr( $', 0, 1 ) eq "''" ) # Yet another ' for pastebin (plz2fix yourself)
		             && ( push @q, quoted_fixan($1) )
		                 || die "Error parsing after >>$1<<: missing quote" )
		        && next;
		push @uq, unquoted_fixan($1);
	}
	push@uq,"";

	# Time to fixan space shuttles!( []'ed elements )
	my $uq = join "\x00", @uq;
	while ( $uq =~ s/\[([^\[\]]*)\]/\$foo_conditional($1)/ig ) {
	}
	( $uq =~ /[\[\]%]/ )
	    && die "Explodan on bad square brackets / variablan syntaxan after >>$`<<";
	    
	my $info = {
		'foo_spam_comment' => 'Some sample data',
		'codec' => 'Vorbis',
		'date' => '2010',
		'state' => 'playing',
		'directoryname' => '2010 Double Counterpoint',
		'playback_time' => '00:50',
		'genre' => 'Game',
		'filesize' => '7054817',
		'tracknumber' => 13,
		'ispaused' => '0',
		'title' => 'Ripple',
		'totaldiscs' => '1',
		'bitrate' => '233',
		'filesize_natural' => '6.73MB',
		'playback_time_seconds' => '50.71',
		'album artist' => 'Alstroemeria Records',
		'_foobar2000_version' => 'foobar2000 v1.0.3',
		'_path_raw' => 'file://H:/Music/Alstroemeria Records/2010 Double Counterpoint/1.13 spctrm + Syrufit - Ripple.ogg',
		'foo_spam_version' => 'v0.8.3',
		'version' => 'v1.0.3',
		'player' => 'foobar2000',
		'path' => 'H:/Music/Alstroemeria Records/2010 Double Counterpoint/1.13 spctrm + Syrufit - Ripple.ogg',
		'track number' => '13',
		'length_seconds' => '252',
		'totaltracks' => 13,
		'artist' => ['spctrm', 'Syrufit'],
		'album' => 'Double Counterpoint',
		'length' => '04:12',
		'playback_time_remaining' => '03:21',
		'isplaying' => '1',
		'filename' => '1.13 spctrm + Syrufit - Ripple',
		'discnumber' => '1',
		'playback_time_remaining_seconds' => '201.29',
		'filename_ext' => '1.13 spctrm + Syrufit - Ripple.ogg'
	};

	my $pretree = prepare_tree( $uq, @q );
	my $tree = finalize_tree( create_tree( "foo_base", $pretree ) );
	return apply_tree( $info, $tree );
}
my $teststr = <<'EOF';
%player%[ (%version%)]: [%album artist% ]'['[%date% ][%album%][ #[%discnumber%.]%tracknumber%[/[%totaldiscs%.]%totaltracks%]]']' [%track artist% - ]%title% '['%playback_time%[/%length%]']'[ %bitrate%kbps][ %filesize_natural%][ %codec%[ %codec_profile%]][ <-- %foo_spam_comment%]
EOF
chomp $teststr;
$ARGV[0] and $teststr = $ARGV[0];
say parse_format( $teststr );
