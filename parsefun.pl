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
					$a =~ s/([^\x00]+)//;
					push @lv, [ $1, -1 ];
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

	my $pretree = prepare_tree( $uq, @q );
	my $tree = finalize_tree( create_tree( "foo_base", $pretree ) );
	return apply_tree( { var => "" }, $tree );
}

exit unless $ARGV[0];
say parse_format( $ARGV[0] );
