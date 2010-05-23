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
			return int if (/^[0-9]+$/);
			return undef;
		};
		my $nonempty = sub {
			$_=shift;
			return undef unless defined && $_ ne '';
			return $_;
		};
		
		$funcs = {
			"foo_base"  => sub { return join "", $apply->(@_) },
			"foo_quote" => sub { return join "", @_ },
			"foo_variable" => sub {
				$_ = shift;
				$has_var[$lex_lvl] = 1;
				if ( defined $info->{$_} ) {
					$var_ok[$lex_lvl] = 1;
					return $info->{$_};
				}
				$all_var_ok = 0;
				return '?';
			},
			"foo_conditional" => sub {
				my @res = $apply->(@_);
				return join "", @res if $all_var_ok;
				$all_var_ok = 1;
				return "";
			},
			"put" => sub {
				return undef unless $checkargs->('put',2,2,@_) && $nonempty->($_[0]);
				if (defined $_[1]) {
					return $heap{$_[0]} = $_[1];
				}
				delete $heap{$_[0]};
				return undef;
			},
			"puts" => sub {
				return undef unless $checkargs->('puts',2,2,@_);
				return defined $funcs->{"put"}->(@_) ? "" : undef;
			},
			"get" => sub {
				return undef unless $checkargs->('get',1,1,@_) && $nonempty->($_[0]);
				return exists $heap{$_[0]} ? $heap{$_[0]} : undef;
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

			"len" => sub {
				return undef unless $checkargs->('len',1,1,@_);
				$_=shift;
				return undef unless defined $_;
				return length($_);
			},
			"repeat" => sub {
				my $c;
				return undef unless $checkargs->('len',2,2,@_) && defined $_[0] && defined ($c = $asint->($_[1]));
				return $c ? ($_[0] x $c) : "";
			},
			"trim" => sub {
				return undef unless $checkargs->('trim',1,1,@_) && defined $_[0];
				($_=shift) =~ /^\s*+(.*?)\s*+$/;
				return $1;
			},
			
			"cut" => sub {
				my $len;
				return undef unless $checkargs->('cut',2,2,@_) && defined $_[0] && defined ($len = $asint->($_[1]));
				return substr $_[0], 0, $len;
			},
			"pad" => sub {
				my $len;
				return undef unless $checkargs->('pad',2,3,@_) && defined $_[0] && defined ($len = $asint->($_[1]));
				my $c = $nonempty->($_[2]) ? substr $_[2], 0, 1 : " ";
				return ($c x ($len - length($_[0]))) . $_[0];
			},
			"pad_right" => sub {
				my $len;
				return undef unless $checkargs->('pad_right',2,3,@_) && defined $_[0] && defined ($len = $asint->($_[1]));
				my $c = $nonempty->($_[2]) ? substr $_[2], 0, 1 : " ";
				return $_[0] . ($c x ($len - length($_[0])));
			},
			"padcut" => sub {
				my $len;
				return undef unless $checkargs->('padcut',2,2,@_) && defined $_[0] && defined ($len = $asint->($_[1]));
				return $funcs->{'cut'}->($funcs->{'pad'}->($_[0],$len),$len);
			},
			"padcut_right" => sub {
				my $len;
				return undef unless $checkargs->('padcut_right',2,2,@_) && defined $_[0] && defined ($len = $asint->($_[1]));
				return $funcs->{'cut'}->($funcs->{'pad_right'}->($_[0],$len),$len);
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
