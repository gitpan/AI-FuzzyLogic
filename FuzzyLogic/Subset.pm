#
# AI::FuzzyLogic::Subset
#

#
# wrapper for individual subsets
#

package AI::FuzzyLogic::Subset;

our $VERSION = '0.01';

our $newname = "FuzzySet00000000"; # static variable

use English::Reference;

sub new {
  my $class = shift; 
  my $type = shift or die 'first arg to constructor is unit type string';
  my $set = shift; ref $set eq 'ARRAY' or die 'second arg to constructor is array reference to a set';
  bless [$set, $type, $newname++], $class;
}

sub clone {
  my $self = shift; $self->isa(__PACKAGE__) or die;
  bless [[ARRAY $self->set()], $self->type(), $newname++], ref $self;
}

sub set  :lvalue { $_[0]->[0] = $_[1] if @_ == 2; $_[0]->[0]; };
sub type :lvalue { $_[0]->[1] = $_[1] if @_ == 2; $_[0]->[1]; };
sub name :lvalue { $_[0]->[2] = $_[1] if @_ == 2; $_[0]->[2]; };

#
# utility routines
#

sub bing {

  # put a little dent into the curve, centered at a given location

  my $self = shift;
  my $set = $self->set();
  my $location = shift; 
  my $degree = shift;

  # make sure we don't try to plot off the side of the graph, and center location
  my $halfsize = scalar ARRAY $set; $halfsize += 0.5; $halfsize /= 2; 
  $location += $halfsize;

  $set->[$location] = $degree;
  $set->[$location - 1] += 0.75 * $degree if $location - 1 >= 0;
  $set->[$location + 1] += 0.75 * $degree if $location + 1 < scalar ARRAY $set;
  $set->[$location - 2] += 0.33 * $degree if $location - 2 >= 0;
  $set->[$location + 2] += 0.33 * $degree if $location + 2 < scalar ARRAY $set;

  $self->normalize();

  return $self;

}

sub unwrap {

  # dump out the numerical contents of our set. this is currently essentially a no-op:
  # it is already stored as an array of numbers. in the future, it may be a string,
  # or it may be PDL, or it may be something else.

  my $self = shift;
  my $set = $self->() or die "set missing. shouldn't happen.";

  return ARRAY $set if wantarray();
  return $set;

}

sub wrap {

  # like above, but put data in. presently a no-op, but would translate to
  # internal format where the internal format to change.

  my $self = shift;
  my @set = @_ or die "set data required";

  $self->set(\@set);
  1;

}

#
# inner routines
#

sub normalize {

  # normalize to 1.0 being the max value for any slot if any slot is over 1.0

  my $self = shift();
  my $set = $self->set();

  my $max; 
  foreach my $i (ARRAY $set) { $max = $i if($max<$i); }
  if($max>1) { foreach my $i (ARRAY $set) { $i/=$max; } }

  $self;

}

sub balance {

  # move the largest segments to the center, tapering outwards towards the sides.
  # fixes sets that are off-center, and makes rounded curves out of jagged ones.
  # real scientific, i know.

  my $self = shift();
  my $set = $self->set();

  my @set = sort { $a <=> $b } ARRAY $set; 
  for(my $i=$#set;$i>0;$i-=2) { push @set, splice @set, $i, 1 }

  if(@set == 3) {
    # that doesn't work for very small sets. fudge it. kinda like a little bubble sort.
    ($set[1], $set[2]) = ($set[2], $set[1]) if $set[2] > $set[1];
  }

  $self->set() = \@set;

  $self;

}

sub centroid_inner {

  # find center of volume

  my $self = shift();
  my $set = $self->set();

  my $left = 0;
  my $half = 0.0;
  my $index = 0;

  map { $half += $_ } ARRAY $set; $half /= 2;

  foreach my $i (ARRAY $set) {
    if($left+$i >= $half) {
      # compute (interpolate) the fraction between fenceposts
      $half -= $left;   
      # $index+($half/$i) adds on a portion of the current bar depending how far $half is through it
      # -0.5 moves us from counting fenceposts to being centered on numbers themselves.
      # dividing that by the size of the set gives us a number such that: 0.0 <= $num <= 1.0.
      return $index - 0.5 unless($i); # degenerate case...
      return ($index+($half/$i)-0.5)/scalar(@$set);
    }
    $index++;
    $left += $i; 
  }

}

sub mean_inner {

  # average height of all of the segments

  my $self = shift();
  my $set = $self->set() or die 'no fuzzy set';

  my $samples;
  my $avg;

  foreach my $i (ARRAY $set) {
    $avg += $i; $samples++;
  }

  return $avg/$samples if($samples);

}

sub stringify {

  my $self = shift;
  my $set = $self->set() or die 'no fuzzy set';
  return '[empty set]' unless scalar ARRAY $set;
  my $inc = 79/scalar(ARRAY $set);
  my $ret .= $self->type() . "\n";

  for(my $y=0.9;$y>0;$y-=0.1) {
    for(my $x=0;$x<scalar ARRAY $set;$x+=1) {
      $ret .= (' ', '*')[$set->[$x] > $y] x scalar $inc;
    }
    $ret .= "\n";
  }

  return $ret;

}

#
# unary operators
#

# there are some docs at the end of AI::FuzzyLogic.pm, in the source code,
# on how these are implemented. the code style is dense and anti-redundancy.
# unlike mutator methods, these return new objects. this is consistent with
# the requirements of the operator overloading interface. we assume that the
# existing object continues to exist, even though it may be immediately
# assigned over top of and dereferenced.

sub bneg {

  # negate set

  my $self = shift;
  my $set = $self->set();
  my @ret = ();

  foreach my $i (ARRAY $set) {
    push @ret, 1.0 - $i;
  }

  __PACKAGE__->new($self->type(), \@ret);
}

sub bpow {

  # take each value to a given power then normalize again.
  # with a value <1, this serves to de-exagerate the curve, moving the line twards the middle.
  # with a value >1, this exagerates the curve, causing it to tend twards the top and bottom.

  my $self = shift;
  my $fac = shift;

  my @ret = ARRAY $self->set();

  foreach my $i (@ret) {
     $i **= $fac;
  }

  return __PACKAGE__->new($self->type(), \@ret)->normalize();

}

sub bpls {

  # normalize the curve to 1.0
  # this is a mutator - we change ourself

  my $self = shift;
  $self->normalize();
  $self;

}

sub bmns {

  # trim the 0's off the beginning and the end - this serves to stretch the curve across the
  # entire range.
  # this is a mutator - we change ourself

  my $self = shift;
  my $set = $self->set();

  my @newset;

  foreach my $i (ARRAY $set) { 
      if($i != 0 .. $i != 0) {
        # starts being true as soon as we pass the 0's, then keeps going till we hit a 0
        push @newset, $i;
      };
  }

  $self->set() = \@newset;
  $self;

}

sub brsh {
}

sub blsh {
}

1;
