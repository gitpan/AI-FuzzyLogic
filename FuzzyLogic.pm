
package AI::FuzzyLogic;

# hi! reading through the code? i try to get the infrastructure stuff out of the
# way first, so it is boring for a while. search for the string "conversion"
# below to find the start of the meat. "operators" follows quickly after.
# unary operators come first, then binary. this is where the guts of this implementation
# of fuzzy logic are. if you're looking for infrastructure, accessors and constructurs
# and such come first, and for anything more complex, i suggest you read the ntoes
# at the end of the file. i've tried to comment and document well, but please
# let me know if something could be improved. good luck!

=head1 NAME

AI::FuzzyLogic - Fuzzy Set Operations and Tools

=head1 SYNOPSIS

  use AI::FuzzyLogic;

  $i = new AI::FuzzyLogic $unittype, @numbers;          # new set with one subset
  $i = new AI::FuzzyLogic 'age', 0, 0.1, 0.2, 0.1, 0;   # same thing

  $i = new AI::FuzzyLogic $subset1, $subset2, $subset3; # new set with several subsets

  # another syntax for building a set with several subsets:
  $i = AI::FuzzyLogic->new(
       AI::FuzzyLogic->new('distance', 0.0, 0.1, 0.1, 0.5, 0.8, 0.6, 0.3, 0.0),
       AI::FuzzyLogic->new('time',     0.3, 0.3, 0.1, 0.1, 0.1, 0.2, 0.3, 0.3),
       AI::FuzzyLogic->new('heat',     0.0, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.2),
  );

  # constructors for explicit combinational behavior:
  $i = new AI::FuzzyLogic::Correlator 'speed', 0.1, 0.3, 0.2, 0.1, 0.1;
  $i = new AI::FuzzyLogic::Permutator 'speed', 0.1, 0.3, 0.2, 0.1, 0.1;
  $i = new AI::FuzzyLogic::Discriminator 'speed', 0.1, 0.3, 0.2, 0.1, 0.1;
  $i = new AI::FuzzyLogic::Abstractor 'speed', 0.1, 0.3, 0.2, 0.1, 0.1;

  # change combinational behavior:
  $set->as_correlator();      # operations work on matching subsets of same type
  $set->as_permutator();      # operations work across all subsets of each set
  $set->as_discriminator();   # operations best matching subset from right for each on left
  $set->as_abstractor();      # operations return one set with one subset summerizing fit

  $i->add_subsets($j);        # combine subsets or other sets in

  abs($i)                     # defuzzify to integer (centroid - curve middle, x axis)
  0+$i                        # defuzzify to integer (mean - average curve height, y axis)

  $a & $b                     # intersection of sets
  $a | $b                     # union of sets
  $i++                        # normalize curve to 1.0
  $i--                        # stretch curve to edges
  ~$i                         # negate set
  $i ** 0.5                   # dialation
  "$i"                        # convert subsets to ASCII graphs

  $a + $b                     # sum sets
  $a - $b                     # subtract sets
  $a * $b                     # multiply sets - useful for sensitivity control
  $a / $b                     # divide sets - useful for sensitivity control

  $h->larger($a)              # boolean: does $h completely encompass $a? 

  $a ^ $b                     # xor: same as ~($a | $b)
  $a < $b                     # compare volume: is $a smaller?
  $a > $b                     # compare volume: is $a larger?

  @sets = $a->unwrap();              # get subsets as list of AI::FuzzyLogic::Subset objects
  @sets = $a->query_type('type');    # get subsets of type 'type' as a list of AI::FuzzyLogic::Subset objects
  $a->change_type('fromtype', 'to'); # change type of subsets of type 'fromtype' to 'to'

=head1 DESCRIPTION

Performs all basic and some advanced operations on Fuzzy Sets. Use English-like, intentionally
vague objects representing concepts with which to make inferences. The inferences might be approximate
reasoning about precise knowledge, or precise reasoning about approximate knowledge. This
vagueness allows the capture and application of human expert knowledge.

Overloads Perl operators to perform operations on Fuzzy Sets. 

Few good introducts to Fuzzy Logic exist. If you find one, let me know, and I'll recommend it,
but I've yet to find a general introduction that introduces the idea, provides a sample
implemenation, and shows how to use the idea and implementation to solve problems. So,
part of the official goal of this project is to introduce Fuzzy Logic to the novice
and get her up and running and productive.

=head3 Other Fuzzy Modules

AI::FuzzyInference, AI::Fuzzy. We don't attempt to provide a structure for building
inference chains - that is left to regular perl code using overloaded operators, C<if>
statements, and the like. We also define a larger set of operations, introduce sets-of-sets
and combinational, permutational behavior for working on them. AI::Fuzzy doesn't fit
with what my understanding of Fuzzy Logic is. Sorry, Tom. See "Collaboration" in the
L<BUGS> section. 

Extensible framework. Modules in this distribution may be subclassed to define new
Fuzzy operations, combinational behaviors, and other features. Extensions may be
added to this distribute (if I like them), or you may distribute them seperately,
with this module as a dependency.

=head3 Terminology

Sets (AI::FuzzyLogic) contain subsets (AI::FuzzyLogic::Subset). Subsets contains elements.
Elements are just numbers in an array (this implementation may change, but it is useful
to think of it this way). 
Elements are also called segments sometimes, as in segments in a LED display.
Combinational Behavior controls what happens when an operation is performed
between two sets, one or both of which have more than one subset.

=cut

our $VERSION='0.03';

use strict;
use warnings;

use English::Reference;
use Scalar::Util 'blessed';

use AI::FuzzyLogic::Subset;

#
# terminology
#

# *_inner routines are logic defined as part of the subset. for example, to compare the
# mean of the curves of two subsets, each subset would be asked for its mean, then those
# numbers would be compared.

#
# debugging
#

use lib '/home/projects/transient';
#use Nark;
#Nark::nark sub { print shift()."\n" };

$SIG{__DIE__} =  $SIG{INT} = sub {
   # help us locate endless loops.... testing.
   # when someone does kill -INT <our pid> from the command line, dump our stack and exit
   print STDERR shift, map { (caller($_))[0] ? sprintf("%s at line %d\n", (caller($_))[1,2]) : ''; } 0..30;
   print STDERR join "\n", @_;
   exit 1;
};

#
# overload
#

# if $_[2] is true, it means that the arguments order was swapped so that
# the object reference would come first. this way, the first two args
# to us are the args to operation that was overloaded, except that the
# overloaded object always comes first in cases where an operation is being
# performed against both an overloaded object and a regular number.

sub objify {

  # if $_[2], then reverse the args.
  # if one of the args isn't an object, create it as a new us.
  # new objects created from numbers are sets with a single subset with that number repeated
  # across three elements.
  # this wraps our internal overloaded methods. our return value is fed directly to them.

  @_ = ($_[1], $_[0]) if $_[2];

  @_ = ($_[0], (__PACKAGE__->new('unknown', ($_[1]) x 3))) if ! ref $_[1];

  return @_;

}

use overload
'&'	=>	sub { band(objify(@_)); },      # set intersection (min)
'|'	=>	sub { bior(objify(@_)); },      # set union (max)
'^'	=>	sub { bxor(objify(@_)); },      # set xor ;)
'+'	=>	sub { $_[2] && !$_[1] ? numify2($_[0]) : badd(objify(@_)); },      
                                                # set summation, or if 0+, defuzzification
'0+'    =>      sub { numify2(@_) },            # defuzzify - mean
'abs'	=>	sub { numify(@_) },             # defuzzify - centroid
'-'	=>	sub { bsub(objify(@_)); },      # set difference
'*'	=>	sub { bmul(objify(@_)); },      # set multiply
'/'	=>	sub { bdiv(objify(@_)); },      # set divide
'neg'	=>	sub { bneg(objify(@_)); },      # opposite - invert set
'~'	=>	sub { bneg(objify(@_)); },      # opposite - invert set
'**'	=>	sub { bpow(@_); },              # dialation (2nd arg must be number)
'<'     =>      sub { bles(objify(@_)); },      # which has less area?
'>'     =>      sub { bgre(objify(@_)); },      # which has more area?
'""'    =>      sub { stringify(@_) },          # make pretty little charts
'%'	=>	sub { bmod(objify(@_)); },      # undef - how many times does one set fit in another?
'<<'	=>	sub { brsh(objify(@_)); },      # undef
'>>'	=>	sub { blsh(objify(@_)); },      # undef
'<=>'	=>	sub { bcmp(objify(@_)); },      # compare volume
'cmp'	=>	sub { bstrcmp(objify(@_)); },   # compare center-of-mass
'bool'  =>      sub { numify2(@_); };           # is this set "true"?

# XXX is this todo or what? suggestions for operations?
# min: turn a set into a singleton based on highest point or center of mass or something. defuz.
# max: normalize the set to exist from 0.0 to 1.0. regenerate sets that got beat down.

#
# constructors
#

=head3 new

C<new()> comes in two basic forms. 

Create a new set, with exactly one subset, from raw input data:

  $i = new FuzzyLogic $unittype, @numbers;          # new set with one subset
  $i = new FuzzyLogic 'age', 0, 0.1, 0.2, 0.1, 0;   # same thing

Create a new set, with potentially many subsets, from several existing subsets.

  $i = new FuzzyLogic $subset1, $subset2, $subset3; # new set with several subsets

Subsets can be obtained form existing sets using the C<unwrap()> method:

  $i = new FuzzyLogic $set1->unwrap(), $set2->unwrap();

C<unwrap()> may return any number of subsets.

Though the module will extract the subsets from sets should sets be passed to
the constructor. This is like perl arrays - combining arrays flattens them
all into one large one. No, there is no equivilent to references. See the
L<BUGS> for another note on this.

AI::FuzzyLogic::Abstractor is the default type of new objects. If something else
is desired, it should be specified explicitly, as the default is likely to change
in future versions.

  # constructors for explicit combinational behavior:
  $i = new AI::FuzzyLogic::Correlator 'speed', 0.1, 0.3, 0.2, 0.1, 0.1;
  $i = new AI::FuzzyLogic::Permutator 'speed', 0.1, 0.3, 0.2, 0.1, 0.1;
  $i = new AI::FuzzyLogic::Discriminator 'speed', 0.1, 0.3, 0.2, 0.1, 0.1;
  $i = new AI::FuzzyLogic::Abstractor 'speed', 0.1, 0.3, 0.2, 0.1, 0.1;

Beware! Once created, you'll need to change the combinational behavior frequently
to get any work done. Use the C<as_correlator()>, C<as_discriminator()>,
C<as_permutator()>, and C<as_abstractor()> methods to change the type of an
existing object.

=cut

sub new {

  my $class = shift; $class = ref $class if ref $class; 

  $class = 'AI::FuzzyLogic::Abstractor' if $class eq 'AI::FuzzyLogic'; # default to this subclass

  my $self = bless [], $class;

  $self->add_subsets(@_);

  $self;

}

#
# accessors
#

=head3 add_subsets

Just like C<new()>, but adds new subsets to an existing set.

  $set->add_subsets(new AI::FuzzyLogic 'foo', 0.0, 0.1, 0.1, 0.1, 0.0);

Newly added subsets retain their type in the new object (though the output of
an operation against an Abstractor is always a single set of type 'abstract').

C<new()> calls this method to do its dirty work.

=cut

sub add_subsets {

  # AI::FuzzyLogic objects are containers of AI::FuzzyLogic::Subset objects. 
  # this method adds a new AI::FuzzyLogic::Subset object to our list.
  # it may have to create one first, if all of the args are numeric.
  # if any of the arguments are AI::FuzzyLogic objects, we must extract the subsets from it.
  # if we get an array ref, we bless it into a new AI::FuzzyLogic::Subset.
  # if any given arg is already an AI::FuzzyLogic::Subset, we add it directly to our list.
  # new() uses this to make sense of its arguments, and it is available for use directly as well.

  my $me = shift;

  if(scalar grep({ ! ref $_ } @_) == scalar @_) {
    # they're all non-reference types
    # print "debug: ", scalar grep({ ! ref $_ } @_), " and ", scalar(@_), "\n";
    my $type = shift or die "add_subsets() all non reference case - expecting type string as first arg";
    push @$me, AI::FuzzyLogic::Subset->new($type, [@_]);
    return $me;
  }

  foreach my $i (@_) {

    push @$me, $i if blessed($i) and $i->isa('AI::FuzzyLogic::Subset');
    push @$me, $i->unwrap() if blessed($i) and $i->isa('AI::FuzzyLogic');
    push @$me, AI::FuzzyLogic::Subset->new('unknown', [map { $_ } @$i]) if ref $i eq 'ARRAY';

  }

  return $me;

}

*add_subset = *add = *add_subsets;

sub import { return 1; }

=head3 query_type

  $set->query_type('speed');

Return the subsets (AI::FuzzyLogic::Subset objects) of a given type ('speed', in
this example. In scalar context, the first is returned. In list context, all
matching subsets are returned. This allows access to subsets directly minipulate
them. This can be used with the constructor to build a new AI::FuzzyLogic object
containing all subsets of a given type:

  $speeds = new AI::FuzzyLogic $old_set->query_type('speed');

Returns C<undef> if none are found.

=cut

sub query_type {

  my $me = shift;
  my $type = shift;

  my @sets = grep { $_->type() eq $type } ARRAY $me;
  return @sets if wantarray();
  return $sets[0] if @sets;
  return undef;

}

=head3 unwrap

  @subsets = $set->unwrap();

Return all subsets from a set. These may be used to construct new sets, or
they may be individually minipulated (type changed, perhaps). Some
operators mutate (change the existing object) while others return new
objects that reflect the changes. The former case will affect the state of
the set from which the subset was obtained, and the latter won't.

Handy for debugging:

  foreach my $i ($set->unwrap()) {
    print "in set: ", $i->type(), "\n";
  }

Subsets also have an C<unwrap()> method that returns an array of scalar
floating point values that describe the set.


=cut

sub unwrap {

  # all of our little Subsets
  # currently, this object is a blessed arrayref, where each element is a subset.
  # this may change in the future. for now, all we have to do is reference ourselves
  # to get our list of subsets.
  # it has come to my attention that this needs to be used far too often to do
  # routine work. attempting to fix API.

  my $me = shift;

  return ARRAY $me;

}

sub subtypes { 

  # we have an as_... method for each of our subtypes. each subtype adds its own as_...
  # method to the base class. these methods rebless the object, changing it from one
  # subtype to another. since they are put into the base class, any subtype cass be
  # converted to any other subtype by calling the right as_... method.
  # this method looks through the symbol table to find out which as_... methods have
  # actually been added, and returns the list of names of them.

  grep { m/^as_/ } keys %{__PACKAGE__.'::'}; 

}

=head3 change_type

  $bar->change_type('abstract', 'foo'); # change result from "abstract" to "foo" type

To make the combinational magic specified by Combinational Behavior work, types
must match up. This means frequently having to change the type of a subset in a set.
Volts may go to ampres to watts, and will need to be renamed at each step. If
sets with only one subset are used, it may be easier to just make all sets into
Permutators:

  my $juice = AI::FuzzyLogic::new('juice', 0.5, 0.5, 0.5)->as_permutator();

This, and the result of all operations on which it is on the left hand side of,
will all combine freely with other types. Otherwise, you'd eventually have
to do:

  $juice->change_type('juice', 'watts'); 

Beware! Type is completely different than combinational behavior. Type
controls how things combine, but the rules ultimately depend on
the combinational behavior of the object on the left of the operation.
Start with the description of the combinational behavior (Abstractor,
Permutator, Discriminator, Correlator) and read how it uses type
information.

=cut

sub change_type {

  # change subsets of one type to another type. this is often required
  # to control combinational behavior.

  my $me = shift;
  my $oldtype = shift() or die "old type (string) required";
  my $newtype = shift() or die "new type (string) required";
  my $count = 0;

  foreach my $i (ARRAY $me) {
    if($i->type() eq $oldtype) {
      $i->type($newtype);
      $count++; 
    }
  }

  return $count;

}

#
# conversion
#

sub stringify { 

  # generate pretty little text graph for each subset in our collection
  # part of overload interface to the world - attempting to convert to string
  # and print a fuzzy object makes the overload interface call this. 
  # using a fuzzy object with the . operator or using it inside of a "" string
  # triggers this.

  my $me = shift;

  my $ret;

  foreach my $subset (ARRAY $me) {
    $ret .= $subset->stringify();
  }

  return $ret;

}

sub numify {

  # find the center of mass of the curve for each set.
  # part of overload interface to the world.
  # this isn't a meaningful operation for sets that contain more than one subset -
  # things kind of get combined in a non-helpful way. 
  # using other operations, distill all of the data down to single sets, 
  # then use this to do "crisp" poerations between this distilled data.
  # fuzzy data must be made crisp at some point if it is to be used in non
  # fuzzy systems. it can be done by converting it to a number, or doing some
  # test between two fuzzy sets that yeilds a yes/no answer.

  # comments to self:

  # this doesn't make any sense, using centroid_inner(). changes to mean_inner().
  # centroid is a function of left-rightness. unless all subsets describe the same thing,
  # this is meaningless. if requesting a number, the user is probably interested in overall fit,
  # which would mean just average of each set, all averaged together.

  # this doesn't make sense averaging the subsets volumes. if the correlator fails to find
  # matching subset types, something could actually rank higher than a case where all subtimes
  # match up. better to sum the averages.

  # more comments to self:

  # duh, it does make sense. people need two means of defuzzifying: centroid and mean.
  # centroid does left-right, mean does up-down.

  my $me = shift;

  my $total; 

  #if(scalar ARRAY $me > 1) { 
  #  warn "Attempting to convert fuzzy subsets to a number: which subset do you want? Use a " .
  #       "discriminator to select only one.";
  #  return 0;
  #}

  foreach my $subset (ARRAY $me) {
    $total += $subset->centroid_inner();
  }

  return undef unless $total;

  return $total / scalar ARRAY $me;

}

sub numify2 {
  my $me = shift;
  my $total;
  foreach my $subset (ARRAY $me) {
    $total += $subset->mean_inner();
  }
  return undef unless $total;
  return $total / scalar ARRAY $me;
}

#
# operators
#

# the fuzzy operations that can be performed defines fuzzy logic, and is the heart of this
# module. unary operators are delegated to the subset itself. binary operators are
# done here. the definition of the actual operation is here, but a lot of infrastructure
# is called upon.

#
# unary operations
#

# each of these creates a new AI::FuzzyLogic object of the same subtype as the current
# object, containing the result of applying the operation to each subset in the current object.
# the results of applying these operations should all be copies, not references to originals.

sub bneg { $_[0]->new(map({ $_->bneg() } ARRAY $_[0])) }
sub bpls { $_[0]->new(map({ $_->bpls() } ARRAY $_[0])) }
sub bpow { $_[0]->new(map({ $_->bpow() } ARRAY $_[0])) }
sub bmns { $_[0]->new(map({ $_->bmns() } ARRAY $_[0])) }
sub brsh { $_[0]->new(map({ $_->brsh() } ARRAY $_[0])) }
sub blsh { $_[0]->new(map({ $_->blsh() } ARRAY $_[0])) }

# except this one, which is a mutator, and changes the originals.

sub balance { foreach my $subset (ARRAY $_[0]) { $subset->balance() }; $_[0]; }

#
# utilities for use by binary operators
#

sub compile {

  # given two subsets and a callback, callback with each set of paired numbers inside those subsets.
  # this stretches out the smaller set to be as large as the larger set, for the purpose of doing
  # element-by-element comparisons.

  # see the notes at the end of this file on how this fits in. 

  my $inta = shift; die 'wtf' unless $inta->isa('AI::FuzzyLogic::Subset');
  my $intb = shift; die 'wtf' unless $intb->isa('AI::FuzzyLogic::Subset');

  my $callback = shift;

  my $seta = $inta->set() or die;
  my $setb = $intb->set() or die;

  my $stepa; my $stepb; my $max;
  my $posa = 0; my $posb = 0;
  my @ret;

  if(scalar(ARRAY $seta) > scalar(ARRAY $setb)) {
    $max = scalar(ARRAY $seta); $stepa = 1; $stepb = scalar(ARRAY $setb)/scalar(ARRAY $seta);
  } else {
    $max = scalar(ARRAY $setb); $stepb = 1; $stepa = scalar(ARRAY $seta)/scalar(ARRAY $setb);
  }

  while($posa<$max && $posb<$max) {
    $seta->[int $posa] ||= 0; # work around for a strange bug that generates warnings XXX
    $setb->[int $posb] ||= 0; # work around for a strange bug that generates warnings XXX
    push @ret, $callback->($seta->[int $posa], $setb->[int $posb]); 
    $posa += $stepa; $posb += $stepb;
  }

  return AI::FuzzyLogic::Subset->new($inta->type() || 'unknown', \@ret);

}

#
# binary operators - discriminators
#

# part of each of these are stock. see the comments at the end of the file about
# using selector() to narrow down which subsets should be combined, and compile()
# to do an element by element comparison between those two subsets.
# just pay attention to the line that starts with "return" - that is the heart
# of each operation, which is applied to each matching element between two subsetsw.

#head3 best
#
#When used with Permutators, the best combination of subsets from the left and
#right is found. With Abstractors, you get a single output set representing 
#the best combination. Perhaps most useful with the Descriminator, with one
#or two or many subsets in the set on the left and several on the right. 
#The best match is found and the subset on the right returned in a new AI::FuzzyLogic object.
#
#  $a->best($b);
#
#Right now, I don't think these work. Everything is included in the output, though
#some subsets are mangled to all-zeros or otherwise molested.
#
#cut

sub best { 
  my $me = shift;
  $me->selector(shift(), sub {
    compile($_[0], $_[1], sub {
      # $_[0] vs $_[1] ---- more points, the closer together each point
      # how closely do two lines follow each other? like smallest() and largest() but without
      # the "flunk" conditions for going over or under.
      return 1.0 - abs($_[0] - $_[1]);
    });
  });
}

sub smallest { 
  my $me = shift;
  $me->selector(shift(), sub {
    my $flunk = 0;
    my $subset = compile($_[0], $_[1], sub {
      # more points the closer each $_[0]->[x] is without going over $_[1]->[x]
      # ie, the smaller one line is, the better. if it goes over, it flunks.
      # 0 condition should 0 entire set, not just that segment - XXX
      return (1 - ($_[1] - $_[0])) if $_[0] < $_[1];
      $flunk = 1; return 0;
    });
    return $flunk ? AI::FuzzyLogic::Subset->new($_[0]->type(), [0, 0, 0, 0, 0]) : $subset; 
  });
}

sub largest { 
  my $me = shift;
  $me->selector(shift(), sub {
    my $flunk = 0;
    my $subset = compile($_[0], $_[1], sub {
      # more point the closer each $_[0]->[x] is without going under $_[1]->[x]
      # ie, larger the better, and cannot go under the second line.
      # 0 condition should 0 entire set, not just that segment - XXX
      return (1 - ($_[0] - $_[1])) if $_[0] > $_[1];
      $flunk = 1; return 0;
    });
    return $flunk ? AI::FuzzyLogic::Subset->new($_[0]->type(), [0, 0, 0, 0, 0]) : $subset; 
  });
}

=head3 larger

  $a->larger($b); # does $a completely encompass $b?

Test if one set fits entirely within another or not. If there are multiple subsets
and combinational behavior and types allow, then it returns true if any matching
subsets on the left are larger than any on the right.

Unlike the above, this is actually beleived to work and has been somewhat tested.

=cut

sub larger {
  my $me = shift;
  my $larger = 1;
  my $any = 0;
  $me->selector(shift(), sub {
    compile($_[0], $_[1], sub {
      # the left side must entirely contain the right side to return true
      $any = 1;
      $larger = 0 if $_[0] < $_[1];
      return 0;
    });
  });
  $any or die "no matching subtypes for object type " . ref($me);
  return $larger;
}

sub centroid {
  my $me = shift;
  $me->selector(shift(), sub {
    # how different is the center of mass between two lines? the lines might have very different
    # height and shape, but if their center of masses match exactly, we get a 1.0
    return abs(centroid_inner($_[0]) - centroid_inner($_[1]));
  });
}

#
# binary operators - operators
#

sub bmul {
  my $me = shift;
  $me->selector(shift(), sub {
    compile($_[0], $_[1], sub {
      # good for sensitivity control - amplify the importantance of some regions 
      return $_[0] * $_[1] > 1 ? 1 : $_[0] * $_[1];
    });
  });
}

sub bdiv {
  my $me = shift;
  $me->selector(shift(), sub {
    compile($_[0], $_[1], sub {
      # good for sensitivity control - dimenish the importantance of some regions 
      return $_[0] / $_[1];
    });
  });
}

sub badd {
  my $me = shift;
  $me->selector(shift(), sub {
    compile($_[0], $_[1], sub {
      return $_[0]+$_[1] > 1 ? 1 : $_[0]+$_[1];
    });
  });
}

sub bsub {
  my $me = shift;
  $me->selector(shift(), sub {
    compile($_[0], $_[1], sub {
      return $_[0]-$_[1] > 0  ? $_[0]-$_[1] : 0;
    });
  });
}

sub bcmp { 
  # compare volumes, to sort by largeness of set
  my $me = shift(); $me->isa(__PACKAGE__) or die __PACKAGE__ . ' required';
  my $them = shift()->numify(); $me->isa(__PACKAGE__) or die __PACKAGE__ . 'required';
  return $me->numify2() <=> $them->numify2();  
}

sub bstrcmp { 
  # compare center of mass, to sort by position of hump (okey, that sounds bad)
  my $me = shift(); $me->isa(__PACKAGE__) or die __PACKAGE__ . ' required';
  my $them = shift()->numify(); $me->isa(__PACKAGE__) or die __PACKAGE__ . 'required';
  return $me->numify() <=> $them->numify();  
}

sub band {
  my $me = shift;
  $me->selector(shift(), sub {
    compile($_[0], $_[1], sub {
      # the quentiscential fuzzy operation - intersection
      return $_[0] < $_[1] ? $_[0] : $_[1];
    });
  });
}

sub bior {
  my $me = shift;
  $me->selector(shift(), sub {
    compile($_[0], $_[1], sub {
      return $_[0] > $_[1] ? $_[0] : $_[1];
    });
  });
}

sub bxor {
  my $me = shift;
  my $max;
  $me->selector(shift(), sub {
    compile($_[0], $_[1], sub {
      # the degree that neither are true. same as an or then a negate, i guess.
      return 1.0 - ( $_[0] > $_[1] ? $_[0] : $_[1] );
    });
  });
}

sub bles {
  my $me = shift;
  my $lhs; my $rhs;
  $me->selector(shift(), sub {
    compile($_[0], $_[1], sub {
      # does the first set have less area then the second? returns an integer.
      # this implementation works - compile() stretches one out as needed.
      $lhs += $_[0]; $rhs += $_[1];
      return 0;
    });
  });
  return $lhs < $rhs; 
}

sub bgre {
  my $me = shift;
  my $lhs = 0; my $rhs = 0;
  $me->selector(shift(), sub {
    compile($_[0], $_[1], sub {
      $lhs += $_[0]; $rhs += $_[1];
      return 0;
    });
  });
  return $lhs > $rhs; 
}

sub bmod {
  my $me = shift;
  my $minfactor = undef;
  $me->selector(shift(), sub {
    compile($_[0], $_[1], sub {
      # how many times does the set on the right fit into set on the left, for the smallest
      # segment? untested.
      my $tmp; 
      if($_[1]) {
        $tmp = $_[0] % $_[1];
        $minfactor = $tmp if ! defined $minfactor;
        $minfactor = $tmp if $tmp < $minfactor;
      } else {
        # zero fits in an infinte number of times, even to zero. increase but don't decrease.
        $minfactor = 10 if ! defined $minfactor;
      }
      return 0;
    });
  });
  return $minfactor;
}

1;

#
# subclasses of AI::FuzzyLogic -
# versions of ourselves have different combinational behaviors 
#

# the base AI::FuzzyLogic set is never used directly. all sets are really a 
# subclass. each subclass adds an as_* method to the base class, AI::FuzzyLogic,
# and each subclass defines its own private selector() method. this selector()
# method is the key difference between each subclass type, and it controls
# how subsets are permutated when two sets are compared against each other.
# read the POD that describes each behavior to understand the purpose of these
# subclasses. see the section about "Combinational Behavior".
# selector() is called from all "binary operators" - operators that require
# two arguments, one on the left, and one on the right. this is part of the
# overload interface. perl invokes the correct routine to deal with an operator
# being used on a fuzzy object. there is also a high level explanation at the
# end of the file, of how operators and selectors and subsets fit together.

=head2 Combinational Behavior

=head3 Abstractors

Abstractors always return exactly one set, which is meant to be a gross summary
of membership of one set in another.
Returns one set, with about as many members as there are subsets in the object on
the right. Gives a membership summery, or a composition of how well
or how poorly all of the various attributes match up, by type. 
If the types don't match up, they are ignored. Otherwise, the comparision
of the matching sets forms a single segment in the output set. The
output set is balanced, with the line the highest in the center.
Useful when used between a set containing patterns to match and set containing
observations. 

For example, subsets may represent color and size. One set, "a", is observed
in the wild (the Internet, through data capture, what have you). Other sets,
"x1", "x2", "x3", etc, each having the same subsets (color and size) are compared
against "a" to find the best match in attempt to classify "a" as being
stereotypical of one of a few known cases.

If the output is a flat zeros, no criteria matched. If it is a low curve,
few things matched, and they matched poorly. If it is a low curve with
some spikes in the middle, a few things matched well, but most criteria
matched poorly. A nice bell curve is a fairly good match on most criteria,
and a solid box with 1's across the board is a perfect fit.

  $set->as_abstractor();

The result is always balanced (the hump, if any, is in the middle).

The single result set contains exactly one subset, which is of type 'abstract'.
To do operations on that with anything other than C<larger()> or a Permutator,
you'll need to change the type to match the desired subset type of the other set.

  my $foo = new AI::FuzzyLogic 'foo', 0.1, 0.2, 0.5, 0.2, 0.1;

  my $bar = $big_old_set->as_abstractor() & $another_big_old_set();

  $bar->change_type('abstract', 'foo'); # change result from "abstract" to "foo" type

  my $baz = $foo & $bar;

Beware! Once created, you'll need to change the combinational behavior frequently
to get any work done. These C<as_...> methods will need to be used over and over.

=cut

package AI::FuzzyLogic::Abstractor;
use base 'AI::FuzzyLogic';
sub AI::FuzzyLogic::as_abstractor { bless $_[0], __PACKAGE__; }
sub selector {
  my $me = shift;
  my $them = shift; $them->isa('AI::FuzzyLogic') or die 'instance of AI::FuzzyLogic needed';
  my $coderef = shift; ref $coderef eq 'CODE' or die 'no coderef';
  my @newset;
  foreach my $mysubset ($me->unwrap()) {
    foreach my $theirsubset ($them->unwrap()) {
      next unless $mysubset->type() eq $theirsubset->type();
      push @newset, $coderef->($mysubset, $theirsubset);
    }
  }
  # for each subset, find the average of all segments; these averages, balanced, are our new set of one subset
  return $me->new('abstract', sort map { $_->mean_inner() } @newset)->balance();
}
1;

=head3 Discriminators

Discriminators pare down sets which have subsets.

Discriminators consider all of the permutations, but throw away all of them except
the set from the right-hand-side which yeildeds the largest resulting set
(defined by volume). Hence, whichever operation is performed on a discriminator
only serves to give a criteria for selecting a set from the right-hand-side.
Discriminators are useful for selecting one optimial case from a number of alternatives.
Like the permutator, except we only keep the highest ranked cross matches.
Always returns exactly one set from the right hand side. The left hand side is considered
to be the rule by which to measure the left.

  $set->as_discriminator();

=cut

package AI::FuzzyLogic::Discriminator;
use base 'AI::FuzzyLogic';
sub AI::FuzzyLogic::as_discriminator { bless $_[0], __PACKAGE__; }
sub selector {
  my $me = shift;
  my $them = shift; $them->isa('AI::FuzzyLogic') or die 'instance of AI::FuzzyLogic needed';
  my $coderef = shift; ref $coderef eq 'CODE' or die 'no coderef';

  my $highestrankedset;
  my $highestrankedvalue;
  my @ret;

  foreach my $mysubset ($me->unwrap()) {
    $highestrankedvalue = 0;
    $highestrankedset = undef;
    foreach my $theirsubset ($them->unwrap()) {
      my $value = $coderef->($mysubset, $theirsubset)->mean_inner();
      if($value > $highestrankedvalue) {
        $highestrankedvalue = $value;
        $highestrankedset = $theirsubset;
      }
    }
    push @ret, $highestrankedset->clone() if $highestrankedset; # XXX there is no clone()!
  }
  return undef unless @ret;
  return $me->new(@ret);
}
1;

=head3 Permutators

Permutators consider every possible permutation between subsets in the object on
the left-hand-side and the subsets in the object on the right-hand-side, and return
an object with a subset for each permutation. Performs the desired operation
as a cartesian product.

  $set->as_permutator();

=cut

package AI::FuzzyLogic::Permutator;
use base 'AI::FuzzyLogic';
sub AI::FuzzyLogic::as_permutator { bless $_[0], __PACKAGE__; }
sub selector {
  # given two objects and a code ref, find cartesian products with coderef performed on them.
  my $me = shift;
  my $them = shift; $them->isa('AI::FuzzyLogic') or die 'instance of AI::FuzzyLogic needed';
  my $coderef = shift; ref $coderef eq 'CODE' or die 'no coderef';
  my @ret;
  foreach my $mysubset ($me->unwrap()) {
    foreach my $theirsubset ($them->unwrap()) {
      push @ret, $coderef->($mysubset, $theirsubset);
    }
  }
  return undef unless @ret;
  return $me->new(@ret);
}
1;

=head3 Correlators

Correlators are like Permutators, except instead of considering all permutations,
they only consider permutations between subsets with matching unit types.
Permutators and Correlators are useful for generating alternative cases, possibly in
several steps, which Discriminators or Abstractors may then select from.

Useful for finding optimal cases. For example, combinations of two or more
gears can be considered, and then in an additional step, the combination best
matching some criteria could be selected.

  $set->as_correlator();

=cut

package AI::FuzzyLogic::Correlator;
use base 'AI::FuzzyLogic';
sub AI::FuzzyLogic::as_correlator { bless $_[0], __PACKAGE__; }
sub selector {
  # perform operations between matching type subsets between two objects
  my $me = shift;
  my $them = shift; $them->isa('AI::FuzzyLogic') or die 'instance of AI::FuzzyLogic needed';
  my $coderef = shift; ref $coderef eq 'CODE' or die 'no coderef';
  my @ret;
  foreach my $mysubset ($me->unwrap()) {
    foreach my $theirsubset ($them->unwrap()) {
      next unless $mysubset->type() eq $theirsubset->type();
      push @ret, $coderef->($mysubset, $theirsubset);
    }
  }
  return undef unless @ret;
  return $me->new(@ret);
}
1;

__END__

=pod

=head1 INTRODUCTION TO FUZZY LOGIC

Since Perl is used rather then a dedicated Fuzzy language (like DCU), 
some aids for constructing control systems and expert systems with fuzzy
logic are provided.

FuzzyLogic overloads most math operators. 

Think of a fuzzy set as a box with a line running through it:

  ______________________________________________
  |                    ___                     |
  |                 __/   \__                  |
  |              __/         \__               |
  |           __/               \__            |
  |        __/                     \__         |
  |     __/                           \__      |
  |  __/                                 \__   |
  | /                                       \_ |
  ----------------------------------------------

The line has several interesting attributes:

=over 1

=item Height at highest point

=item Average height, looking up and down, on the Y axis

=item Volume under the line

=item Center of mass, looking left to right, on the X axis

=back

The X axis (left to right) is a continuum of some user defined metric.
The metric can be anything measureable, but it is up to you to combine
things in useful ways. Speed by time is distance, volts over
resistance is ampres, and so forth. If you want to write a metaphysical
module that couples color with mood, you're on your own. Each set
measures one I<kind> of thing: speed, distance, time, voltage, 
color, age, irritation, whatever. If the center of mass of the
line is on the far left, it means "slow", "short", "quick",
"low power", "red", "young", or "happy", respectively. The
exact meaning of the left and the right depends on what the metric
is. The system doesn't understand metrics beyond recognizing two
metrics as being the same or different.

The Y axis of the box represents degree of membership. The top is
expressed as "1.0". The bottom represents lack of membership and is
expressed as "0.0". While up and down represent degree of membership,
left and right are user-defined: they reprsent some continuum of
which membership is measured. The above set could represent someone
who is middle aged: they aren't young, they aren't old. The closer you
get to the middle, the stronger the membership.  "Age" would be
our "unit type". The unit type describes what the continuum
measures: age, color, speed, time, distance, size, or any other
quality or quantity.

The line itself represents a mapping between the metric and the
degree of membership. A "sharp" classification would give a 
precise X value and a precise Y value. A statistical system
might give many precise X and Y values and make inferances from
that. A Fuzzy system has a degree of membership (possibly 0)
for I<each> value of X.

The continuum is important. As operations are performed, and degrees
of membership are measured, this line will start to fall like a 
circus tent, only being proped up in a few random places.

No definate meanings are defined for unit types, but it is handy
to compose sets out of subsets, where each set might have a
subset for color, size, and speed, for instance. We'll see
shortly how unit types help with classification operations
in the Purmutations section.

"Set", "curve", and "object" are used interchangeable to mean a fuzzy set. 
"Subset" and
"property" are used interchangeable to mean one fuzzy set in an object that
contains several.

=head2 Membership Tests

Membership tests are the heart of Fuzzy Logic. 

A fuzzy set can be thought of as a curve inside of a box. The top of the box is 1.0, the
bottom 0.0.

  ______________________________________
  |               __                    |
  |            __/  \___                |
  |         __/         \__             |
  |     ___/               \__          |
  | ___/                      \____     |
  |/                               \_   |
  --------------------------------------

Binary operators perform membership tests:

  $blue + $red;    # the sum of two sets - purple
  $sky & $blue;    # intersection of two sets - still blue
  $sky - $blue;    # the sky without blueness - redish hues show through
  $sky | $white;   # blue is what the sky and white have in common

To make any useful deductions, several membership tests must be chained together.

Fuzzy Sets have subsets. For instance, "Size" may
be a collection of the subsets representing "Small", "Medium", and "Large". Comparing
a set with the "Size" set will yield multiple results, one of which will
have more volume then the others. Discriminator operations may be
used to select a single set from a set that contains subsets, among other things.
After discrimination, operations are available to "defuzzify" a set, and give precise numeric
output.

  my $small = new AI::FuzzyLogic 'size', 0.3, 0.3, 0.3;

One of a few cases will apply to any given instance of AI::FuzzyLogic:

=over 2

=item Contains several subsets, each of different unit-types 

For example, speed, distance, temperature, etc. Each of these attributes describe the
same object: perhaps a runner in a race, measured at a point in time. 

=item Contains several things of the same unit type 

For a unit type of "size", subsets might represent small, large, huge, etc. This
will likely be used to describe, categorize, recognize, or classify another set that
describes something concrete, like the runner in the previous item. 

=item Output of some operation on sets of sets representing membership

To consider complex cases, multiple attributes must be distilled. To
determine if a runner is holding steady and not losing momentum, you
could test their distance, take that result, and test it against their
speed. This might be a good way to predict a winner.

=back

=head2 Manual Discrimination

Discriminator methods (smallest(), largest(), best(), and centroid()) take a set with
subsets and another fuzzy object, and match them up according to some heuristic.
This is useful to select a single set out of a pool of subsets. This can be done
to select a type of control operation to perform from an exclusive pool, to
select a hedge to describe a set, or to rid yourself of the subsets in general.
This is designed to be used on objects of type FuzzyLogic::Discriminator, but may
be useful with other subtypes as well.

For example, you may compare several hedges to a set to find out which description
best suits it. You may compapre a set of speeds against a volicity. You may compare
a set of sizes against an object. You may compare a set of known colors against a
new color.

Or, given a series of objects, you may wish to know which is the fastest, largest,
or redest of the lot.

  my $fast =    new FuzzyLogic 'speed', 1.0, 0.9, 0.7, 0.2, 0.0, 0.0, 0.0, 0.0;
  my $medium =  new FuzzyLogic 'speed', 0.0, 0.1, 0.3, 1.0, 1.0, 0.3, 0.1, 0.0;
  my $slow =    new FuzzyLogic 'speed', 0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.7, 1.0;
  my $moving =  new FuzzyLogic 'speed', 0.5, 0.7, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0;
  my $stopped = new FuzzyLogic 'speed', 1.0, 0.3, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0;
  $speeds = new FuzzyLogic $slow, $medium, $fast, $moving, $stopped;

  # which speed hedge best describes the speed of the fuzzy object $volicity?
  # in this example, this would be the prefered method. it asks the system
  # to find a curve that closely resembles the input, considering both X
  # and Y axises.
  # probably one of fast, medium, or slow
  $volicity->best($speeds);

  # if moving and stopped weren't options, this would work well, but they are
  # reduced to their average X position by center of weight. 
  $volicity->centroid->($speeds); 

  # which speed hedge, if any, would be an understatement? this would be the
  # absolutely most specific statement that could be made.
  $volicity->smallest($speeds);

  # which speed hedge, if any, would be an overstatement, completely describing
  # the speed, even if it has to exaggerate a little? this would be the most 
  # general statement that could be made. probably moving or stopped, as they
  # are the most emcompassing, but likely one of fast, medium, or slow as well.
  $volicity->largest($speeds);

Like operators, manual discriminators are subject to the laws of combination
behavior.

C<larger()> is a special case - rather than filtering, it performs a simple
binary test - does one set fit entirely within another? Doesn't work when
more than (or less than) one subset is present in either set. 

  # larger(), simple case:

  if($a->larger($b)) {
    # condition met
  }

Useful for after abstracting with Abstractor:

  # initialization time:

  $a = new AI::FuzzyLogic (
       AI::FuzzyLogic::Subset->new('foo', 0.0, 0.1, 0.3, 0.1, 0.0),
       AI::FuzzyLogic::Subset->new('bar', 1.0, 0.9, 0.7, 0.3, 0.1),
  );

  $a->as_abstractor();

  $b = new AI::FuzzyLogic 'abstract', 0.2, 0.3, 0.2;

  # run time:

  while(1) {
    
    # create $c here from sensor input or what not
  
    my $d = $a & $c;

    $d->balance(); # move large segments to the middle, making a nice hump

    if($d->larger($b)) {
      # conditions in $a met within criteria required by $b - match successful
    }

    sleep 1;

  }

This is a classic two-stage filtering: each criteria is individually operated on in the
C<$a & $c> line, doing a membership function. The C<$d->balance()> and C<$d->larger($b)> 
take that and make a final descision on whether or not the degree of membership is
enough.

=head2 Combinational Behavior

Subclasses of the FuzzyLogic module combine their
subsets in different ways. All Fuzzy objects are actually a subclass. If none
is specified, new Fuzzy objects are instances of AI::FuzzyLogic::Abstractor.
Depending which subclasses are loaded, one of them will take over the position 
of being the default type. There are four types defined:
Discriminators, Correlators, Permutators, and Abstractors.

Combinational behaviors pare down possibilities, generate possibilities
that may be later pared down, or apply an operation to every subset in a set
without changing their numbers. This is an extention to normal Fuzzy systems.
It should be thought of as glue for making useful Fuzzy operations tolerable
to code in Perl, using overloaded operators. Of course, it wouldn't be needed
with hyperoperators: C<@a = (new AI::FuzzyLogic ..., new AI::FuzzyLogic ...); @a = @a + @b;>

The object on the left-hand-side of the operator specifies
the details of how the two subsets in the objects combine, and specifies
the default combinational behavior of the resulting object.
Left-hand-side and right-hand-side refer to the two sets on either side of an
operator. For example, given: C<$seta * $setb>, C<$seta> is on the left-hand-side
and C<$setb> is on the right-hand-side. 

Combination behavior of an existing object may be changed with mutator
methods. The same object, containing the same data but of the desired type, is
returned, and modified in-place. The methods are:

  $set->as_correlator();
  $set->as_permutator();
  $set->as_discriminator();
  $set->as_abstractor();

Of course, this never hurts:

  $set = $set->as_correlator();

=head2 Operators

Unary operators behave in a predictable way. Binary operators act on two arguments and
combine them in some form to create a result. How they are combined depends on the
combinational behavior of the argument on the left.

  &       # set intersection (min)

The portion of the two sets that overlap is the result. This is the
standard membership test. Keep the Abstractor in mind as you use this.
See also the C<larger()> method for a similar test that instead returns
a boolean value.

  |       # set union (max)

Useful for building sets as compositions of other sets and for judgeing fit
in some situations.
The highest point from each line in each set is the output set. That is,
the output contains the largest values from both input sets.
Useful for building sets as a composite of other sets.

  ^       # set xor ;)

Defined for completeness. Let me know if you find a use for it.
Output set plots the degree that neither input set registers.

  +       # set summation

Useful for building sets as composites of other sets. Output line is
as high as the first input plus the second input. Good for massaging
data before abstracting. Keep the Correlator in mind when using this.

  -       # set difference

Useful for building sets as composits of others. Output set is
first input set minus the height of the second.

  *       # set multiply

For building composite sets. Keep the Correlator in mind when using
this. Given an observation set (perhaps created from sensor data)
and a set representing a floating sensitivity that goes up and down
as needed to avoid feedback and false positives, the two sets would
be multiplied together to apply the sensitivity control to the 
observation data. Since a set can't (in a non buggy implementation)
contain values greater than 1.0, multiplication only serves to
scale sets downwards, never up. Values get smaller. See C<**> below
for a way to scale things up.

  /       # set divide

For building composite sets. Haven't found a purpose aside from the
obvious manual tweaking of sets.

  neg()   # opposite

  ~       # opposite

Output set is input sets high points converted to low points, and low points
converted to high points. When a set contains data of the wrong sense, this
allows you to make another set from it of the right sense. For example, if
you have a decaying frequency average, and you want reduce input sensitivity
when the event happens frequently, you would invert the running average
and multiply that by observation data before testing it. In other words,
frequency can be converted to infrequency. Kind of obvious, really.

  **      # dialation (2nd arg must be number)

Scales entire set to some degree. The argument is an exponent that is
applied to each individual segment in each subset in the set.

  <       # which has less area?

Compare sets by volume.

  >       # which has more area?

Compare sets by volume.

  ""      # make pretty little charts

A box with a line plotted through it will be output for each subset.
Meant to be human-readable.

  abs	  # defuzzify - centroid

Converts sets into a single integer between 0 and 1. 
Number is the relative position of the center of the "hump", along the X axis.
The X axial meaning is decided by the user, so this value would be the point
on the continuum where this observation best fits in.
Calculation is a center of mass calcuation, where half of the volume is on
each side of this point.
Useful for arriving at a quantitative value after all
Fuzzy processing is done. The number will usually represent degree of membership,
and may be the result of several chained membership tests.

  0+      # defuzzify - average height

Converts set into a single integer between 0 and 1. 
Number is average of line height along the Y axis.
The Y axis represents degree of membership, so the result is the average degree of
membership over the measured continuum.
Useful for arriving at a quantitative value after all
Fuzzy processing is done. The number will usually represent degree of membership,
and may be the result of several chained membership tests.

=head2 Subset Objects

AI::FuzzyLogic::Subset and AI::FuzzyLogic::MetaSubset implement subsets - one
AI::FuzzyLogic object may contain any number of subsets. Subsets store
three fields - name, type, and the set itself. The set shouldn't be directly
access - C<unwrap()> should be called to get a set of useful values representing
it. If you're writing something to muck around in the internals, use the C<wrap()>
method. Pass it an array (by value) of data and it will dutifully load it in.
Note that the cleanest solution would be to subclass Subset or MetaSubset
and add additional accessors there, rather than having external code work
on the data. However, the system isn't faithful about maintaining the
exact subclass when operations are done on subsets. 

  my $name = $subset->name();    # query name
  $subset->name('foo');          # set name
  $subset->name('foo') = 'name'; # also set name

To get to set data as an array of scalar floats:

  my @setdata = $subset->unwrap();

Other accessors use the same conventions. MetaSubset subclasses subset to add
an C<attribute()> method:

  my $attrib = $metasubset->attribute();
  $metasubset->attribute(new Foo::Bar $data, $moredata); 

The attribute field can hold objects, string, arrays (by reference) or whatever
scalar data. I created it to associate data with sets in a search engine type
thing. When a match is found, I want to know the information about the thing
found. Merely saying "20 things found" isn't very useful.

=head1 EXAMPLES

  $apple = new AI::FuzzyLogic  'color', 0.9, 0.3, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0; 
  $orange = new AI::FuzzyLogic 'color', 0.0, 0.0, 0.1, 0.7, 0.5, 0.3, 0.1, 0.0;

  $red = new AI::FuzzyLogic    'color', 1.0, 0.7, 0.2, 0.1, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0;
  $orange = new AI::FuzzyLogic 'color', 0.3, 0.7, 1.0, 0.7, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0;
  $green = new AI::FuzzyLogic  'color', 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 1.0, 0.5, 0.0;

  $colors = new AI::FuzzyLogic $red, $orange, $green;

  # try to decide what kind of fruit we have by their color
  $fruitbycolor = $fruit->best($colors);
  print $fruitbycolor;

  # flat lines as sets to test degree of membership in output from another test
  my $hedges = AI::FuzyLogic->new(
    AI::FuzzyLogic->new('not',        0.0, 0.0),
    AI::FuzzyLogic->new('slightly',   0.1, 0.1),
    AI::FuzzyLogic->new('notvery',    0.2, 0.2),
    AI::FuzzyLogic->new('kindof',     0.3, 0.3),
    AI::FuzzyLogic->new('moderately', 0.4, 0.4),
    AI::FuzzyLogic->new('quite',      0.7, 0.7),
    AI::FuzzyLogic->new('definately', 0.8, 0.8),
    AI::FuzzyLogic->new('extremely',  1.0, 1.0)
  )->as_discriminator();

  # tell us how orangy our orange is, and how appley our apple is
  print $hedges->best($fruitbycolor)->type();

  $outputset = $hedges & $green; # how green is green?
  print $outputset->type(); # outputs one of: not, slightly, notvery, kindof, ...

=head1 REFERENCES

DCU, ...
Introduction to Logic: Predicate Logic, Howard Pospesel, Prentice-Hall, Inc.
0-13-486225-2

=head1 SEE ALSO

L<http://perldesignpatterns.com/?FuzzyLogic> was notes from a series of
Perl Mongers presentations, and should serve a good forum/feedback area/list of
resources/whatever. Examples include a Fuzzy control system and a Fuzzy
classification system. I'm supposed to do a Fuzzy expert system for a PM
meeting, sooner or later.

=head1 BUGS

A huge number of fuzzy set operations have been defined at different points by
different people. Only a small subset of those have been included here.

No effort is made to build lookup tables to increase performance. Subsets
could also be coerced into standard sizes for speed, but aren't.

Combinational behavior has no counterpart in formal Fuzzy Logic and is
difficult to explain.

When one FuzzyLogic object is passed to the constructor and made part of
another, a reference is shared. This creates action-at-a-distance problems
as mutators applied to one (like C<balance()>) affect the other as well.
A copy should be made in this case.

Subsets are objects - AI::FuzzyLogic::Subset or AI::FuzzyLogic::MetaSubset.
MetaSubets are currently undocumented, but allow users to attach additional
data to subsets for her own use. The objects implement actual Fuzzy (Sub)Set
storage as an array of scalars. This makes very poor use of memory. In the 
future, PDL or strings may be used. Don't rely on a per-segment dynamic range
of more than 0-200 to represent 0.0 - 0.1. In other words, pretend that each
element in a set is accurate to half of 1/100th of a unit. Use the accessors
to fetch the data as an array, don't grab it directly.

C<smallest()>, C<largest>, C<best>, and C<centroid> don't work. Rather than filtering sets,
they just mangled failing members. Also, they are named badly. 

Can't remember what library book had the DCU system. I just took a lot of notes
and never even checked the book out. Also, some references are missing.

The system isn't faithful about maintaining the exact subclass when operations 
are done on subsets. The package name is hardcoded in numerous places,
when it should be taken from any existing object.

No unit tests. Sorry. I tested with demo applications. I'll have to mine those
for tests. No tests exists for things I don't use. Some operators may not
work at all.

This is an alpha "I've been sitting on the code for too long and it is time
to release it as it is because it is all I'm going to do without some feedback,
or encouragement" release. 

Collaboration: I might have attempted collaboration with an author of an
existing module, but of the dozens of times I've approached people to
let me work with them on something, not once has anyone bitten. Once I
was approached to collaborate, and the approacher quickly changed his mind
after I enthusasiticaly shared a few thoughts and ideas and asked for his.
I know this isn't fair, but I give up. That this project wasn't born out of
collaboration or extension of an existing project, I consider a bug.
Either I'm a bigger freak than I think I am, or the community has a bad
attitude. That is also a bug.

Should be possible to ->clone() sets and subsets.

Implementation notes follow after this POD in the source code to this module.

=head1 EXPORT

Wussat? Why not an INHERIT section? C'mon, get with the times, Perl people!

=head1 AUTHOR

Scott Walters, scott@slowass.net, based on Math::BigInt, DCU, ...
Math::BigInt was written by Mark Biggar, overloaded interface by Ilya Zakharevich.
Ilya's example code was heavily relied upon. Thanks to Phoenix Perl Mongers
for thier feedback (silenced awe and awkward confused sidelong glances).

=cut

__END__

#
# TODO
#

# x. should be possible to ->clone() sets and subsets.
# x. use constraint system to interconnect fuzzy objects and model value-change cascade - maybe
# x. breadth first path discovery for finding routes for establishing membership.. isa->isa->isa..
# x. generic set extra/interpolate routines. eg, scale set to 10 segments, weither up or down.
# v/. external containment: some other object should handle finding everything of a type, etc.
# v/ set_foo(array or arrayref), add_foo(array or arrayref), query - AUTOLOAD
# v/ on creation or afterwards, let user specify weither we permutate, discriminate, etc.

#
# NOTES
#

Other Operators
----- ---------

. standard additive model ?
. best-fit, exponential penalty for non-match per segment - how closely two curves match

Overload
--------

# overload allows us to redefine:
#     +   +=   -   -=   *   *=   /  /=  %  %=
#     **  **=  <<  <<=  >>  >>=  x  x=  .  .=
#     <    <=   >    >=   ==   !=   <=>
#     lt   le   gt   ge   eq   ne   cmp
#     &    ^   |   neg   !   ~
#     ++   --
#     atan2   cos   sin   exp   abs   log   sqrt
#     bool   ""   0+

Constraint System
---------- ------

Re: constraint system to model value changes, great, but most inferenances
go one way - a one-way gate should be introduced. Rather than forcing all of the
change into a single output, a supersitition object could be propogated down that
represents all of the possible output state combinations, when there is more than
one available output.

Binary Operations
------ ----------

Binary Operations - two mechanisms exist to make these easier to code and cut
down on cut and paste. A "selector", defined differently by each subclass of 
AI::FuzzyLogic, desides which subsets from two sets should be compared. It has
the job of filtering out undesireable comparisons or permutations. The definition
of selector() is what gives each subclass of AI::FuzzyLogic its personality.
Then there is compile(), which is defined once. This gives an element by element
matchup between two subsets - each element being a scalar. This is the lowest
level comparison possible between two sets. compile() takes a callback, of
which operation should be performed on corresponding array elemements. The
size of the output array is as large as the larger of the two input arrays
and is accumulated from the return value of the callback being called repeatedly.

The call nesting looks something like:

  selector      <-- returns an AI::FuzzyLogic object
    sub         <-- passes Subset through
      compile   <-- return an AI::FuzzyLogic::Subset object 
        sub     <-- returns a single float between 0 and 1. performs actual logic operation.

Overloaded operators must return objects. Unary operators return the input object,
modified. Binary operators return a new object based on the two input objects.
selector() handles the creation of new objects for binary operators, while
unary operators merely return $_[0].

XXX - selectors could call compile() directly, rather than needing another stack
frame and code reference - atleast, the way all of them are defined now. except one!
whooops, several of them now have extra logic wedge in there. i'm liking the original
design better again. 

--------------------

Speed and memory - subsets could be strings of binary data, where each byte (0-255)
was mapped to a value from 0.0 to 1.0. This should provide plenty of resolution for
most purposes. I usually use int 0.1, 0.2, 0.3, etc and sometimes 1/100ths. To
save rounding errors, 0-200 could be used.

--------

Extensions - subclasses (search for "package" in this file - there are 4 subclasses
and one base) can be created for reasons other than adding new combinational behaviors.
Different operator behavior can be defined. I might develop this interface better 
if there is demand.

