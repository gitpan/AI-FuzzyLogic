
package AI::FuzzyLogic::MetaSubset;

use AI::FuzzyLogic;
use AI::FuzzyLogic::Subset;

use base 'AI::FuzzyLogic::Subset';

# a subset that packages meta-data

sub attribute :lvalue {
  $_[0]->[3] = $_[1] if @_ == 2;
  $_[0]->[3];
}

sub set_attribute {
  $_[0]->[3]->{$_[1]} = $_[2];
}

sub get_attribute {
  return $_[0]->[3]->{$_[1]};
}

