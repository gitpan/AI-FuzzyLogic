use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'		=> 'AI::FuzzyLogic',
    'VERSION_FROM'	=> 'FuzzyLogic.pm', 
    'PREREQ_PM'		=> {
      'English::Reference' => 0.01,
      'Scalar::Util'       => 1.11,
    }, 
    ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM => 'FuzzyLogic.pm',
       AUTHOR     => 'Scott Walters <scott@slowass.net>') : ()),
);
