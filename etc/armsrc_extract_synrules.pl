#!/usr/bin/perl
#
# File:    armsrc_extract_synrules.pl
#
# Purpose: Extract syntax rules from Ada Reference Manual source files (Scribe),
#          see http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ARM/source/
#          The format generated is intended to be more easily usable as a basis
#          for developing an input grammar for parser generators such as bison
#          and antlr.
#          Keywords are converted to upper case.
#          Section numbers are prefixed by //.
#
# Usage:   Provide one or more MSS files as argument.
#          To get the full syntax rules, at the time of writing (Sep 2022)
#          provide the following list:
#          02.mss 03a.mss 03b.mss 03c.mss 04a.mss 04b.mss 05.mss 06.mss 07.mss
#          08.mss 09.mss 10.mss 11.mss 12.mss 13a.mss 13b.mss safety.mss
#          obsolescent.mss
#
# Options: Must precede filename arguments.
#          -m | --markdown         Generate GitHub flavor markdown.
#
# Version: 2022-09-18
#
# Copyright (C) 2022, O. Kellogg <okellogg@users.sourceforgenet>
#
# This program is distributed under the same terms as perl itself, see
#          https://dev.perl.org/licenses/artistic.html
#

sub preprocess;
sub processChg;
sub escape;

@ARGV or die "Provide at least one MSS file\n";

my $markdown = 0;

if ($ARGV[0] eq "-m" || $ARGV[0] eq "--markdown") {
  $markdown = 1;
  shift @ARGV;
}

my %closing = ( '(' => ')', '[' => ']', '{' => '}', '<' => '>',
                '"' => '"', '`' => "'", '%' => '%' );

my %Name2Sec =
  (
    "Character Set"                                            => "2.1",
    "Lexical Elements, Separators, and Delimiters"             => "2.2",
    "Identifiers"                                              => "2.3",
    "Numeric Literals"                                         => "2.4",
    "Decimal Literals"                                         => "2.4.1",
    "Based Literals"                                           => "2.4.2",
    "Character Literals"                                       => "2.5",
    "String Literals"                                          => "2.6",
    "Comments"                                                 => "2.7",
    "Pragmas"                                                  => "2.8",
    "Reserved Words"                                           => "2.9",
    "Declarations"                                             => "3.1",
    "Types and Subtypes"                                       => "3.2",
    "Type Declarations"                                        => "3.2.1",
    "Subtype Declarations"                                     => "3.2.2",
    "Classification of Operations"                             => "3.2.3",
    "Subtype Predicates"                                       => "3.2.4",
    "Objects and Named Numbers"                                => "3.3",
    "Object Declarations"                                      => "3.3.1",
    "Number Declarations"                                      => "3.3.2",
    "Derived Types and Classes"                                => "3.4",
    "Derivation Classes"                                       => "3.4.1",
    "Scalar Types"                                             => "3.5",
    "Enumeration Types"                                        => "3.5.1",
    "Character Types"                                          => "3.5.2",
    "Boolean Types"                                            => "3.5.3",
    "Integer Types"                                            => "3.5.4",
    "Operations of Discrete Types"                             => "3.5.5",
    "Real Types"                                               => "3.5.6",
    "Floating Point Types"                                     => "3.5.7",
    "Operations of Floating Point Types"                       => "3.5.8",
    "Fixed Point Types"                                        => "3.5.9",
    "Operations of Fixed Point Types"                          => "3.5.10",
    "Array Types"                                              => "3.6",
    "Index Constraints and Discrete Ranges"                    => "3.6.1",
    "Operations of Array Types"                                => "3.6.2",
    "String Types"                                             => "3.6.3",
    "Discriminants"                                            => "3.7",
    "Discriminant Constraints"                                 => "3.7.1",
    "Operations of Discriminated Types"                        => "3.7.2",
    "Record Types"                                             => "3.8",
    "Variant Parts and Discrete Choices"                       => "3.8.1",
    "Tagged Types and Type Extensions"                         => "3.9",
    "Type Extensions"                                          => "3.9.1",
    "Dispatching Operations of Tagged Types"                   => "3.9.2",
    "Abstract Types and Subprograms"                           => "3.9.3",
    "Interface Types"                                          => "3.9.4",
    "Access Types"                                             => "3.10",
    "Incomplete Type Declarations"                             => "3.10.1",
    "Operations of Access Types"                               => "3.10.2",
    "Declarative Parts"                                        => "3.11",
    "Completions of Declarations"                              => "3.11.1",
    "Names"                                                    => "4.1",
    "Indexed Components"                                       => "4.1.1",
    "Slices"                                                   => "4.1.2",
    "Selected Components"                                      => "4.1.3",
    "Attributes"                                               => "4.1.4",
    "User-Defined References"                                  => "4.1.5",
    "User-Defined Indexing"                                    => "4.1.6",
    "Literals"                                                 => "4.2",
    "User-Defined Literals"                                    => "4.2.1",
    "Aggregates"                                               => "4.3",
    "Record Aggregates"                                        => "4.3.1",
    "Extension Aggregates"                                     => "4.3.2",
    "Array Aggregates"                                         => "4.3.3",
    "Delta Aggregates"                                         => "4.3.4",
    "Container Aggregates"                                     => "4.3.5",
    "Expressions"                                              => "4.4",
    "Operators and Expression Evaluation"                      => "4.5",
    "Logical Operators and Short-circuit Control Forms"        => "4.5.1",
    "Relational Operators and Membership Tests"                => "4.5.2",
    "Binary Adding Operators"                                  => "4.5.3",
    "Unary Adding Operators"                                   => "4.5.4",
    "Multiplying Operators"                                    => "4.5.5",
    "Highest Precedence Operators"                             => "4.5.6",
    "Conditional Expressions"                                  => "4.5.7",
    "Quantified Expressions"                                   => "4.5.8",
    "Declare Expressions"                                      => "4.5.9",
    "Reduction Expressions"                                    => "4.5.10",
    "Type Conversions"                                         => "4.6",
    "Qualified Expressions"                                    => "4.7",
    "Allocators"                                               => "4.8",
    "Static Expressions and Static Subtypes"                   => "4.9",
    "Statically Matching Constraints and Subtypes"             => "4.9.1",
    "Image Attributes"                                         => "4.10",
    "Simple and Compound Statements - Sequences of Statements" => "5.1",
    "Assignment Statements"                                    => "5.2",
    "Target Name Symbols"                                      => "5.2.1",
    "If Statements"                                            => "5.3",
    "Case Statements"                                          => "5.4",
    "Loop Statements"                                          => "5.5",
    "User-Defined Iterator Types"                              => "5.5.1",
    "Generalized Loop Iteration"                               => "5.5.2",
    "Procedural Iterators"                                     => "5.5.3",
    "Block Statements"                                         => "5.6",
    "Parallel Block Statements"                                => "5.6.1",
    "Exit Statements"                                          => "5.7",
    "Goto Statements"                                          => "5.8",
    "Subprogram Declarations"                                  => "6.1",
    "Preconditions and Postconditions"                         => "6.1.1",
    "The Global and Global'Class Aspects"                      => "6.1.2",
    "Formal Parameter Modes"                                   => "6.2",
    "Subprogram Bodies"                                        => "6.3",
    "Conformance Rules"                                        => "6.3.1",
    "Inline Expansion of Subprograms"                          => "6.3.2",
    "Subprogram Calls"                                         => "6.4",
    "Parameter Associations"                                   => "6.4.1",
    "Return Statements"                                        => "6.5",
    "Nonreturning Subprograms"                                 => "6.5.1",
    "Overloading of Operators"                                 => "6.6",
    "Null Procedures"                                          => "6.7",
    "Expression Functions"                                     => "6.8",
    "Package Specifications and Declarations"                  => "7.1",
    "Package Bodies"                                           => "7.2",
    "Private Types and Private Extensions"                     => "7.3",
    "Private Operations"                                       => "7.3.1",
    "Type Invariants"                                          => "7.3.2",
    "Default Initial Conditions"                               => "7.3.3",
    "Stable Properties of a Type"                              => "7.3.4",
    "Deferred Constants"                                       => "7.4",
    "Limited Types"                                            => "7.5",
    "Assignment and Finalization"                              => "7.6",
    "Completion and Finalization"                              => "7.6.1",
    "Declarative Region"                                       => "8.1",
    "Scope of Declarations"                                    => "8.2",
    "Visibility"                                               => "8.3",
    "Overriding Indicators"                                    => "8.3.1",
    "Use Clauses"                                              => "8.4",
    "Renaming Declarations"                                    => "8.5",
    "Object Renaming Declarations"                             => "8.5.1",
    "Exception Renaming Declarations"                          => "8.5.2",
    "Package Renaming Declarations"                            => "8.5.3",
    "Subprogram Renaming Declarations"                         => "8.5.4",
    "Generic Renaming Declarations"                            => "8.5.5",
    "The Context of Overload Resolution"                       => "8.6",
    "Task Units and Task Objects"                              => "9.1",
    "Task Execution - Task Activation"                         => "9.2",
    "Task Dependence - Termination of Tasks"                   => "9.3",
    "Protected Units and Protected Objects"                    => "9.4",
    "Intertask Communication"                                  => "9.5",
    "Protected Subprograms and Protected Actions"              => "9.5.1",
    "Entries and Accept Statements"                            => "9.5.2",
    "Entry Calls"                                              => "9.5.3",
    "Requeue Statements"                                       => "9.5.4",
    "Delay Statements, Duration, and Time"                     => "9.6",
    "Formatting, Time Zones, and other operations for Time"    => "9.6.1",
    "Select Statements"                                        => "9.7",
    "Selective Accept"                                         => "9.7.1",
    "Timed Entry Calls"                                        => "9.7.2",
    "Conditional Entry Calls"                                  => "9.7.3",
    "Asynchronous Transfer of Control"                         => "9.7.4",
    "Abort of a Task - Abort of a Sequence of Statements"      => "9.8",
    "Task and Entry Attributes"                                => "9.9",
    "Shared Variables"                                         => "9.10",
    "Conflict Check Policies"                                  => "9.10.1",
    "Example of Tasking and Synchronization"                   => "9.11",
    "Separate Compilation"                                     => "10.1",
    "Compilation Units - Library Units"                        => "10.1.1",
    "Context Clauses - With Clauses"                           => "10.1.2",
    "Subunits of Compilation Units"                            => "10.1.3",
    "The Compilation Process"                                  => "10.1.4",
    "Pragmas and Program Units"                                => "10.1.5",
    "Environment-Level Visibility Rules"                       => "10.1.6",
    "Program Execution"                                        => "10.2",
    "Elaboration Control"                                      => "10.2.1",
    "Exception Declarations"                                   => "11.1",
    "Exception Handlers"                                       => "11.2",
    "Raise Statements and Raise Expressions"                   => "11.3",
    "Exception Handling"                                       => "11.4",
    "The Package Exceptions"                                   => "11.4.1",
    "Pragmas Assert and Assertion_Policy"                      => "11.4.2",
    "Example of Exception Handling"                            => "11.4.3",
    "Suppressing Checks"                                       => "11.5",
    "Exceptions and Optimization"                              => "11.6",
    "Generic Declarations"                                     => "12.1",
    "Generic Bodies"                                           => "12.2",
    "Generic Instantiation"                                    => "12.3",
    "Formal Objects"                                           => "12.4",
    "Formal Types"                                             => "12.5",
    "Formal Private and Derived Types"                         => "12.5.1",
    "Formal Scalar Types"                                      => "12.5.2",
    "Formal Array Types"                                       => "12.5.3",
    "Formal Access Types"                                      => "12.5.4",
    "Formal Interface Types"                                   => "12.5.5",
    "Formal Subprograms"                                       => "12.6",
    "Formal Packages"                                          => "12.7",
    "Example of a Generic Package"                             => "12.8",
    "Operational and Representation Aspects"                   => "13.1",
    "Aspect Specifications"                                    => "13.1.1",
    "Packed Types"                                             => "13.2",
    "Operational and Representation Attributes"                => "13.3",
    "Enumeration Representation Clauses"                       => "13.4",
    "Record Layout"                                            => "13.5",
    "Record Representation Clauses"                            => "13.5.1",
    "Storage Place Attributes"                                 => "13.5.2",
    "Bit Ordering"                                             => "13.5.3",
    "Change of Representation"                                 => "13.6",
    "The Package System"                                       => "13.7",
    "The Package System.Storage_Elements"                      => "13.7.1",
    "The Package System.Address_To_Access_Conversions"         => "13.7.2",
    "Machine Code Insertions"                                  => "13.8",
    "Unchecked Type Conversions"                               => "13.9",
    "Data Validity"                                            => "13.9.1",
    "The Valid Attribute"                                      => "13.9.2",
    "Unchecked Access Value Creation"                          => "13.10",
    "Storage Management"                                       => "13.11",
    "Storage Allocation Attributes"                            => "13.11.1",
    "Unchecked Storage Deallocation"                           => "13.11.2",
    "Default Storage Pools"                                    => "13.11.3",
    "Storage Subpools"                                         => "13.11.4",
    "Subpool Reclamation"                                      => "13.11.5",
    "Storage Subpool Example"                                  => "13.11.6",
    "Pragma Restrictions and Pragma Profile"                   => "13.12",
    "Language-Defined Restrictions and Profiles"               => "13.12.1",
    "Streams"                                                  => "13.13",
    "The Streams Subsystem"                                    => "13.13.1",
    "Stream-Oriented Attributes"                               => "13.13.2",
    "Freezing Rules"                                           => "13.14",
    "Pragma Normalize_Scalars"                                 => "H.1",
    "Documentation of Implementation Decisions"                => "H.2",
    "Reviewable Object Code"                                   => "H.3",
    "Pragma Reviewable"                                        => "H.3.1",
    "Pragma Inspection_Point"                                  => "H.3.2",
    "High Integrity Restrictions"                              => "H.4",
    "Aspect No_Controlled_Parts"                               => "H.4.1",
    "Pragma Detect_Blocking"                                   => "H.5",
    "Pragma Partition_Elaboration_Policy"                      => "H.6",
    "Extensions to Global and Global'Class Aspects"            => "H.7",
    "The Use_Formal and Dispatching Aspects"                   => "H.7.1",
    "Allowed Replacements of Characters"                       => "J.2",
    "Reduced Accuracy Subtypes"                                => "J.3",
    "The Constrained Attribute"                                => "J.4",
    "ASCII"                                                    => "J.5",
    "Numeric_Error"                                            => "J.6",
    "At Clauses"                                               => "J.7",
    "Interrupt Entries"                                        => "J.7.1",
    "Mod Clauses"                                              => "J.8",
    "The Storage_Size Attribute"                               => "J.9",
    "Pragma Inline"                                            => "J.15.1",
    "Pragma No_Return"                                         => "J.15.2",
    "Pragma Pack"                                              => "J.15.3",
    "Pragma Storage_Size"                                      => "J.15.4",
    "Interfacing Pragmas"                                      => "J.15.5",
    "Pragma Unchecked_Union"                                   => "J.15.6",
    "Pragmas Interrupt_Handler and Attach_Handler"             => "J.15.7",
    "Shared Variable Pragmas"                                  => "J.15.8",
    "Pragma CPU"                                               => "J.15.9",
    "Pragma Dispatching_Domain"                                => "J.15.10",
    "Pragmas Priority and Interrupt_Priority"                  => "J.15.11",
    "Pragma Relative_Deadline"                                 => "J.15.12",
    "Pragma Asynchronous"                                      => "J.15.13",
    "Elaboration Control Pragmas"                              => "J.15.14",
    "Distribution Pragmas"                                     => "J.15.15"
  );

my @out;    # Preprocessed lines but with @Chg not yet resolved
my $index;  # Index into @out on printing (only used for debug info)

# In markdown mode, section numbers are linked to sections under this URL:
my $armUrl = "http://www.ada-auth.org/standards/2xrm/html";

foreach my $mss (@ARGV) {
  open(MSS, "<", "$mss") or die "Cannot open file $mss\n";
  @out = ();
  my $line;
  while (($line = <MSS>)) {
    chomp $line;
    $line =~ s/\r//;
    $line =~ s/^\@noprefix//;
  parse:
    if ($line =~ /^\@Labeled(Sub)?Clause\{([^}]+)/i) {
      push @out, $2;  # section
    } elsif ($line =~ /^\@LabeledAdded(Sub)?Clause\{([^}]+)/i) {
      my $section = $2;
      $section =~ s/^Version=.\d., ?Name=.//;
      $section =~ s/.$//;
      push @out, $section;
    } elsif ($line =~ /^\@LabeledRevised(Sub)?Clause\{([^}]+)/i) {
      # Version=[3],New=[Operational and Representation Aspects],Old=[Representation Items]}
      my $section = $2;
      $section =~ s/^Version=.\d., ?New=.//;
      $section =~ s/[}\])>"%].*$//;
      push @out, $section;
    } elsif ($line =~ /^\@(Added)?Syn(\W).*?lhs=(.*)$/) {
      my($synSep, $rest) = ($2, $3, $4);
      $rest =~ s/\s+$//;
      my $synClose = $closing{$synSep};
      $rest = preprocess($rest);
      if ($rest && substr($rest, -1) eq $synClose) {
        push @out, postprocess($rest);
        next;
      }
      while (($line = <MSS>)) {
        chomp $line;
        $line =~ s/\r//;
        $line =~ s/\s+$//;
        $line =~ s/^\@noprefix//;
        if (!$line or $line =~ /^\@(begin|end|comment)\b/i) {
          last;
        }
        if ($line =~ /^\@Syn/) {
          push @out, postprocess($rest);
          goto parse;
        }
        $rest .= "\t" . preprocess($line);
      }
      push @out, postprocess($rest);
    }
  }
  close(MSS);

# Rules with nested quotation marks:
#
#  4.3.3
# <positional_array_aggregate>,rhs="
#     (expression, expression {, expression})
#   | (expression {, expression}, OTHERS => expression)@Chg{New=[
#   | (expression {, expression}, OTHERS => <>)@Chg{New="
# 
#  4.3.3
# <named_array_aggregate>,rhs="
#     @Chg{New="(array_component_association_list
# 
#  4.3.5
# <@Chg{New=<container_element_association>,Old=<>}>,
# rhs="@Chg{New="
# 
#  4.4
# <relation>,rhs="
#      simple_expression [relational_operator simple_expression]
#    | @Chg{New=[tested_simple_expression],Old=[simple_expression]} [NOT] IN @Chg{New=[membership_choice_list@Chg{New=[
#    | raise_expression],Old=[]}],Old="range
# 

  my $section = "";
  for ($index = 0; $index < scalar(@out); ++$index) {
    my $el = $out[$index];
    if ($el =~ /^\w/) {
      if ($Name2Sec{$el}) {
        $section = $Name2Sec{$el};
      } else {
        $section = "No Name2Sec entry for $el";
      }
      next;
    }
    if ($markdown) {
      my $htmlSection = $section;
      $htmlSection =~ s/\./-/g;
      print "[$section](${armUrl}/RM-${htmlSection}.html)\\\n";
    } else {
      print "// $section\n";
    }
    $el = processChg($el);
    $el =~ s/\s+$//;
    if ($el =~ /^\W(\w+)\W,(\s*)rhs=\W(.*?)\W$/i) {
      my ($lhs, $sep, $rhs) = ($1, $2, $3);
      $sep =~ s/\t//g;
      if ($lhs eq "extended_return_statement") {
        # Ugly hack due to nested @Chg in the Scribe, which we cannot handle yet.
        # See comment at sub processChg.
        $rhs =~ s/Old=<defining_identifier : \[CONSTANT//;
      }
      if ($markdown) {
        $rhs =~ s/\b([A-Z][A-Z]+)/\*\*\L$1\E\*\*/g;
        my @lines = split(/\t/, $rhs);
        print "$lhs := ";
        for (my $i = 0; $i < scalar(@lines); ++$i) {
          my $l = $lines[$i];
          if ($l =~ /^( +)/) {
            my $nSpaces = length($1);
            $l =~ s/^ +//;
            print('&nbsp;' x $nSpaces);
          }
          if ($i < scalar(@lines) - 1) {
            print "$l\\\n";
          } else {
            print "$l\n\n";
          }
        }
      } else {
        $rhs =~ s/\t/\n/g;
        # Remove italic rule prefixes. (TODO when adding ANTLR mode, make a synthetic rule)
        $rhs =~ s/\*(\w+)\*/$1/g;
        print "$lhs ::= $sep$rhs\n\n";
      }
    } else {
      # <delta_constraint>,        rhs="DELTA static_simple_expression [range_constraint]"
      warn "LHS/RHS regex did not match at $el\n\n";
    }
  }
}

# There is one rule that this subroutine cannot handle:
#
# @AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=[extended_return_statement],Old=[]}>,
# rhs="@Chg{Version=[2],New=<
#     @key{return} @Chg{Version=[3],New=<@Syn2{extended_return_object_declaration}>,Old=<@Syn2{defining_identifier} : [@Chg{Version=[3],New=<@Key{constant}>,Old=[@Key{aliased}]}] @Syn2{return_subtype_indication} [:= @Syn2{expression}]>} [@Key{do}
#         @Syn2{handled_sequence_of_statements}
#     @key{end} @key{return}];>,Old=[]}"}
sub processChg {
  my $rhs = shift;
  my $orig = $rhs;
  my $loopcnt = 0;
  my $res = "";
  for (my $i = 0; $i < length($rhs); ++$i) {
    my $tail = substr($rhs, $i);
    if (++$loopcnt > 800) {
      die "\n\nprocessChg: endless loop on $orig\n";
    }
    if ($tail =~ /^\@Chg(\W)\t?New=(\W)/) {
      my ($chgSep, $newSep) = ($1, $2);
      my $chgDelim = $closing{$chgSep};
      my $newDelim = $closing{$newSep};
      $chgDelim or die "$index: Expecting delimiter at Chg=$chgSep in $tail\n";
      $newDelim or die "$index: Expecting delimiter at New=$newSep in $tail\n";
      $tail =~ s/^\@Chg\W\t?New=\W//;
      unless ($tail) {
        warn "$index : Cannot handle incomplete \@Chg{New=\n";
        last;
      }
      my $newRegex = escape($newDelim);
      $tail =~ s/([^$newRegex]*?)${newRegex},\s*/$1/;
      if ($tail =~ /Old=(\W)/) {
        my $oldSep = $1;
        my $oldDelim = $closing{$oldSep};
        my $oldRegex = escape($oldDelim);
        my $chgRegex = escape($chgDelim);
        $tail =~ s/Old=\W[^$oldRegex]*?$oldRegex$chgRegex//;
      } else {
        warn "$index : Expecting Old= in $tail\n";
      }
      $rhs = $tail;
      $i = -1;
    } else {
      $res .= substr($tail, 0, 1);
    }
  }
  return $res;
}

sub preprocess {
  my $line = shift;
  $line =~ s/\@Syn[2F][{<(\[](\w+)[\])>}]/$1/gi;
  $line =~ s/\@SynI[{<(\[](\w+)[\])>}]/\*$1\*/gi;
  $line =~ s/\@key\W([\w ]+)\W/\U$1\E/gi;
  $line =~ s/\@\\/ /g;
  $line =~ s/\@;//g;
  $line =~ s/\@\@/\@/g;
  $line =~ s/\@en\b/-/g;
  $line =~ s/\@SingleQuote/'/gi;
  $line =~ s/Version=\W\d\W,//gi;
  # @SynI{@Chg{Version=[3],New=[procedure_or_entry_],Old=[entry_]}}
  # Note: "Version=[n]," has already been zapped
  $line =~ s/\@Syn[2FI]\W\@Chg\WNew=\W(\w+)\W, ?Old=\W\w+\W\W\W/$1/i;
  return $line;
}

sub escape {
  my $char = shift;
  return "\\" . $char;
}

sub postprocess {
  my $str = shift;
  $str =~ s/\@Comment.*$//;
  chop $str;
  return $str;
}

