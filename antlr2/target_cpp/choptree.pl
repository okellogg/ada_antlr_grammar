#!/usr/bin/perl
#
# choptree - pretty print tree string produced by ANTLR
#

my $ilevel = 0;

sub space {
   return "  " x $ilevel;
}

@ARGV or die "choptree : provide the ANTLR tree string or the name of a file containing it\n";

my @input;

if (scalar(@ARGV) == 1) {
   if ( $ARGV[0] =~ / \(.*\) / ) {
      @input = split( / /, $ARGV[0] );
   } else {
      my $filename = $ARGV[0];
      -e "$filename" or die "choptree : argument is neither a tree string nor a file name\n";
      open(IN, "<$filename") or die "choptree : cannot open file $filename\n";
      my $line = <IN>;
      chomp $line;
      @input = split( / /, $line );
   }
} else {
   @input = @ARGV;
}

for (my $i = 0; $i < scalar(@input); ++$i) {
   my $tok = $input[$i];
   if ($tok eq '(') {
      print(space . $tok);
      while ($i + 1 < scalar(@input)) {
         $input[$i + 1] =~ /^\w/ or last;
         $tok = $input[++$i];
         print " $tok";
      }
      print "\n";
      ++$ilevel;
   } elsif ($tok eq ')') {
      --$ilevel;
      print(space . $tok . "\n");
   } else {
      print(space . "$tok");
      while ($i + 1 < scalar(@input)) {
         $input[$i + 1] =~ /^\w/ or last;
         $tok = $input[++$i];
         print " $tok";
      }
      print "\n";
   }
}


