#!/usr/bin/perl
#
# choptree - pretty print tree string produced by ANTLR
#

my $ilevel = 0;

sub space {
   return "  " x $ilevel;
}

my @input;

sub choptree {
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
}

@ARGV or die "choptree : provide the ANTLR tree string or the name of a file containing it\n";

if (scalar(@ARGV) == 1) {
   if ( $ARGV[0] =~ / \(.*\) / ) {
      @input = split( / /, $ARGV[0] );
      choptree;
   } else {
      my $filename = $ARGV[0];
      -e "$filename" or die "choptree : argument is neither a tree string nor a file name\n";
      open(IN, "<$filename") or die "choptree : cannot open file $filename\n";
      while (<IN>) {
         my $line = <IN>;
         chomp $line;
	 if ($line =~ /^ *\(/) {
            @input = split( / /, $line );
	    choptree;
	 } else {
	    print "$line\n";
	 }
      }
      close IN;
   }
} else {
   @input = @ARGV;
   choptree;
}


