#!/usr/bin/env perl6

my ($act,$file,$name) = @*ARGS[0..2];

my regex cp { <[0..9 A..Z]>+ };

sub for-set ($line) {
    given $line {
	when /^(<cp>) \s* \; \s* $name/ {
	    take "['\\x%s']".sprintf($/[0]);
        }

	when /(<cp>) '..' (<cp>) \s* \; \s* $name/ {
	    take "['\\x%s'..'\\x%s']".sprintf($/[0], $/[1]);
	}
    }
}

sub for-break-test ($line) { }



my %actions = set => &for-set,
              break-test => &for-break-test;

my @result = gather for open($file).slurp-rest.lines -> $line {
    %actions{$act}($line);
}

my $out = open("$name.hs", :w);

$out.say("module $name");
$out.say("x$name = [");
$out.say(@result.map({"\t$_"}).join(",\n"));
$out.say("  ]");

#close($out);

say "done {@result.elems}";
