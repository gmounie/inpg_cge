#!/usr/bin/perl6 -w



my $fh = open @*ARGS[0], :r;

# skip l'entête
my $ligne = $fh.get;

my %bigraph;

for $fh.lines -> $ligne {
    chomp($ligne);
    
    if ($ligne ne "") {
	for split(";", $ligne) -> $compet {
	    my Str $val = ~($compet);
	    $val ~~ s /^\s+//;
	    $val ~~ s /\s+$//;
	    
	    if defined %bigraph{"$val"} {
		%bigraph{"$val"} = %bigraph{"$val"} + 1;
	    } else {
		%bigraph{"$val"} = 1;
	    }
	}
    }
}

my $fh2 = open @*ARGS[0], :r;
my $ligne2 = $fh2.get;
say $ligne2;

for $fh2.lines -> $ligne {
    chomp($ligne);
    
    if ($ligne ne "") {
	my $nouvligne;
	my @motsclefs;
	for split(";", $ligne) -> $compet {
	    my Str $val = ~($compet);
	    $val ~~ s /^\s+//;
	    $val ~~ s /\s+$//;
	    
	    if %bigraph{"$val"} > 31 {
		@motsclefs.push($val)
	    } 
	}
	say @motsclefs.join(";");
    }
}

# say %bigraph.kv;

# for %bigraph.kv -> $mykey,$myval {
#     if $myval > 31 { 
# 	say "clef $mykey " ~ $myval  ~ ";";
#     }
# }
