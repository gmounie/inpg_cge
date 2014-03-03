#!/usr/bin/perl -w

use Modern::Perl;


open my $fh, "<", $ARGV[0];

# skip l'entête
<$fh>;

my %bigraph;

say "graph { graph [splines=true,overlap=false];";

my $indice=0;
foreach my $ligne ( <$fh> ) {
    chomp($ligne);
    
    if ($ligne ne "") {
	foreach my $compet (split(";", $ligne)) {
	    $compet =~ s/^\s+|\s+$//g;

	    say "$indice [shape=box, width=0.1];";
	    say "$indice -- \"$compet\";";
	}
	$indice++;
    }
}

say "}";
