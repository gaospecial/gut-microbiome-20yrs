#!/usr/bin/perl
use Data::Dumper;

my $ut = shift or die;
my $wos = shift or die;
my $out = shift or die;

open FILE, $ut or die;

chomp( my @ut = <FILE> );

print scalar(@ut), " record in total.\n";

my %out;

map {$out{$_}++} @ut;

print scalar(keys(%out)), " unique UT.\n";
# print Dumper \@legal_doi;
# print Dumper \%out;

open FILE, $wos or die;
$/ = "\n\n";

my $n;
my $N;

open OUT, "> $out" or die;

while (<FILE>){
	$N++;
	# print "Processed ", $N, " records.\n" if $N % 1000 == 0;
	my $ut = lc($1) if ($_ =~ /\nUT (.*?)\n/);
	if ($out{$ut}){
		print OUT $_, "\n";
		$n++;
		# print "Found ", $n, " core articles.\n" if $n % 100 == 0;
	}
}

print "Found $n core articles in $N records.\n";
print "Done!\n";