#!/usr/bin/perl -w

use strict;
use warnings;

my %rel;
my %rel_back;

my $to_org = 0;
my $org_level = 1;
sub star{
	my $num = shift;
	return '*'x$num;
}

sub say{
    print @_;
    print "\n";
};

sub init_mind {
    say '<map version="0.9.0" encoding="utf-8">' unless $to_org ;
	say '#+STARTUP: hidestars' if  $to_org ;
}

sub fini_mind {
    say '</map>' unless $to_org;
}

sub open_class {
    my $center_class = shift;
    say "<node text=\"$center_class\">" unless $to_org;
	say star($org_level++) . " " . $center_class if $to_org;
}

sub open_left_class {
    my $base = shift;
    say "<node text=\"$base\" style=\"bubble\" position=\"left\">" unless $to_org;
	say star($org_level) . " " . $base if $to_org;
}

sub open_right_class {
    my $base = shift;
    say "<node text=\"$base\" style=\"bubble\" position=\"right\">" unless $to_org;
	say star($org_level) . " " . $base if $to_org;
}


sub close_class() {
    say "</node>" unless $to_org;
	$org_level-- if $to_org;
}

sub add_base_classes {
    my $center_class = shift;
    return unless exists $rel{$center_class};
    my @base_list = split /,/,$rel{$center_class};
    for my $base (@base_list) {
        &open_left_class($base);
		$org_level++ if $to_org;
        &add_base_classes($base);
        &close_class;
    }
}

sub add_child_classes {
    my $center_class = shift;
    return unless exists $rel_back{$center_class};
    my @child_list = split /,/,$rel_back{$center_class};
    for my $child (@child_list) {
        &open_right_class($child);
		$org_level++ if $to_org;
        &add_child_classes($child);
        &close_class;
    }
}


sub main {

	#to org mode support
	$to_org = 1 if ($ARGV[2] && $ARGV[2] eq "--org");

    my $FILE;
    open $FILE, "<", $ARGV[0];
    my $line;
    my @tail_bases;
    while ($line=<$FILE>) {
        chop $line;
        my ($cls, $base)  = split /\s+--D\s+/, $line;
		next unless $cls && $base;
        $cls =~ s/\s//g;
        $base =~ s/\s//g;
        if (exists $rel{$cls}) {
            $rel{$cls} = $rel{$cls} . "," . $base;
        } else {
            $rel{$cls} = $base;
        }
        push @tail_bases, $base;
    }
    for my $base (@tail_bases) {
        $rel{$base} = "" unless exists $rel{$base};
    }

    for my $key (keys %rel) {
        my $val = $rel{$key};
        my @base_list = split /,/,$val;
        for my $base (@base_list) {
            if (exists $rel_back{$base}) {
                $rel_back{$base} = $rel_back{$base} . "," . $key;
            } else {
                $rel_back{$base} = $key;
            }
        }
    }

    if ($ARGV[1] && exists $rel{$ARGV[1]}) {
        my $center_class = $ARGV[1];
        &init_mind;
        &open_class($center_class);
        &add_base_classes($center_class);


		&close_class if $to_org;
		$org_level = 1 if $to_org;
		&open_class($center_class) if $to_org;

        &add_child_classes($center_class);
        &close_class;
        &fini_mind;
    }
}

&main;
