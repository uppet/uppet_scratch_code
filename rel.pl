#!/usr/bin/perl -w

use strict;
use warnings;

my %rel;
my %rel_back;

sub say{
    print @_;
    print "\n";
};

sub init_mind {
    say '<map version="0.9.0" encoding="utf-8">';
}

sub fini_mind {
    say '</map>';
}

sub open_class {
    my $center_class = shift;
    say "<node text=\"$center_class\">";
}

sub open_left_class {
    my $base = shift;
    say "<node text=\"$base\" style=\"bubble\" position=\"left\">";
}

sub open_right_class {
    my $base = shift;
    say "<node text=\"$base\" style=\"bubble\" position=\"right\">";
}


sub close_class() {
    say "</node>";
}

sub add_base_classes {
    my $center_class = shift;
    return unless exists $rel{$center_class};
    my @base_list = split /,/,$rel{$center_class};
    for my $base (@base_list) {
        &open_left_class($base);
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
        &add_child_classes($child);
        &close_class;
    }
}


sub main {

    my $FILE;
    open $FILE, "<", $ARGV[0];
    my $line;
    my @tail_bases;
    while ($line=<$FILE>) {
        chop $line;
        my ($cls, $base)  = split /\s+--D\s+/, $line;
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
        &add_child_classes($center_class);
        &close_class;
        &fini_mind;
    }
}

&main;
