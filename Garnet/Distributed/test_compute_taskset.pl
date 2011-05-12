my $nnodes=13;
for my $ncores (1,2,4,8,16) {


for my $node_id(0..$nnodes) {
print $node_id,' ',compute_taskset($node_id,$nnodes,$ncores),"\n";
}
}

sub compute_taskset {
    (my $node_id, my $nnodes, my $ncores)=@_;
    if ($ncores>$nnodes) {
        my $mask=sprintf('%x',2**$node_id); 
        return "taskset 0x$mask "; 
    } elsif ($ncores==1) {
        if ($node_id==0) {
            return 'taskset 0x1 ';
        } else {
            return 'taskset 0x2 ';
        }
    } else {
            my $nodes_per_core=int(($nnodes+1)/$ncores); # e.g. if nnodes+1 is 13 and ncores is 4, we have 13/4 = 4, 
            if ($nodes_per_core<$nnodes/$ncores) {
                $nodes_per_core++;
            }
            my $taskset='';
            for my $i (0..$ncores-1) {
                if ($node_id-$nodes_per_core*$i<$nodes_per_core and $node_id-$nodes_per_core*$i>=0) {
                    $taskset= 'taskset 0x'.(2**$i).' '; # so 0,1,2,3 go on core 1;     
                    last;                    
                } else {
                    $taskset= 'taskset 0x'.(2**$ncores).' ';
                }
            }
            return $taskset;    
    }
}

