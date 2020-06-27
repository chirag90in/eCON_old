#!/bin/bash

#Motion derivative
1d_tool.py -overwrite -infile MAX111_Main_motion.1D -set_nruns 6 -derivative -write MAX111_Main_motion_deriv.1D


set fileRawMotion = MAX111_Main_motion.1D
set fileDerMotion = MAX111_Main_motion_deriv.1D
set nruns = 6
set nvolume = 336
## Building run onset list
    set concatList = ""
    set i = 0
    set idx = 0
    while ( $i < $nruns )
        @ idx = $i * $nvolume
        set concatList = "$concatList"" ""$idx"
        @ i += 1
    end
    echo $concatList
        rm -f runConcatInfo.1D

    echo $concatList > runConcatInfo.1D
    # Pipe the concat list to an actual 1D file, load it in 		
    
    ## Doing first stage regression (censor shock, motion, drifts)
    ##
    echo " "
    echo "**** Doing regression NUMBER 1 (of 1) ****"	

    3dDeconvolve -overwrite -force_TR 1.25 \
    -input MAX111_L_Hab_timeseries.1D \
    -polort A \
    -local_times \
    -concat runConcatInfo.1D \
    #-censor "$fileShock16s" \
    -num_stimts 13 \
    -tout \
    -stim_times 1 FNS.txt 'BLOCK(16.25,1)' -stim_label 1 FNS \
    -stim_times 2 FNT.txt 'BLOCK(16.25,1)' -stim_label 2 FNT \
    -stim_times 3 FPS.txt 'BLOCK(16.25,1)' -stim_label 3 FPS \
    -stim_times 4 FPT.txt 'BLOCK(16.25,1)' -stim_label 4 FPT \
    -stim_times 5 RNS.txt 'BLOCK(16.25,1)' -stim_label 5 RNS \
    -stim_times 6 RNT.txt 'BLOCK(16.25,1)' -stim_label 6 RNT \
    -stim_times 7 RPS.txt 'BLOCK(16.25,1)' -stim_label 7 RPS \
    -stim_times 8 RPT.txt 'BLOCK(16.25,1)' -stim_label 8 RPT \
    -stim_times 9 rate_FNS.txt 'BLOCK(2,1)' -stim_label 9 r_FNS \
    -stim_times 10 rate_FNT.txt 'BLOCK(2,1)' -stim_label 10 r_FNT \
    -stim_times 11 rate_FPS.txt 'BLOCK(2,1)' -stim_label 11 r_FPS \
    -stim_times 12 rate_FPT.txt 'BLOCK(2,1)' -stim_label 12 r_FPT \
    -stim_times 13 rate_RNS.txt 'BLOCK(2,1)' -stim_label 13 r_RNS \
    -stim_times 14 rate_RNT.txt 'BLOCK(2,1)' -stim_label 14 r_RNT \
    -stim_times 15 rate_RPS.txt 'BLOCK(2,1)' -stim_label 15 r_RPS \
    -stim_times 16 rate_RPT.txt 'BLOCK(2,1)' -stim_label 16 r_RPT \
    -ortvec "$fileRawMotion"'[1..6]' 'MotionParam' \
    -ortvec "$fileDerMotion"'[0..5]' 'MotionParamDerv' \
    -num_glt 6 \
    -gltsym "SYM: FPT FNT -FPS -FNS" -glt_label 1 FPT_FNT_vs_FPS_FNS \
    -gltsym "SYM: FPT -FNT FPS -FNS" -glt_label 2 FPS_FPT_vs_FNS_FNT \
    -gltsym "SYM: FPT -FNT -FPS FNS" -glt_label 3 Interaction \
    -gltsym "SYM: FPT -FPS" -glt_label 4 FPT_vs_FPS \
    -gltsym "SYM: FNT -FNS" -glt_label 5 FNT_vs_FNS \
    -gltsym "SYM: RPT RNT -RPS -RNS" -glt_label 6 RPT_RNT_vs_RPS_RNS \
    -x1D "$out"/subj"$subj".xmat.1D \
    -x1D_uncensored "$out"/MAX"$subj"_uncens.xmat.1D \
    -x1D_stop