#!/usr/bin/env AFNI_Batch_R

##!/usr/bin/env afni_run_R

# Command line to run this script: RBA.R dataStr.txt diary.txt &
# (Output is a file in which the running progress including 
# error messages will be stored)

first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
   #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))
ExecName <- 'RBA'

# Global variables

#################################################################################
##################### Begin RBA Input functions ################################
#################################################################################

#The help function for RBA batch (AFNI-style script mode)
help.RBA.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
                      Welcome to RBA ~1~
    Region-Based Analysis Program through Bayesian Multilevel Modeling 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 1.0.0, Feb 17, 2020
Author: Gang Chen (gangchen@mail.nih.gov)
Website - https://afni.nimh.nih.gov/gangchen_homepage
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Usage: ~1~
------ 
 RBA performs region-based analysis (RBA) as theoretically elaborated in the
 manuscript: https://rdcu.be/bhhJp and is conducted with a shell script (as
 shown in the examples below). The input data should be formulated in a
 pure-text table that codes the regions and variables. The response variable
 is some effect at the individual subject level.

 Thanks to Paul-Christian Bürkner and the Stan/R communities for the strong support.

 Citation: ~1~
 If you want to cite the approach for RBA, consider the following:~2~

 Chen G, Xiao Y, Taylor PA, Riggins T, Geng F, Redcay E, 2019. Handling Multiplicity
 in Neuroimaging through Bayesian Lenses with Multilevel Modeling. Neuroinformatics.
 https://rdcu.be/bhhJp

=============================== 
 Read the following carefully!!!
 ===============================
 A data table in pure text format is needed as input for an RBA script. The
 data table should contain at least 3 columns that specify the information
 about subjects, regions and the response variable values with the following
 fixed header. The header lables are case-sensitive, and their order does not
 matter.

 Subj   ROI        Y      Age
 S1     Amyg    0.2643    11
 S2     BNST    0.3762    16
 ...

 0) You are performing Bayesian analysis!!! So, you will directly obtain
    the probability of an effect being positive or negative with your data,
    instead of witch hunt-hunting the straw man of p-value (weirdness of your
    data when pretending that absolutely nothing exists).

 1) Avoid using pure numbers to code the labels for categorical variables. The
    column order does not matter. You can specify those column names as you
    prefer, but it saves a little bit scripting if you adopt the default naming
    for subjects (\'Subj\'), regions (\'ROI\') and response variable (\'Y\').

 2) Add more columns if explanatory variables are considered in the model. Currently
    only between-subjects variables (e.g., sex, patients vs. controls, age) are
    allowed. Capability of modeling within-subject or repeated-measures variables
    may be added in the future. Each label in a between-subjects factor (categorical
    variable) should be coded with at least 1 character (labeling with pure numbers
    is fine but not recommended). If preferred, you can quantitatively code the
    levels of a factor yourself by creating k-1 columns for a factor with k levels.
    However, be careful with your coding strategy because it would impact how to
    interpret the results. Here is a good reference about factor coding strategies:
    https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/

 3) It is strongly suggested that a quantitative explanatory variable be
    standardized with option -stdz; that is, remove the mean and scale by
    the standard deviation. This will improve the chance of convergence
    with each Markov chain. If a between-subjects factor (e.g., sex) is
    involved, it may be better to standardize a quantitative variable
    within each group in terms of interpretability if the mean value differs
    substantially. However, do not standardize a between-subjects factor if
    you quantitatively code it. And do not standardize the response variable
    if the intercept is of interest!

 4) For within-subject variables, try to formulate the data as a contrast
    between two factor levels or as a linear combination of multiple levels.

 5) The results from RBA are effect estimates for each region. They can be
    slightly different across different runs or different computers and R
    package versions due to the nature of randomness involved in Monte Carlo
    simulations.

 6) The evidence for region effects in the output can be assessed through P+,
    the probability that the effect is postive conditional on the current
    dataset. The following criterion for highlighting the results is only
    suggestive:
     P+ >= 0.975 or <= 0.025 - very strong evidence
     P+ >= 0.95 or <= 0.05   - strong evidence
     P+ >= 0.90 or <= 0.10   - moderate evidence
     else                    - little evidence

    The same criterion is applied to the color coding in the posterior
    distribution plots generated with the option -PDP:
     P+ >= 0.975 or <= 0.025 - green:    very strong evidence
     P+ >= 0.95 or <= 0.05   - yellow:   strong evidence
     P+ >= 0.90 or <= 0.10   - gray:     moderate evidence
     else                    - no color: little evidence

 7) WARNING: If the results are pretty homogenized across regions, it is an
    indication that presumably partial pooling becomes full pooling. Most
    likely the cross-region variability is so negligible that the model
    renders the overall average as individual effects for all regions. When
    this occurs, you may need much more data for the model to differentiate
    the subtle effects.

 =========================
 
 Installation requirements: ~1~
 In addition to R installation, the R package "brms" is required for RBA. Make
 sure you have the most recent version of R. To install "brms", run the following
 command at the terminal:

 rPkgsInstall -pkgs "brms" -site http://cran.us.r-project.org"

 Alternatively you may install them in R:

 install.packages("brms")
 
 Running: ~1~
 Once the RBA command script is constructed, it can be run by copying and
 pasting to the terminal. Alternatively (and probably better) you save the 
 script as a text file, for example, called myRBA.txt, and execute it with the 
 following  (assuming on tcsh shell),
 
 nohup tcsh -x myRBA.txt > diary.txt &
 nohup tcsh -x myRBA.txt |& tee diary.txt &

 The advantage of the commands above is that the progression is saved into
 the text file diary.txt and, if anything goes awry, can be examined later.
 The \'nohup\' command allows the analysis running in the background even if
 the terminal is killed.'
 
   ex1 <- 
"\n--------------------------------
Examples: ~1~

Example 1 --- Simplest scenario. Values from regions are the input from
          each subject. No explanatory variables are considered. Research
	  interest is about the population effect at each region.

   RBA -prefix output -dataTable myData.txt  \\

   The above script is equivalent to

   RBA -prefix myResult -chains 4 -iterations 1000 -model 1 -EOI 'Intercept' \\
   -r2z -dataTable myData.txt  \\

   The 2nd version above is recommended because of its explicit specifications.

   The input file 'myData.txt' is a data table in pure text format as below: 
                                                             
     Subj  ROI          Y
     S01   lFFA       0.162
     S02   lAmygdala -0.598
     S03   DMNLAG     0.249
     S04   DMNPCC     0.568
     ...
 \n"         
     
   ex2 <-
"--------------------------------
Example 2 --- 2 between-subjects factors (sex and group): ~2~

   RBA -prefix output -Subj subject -ROI region -Y zscore -PDP 4 4 \\
   -chains 4 -iterations 1000 -model '1+sex+group' \\
   -cVars 'sex,group' -EOI 'Intercept,sex,group' \\
   -dataTable myData.txt

   The input file 'myData.txt' is formatted as below:
   
   subject region  zscore  sex group
   S1      DMNLAG  0.274    F  patient
   S1      DMNLHC  0.443    F  patient
   S2      DMNRAG  0.455    M  contorl
   S2      DMNRHC  0.265    M  control
   ...

   Notice that the interaction between 'sex' and 'group' is not modeled in
   this case. The option -PDP 4 4 allows the program to generate a 4 x 4
   posterior distribution plots for the 16 regions.
\n"
     
   ex3 <-
"---------------------------------
Example 3 --- one between-subjects factor (sex), one within-subject factor (two
   conditions), one between-subjects covariate (age), and one quantitative
   variable: ~2~
    
   RBA -prefix result -PDP 4 4 -Subj Subj -ROI region -Y value \\
   -chains 4 -iterations 1000 -model '1+sex+age+SA' -qVars 'sex,age,SA' \\
   -EOI 'Intercept,sex,age,SA' -dataTable myData.txt

   The input file 'myData.txt' is formatted as below:
   
   Subj   region   value sex  age   SA
   S1    DMNLAG    0.274  1  1.73  1.73
   S1    DMNLHC    0.443  1  1.73  1.73
   S2    DMNRAG    0.455 -1 -0.52  0.52
   S2    DMNRHC    0.265 -1 -0.52  0.52
   ...

   Notice

   1) the 'Y' column is the contrast between the two conditions.
   2) since we want to model the interaction between 'sex' and 'age', 'sex' is
      coded through deviation coding.
   3) 'age' has already been standardized within each sex due to large age
      difference between the two sexes.
   4) the 'SA' column codes for the interaction between 'sex' and 'age', which
      is the product of the two respective columns.
   
   Options: ~1~
   \n"   
   
   parnames <- names(params)
   ss <- vector('character')
   if(alpha) {
       parnames <- sort(parnames)   
       ss <- paste('Options in alphabetical order:\n',
                  '------------------------------\n', sep='')
   } else ss <- paste('Options:\n', '--------\n', sep='')
   for(ii in 1:length(parnames)) {
      op <- params[parnames[ii]][[1]]
      if(!is.null(op$help)) ss <- c(ss , paste(itspace, op$help, sep='')) else
         ss <- c(ss, paste(itspace, parnames[ii], '(no help available)\n', sep='')) 
   }
   ss <- paste(ss, sep='\n')
   cat(intro, ex1, ex2, ex3, ss, sep='\n')
   
   if (adieu) exit.AFNI();
}
   
# options list 
read.RBA.opts.batch <- function (args=NULL, verb = 0) {
   params <- list (
      '-prefix' = apl(n = 1, d = NA,  h = paste(
   "-prefix PREFIX: Prefix is used to specify output file names. The main output is",
   "        a text with prefix appended with .txt and stores inference information ",
   "        for effects of interest in a tabulated format depending on selected ",
   "        options. The prefix will also be used for other output files such as ",
   "        visualization plots, and saved R data in binary format. The .RData can",
   "        be used for post hoc processing such as customized processing and plotting.",
   "        Remove the .RData file to save disk space once you deem such a file is no",
   "        longer useful.\n", sep = '\n'
                     ) ),

      '-chains' = apl(n = 1, d = 1, h = paste(
   "-chains N: Specify the number of Markov chains. Make sure there are enough",
   "         processors available on the computer. Most of the time 4 cores are good",
   "         enough. However, a larger number of chains (e.g., 8, 12) may help achieve",
   "         higher accuracy for posterior distribution. Choose 1 for a single-processor",
   "         computer, which is only practical only for simple models.\n", sep = '\n'
                     ) ),

      '-iterations' = apl(n = 1, d = 1, h = paste(
   "-iterations N: Specify the number of iterations per Markov chain. Choose 1000 (default)",
   "         for simple models (e.g., one or no explanatory variables). If convergence",
   "         problem occurs as indicated by Rhat being great than 1.1, increase the number of",
   "         iterations (e.g., 2000) for complex models, which will lengthen the runtime.",
   "         Unfortunately there is no way to predict the optimum iterations ahead of time.\n", sep = '\n'
                     ) ),

      '-verb' = apl(n = 1, d = 1, h = paste(
   "-verb VERB: Speicify verbose level.\n", sep = '\n'
                     ) ),

      '-model' = apl(n = 1, d = 1, h = paste(
   "-model FORMULA: This option specifies the effects associated with explanatory",
   "         variables. By default (without user input) the model is specified as",
   "         1 (Intercept). Currently only between-subjects factors (e.g., sex, ",
   "         patients vs. controls) and quantitative variables (e.g., age) are",
   "         allowed. When no between-subject factors are present, simply put 1",
   "         (default) for FORMULA. The expression FORMULA with more than one",
   "         variable has to be surrounded within (single or double) quotes (e.g.,",
   "         '1+sex', '1+sex+age'. Variable names in the formula should be consistent",
   "         with the ones used in the header of data table. A+B represents the",
   "         additive effects of A and B, A:B is the interaction between A",
   "         and B, and A*B = A+B+A:B. Subject as a variable should not occur in",
   "         the model specification here.\n", sep = '\n'
             ) ),

      '-dbgArgs' = apl(n=0, h = paste(
   "-dbgArgs: This option will enable R to save the parameters in a file called",
   "         .RBA.dbg.AFNI.args in the current directory so that debugging can be",
   "         performed.\n", sep='\n')),

      '-MD' = apl(n=0, h = paste(
   "-MD: This option indicates that there are missing data in the input. With n",
   "         regions, at least n(n-1)/2 values are assumed from each subject in the",
   "         input with no missing data (default). When missing data are present,",
   "         invoke this option so that the program will handle it properly.\n", sep='\n')),

      '-r2z' = apl(n=0, h = paste(
   "-r2z: This option performs Fisher transformation on the response variable",
   "         (column Y) if it is correlation coefficient.\n", sep='\n')),

      '-cVars' = apl(n=c(1,100), d=NA, h = paste(
   "-cVars variable_list: Identify categorical (qualitive) variables (or",
   "         factors) with this option. The list with more than one variable",
   "         has to be separated with comma (,) without any other characters such",
   "         as spaces and should be surrounded within (single or double) quotes.",
   "         For example, -cVars \"sex,site\"\n",
             sep = '\n'
             ) ),

      '-qVars' = apl(n=c(1,100), d=NA, h = paste(
   "-qVars variable_list: Identify quantitative variables (or covariates) with",
   "         this option. The list with more than one variable has to be",
   "         separated with comma (,) without any other characters such as",
   "         spaces and should be surrounded within (single or double) quotes.",
   "         For example, -qVars \"Age,IQ\"\n",
             sep = '\n'
             ) ),

      '-stdz' = apl(n=c(1,100), d=NA, h = paste(
   "-stdz variable_list: Identify quantitative variables (or covariates) to be",
   "         standardized. To obtain meaningful and interpretable results and to",
   "         achieve better convergence of Markov chains with reasonable iterations,",
   "         it is recommended that all quantitative variables be standardized",
   "         except for the response variable and indicator variables that code for",
   "         factors. For example, -stdz \"Age,IQ\". If the mean of a quantitative",
   "         variables varies substantially between groups, it may make sense to",
   "         standardize the variable within each group before plugging the values",
   "         into the data table. Currently RBA does not offer the option to perform",
   "         within-group standardization.\n",
             sep = '\n'
             ) ),

         '-scale' = apl(n = 1, d = 1, h = paste(
   "-scale d: Specify a multiplier for the Y values. When the values for response",
   "         are too small or large, it may create a convergence problem for MCMC. To",
   "         avoid the problem, set a scaling factor so that the range of value is",
   "         around 1-10. The results will be adjusted back to the orignal scale.\n", sep = '\n'
                     ) ),
   
      '-EOI' = apl(n=c(1,100), d=NA, h = paste(
   "-EOI variable_list: Identify effects of interest in the output by specifying the",
   "         variable names separated with comma (,). For example, -EOI \"sex,age\".",
   "         By default the Intercept is considered to be an effect of interest.",
   "         Currently only variables, not their interactions, can be directly",
   "         requested for output. However, most interaction effects can be obtained by",
   "         either properly coding the variables (see example 3) or post processing.\n",
             sep = '\n'
             ) ),

      '-qContr' = apl(n=c(1,100), d=NA, h = paste(
   "-qContr contrast_list: Identify comparisons of interest between quantitative",
   "         variables in the output separated with comma (,). It only allows for",
   "         pair-wise comparisons between two quantitative variables. For example,",
   "         -qContr \"age vs IQ, age vs weight, IQ vs weight\", where V1, V2, and V3 are three",
   "         quantitative variables and three comparisons, V1 - V2, V1 - V3 and V2 - V3",
   "         will be provided in the output. Make sure that such comparisons are",
   "         meaningful (e.g., with the same scale and unit. This can be used to",
   "         formulate comparisons among factor levels if the user quantitatively",
   "         codes the factor levels.\n",
             sep = '\n'
             ) ),

      '-Y' = apl(n = 1, d = NA,  h = paste(
   "-Y var_name: var_name is used to specify the column name that is designated as",
   "        as the response/outcome variable. The default (when this option is not",
   "        invoked) is 'Y'.\n", sep = '\n'
                     ) ),

      '-distr' = apl(n = 1, d = NA,  h = paste(
   "-distr distr_name: Use this option to specify the distribution for the response",
   "        variable. The default is Gaussian when this option is not invoked.",
   "        When skewness or outliers occur in the data, consider adopting the Student's",
   "        t-distribution by using this option with 'student'.\n", sep = '\n'
                     ) ),

      '-Subj' = apl(n = 1, d = NA,  h = paste(
   "-Subj var_name: var_name is used to specify the column name that is designated as",
   "        as the measuring unit variable (usually subject). The default (when this",
   "        option is not invoked) is 'Subj'.\n", sep = '\n'
                     ) ),

      '-ROI' = apl(n = 1, d = NA,  h = paste(
   "-ROI var_name: var_name is used to specify the column name that is designated as",
   "        as the region variable. The default (when this option is not invoked) is",
   "        'ROI'.\n", sep = '\n'
                     ) ),

      '-PDP' = apl(n = 2, d = NA, h = paste(
   "-PDP nr nc: Specify the layout of posterior distribution plot (PDP) with nr rows",
   "         and nc columns among the number of plots. For example, with 16 regions,",
   "         you can set nr = 4 and nc = 4. The region names will be shown in each plot.",
   "         So label the regions concisely.\n", sep = '\n'
                     ) ),

     '-dataTable' = apl(n=c(1, 1000000), d=NA, h = paste(
   "-dataTable TABLE: List the data structure in a table of long format (cf. wide",
   "         format) in R with a header as the first line. \n",
   "         NOTE:\n",
   "         1) There should have at least three columns in the table. These minimum",
   "         three columns can be in any order but with fixed and reserved with labels:",
   "         'Subj', 'ROI', and 'Y'. The column 'ROI' is meant to code the regions",
   "         that are associated with each value under the column Y. More columns can",
   "         be added in the table for explanatory variables (e.g., groups, age, site)",
   "         if applicable. Only subject-level (or between-subjects) explanatory variables",
   "         are allowed at the moment. The labels for the columns of 'Subj' and 'ROI'",
   "         can be any identifiable characters including numbers.",
   "         2) Each row is associated with one and only one 'Y' value, which is the",
   "         response variable in the table of long format (cf. wide format) as",
   "         defined in R. With n subjects and m regions, there should have totally mn",
   "         rows, assuming no missing data. ",
   "         3) It is fine to have variables (or columns) in the table that are not used",
   "         in the current analysis.",
   "         4) The context of the table can be saved as a separate file, e.g., called",
   "         table.txt. In the script specify the data with '-dataTable table.txt'.",
   "         This option is useful when: (a) there are many rows in the table so that",
   "         the program complains with an 'Arg list too long' error; (b) you want to",
   "         try different models with the same dataset.\n",
             sep = '\n'
                     ) ),

      '-help' = apl(n=0, h = '-help: this help message\n'),
      '-show_allowed_options' = apl(n=0, h=
   "-show_allowed_options: list of allowed options\n" )
            )
                     
   ops <- parse.AFNI.args(args, params, other_ok=FALSE)
   if (verb) show.AFNI.args(ops, verb=0, hstr='')
   if (is.null(ops)) 
      errex.AFNI('Error parsing arguments. See RBA -help for details.')

   #Parse dems options
   #initialize with defaults
      lop <- AFNI.new.options.list(history = '', parsed_args = ops)
      lop$chains <- 1
      lop$iterations <- 1000
      lop$model  <- 1
      lop$cVars  <- NULL
      lop$qVars  <- 'Intercept'
      lop$stdz   <- NA
      lop$EOI    <- 'Intercept'
      lop$qContr <- NA
      lop$Y      <- 'Y'
      lop$distr  <= 'gaussian'
      lop$Subj   <- 'Subj'
      lop$ROI    <- 'ROI'
      lop$PDP    <- NA

      lop$dbgArgs <- FALSE # for debugging purpose
      lop$MD      <- FALSE # for missing data 
      lop$student <- FALSE
      lop$r2z     <- FALSE # Fisher transformation
      lop$verb    <- 0
      lop$scale   <- 1
   #Get user's input
   for (i in 1:length(ops)) {
      opname <- strsplit(names(ops)[i],'^-')[[1]];
      opname <- opname[length(opname)];
      switch(opname,
             prefix = lop$outFN  <- pprefix.AFNI.name(ops[[i]]),
             chains   = lop$chains <- ops[[i]],
             iterations = lop$iterations <- ops[[i]],
             verb   = lop$verb   <- ops[[i]],
             model  = lop$model  <- ops[[i]],
             cVars  = lop$cVars  <- ops[[i]],
             qVars  = lop$qVars  <- ops[[i]],
             stdz   = lop$stdz   <- ops[[i]],
             EOI    = lop$EOI    <- ops[[i]],
             qContr = lop$qContr <- ops[[i]],
             Y      = lop$Y      <- ops[[i]],
             distr  = lop$distr  <- ops[[i]],
             Subj   = lop$Subj   <- ops[[i]],
             ROI    = lop$ROI    <- ops[[i]],
             PDP    = lop$PDP    <- ops[[i]],
             help    = help.RBA.opts(params, adieu=TRUE),
             dbgArgs = lop$dbgArgs <- TRUE,
             MD      = lop$MD      <- TRUE,
             
             scale   = lop$scale   <- ops[[i]],
             r2z     = lop$r2z     <- TRUE,
             dataTable  = lop$dataTable <- read.table(ops[[i]], header=T),
             )
   }

   return(lop)
}# end of read.RBA.opts.batch


                               
