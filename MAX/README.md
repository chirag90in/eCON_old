# Mood-Anxiety (MAX)

In the MAX experiment participants were presented with two conditions: threat and safe. Each condition was presented as a block of length 16.25 seoncds. Thirty-six regions of interest (ROI) were selected to look at the temporal evolution of the responses during both conditions. ROI analysis on the 36 selected ROIs was performed on each subject's fMRI data. Evolution of the response during presentation of the two conditions was estimated by specifying an estimation window from the onset of the block until 20 seconds after. Estimation window is kept a little longer (20s) than the block length (which is 16.25s) to observe decay in the response.

Following notebooks were used to extract, process, organize the estimated responses, and run BML.

00-estimates-resp.ipynb: extracts estimated responses from individual level ROI analysis outputs, generates a figure showing average responses in each ROI, and outputs the estimated responses in a plain text file (output files: MAX_neutral_estimated_response.txt, MAX_neutral_mean_estimated_response.txt')  

01-STAI_corr.ipynb: takes the estimated response data from MAX_neutral_estimated_response.txt and appends two new columns it (STATE and TRAIT). These are state and trait anxiety scores of the subjects  

02-aggregate_df.ipynb: This notebook aggregates the response estimates from early (2.5-8.75s) and late (10-16.25s) phase into single values by taking their weighted average. Now there are only two estimates per subject and roi: one for the early phase and other of the late phase 
(output file: MAX_neutral_early_late.txt)  

03-MAX-early.r: Runs BML on the the early phase. (input file: MAX_neutral_early_late.txt, output file: 03-MAX-early.RData)  

04-MAX-late.r: Runs BML on the late phase. (input file: MAX_neutral_early_late.txt, output file: 03-MAX-late.RData)  
