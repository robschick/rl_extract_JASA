This is a targets based workflow to estimate sound pressure level of tagged animals during a controlled exposure experiment off Cape Hatteras, NC, USA. This is in support of the following in review manuscript: Schick, R.S., Kaney, N., Zheng, L., Cioffi, W., Foley, H., Swaim, Z., Margolina, T., Joseph, J. E., and B.L. Southall. Using auxiliary data to quantify and refine estimates of received levels from simulated mid-frequency sonar. Journal of the Acoustical Society of America (_In review_). 

The entire workflow takes approximately 4 hours to run on a 2022 MacBook Air with an M2 chip and 24 GB of RAM.

Note that a few of the directories are hard coded. This is because some of the files, notably the sound propagation output files, are quite large (~4 GB each). Because this workflow supports the entire Atlantic BRS project, keeping the large files in one place outside of this repository facilitates computing.

To view the workflow in RStudio, first load the targets library:

`library(targets)`

and then source:

`tar_visnetwork()`

Individual nodes in the network can be run with the `tar_make()` command, e.g., `tar_make(locs)`. The entire workflow can be run with `tar_make()`, but all the data must be available. We will make these available upon acceptance of the manuscript.
