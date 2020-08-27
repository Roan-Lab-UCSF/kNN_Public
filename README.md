# Welcome to the page for our kNN Script

This script was made to map a set of test cells to their k nearest neighbors in a set of atlas cells. Particularly, this script was meant to take CyTOF data and FCS files as input.

kNN stands for k nearest neighbor and it is a machine learning algorithm which takes points of data and finds the most similar data point in a pool of data points. In other words, it finds the nearest neighbors of a given set of data points. k in kNN stands for the number of nearest neighbors that the user wants to find. Similarity between data points in this script is decided based off of Euclidean distance between data points. Also, "data points" in this script are meant to be individual cells and their "coordinates" are all the parameters of CyTOF data collected for each cell.


## How to Use the Script

When using the script, there is a defined section called "settings" where the user should input the names of the files that the user is trying to run kNN on. Note that the files should be in the same directory that the actual script is in.

The designated predictory file should contain all the cells that the user wants to find a nearest neighbor for. The designated training file is the pool of cells that will be considered nearest neighbors. For example, any cells that are in the predictory file will only have nearest neighbors chosen in the training file. No cell in the predictory file will have a nearest neighbor that is in the predictory file unless it is also in the training file.

In the additional settings column there are vectors titled "HIVrelated_parameters", "Junk_parameters", and "Remov_parameters". Any parameters added to any of these vectors will cause them to be omitted from the kNN algorithm and they won't be used to find nearest neighbors or determine Euclidean distance between points.

The script should output an FCS file that contains cells from the training file that were identified as the nearest neighbor of the cells in the predictory file.

Please do not change k in this script, it was only meant to find one nearest neighbor for each cell in the predictory file.

If you have any questions or issues feel free to submit them in the issues section of the Github!


