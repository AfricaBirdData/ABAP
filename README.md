# SABAP
Code for downloading and working with SABAP2 data

This packages provides functionality to access and download data from the Southern Africa Bird Atlas Project.

A typical workflow entails defining a region and a species of interest, e.g. say we are intereted in the 
occupancy of the African Black Duck in the North West province of South Africa:

First find the SABAP2 code for the species of interest

require(dplyr)

spp <- searchSabapSpecies("Duck") %>% filter(Common_species == "African Black") %>% pull(Spp)

Then, download the data recorded for the region of interest:

dat <- getSabapData(spp, region_type = "province", region = "North West")

## INSTRUCTION TO INSTALL

1. Clone the repository to your local machine:
   - In RStudio, create a new project
   - In the 'Create project' menu, select 'Version Control'/'Git'
   - Copy the repository URL (click on the 'Code' green button and copy the link)
   - Choose the appropiate directory and 'Create project'
2. Install the package 'devtools' in case you don´t have it and run devtools::install() from the project directory
3. Remember to pull the latest version regularly

## INSTRUCTIONS TO CONTRIBUTE CODE

For site owners:

There is the danger of multiple people working simultaneously on the project code. If you make changes locally on your computer and, before you push your changes, others push theirs, there might be conflicts. This is because the HEAD pointer in the main branch has moved since you started working. 

To deal with these lurking issues, I would suggest opening and working on a topic branch. This is a just a regular branch that has a short lifespan. In steps:

- Open a branch at your local machine
- Push to the remote repo
- Make your changes in your local machine
- Commit and push to remote
- Merge your changes:
  - In the GitHub repo you will now see an option that notifies of changes in a branch: click compare and pull request.
  - If there are no conflicts 'merge pull request'
- Delete the branch. You will have to delete it in the remote repo (GitHub) and also in your local machine. In your local machine you have to use Git directly, apparently RStudio doesn´t do it:
  - In your local machine, change to master branch.
  - Either use the Git GUI (go to branches/delete/select branch/push).
  - Or use the console typing 'git branch -d your_branch_name'.

Opening branches is quick and easy, so there is no harm in opening multiple branches a day. However, it is important to merge and delete them often to keep things tidy. Git provides functionality to deal with conflicting branches. More about branches here:

https://git-scm.com/book/en/v2/Git-Branching-Branches-in-a-Nutshell

Another idea is to use the 'issues' tab that you find in the project header. There, we can identify issues with the package, assign tasks and warn other contributors that we will be working on the code.
