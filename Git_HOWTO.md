---
title: How to setup GitHub within R Studio
subtitle:
author:
  - Leander Kühne
abstract:
tags:
  - masterThesis
  - microbialEcology
  - microfluidics
created: 2026-05-06
date: 2026-05-06
documentclass: slides
template: blood
toc: true
numbersections: true
link-citations: true
colorlinks: true
---

# 1. Verify Git installation
- Go to terminal tab inside RStudio and check if and where git is installed 
	--> `which git` prints the directory to which git is installed
	--> `git --version` prints the installed version of git

---
# 2. Edit Git Config File
- Inside the console bring up the git config file with:
```R
#install.packages("usethis") # Uncomment if "usethis" package needs to be installed
library(usethis) 
edit_git_config()
```

- Edit git config file and add your name and the email address you registered with on GitHub
```R
[user]
  name = "Your Name"
  email = "myemail@mail.com"
```

- Save and close `.gitconfig` file

---
# 3. Initialize Git Repository
- Create Project in RStudio (File > New Project)
- Initialize git repo either by ticking the box 'Create a git repository' in the Project setup window or by running `use_git()` from `usethis` package in the console. Confirm the commit and restart RStudio
- After restart you should see a 'Git' tab inside RStudio

---
# 4.  Make Commits and  View Commit History
All changed/added files will show up in the git tab. You can tick the checkbox to stage them for commiting. Add a commit message and press 'Commit' button. 
The history shows the differences between the current and previous commits. 

---
# 5. Create Personal Access Token (PAT) on GitHub
- Create an account on GitHub
- Create PAT by running this inside the RStudio console:
```R
library(usethis)
create_github_token()
# This will redirect you to the GitHub PAT setup site. 
```
- Give a name to the Token (e.g. 'R:GITHUB_PAT')
- Leave scopes/settings as they are and hit 'Generate Token'
- **Copy and save the token in your password manager of choice or somewhere safe!**

---
# 6. Store PAT Connect RStudio and GitHub
- Store your PAT in R Studio using `gitcreds`:
```
library(gitcreds)
gitcreds_set()
```
- Paste your PAT
- Connect your specific project to GitHub: 
```R
library(usethis)
use_github()
```

---
# 7. General Workflow PUSH
- Create/edit file
- Stage files for commiting
- Add commit message
- Hit 'Commit'
- Hit 'Push'

You can also do this even faster inside the terminal: 
```bash
git add . #Stages all changed files for commiting
git commit -m "Your Commit message" #Do the commit
git push #Push changes to GitHub
```

---
# Concluding Remarks
- Credits go to [this YT series](https://www.youtube.com/playlist?list=PLSviU861UtD81AuyYb3SbndmAA_qTCoLe) by *Vafa Saboori*
- You don't need to push after every commit
- Properly using commit messages helps a lot with understanding what you wanted to do
- Think about file size, maybe create separate folder for images/plot renderings outside the git repository or and only include the most important ones