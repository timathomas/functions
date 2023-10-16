# Common Codes
## Servers
```
vnc://d-128-95-71-218.dhcp4.washington.edu/
smb://d-128-95-71-218.dhcp4.washington.edu/
smb://udrive.uw.edu/udrive/t77
smb://netid.washington.edu/csde/shares/net
smb://netid.washington.edu/csde/homes/fellows/t77
```

- - - -
## R 
**Tidy Census**
* [Basic usage of tidycensus • tidycensus](https://walkerke.github.io/tidycensus/articles/basic-usage.html)
* [Julia Silge - Using tidycensus and leaflet to map Census data](https://juliasilge.com/blog/using-tidycensus/)
### Install MRAN on linux or unix
[Install Microsoft open R:](https://docs.microsoft.com/en-us/machine-learning-server/r-client/install-on-linux)

### Install MRAN on linux
Install Microsoft open R: https://docs.microsoft.com/en-us/machine-learning-server/r-client/install-on-linux
### R for data science
[R for Data Science](http://r4ds.had.co.nz/index.html)

### Rstudio Cheatsheets
[Cheatsheets – RStudio](https://www.rstudio.com/resources/cheatsheets/)

- - - -

## Terminal

### Restart mac remotely and log in

```
sudo fdesetup authrestart
```

### How to sftp 
Nice [short tutorial](https://www.cs.fsu.edu/~myers/howto/commandLineSSH.html)

```
sftp netid\\t77@csde-fs1.csde.washington.edu:/net
```
* `put` -- copy a file from the local machine to the remote machine
* `get` -- copy a file from the remote machine to the local machine
* `ls` -- get a directory listing on the remote machine
* `cd` -- change your current working directory on the remote machine
* `lls` -- get a directory listing on the local machine
* `lcd` -- change your current working directory on the local machine

#### update-alternatives
[alternatives - Linux Command](https://www.lifewire.com/alternatives-linux-command-4091710)
* change R path to other path
```
whereis R

sudo update-alternatives --install /usr/bin/R R /usr/lib/R/bin/R 1
```

#code/Terminal/tmux#
[amazon ec2 - keep server running on EC2 instance after ssh is terminated - Stack Overflow](https://stackoverflow.com/questions/21193988/keep-server-running-on-ec2-instance-after-ssh-is-terminated)

### tmux
```
ctrl-b d # detatch
tmux attach
tmux attach -d #connect to new session

ctrl-b [
```
#code/Terminal
File size 

*Directory Contents*

```
du -sh * 
```

*Available disk space* 

```
df -h
```



* Transfer file from computer to AWS ec2
```
scp -i my.pem
/file/to/copy/path
user@server:/copy/to/path
```
	* Example
```
# TO AWS
scp -i ../.ssh/t77_HILD.pem UDPersons_2004-2013.csv ubuntu@ec2-54-191-49-42.us-west-2.compute.amazonaws.com:~/data/Evictions

# From AWS
scp -i t77_HILD.pem ubuntu@ec2-34-219-3-214.us-west-2.compute.amazonaws.com:~/data/Housing/temp/180615_temp.RData ~/Temp/.
```
	* url for shortcuts [A Quick Cheat Sheet to the Unix/Mac Terminal](https://learntocodewith.me/command-line/unix-command-cheat-sheet/)

`less` = opens up a file and looks at it
### Syncthing
```
To have launchd start syncthing now and restart at login:
  brew services start syncthing
Or, if you don't want/need a background service you can just run:
  syncthing
```
### ssh into remote Mac
1. Open terminal
2. ssh timothythomas@d-128-95-71-218.dhcp4.washington.edu



### Unzipping
[unzip](http://magma.maths.usyd.edu.au/magma/faq/extract)
`tar xopf blas-3.8.0.tar`

### zip

`zip file.zip filname`
`cat listOfFolders.txt | zip -r@ part1.zip`

To compress

```
tar -jcvf archive_name.tar.bz2 folder_to_compress
```

To extract

```
tar -jxvf archive_name.tar.bz2
```

### BLAS and LAPACK
[HowTo: Install LAPACK and BLAS on Mac OS | pheiter](https://pheiter.wordpress.com/2012/09/04/howto-installing-lapack-and-blas-on-mac-os/)
Need [Gfortran](https://www.scivision.co/install-latest-gfortran-on-ubuntu/)

### Install Brew
`/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`

### Mendeley fix
Taken from [Mendeley grievances and quick hack – Koen Hufkens](http://www.khufkens.com/2016/02/27/mendeley-grievances/)
`ln -s /Users/timothythomas/Dropbox/Mendeley\ Ltd. /Users/timothythomas/Library/Application\ Support/Mendeley\ Ltd.`

### Search for key term
[macos - How to find files with certain text in the Terminal - Super User](https://superuser.com/questions/162999/how-to-find-files-with-certain-text-in-the-terminal/163002)
`grep -r 'text goes here' path_goes_here`

### screen
```
screen
ctrl-a ctrl-d
screen -r #reattach
ctrl-a [ #copy mode, allowing you to scroll
exit
```

### Pandoc
[Pandoc - Demos](https://pandoc.org/demos.html)

### Utilities
`pmset -g batt` Battery percent and time

### Stop Process
`ctrl - z`

### Temperature Sensor Stats
`iStats -f`

### Chrome full screen
```
/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --app=https://buffalo.csde.washington.edu:8787
```

- - - -
## Git
CheatSheet

https://services.github.com/on-demand/downloads/github-git-cheat-sheet.pdf

#code/terminal/common codes/git training#
https://arokem.github.io/2017-12-11-escience
GitHub -
	* Can make the readme file as a webpage for use.
Jupyter Notbook
	* 20.5 is a floating number

**Clone Branch**
[clone remote branch with git · GitHub](https://gist.github.com/fabianmoronzirfas/4023446)
```
git clone https://github.com/arokem/Housing.git
cd Housing
git checkout uw
git pull
```
### Set up git server
[How To Set Up A Secure Git Server At Home (OSX) — Tom Dalling](https://www.tomdalling.com/blog/software-processes/how-to-set-up-a-secure-git-server-at-home-osx/)

1. Create .git `git init --bare ~/Git/BuffaloShare/Merge_PHA_SHA.git`
2. Clone on little buffalo `git clone ssh://username@d-###-##-##-###.dhcp4.washington.edu/Users/username/Git/BuffaloGit/Merge_PHA_HMIS.git`

### Check out git status of all enclosed folders
```
find . -maxdepth 1 -mindepth 1 -type d -exec sh -c '(echo {} && cd {} && git status -s && echo)' \;
```

### Auto pull all the git folders
1. Navigate to `Git`
2. Run `ls | xargs -I{} git -C {} pull`
	1. [bash - Run git pull over all subdirectories - Stack Overflow](https://stackoverflow.com/questions/3497123/run-git-pull-over-all-subdirectories)

- - - -
## Python
#code/Python#
[numpy/HOWTO_DOCUMENT.rst.txt at master · numpy/numpy · GitHub](https://github.com/numpy/numpy/blob/master/doc/HOWTO_DOCUMENT.rst.txt)
[Python Tutor - Visualize Python, Java, JavaScript, TypeScript, Ruby, C, and C++ code execution](http://pythontutor.com)
import math
```
wget http://swcarpentry.github.io/python-novice-gapminder/files/python-novice-gapminder-data.zip
```
Naming an object is normal. Then, when you want to run a function, you use `data.fun()`
	* example:
```
df1 = pd.read_csv("data/...", index_col="country")
df1.describe() # is like summary()

# you can also add functions at the end
df1.loc[0:5,0:5].describe()
```

```
import boto3

client = boto3.Session(profile_name='default')

s3 = client.resource('s3')
bucket = s3.Bucket('hild-datasets')

bucket.upload_file("/home/ubuntu/data/Housing/OrganizedData/pha_longitudinal_kc.csv", "pha_longitudinal_kc.csv", ExtraArgs={"ServerSideEncryption": "AES256"})
bucket.upload_file("/home/ubuntu/data/Housing/OrganizedData/pha_longitudinal.csv", "pha_longitudinal.csv", ExtraArgs={"ServerSideEncryption": "AES256"})
```

### Install pip
[How to install pip with Python 3? - Stack Overflow](https://stackoverflow.com/questions/6587507/how-to-install-pip-with-python-3)
```
sudo apt-get install python3-pip
```

#### Install packages
`python3 -m pip install [package]`

- - - -
## AWS
https://006732607436.signin.aws.amazon.com/console
* Open AWS and start tthomas machine.
* cd into folder
### Transferring data
[Transferring Files between your laptop and Amazon instance — Angus 5.0 documentation](https://angus.readthedocs.io/en/2014/amazon/transfer-files-between-instance.html)
* From Laptop to AWS Instance
```
scp -i ~/.ssh/t77_HILD.pem ~/Downloads/Files/Elite_2017_Sept2018.xlsx ubuntu@ec2-54-188-168-238.us-west-2.compute.amazonaws.com:~/data/SHA
```
* copy data from s3 to machine.
	* `aws s3 ls s3://seattle-ha/sha_data/`
	* copy whole file  `aws s3 cp s3://seattle-ha/sha_data/ . --recursive`
	* Copy one file  `aws s3 cp s3://seattle-ha/Filename . `
		* this downloads all the data in the folder over to the directory
		* From ec2 to laptop `scp -i ~/.ssh/jh_hild.pem ubuntu@ec2-34-209-122-30.us-west-2.compute.amazonaws.com:/home/ubuntu/data/courses_labels_b.csv .`
* Copy from machine to s3
```
aws s3 cp s3://hild-datasets . --recursive --sse

aws s3 cp ~/data/SHA/3_Yardi_Household_Data_With_SSN_Sept2018.csv s3://seattle-ha/sha_data/ --sse

aws s3 cp hild_df_190416.csv s3://hild-datasets/ --sse AES256

```
* Get Field Name Mapping.xlsx file into the right spot
### Create AWS Machine
1. Create machine image
2. Start new machine with that hard drive and security
3. AMI
4. LAUNCH
5. Choose r series (opt. Memory)
6. Launch wizard 12

### Puget setup
```
# go into puget, install puget
sudo python setup.py install


```

### Setup jupyter notebooks
[Run Project Jupyter Notebooks On Amazon EC2](https://chrisalbon.com/software_engineering/cloud_computing/run_project_jupyter_on_amazon_ec2/)

- - - -
## Buffalo.csde.washington.edu
## SSHFS
Had to create a directory that wasn’t named after something else. This allows permissions `mkdir R_Drive`

```
<mount:>

fusermount -uz R_Drive

sshfs -o idmap=user netid\\t77@csde-fs2.csde.washington.edu:/net R_Drive
sshfs -o idmap=user netid\\t77@csde-fs2.csde.washington.edu:/csde-fellows/t77 H_Drive

```
also
```
ssh netid\\t77@csde-fs2.csde.washington.edu

<unmount:>

sudo umount R_Drive

<check mount status>

df -h R_Drive

```


## buffalo.csde.washington.edu
R is located in `/usr/lib/R/bin/R`

## Reboot operations

```
sudo systemctl reboot -i
```

Startup commands

```
fusermount -uz R_Drive
fusermount -uz H_Drive

sshfs -o idmap=user netid\\t77@csde-fs1.csde.washington.edu:/csde-fellows/t77 H_Drive

sshfs -o idmap=user t77@sftp.udrive.uw.edu:/udrive/t77 udrive

sshfs -o idmap=user netid\\t77@csde-fs1.csde.washington.edu:/CSDE_Net R_Drive

sudo rstudio-server start

syncthing
```

## Windows install
UNPLUG EXTERNAL DRIVES!!!

- - - -
## rmate

[Sublime Text editor locally to edit code files on a remote server](https://fearby.com/article/how-to-use-sublime-text-editor-locally-to-edit-code-files-on-a-remote-server-via-ssh/)

## R Install 3.5.1
[Installing R on Windows, Mac and Ubuntu (article) - DataCamp](https://www.datacamp.com/community/tutorials/installing-R-windows-mac-ubuntu)
```
brew install openblas
brew install r —with-openblas
echo ‘Sys.setlocale(category="LC_ALL", locale = "en_US.UTF-8")' >> ~/.bash_profile
```

## GIT Pulling and stuff
```
git pull upstream master
git push origin master
git checkout -b <branch>
```
* Make edits
* go to gitlab and create a pull request
	* Label it as WIP so no one can merge or mess with it until you are done.
*

Only merge branches to the master


```
pulling case numbers
../ikennedy/selenium_tests/eviction_records/county_dl_scripts/county_dl.py
```

# Permissions

## Allow group access
 ```
 chmod g+rw /data/results/esri_addresses.csv.bz2
 ```


# Setup puget

```
sudo python setup.py install
import boto3
import os.path as op

import sys

import pandas as pd
import numpy as np
import networkx

import recordlinkage as rl
from dateutil import parser
```

## Git LFS

```
brew install git-lfs
git clone git@gitlab.com:timathomas/talks.git
git lfs install
git lfs track "*.pdf" "*.key"
git add .gitattributes
git lfs fetch origin master
```

# Password Protect Website

staticrypt https://github.com/robinmoisson/staticrypt

```
npm install staticrypt
staticrypt test.html MY_PASSPHRASE
```