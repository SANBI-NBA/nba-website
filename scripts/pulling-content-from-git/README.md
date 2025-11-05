## Method for compiling content from contributing content repos

The workflow described here should be set up outside the `nba-website` repo, but I am putting it here to keep everything together.

### Workflow

1.  Make a folder for collecting NBA content repos (all should be cloned to one parent folder).

2.  Clone each of the repos listed in [nba-website/content#readme](https://github.com/SANBI-NBA/nba-website/tree/main/content#readme) to this folder.

3.  Add the script `update_repos.sh` to the parent folder.

4.  Open a git bash terminal in the parent folder and run this code:

```         
chmod +x update_repos.sh 
./update_repos.sh
```

This will run `update_repos.sh` which iterates through all the cloned repos and will checkout to main and then pull from main. Important to note that NBA website will only be compiled from main branches on content repos. For new content to be pulled through to the website, editing or development branches must first be merged to main.

## Collect step: 

The next step is to collect the webcontent and include it in the nba-website repo

BEFORE doing this for the first time create a .Renviron txt file and add this line

CONTENT_PATH="C:/Rdata/nba-webcontent"
