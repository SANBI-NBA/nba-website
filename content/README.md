# NBA website content

BIANCA TESTING THE DEV SITE

The NBA website is compiled from the contributions of a team of scientists. Each contributor maintains their own repository for their section of NBA content. Each contributor's repository follows a template folder structure for compiling their content, documented [here](https://github.com/SANBI-NBA/templates).

The folders in here are compiled from the **quarto** folder inside each contributor's repository. It is essential that all files necessary for the successful rendering of their content is stored inside the quarto folder, as other content (e.g. in the root folder) is ignored.

## List of source repositories for each content folder

### **context**

A folder containing miscellaneous contextual pages for the website. Some are part of the main website navigation, others are linked in the footer.

**Maintainer:** Carol Poole, Andrew Skowno

**Source:** <https://github.com/SANBI-NBA/about_webpages>

### key-messages

Policy advice based on the findings of the NBA, summarised into 15 key messages

**Maintainer:** Andrew Skowno

**Source:** <https://github.com/SANBI-NBA/key-messages>

### integrated-findings

A section summarising overall status and trends of South Africa's biodiversity, including the headline indicators for species and ecosystems.

**Maintainer:** Andrew Skowno

**Source:** <https://github.com/SANBI-NBA/integrated_findings>

### terrestrial

Detailed findings for the terrestrial realm.

**Maintainer:** Maphale Monyeki

**Source:** <https://github.com/SANBI-NBA/terrestrial>

### freshwater

Detailed findings for the freshwater realm.

**Maintainer:** Nancy Job

**Source:** <https://github.com/SANBI-NBA/freshwater>

### estuaries

Detailed findings for the estuarine realm.

**Maintainer:** Lara van Niekerk

**Source:** <https://github.com/SANBI-NBA/estuaries>

### marine

Detailed findings for the marine realm.

**Maintainer:** Jock Currie, Natasha Besseling

**Source**: <https://github.com/SANBI-NBA/marine>

### coast

Detailed findings for the coast.

**Maintainer:** Linda Harris

**Source:** <https://github.com/lrharris/nba2025_coast>

### subantarctic

Detailed findings for South Africa's Sub-Antarctic territory (Prince Edward Islands).

**Maintainer:** Stephni van der Merwe, Andrew Skowno

**Source:** <https://github.com/SANBI-NBA/subantarctic>

### species

Detailed findings for species.

**Maintainer:** Shae-Lynn Hendricks

**Source:** <https://github.com/SANBI-NBA/species>

### genetics

Indicators for genetic diversity.

**Maintainer:** Jessica da Silva

**Source:** <https://github.com/SANBI-NBA/genetics>

## Workflow for extracting website content from repositories

1.  Clone each of the repos listed above - it is best to place them within a parent folder to enable recursive content extraction.

2.  Make sure that the local folder name containing the repo matches the content folder listed here.

3.  Run the script `collect-content.R` in the `scripts` folder to copy the contents of the `quarto` folder of each repo to here.

4.  The folder `scripts/pulling-content-from-git` contains a bash script for pulling the latest version of each content repo. The workflow for content improvement is to list tasks in the issues of each of the source repos. When contributors have addressed the issues, a new version of their repo is pulled using `scripts/pulling-content-from-git/update_repos.sh` and then the updated content is collected back into `content` using `scripts/collect-content.R`.
