# join_liss

The `join_liss` R function joins all .sav files in a specified folder and returns a longitudinal file while preserving variables' sequence, units and scale.

The LISS panel is a representative sample of Dutch individuals. The panel includes a series of 'modules' of questions sent to respondents at various times throughout the year. For example, the Health module, collected in November and December, and the Personality and Income modules, collected in May-June and June-July, respectively. Respondents may participate in all or only a subset of modules and years.  

The panel spans 13 waves, or years (2007-2022), with each one covering 11 modules (including monthly background data) across roughly 7,500 individuals and 5,000 households per wave. Due to drop-out and attrition, the merged data contains 16,559 unique individuals and 6,601 households.

Before analyzing these data, the relevant modules must first be merged into a longitudinal file. From a practical standpoint, merging waves outside the study's scope appears to be a waste of time. However, using all available data will lead to more reliable outlier detection and missing value imputation. After data cleaning, a subset with only relevant waves and modules can be made to increase analysis speed and efficiency. 

The `join_liss` function does the whole job in one go and even accounts for panel consistency to ensure merged variables are in the same order, scale, and units across all waves. 

After signing a statement regarding data usage, academic researchers can access the LISS data at no cost (see www.lissdata.nl/access-data). Then store all .sav files in a folder and run the function on that folder. Joining all files can take a minute or two (or a bit longer depending on your computer's speed).

The helper function `get_liss` downloads all files by opening a new tab using the default browser (= 293 tabs!). This only works after manually logging in. If the browser freezes from opening multiple tabs, add a pause in between downloads using the .wait argument.

```R
# Download all .sav files from the LISS panel (after logging in manually first).
# .wait: add a pause in between consecutive downloads (i.e., opening a new tab)
get_liss(.wait = 1)

# .path: join all .sav files by specifying the path to the folder; regardless of order/subfolder.
# .all_bck: join all background data (TRUE) or the intersection across all modules (FALSE, by default).
df <- join_liss(.path = "/Users/siardv/Documents/liss", .all_bck = FALSE)
```


To do, add function to:
- download files silently in the background;
- determine the optimal sequence of waves considering attrition and dropouts;
- resolve violations of the temporal homogeneity assumption in either all or a defined subset of items (i.e., inconsistencies in time-invariant variables).
