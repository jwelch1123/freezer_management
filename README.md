# cellpaste_db
## Database entry and visualization app for cell pastes

### Purpose:
At the company where I work, we routinely produce material which is used in experimental and production workflows. Multiple types of material  exists (given the label PP###) and it is utilized for a variety of purposes by different groups at the company. This material can come from one-or-more of 6 lines and after harvest is stored in bags, and placed in one of several freezers. Each bag is unique, containing that specific batch (produced on a date from lines 1-6), of a label PP###, and a specific weight. Our current solution is an excel sheet where additions and removals are manually tracked, this leads to many errors where material is misplaced or not updated leading to an inaccurate assessment of our inventory. 
  
This is not ideal for multiple reasons so I interviewed the stakeholders to identify their needs in designing a new solution. There are two groups of stakeholders, Managers and Operators. Managers coordinate the production and usage of material, and create aims Operators work to achieve. Operators design and execute experiments which use this material. I interview 2 Managers and 3 Operators and compiled their requirements.
 
##### Managers:
- Understand how certain types of material are being used and by whom.
- Clear understanding of weight of material of given requirements.
- Tracking of what purpose material is used for. (experimental vs production)
- Tracking what group used material and how much.
- Run Charts for predicting need of material.
- No data loss if Shiny App is not maintained. 
  
##### Operators:
- Standardization in naming conventions. 
- Ease of entering or changing weight of material as it is used.
- Easily remove a unique bag from the inventory entirely.
- Understand who else might want to use the same material.
- How long has material been stored for?

Using all this feedback I started to build my project.
  
### Overview:
  The current app consists of 5 pages. Two are dedicated two viewing information of the current inventory and a historic record of changes. Two other pages are used for entering a batch into the record and changing its value, respectively. The final page, Settings, is used to reset the databases in the need of a clean slate. Here is a more in-depth view of each page:
  
#### Viewer:
This page acts as a flexible visualization tool for understanding what is or was available to use. 

When viewing the inventory, a breakdown of each batch and strain (PP###) is shown. This can be further filtered for specific strains, batches, or other features as desired. In the upper right a summary table shows high level information of filter level selected. When strains are selected, strain level information is shown by strain (number of batches, total weight, all storage locations), when batch is selected, batch relevant information is shown, such as the strain associated with that batch, the stored weight, number of bags, etc. A graph and table are also displayed, showing the Weights of each strain or batch broken down by specific bag. Hovering over the graph will show meta-data such as storage location, material type and unique bag. 

When viewing the ledger, the most prominent feature is the run chart graph which shows all entries and removals for the material selected. This can be refined by using the same filters mentioned above (strain or batch) but also includes powerful meta-data filters such as storage location, group making the change and other information. The summary table once again shows Strain or batch contextual information.
  
#### Inventory & Ledger: 
This page contains two tables which can be used to visualize static views of the ledger and inventory. The built in search functions of the DT package are of great help but broadly this page exists as a fail safe. 
    
#### Batch Input:
This page is utilized when a new batch has been produced and is going to be stored in a freezer. Multiple pieces of information are collected and many entries utilized drop down selections with the ability to manually type an answer. This allows ease of use on a phone when preforming routine work but is flexible if needs change.
  
#### Batch Modification:
This page is very similar to Batch Input but is separated to lessen operator error when if batches are erroneously marked. This page utilizes drop down entries with the ability to manually enter new answers as well. Information like the Operator, Working Group, and Purpose of modification are also collected. Unique bags can also be easily removed by selecting a check box, which automatically calculates the amount remaining and removes it from the inventory (in the case of cleaning out a freezer).
  
#### Settings:
This page exists to reset the inventory and ledger. In the case of cleaning or a compromised freezer, starting from scratch with a new data base might be easier than manually curating every value. A confirmation is needed before the reset, an extra confirmation to reset the ledger as well. Archives of both are created and an empty data base saved in the old one's place. 
  
### Conclusions:
This app will be deployed in my workplace and was generally designed for ease of use on a phone or computer as is common in wet lab work. It addresses nearly all concerns and needs expressed by the stakeholders and replaces the previous error prone system. I hope to continue updating it with feedback from my coworkers and learning more to improve my implementation of this solution. 
  
### Future Work:
It would be better to use more strict input validation to prevent some common errors which arrive from poorly formatted entries. Currently multiple batches are physically harvested at the same time but one 1 can be uploaded to the inventory at a time. Integrating the Strain, Batch and other filters would make for a more unified experience. These and several other improvements would make the app easier to use but don't expand its capabilities. 

Several features which were requested was drawing on data from other databases kept by the company. This would make it easier to decide which batch is suitable for a given experiment and allow better communication between groups who generally keep their data siloed. This wasn't included in this version due to my inexperience and some poor data formatting in these other databases (inconsistent naming, common reformatting). 

In the future, this could be deployed as a free-to-use web app for anyone (or more likely lab) who wants to keep track of their freezer contents. However there are large competitors in this space so it will likely stay a niche utility as it was designed to be.  
  