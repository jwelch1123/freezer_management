# cellpaste_db
Database entry and visualization app for cell pastes

The shiny app project for NYCDSA which will involve a simple database for freezer inventory. This database can be explored through multiple requested visualizations but also includes a feature to update the database as new material is added.

Guide:
The landing page 'Viewer' is the man way to interact with the stored data.
You can visualize what is currently stored in the freezer, filter for specific items of interest, and get specific information regarding each batch.
The filters are not limited to Strain or Batch but a flexible filter is also available which can take other column such as where the material is stored and the operator who placed it there.
You can switch between a current inventory view and the ledger which is all modifications to the database. 
This ledger view allows you to visualize usage across time with a variety of filters applied.
You can view for a specific batch, a strain, or a user or group.
The summary table changes based on the specific information relevant to your question. You get a different view depending on if you're looking per batch or strain. 

Further more, there is the 'Inventory & Ledger' page which contains a static view of each database, this is mainly for the not so tech literate. 

Batch Input and Batch Modification act to collect information on batches being submitted to the database or being modified. Modification can include updates to values or the standard removal of some or all of the batch. Input selections are based on previous inputs in the ledger but new values can be entered as well. 

Finally: the 'Settings' page currently offers 1 functionally, to reset the databases in the need of a 'clean out'. This feature was specifically requested but due to the difficulty recovering the data it was placed away from the rest of the features. The inventory will always be reset but the ledger is optional. A confirmation check box needs to be selected or nothing will happen. When the databases are reset, archives are created and labeled with the data of reset. These are stored in the same directory as the app but could be moved as needed. 

I built this to help the company I am employed at so there may not be much utility to anyone else but if it turns out useful for you I am glad. 