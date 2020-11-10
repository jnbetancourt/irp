
library(googledrive)

# Sample Data Frame - Conveniently upload and download dataframes to GoogleDrive
# without ever leaving RStudio
df <-  data.frame(id = c(1,2,3), name= c("a","b","c"), year = c(2000, 2001, 2002))
print(df)

# Writes the Data Frame to a local CSV File
write.csv(df, "~/temp/mydataframe.csv")

drive_upload("~/580/IRP/mydataframe.csv", media = "~/temp/mydataframe.csv", 
             overwrite = TRUE)

drive_download("~/580/IRP/mydataframe.csv", 
               path = "~/temp/mydataframe2.csv", overwrite = TRUE)

df2 <- read.csv("~/temp/mydataframe.csv")

#####
# Part 2
##### Reproducible R file

drive_upload("~/580/IRP/public_data.csv", media = "~/temp/mydataframe.csv", 
             overwrite = TRUE)

# Now anyone can view the file
google_drive_file <- drive_share_anyone("~/580/IRP/public_data.csv")
print(google_drive_file$id)

drive_download(as_id("1vl-atwEXksMLXRfMzk12ITqRxQPo7ydu"), path = "~/temp/mydataframe3.csv", overwrite = TRUE)

# Caveat if you overwrite it will change

# Use drive update instead fix this later

# Use case: Maybe you have a group project making an R package and you need to keep 
# a backup to google drive
































