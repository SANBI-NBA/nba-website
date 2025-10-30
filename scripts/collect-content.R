# A script to copy content from the NBA content repos into the website repo
# Set up a  content folder (not in this repo) and clone each of the individual repositories into that folder
# Make sure that the folder names match the folder names in the content folder in this repo
# Add the path to the content folder to an .Renviron file in the root folder of the project
# The script searches for a folder named quarto in each repo and copies the content of just that folder to the
# folder matching its name in the content folder in this repo

library(fs) # A package for interacting with file systems

# Get path to content directory from Renviron
readRenviron(".Renviron")
content_dir <- Sys.getenv("CONTENT_PATH")

# Path to content directory in this repo
website_dir <- "content" # This needs to be set relative to the project path, not this script

# Find the quarto folder inside each repo in the content directory
quarto_dirs <- dir_ls(content_dir, type = "directory", recurse = TRUE, regexp = "quarto$")

# Loop through each directory in quarto_dirs
# Find the name of the parent folder = destination directory in content in this repo
# If it doesn't exist, create a folder for it
# Copy all content from the quarto folder to this repo
# ChatGPT wrote this code hence all the silly icons in the output

for(qdir in quarto_dirs){
  
  # Get the parent folder name (one level above 'quarto')
  message("\n---\nProcessing: ", qdir)
  
  parent_folder <- path_file(path_dir(qdir))
  
  # Define the corresponding destination folder inside 'website-content'
  dest_dir <- path(website_dir, parent_folder)
  
  # Create destination folder if it doesn’t exist
  if (!dir_exists(dest_dir)) {
    dir_create(dest_dir, recurse = TRUE)
    message("Created destination folder: ", dest_dir)
  } else {
    message("Destination folder already exists: ", dest_dir)
    # First delete everything in the destination folder
    # This gets around renamed files remaining in the destination folder
    # And files that were deleted from the folder
    items_to_delete <- dir_ls(dest_dir, all = TRUE)
    
    if (length(items_to_delete) == 0) {
      message("⚠️ No items found in: ", dest_dir)
    } else {
      message("Items to delete:")
      print(items_to_delete)
      
      # Delete each file/folder
      for (item in items_to_delete) {
        dest_path <- path(dest_dir, path_file(item))
        if (is_dir(item)) {
          message("📁 Deleting folder: ", item)
          dir_delete(item)
        } else {
          message("📄 Deleting file: ", item)
          file_delete(item)
        }
      }
    }
 }
  
  # Check what’s inside this 'quarto' folder
  items_to_copy <- dir_ls(qdir, all = TRUE)
  
  if (length(items_to_copy) == 0) {
    message("⚠️ No items found in: ", qdir)
  } else {
    message("Items to copy:")
    print(items_to_copy)
    
    # Copy each file/folder
    for (item in items_to_copy) {
      dest_path <- path(dest_dir, path_file(item))
      if (is_dir(item)) {
        message("📁 Copying folder: ", item, " → ", dest_path)
        dir_copy(item, dest_path, overwrite = TRUE)
      } else {
        message("📄 Copying file: ", item, " → ", dest_path)
        file_copy(item, dest_path, overwrite = TRUE)
      }
    }
  }
}