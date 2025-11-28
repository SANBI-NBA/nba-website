library(yaml)

# Get all .qmd files recursively

qmd_files <- list.files("content/", pattern = "\\.qmd$", 
                        
                        recursive = TRUE, full.names = TRUE)

# Check each file for embed-resources: true

files_with_embed <- c()

for (file in qmd_files) {
  
  # Read file content
  
  lines <- readLines(file, warn = FALSE)
  
  # Find YAML frontmatter (between --- markers)
  
  yaml_start <- which(lines == "---")[1]
  
  yaml_end <- which(lines == "---")[2]
  
  if (!is.na(yaml_start) && !is.na(yaml_end)) {
    
    yaml_content <- paste(lines[(yaml_start + 1):(yaml_end - 1)], collapse = "\n")
    
    # Parse YAML
    
    tryCatch({
      
      yaml_data <- yaml.load(yaml_content)
      
      # Check for embed-resources: true
      
      if (!is.null(yaml_data$format$html$`embed-resources`) && 
          
          yaml_data$format$html$`embed-resources` == TRUE) {
        
        files_with_embed <- c(files_with_embed, file)
        
      }
      
    }, error = function(e) {
      
      # Skip files with YAML parsing errors
      
    })
    
  }
  
}

# Output results

if (length(files_with_embed) > 0) {
  
  cat("Files with embed-resources: true:\n")
  
  cat(paste(files_with_embed, collapse = "\n"), "\n")
  
} else {
  
  cat("No files found with embed-resources: true\n")
  
}
