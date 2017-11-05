#!/usr/bin/Rscript

library(miniCRAN)
library(aws.s3)
library(RCurl)

get_dependencies = function(package_name, package_matrix, suggests=FALSE) {
  dependencies = pkgDep(package_name, package_matrix, suggests=suggests)
  if (length(dependencies) == 1) {
    dependencies[1]
  } else {
    rest_of_dependencies = lapply(dependencies[2:length(dependencies)], function(d){pkgDep(d, package_matrix, suggests=FALSE)})
    sorted_dependencies = rest_of_dependencies[order(sapply(rest_of_dependencies, length))]
    results = unlist(lapply(sorted_dependencies, function(d) {get_dependencies(d, package_matrix, suggests=suggests)}))
    c(results, dependencies[1])
  }
}


upload_package_to_s3 = function(package_name, package_matrix, bucket = "vium-r-packages") {
  # Get the download url from CRAN
  version = package_matrix[package_name, 'Version']
  repository = package_matrix[package_name, 'Repository']
  package_filename = sprintf("%s_%s.tar.gz", package_name, version)
  url = sprintf("%s/%s", repository, package_filename)
  
  # Download the file locally
  temp_filename = tempfile()
  download.file(url, temp_filename)
  
  # Upload the file to s3
  put_object(temp_filename, package_filename, bucket, headers = list(`x-amz-acl` = "public-read"))
  
  # return the website URL
  www_url = sprintf("http://%s.s3-website-us-west-2.amazonaws.com/%s", bucket, package_filename)
  
  # should return package_name, version, and url then put all in df
  list(package=package_name, version=version, url=www_url)
}

upload_dependencies_for_package =  function(package_name, package_matrix, suggests = FALSE, bucket = "vium-r-packages") {
  dependencies = get_dependencies(package_name, package_matrix, suggests = suggests)
  results = lapply(dependencies, function(d) {upload_package_to_s3(d, package_matrix)})
  as.data.frame(do.call(rbind, results))
}


format_dependencies = function(dependencies_df) {
  dependencies = dependencies_df$package
  main_package = dependencies[length(dependencies)]
  formatted_strings = c("PY_LIBRARY(\n")
  formatted_strings = c(formatted_strings, paste0("  NAME      rpkg_", main_package, "\n", sep = ""))
  formatted_strings = c(formatted_strings, "  SOURCES   __init__.py\n")
  formatted_strings = c(formatted_strings, "  DEPENDS   //thirdparty:rbase\n")
  package_strings = unlist(lapply(dependencies, function(d) {paste0("            R://", d, "\n", sep = "")}))
  formatted_strings = c(formatted_strings, package_strings)
  formatted_strings = c(formatted_strings, ")\n")
  paste0(formatted_strings, collapse = "")
}

# package_name = 'dplyr'
# dependencies = get_dependencies(package_name, package_matrix)
# result = upload_package_to_s3(package_name, package_matrix)

format_contraints = function(dependencies_df) {
  # devtools==1.13.0 https://cran.r-project.org/src/contrib/Archive/devtools/devtools_1.13.0.tar.gz
  formatted_strings = apply(dependencies_df, 1, function(r) {sprintf("%s==%s %s", r$package, r$version, r$url)})
  paste0(formatted_strings, collapse = "\n")
}

main = function() {
  args = commandArgs(trailingOnly=TRUE)
  package_name = args[1]
  package_matrix = available.packages()
  
  if (! package_name %in% package_matrix) {
    print(sprintf("No such package, '%s", package_name))
    print("Exiting.")
  } else {
    print(sprintf("Generating dependencies and uploading packages for package '%s", package_name))
    df = upload_dependencies_for_package(package_name, package_matrix)
    print("\n")
    print(format_dependencies(df))
    print("\n")
    print(format_contraints(df))
  }
}

if (! interactive()) {
  main()
}