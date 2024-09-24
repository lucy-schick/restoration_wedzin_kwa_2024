# one time only - made a mistake where the photos weren't being resized so this addresses the first couple days of photos

dir_project <- 'restoration_wedzin_kwa'
dir_photos_mergin_raw <- paste0("~/Projects/gis/", dir_project, "/ignore_mobile/photos/")
dir_photos_mergin_resized <- paste0("~/Projects/gis/", dir_project, "/ignore_mobile/photos_resized/")


# create the target directory
fs::dir_create(dir_photos_mergin_resized, recurse = TRUE)

## Clean up the mergin file----------------------------------------------------------------------------------------------------
# remove photos.txt file included in project when created (was to allow mergin git to see the photos dir) but needs
# to be removed or ignored to not break fpr_photo_resize_batch
fs::file_delete(
  paste0(dir_photos_mergin_raw, "/photos.txt")
)

fpr::fpr_photo_resize_batch(
  dir_source = dir_photos_mergin_raw,
  dir_target = paste0(dir_photos_mergin_resized)
)

# quick check to see if the photos are all accounted for
identical(
  length(
    list.files(dir_photos_mergin_raw, full.names = T, recursive = T)),
  length(
    list.files(dir_photos_mergin_resized, full.names = T, recursive = T))
)


# erase all the photos in the original directory by deleting the directory
fs::dir_delete(dir_photos_mergin_raw)

################################################################################################################
#-----------------DO A SYNC to MERGIN SO it doesn't choke.  just safet this way---------------------------------------------------
################################################################################################################


# copy over the resized photos back to the proper directory
fs::dir_copy(dir_photos_mergin_resized, dir_photos_mergin_raw)

# remove the resized directory
fs::dir_delete(dir_photos_mergin_resized)


# recreate the photos.txt file so the form still works if nothing was in it
fs::file_create(
  paste0(dir_photos_mergin_raw, "photos.txt")
)
