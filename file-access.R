# read/write files from s3 to local disk/s3 if filename has protocol s3://

library('aws.s3')

strip_protocol <- function(filename) {
  if (grepl("s3://", filename) || grepl("file://", filename)) {
    split <- strsplit(filename, '/')[[1]]
    return(basename(paste(tail(split, length(split) - 2), sep='', collapse='/')))
  } else {
    return (filename)
  }

}

get_bucket_and_object <- function(filename) {
  split <- strsplit(filename, '/')[[1]]
  bucket <- split[3]
  object <- paste(tail(split, length(split) - 3), sep='', collapse='/')
  return (list("bucket" = bucket, "object" = object))
}

read_s3 <- function(filename) {
  # download file from s3 to local disk if needed
  if (grepl("s3://", filename)) {
    names <- get_bucket_and_object(filename)

    if (Sys.getenv("AWS_ACCESS_KEY_ID") == "" || Sys.getenv("AWS_SECRET_ACCESS_KEY") == "") {
      print("Environment variable AWS_ACCESS_KEY_ID or AWS_SECRET_ACCESS_KEY not set")
      quit(status=1)
    }

    print(paste("Downloading", filename))
    if (grepl("fmi.fi", Sys.getenv("AWS_S3_ENDPOINT"))) {
      # special fiddling with non-AWS s3
      save_object(names$object, "", region=names$bucket,base_url=Sys.getenv("AWS_S3_ENDPOINT"),use_https=F,show_progress=T)
    } else {
      save_object(names$object, names$bucket, show_progress=T)
    }
    return (basename(filename))
  } else {
    return (filename)
  }
}

write_s3 <- function(filename) {
  # write file from local disk to s3 if needed
  if (grepl("s3://", filename)) {
    local <- strip_protocol(filename)
    if (Sys.getenv("AWS_ACCESS_KEY_ID") == "" || Sys.getenv("AWS_SECRET_ACCESS_KEY") == "") {
      print("Environment variable AWS_ACCESS_KEY_ID or AWS_SECRET_ACCESS_KEY not set")
      quit(status=1)
    }
    names <- get_bucket_and_object(filename)

    print(paste("Uploading", filename))

    if (grepl("fmi.fi", Sys.getenv("AWS_S3_ENDPOINT"))) {
      # special fiddling with non-AWS s3
      put_object(file=local, object=names$object, bucket="", region=names$bucket, multipart=T, show_progress=T,base_url=Sys.getenv("AWS_S3_ENDPOINT"))
    } else {
      put_object(file=local, object=names$object, bucket=names$bucket, multipart=T, show_progress=T)
    }

  } else {
     # do nothing
  }
}
