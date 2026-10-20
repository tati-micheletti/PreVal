# Download the Copernicus HRL Tree Cover and Forests layers (100 m, yearly, 2018 onward) for Germany from the Copernicus Data Space (CDSE) by API.
#
#   Step 1, no login needed: write the list of files and check it
#       Rscript birds/download_cdse_clms.R --list
#   Step 2: download (YOUR OWN terminal; the password never goes into this file or into the chat). In PowerShell:
#       $env:CDSE_USER = "your CDSE e-mail"
#       $p = Read-Host "CDSE password" -AsSecureString
#       $env:CDSE_PASS = [Runtime.InteropServices.Marshal]::PtrToStringAuto([Runtime.InteropServices.Marshal]::SecureStringToBSTR($p))
#       Rscript birds/download_cdse_clms.R
#   Files land in data_birds/downloads/<folder>/ (git-ignored); finished files are checked against the catalogue's MD5 and skipped on a re-run.
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages({library(curl); library(jsonlite)})
args <- commandArgs(trailingOnly = TRUE)
listOnly <- "--list" %in% args
years <- 2018:2024
# product codes in the catalogue: TCD = tree cover density, BCD = broadleaved cover density, CCD = coniferous cover density (all 100 m, yearly)
products <- c(TCD = "hrl_tcd", BCD = "hrl_bcd", CCD = "hrl_ccd")
root <- file.path("data_birds", "downloads")
BOX <- "POLYGON((5.8 47.2,15.1 47.2,15.1 55.1,5.8 55.1,5.8 47.2))"        # Germany with a margin; tiles are small
cat_url <- "https://catalogue.dataspace.copernicus.eu/odata/v1/Products"

listProducts <- function(code, yr) {
  nm <- sprintf("CLMS_HRLVLCC_%s_S%d_R100m", code, yr)
  f <- sprintf("Collection/Name eq 'CLMS' and contains(Name,'%s') and OData.CSC.Intersects(area=geography'SRID=4326;%s')", nm, BOX)
  u <- paste0(cat_url, "?", "%24filter=", URLencode(f, reserved = TRUE), "&%24top=1000&%24orderby=Name%20asc")
  r <- curl_fetch_memory(u)
  if (r$status_code != 200) stop("catalogue query failed (", r$status_code, ") for ", nm)
  v <- fromJSON(rawToChar(r$content), simplifyVector = FALSE)$value
  rbindlist0 <- function(x) do.call(rbind, lapply(x, function(p) data.frame(code = code, year = yr, name = p$Name, id = p$Id, bytes = p$ContentLength,
                                                                           md5 = tolower(Filter(function(z) z$Algorithm == "MD5", p$Checksum)[[1]]$Value), stringsAsFactors = FALSE)))
  if (length(v)) rbindlist0(v) else NULL
}
man <- do.call(rbind, lapply(names(products), function(code) do.call(rbind, lapply(years, function(y) listProducts(code, y)))))
man$folder <- products[man$code]; man$file <- file.path(root, man$folder, paste0(man$name, ".tif"))
dir.create(root, recursive = TRUE, showWarnings = FALSE)
write.csv(man, file.path(root, "clms_manifest.csv"), row.names = FALSE)
cat(sprintf("%d files (%.0f MB) for %s, years %d-%d. List written to %s\n", nrow(man), sum(man$bytes) / 1e6, paste(names(products), collapse = ", "), min(years), max(years), file.path(root, "clms_manifest.csv")))
print(table(man$code, man$year))
if (listOnly) quit(save = "no")

user <- Sys.getenv("CDSE_USER"); pass <- Sys.getenv("CDSE_PASS")
if (!nzchar(user) || !nzchar(pass)) stop("Set CDSE_USER and CDSE_PASS in your own terminal first (see the top of this file).")
tok <- NULL; tokTime <- 0
getToken <- function(force = FALSE) {
  if (!force && !is.null(tok) && as.numeric(Sys.time()) - tokTime < 420) return(tok)       # tokens last about 10 minutes
  # the token endpoint wants an urlencoded body (a multipart form is refused with "invalid client"); values are escaped, so special characters are safe
  body <- paste0("client_id=cdse-public&grant_type=password&username=", curl_escape(enc2utf8(user)), "&password=", curl_escape(enc2utf8(pass)))
  h <- new_handle(); handle_setopt(h, postfields = body); handle_setheaders(h, "Content-Type" = "application/x-www-form-urlencoded")
  r <- curl_fetch_memory("https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token", handle = h)
  if (r$status_code != 200) {
    why <- tryCatch(fromJSON(rawToChar(r$content))$error_description, error = function(e) "(no reason given)")
    stop("Login failed (", r$status_code, "): ", why, "
User used: ", user, " | password length: ", nchar(pass),
         "
Check: (1) the same e-mail and password work at https://identity.dataspace.copernicus.eu ; (2) the account has no two-factor code ",
         "(a password-only login is needed for this script); (3) you do not log in through Google or another provider (then there is no password yet).")
  }
  tok <<- fromJSON(rawToChar(r$content))$access_token; tokTime <<- as.numeric(Sys.time()); tok
}
failed <- character(); done <- 0L
for (i in seq_len(nrow(man))) {
  f <- man$file[i]; dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(f) && unname(tools::md5sum(f)) == man$md5[i]) { done <- done + 1L; next }
  ok <- FALSE
  for (attempt in 1:3) {
    h <- new_handle(); handle_setheaders(h, Authorization = paste("Bearer", getToken(force = attempt > 1)))
    handle_setopt(h, followlocation = TRUE, unrestricted_auth = TRUE)
    res <- tryCatch(curl_download(sprintf("https://download.dataspace.copernicus.eu/odata/v1/Products(%s)/$value", man$id[i]), f, handle = h, quiet = TRUE), error = function(e) NULL)
    if (!is.null(res) && file.exists(f) && unname(tools::md5sum(f)) == man$md5[i]) { ok <- TRUE; break }
  }
  if (ok) done <- done + 1L else { failed <- c(failed, man$name[i]); if (file.exists(f)) file.remove(f) }
  if (i %% 25 == 0) cat(sprintf("%d of %d files done\n", done, nrow(man)))
}
cat(sprintf("Finished: %d of %d files verified. Failed: %d\n", done, nrow(man), length(failed)))
if (length(failed)) { writeLines(failed, file.path(root, "clms_failed.txt")); cat("Failed names written to ", file.path(root, "clms_failed.txt"), ". Run the script again.\n") }
