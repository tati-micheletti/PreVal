checkMovebankCredentials <- function(out){
  if ("caribouLocPrep" %in% out$modules){
    if (any("YT" %in% out$params$caribouLocPrep$jurisdiction,
            "NT" %in% out$params$caribouLocPrep$jurisdiction)){
      if (out$params$caribouLocPrep$MoveBankUser == "" || out$params$caribouLocPrep$MoveBankPass == "") {
        errorMessage <- paste(
          "Error: You requested data hosted in MoveBank, but Movebank credentials were not found.",
          "To solve this, please follow these steps in your R console:",
          "1. Install the 'usethis' package if you haven't already:",
          "   install.packages('usethis')",
          "2. Run the following command. It will open a special file named '.Renviron' for editing:",
          "   usethis::edit_r_environ()",
          "3. Add the following two lines to the '.Renviron' file, replacing the placeholders",
          "   with your actual Movebank username and password:",
          "   MOVEBANK_USERNAME=\"your_username_here\"",
          "   MOVEBANK_PASSWORD=\"your_password_here\"",
          "4. Save the '.Renviron' file and **IMPORTANT: Restart your R session**.",
          "   (In RStudio: Session > Restart R or Ctrl+Shift+F10)",
          "After restarting, re-run this script.",
          "ALTERNATIVELY, simply pass both `MoveBankPass` and `MoveBankUser` as parameters to the module.",
          sep = "\n"
        )
        stop(errorMessage)
      } else {
        message("Movebank credentials seem to be correctly set.")
      }
    }
  }
  }
