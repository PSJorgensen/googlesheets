% Generated by roxygen2 (4.1.0.9001): do not edit by hand
% Please edit documentation in R/register.R
\name{list_sheets}
\alias{list_sheets}
\title{Get a listing of spreadsheets}
\usage{
list_sheets()
}
\value{
a tbl_df, one row per sheet
}
\description{
Lists spreadsheets that the authorized user would see in the Google Sheets
home screen: \url{https://docs.google.com/spreadsheets/}. For these sheets,
get sheet title, sheet key, owner, user's permission, date-time of last
update, version (old vs new Sheets), various links, and an alternative key
(only relevant to old Sheets).
}
\details{
This function returns the information available from the
\href{https://developers.google.com/google-apps/spreadsheets/#retrieving_a_list_of_spreadsheets}{spreadsheets
feed} of the Google Sheets API.

This listing give the user a partial view of the sheets available for access
(why just partial? see below). It also gives a map between readily available
information, such as sheet title, and more obscure information you might use
in scripts, such as the sheet key. This sort of "table lookup" is implemented
in the \code{googlesheets} helper function \code{\link{identify_ss}}.

Which sheets show up here? Certainly those owned by the authorized user. But
also a subset of the sheets owned by others but visible to the authorized
user. We have yet to find explicit Google documentation on this matter.
Anecdotally, sheets shared by others seem to appear in this listing if the
authorized user has visited them in the browser. This is an important point
for usability because a sheet can be summoned by title instead of key only if
it appears in this listing. For shared sheets that may not appear in this
listing, a more robust workflow is to extract the key from the browser URL
via \code{\link{extract_key_from_url}} and explicitly specify the sheet in
\code{googlesheets} functions by key.
}
\examples{
\dontrun{
list_sheets()
}
}

