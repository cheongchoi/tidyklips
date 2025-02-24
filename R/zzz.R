.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Please download klips data from  website 'https://www.kli.re.kr/klips' and unzip in the folder named 'data'"
  )
}
