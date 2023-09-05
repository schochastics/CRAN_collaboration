getAuthor <- function(x) {
  if (is.na(x)) {
    return(NA)
  }
  x <- stringr::str_remove_all(x, "#.*$")
  x <- paste0("format(", x, ",include=c('given','family'))")
  a <- textConnection(x)
  on.exit(close(a))
  dget(a)
}

author_cleaner <- function(db) {
  authors <- lapply(db$"Authors@R", getAuthor)
  authors <- suppressWarnings(map(authors, function(x) {
    stringr::str_remove_all(x, "<.*>|\\[.*\\]") |>
      stringr::str_remove_all("\\s*[[:alnum:].-_]+@[[:alnum:].-]+$")
  }))
  del_authors <- readLines("data/delete_authors.txt")
  pkgs <- db$Package
  bip <- db %>%
    as_tibble() %>%
    select(Package, Author) %>%
    dplyr::mutate(Author = stringr::str_remove_all(stringr::str_squish(Author), "<.*>|\\[.*\\]")) |>
    dplyr::mutate(Author = stringr::str_trim(stringr::str_remove_all(Author, "\\s*and contributors|\\s*with.*"))) |>
    dplyr::mutate(authorsR = authors) %>%
    tidyr::unnest(cols = authorsR) %>%
    dplyr::mutate(authorsR = coalesce(authorsR, Author)) %>%
    dplyr::mutate(authorsR = stringr::str_replace_all(authorsR, "Alain Vandal Robert Gentleman", "Alain Vandal, Robert Gentleman")) |>
    dplyr::mutate(authorsR = stringr::str_split(authorsR, ",| and |, and|,and|&|;| / | e ")) |>
    tidyr::unnest(cols = authorsR) |>
    select(-Author) %>%
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "\\(|\\)")) %>%
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "\\[.*\\]") %>% stringr::str_trim()) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " SpookyHash algorithm|1999-2016 The ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, ". R port by Gavin L. Simpson. Function simpls based on simpls.fit package pls by Ron Wehrens")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " see COPYRIGHTS file for the authors of the java libraries")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, '"')) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, ". Function fbvpot by Chris Ferro.")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " FORTRAN code lsq.f90: weighted least-squares module")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Project contact person: Melchinger@uni-hohenheim.de")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Fortran routines BNVU")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, ". Ported to R by B. D. Ripley up to version 2.0")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " R code")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " based on Daniel Meyer's code.")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "\\s+\\.$")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Department of.*")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Developer")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Compiled by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Md.|MD |Md ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "GNU Octave code")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " partly based on code from Robert Gentleman")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " for the Pacific Climate Impacts Consortium")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, ". Art Owen for el.test. Yifan Yang for some C code.")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " under the supervision of Dr. Mai Zhou")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Original by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " programming")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "[0-9]{4}\\-[0-9]{4}\\-[0-9]{4}\\-[0-9]{4}")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " based on .*")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Stanford Chad Hazlett UCLA")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " aka Produnis")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Original S functions written by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " author of the C\\+\\+ RNG code")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Scientific Software - Dr. ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "R port by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " \\\\email\\{paul.conn@@noaa.gov\\}")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Jie Fu Yu made a prototype")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Originally written for S-Plus by: ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Foundation SmarterPoland.pl")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, ". Author of h2 is Thomas Mueller.")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "originally written by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " . Ported to S-PLUS by Insightful Corp.")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "originally written as R2WinBUGS by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " . Adapted to R2OpenBUGS from R2WinBUGS by Neal Thomas.")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Fortran contributions from Cleve Moler dposl/LINPACK")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "S original by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " for CVODE/SUNDIALS")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " \\| file AUTHORS")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " InfoFarm")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "originally written by Subbiah M packaged by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Ported to R by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Technology NIST http://www.itl.nist.gov/div898/strd/nls/nls_main.shtml R port by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Package developer")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " HHU of Duesseldorf / Germany")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Fortran Code")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " C function 'BinDist2' obtained from package 'stats'")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " the.matrix data frame")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "based on the program by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Fortran code by R. J. Renka. R functions by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "E. D. Chow porting to R by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "\\s*Ph\\.D\\.")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, ". Uses Alan Miller's Fortran utilities")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, ". Derived from mda:mars by Trevor Hastie")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Univ Lille-Nord de France")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " http://joshuamccrain.com/")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " https://caleblucas.com/")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "following earlier work by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " R package")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, ". Original by Joseph L. Schafer .")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " -- Universita' degli Studi di Milano")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " \\\\")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " original C++ code from Arnost Komarek based on ars.f written by P. Wild")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "contributions from ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "example for simHMM from ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, ". R version by Ray Brownrigg .")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Packaged for R by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " S original")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "S scripts originally by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " . Toplevel R functions")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " see file LICENSEMEDIA for credits on sounds")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " https://www.wur.nl/en/Persons/Sanne-dr.-SJP-Sanne-van-den-Berg.htm")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " https://pabrod.github.io")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "original C code for ARMS by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Prof. ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " CNRS|MNHN")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " MNHN")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " et al. cph")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " USDA-ARS")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "apart from a set of Fortran-77 subroutines written by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " State Office for Consumer Protection Saxony-Anhalt")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " C original")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "GUI by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "R interface etc by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " \\code\\{import.ascii\\}")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "database version of the dictionary ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " for the original dictionary")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Fortran code by Alan Genz. by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Universite du Quebec a Montreal")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Universite de Liege")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "NAEP Primer data by Emmanuel Sikali. Creation of R package by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " for 'libnabo'")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "enhanced by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "\\. Contributors: Clement Chevalier")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "tools/make_cpp.R")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "\\. Includes R source code and/or documentation written by Dirk Enzmann")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " \\. src/depth.f contains eigen")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " www.hamedhaseli.webs.com")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Additional contributions Ziyu Zheng")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Additional contributions Bikash Parida Jacob Davies")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "https://orcid.org/[0-9]{4}\\-[0-9]{4}\\-[0-9]{4}\\-[0-9]{4}")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "S original by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Dr\\. |Dr ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "Original S code by ")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Universite de Bretagne Sud")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Institut Pasteur")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " Agrocampus Ouest")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "\\. Earlier developements by Holger Dette")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, " https://orcid.org/")) |>
    dplyr::mutate(authorsR = stringr::str_remove_all(authorsR, "\\s*[[:alnum:].-_]+@[[:alnum:].-]+$")) |>
    dplyr::filter(authorsR != "" & !authorsR %in% pkgs & !stringr::str_detect(authorsR, "with contributions ")) |>
    dplyr::filter(!stringr::str_detect(authorsR, "University|Universidad|contributors|Institute|Army Research")) |>
    dplyr::filter(!authorsR %in% del_authors) |>
    dplyr::mutate(authorsR = stringr::str_squish(authorsR))

  # 	Csillery Katalin [aut], Lemaire Louisiane [aut], Francois Olivier [aut], Blum Michael [aut, cre]
  bip <- bip |>
    dplyr::mutate(authorsR = if_else(authorsR %in% c("Vincent A. Traag", "V.A. Traag", "Vincent Traag"), "Vincent A. Traag", authorsR)) |>
    dplyr::mutate(authorsR = if_else(authorsR == "Csillery Katalin", "Katalin Csillery", authorsR)) |>
    dplyr::mutate(authorsR = if_else(authorsR == "Lemaire Louisiane", "Louisiane Lemaire", authorsR)) |>
    dplyr::mutate(authorsR = if_else(authorsR == "Francois Olivier", "Olivier Francois", authorsR)) |>
    dplyr::mutate(authorsR = if_else(authorsR == "Blum Michael", "Michael Blum", authorsR)) |>
    dplyr::mutate(authorsR = if_else(authorsR == "Alicja Wolny--Dominiak", "Alicja Wolny-Dominiak", authorsR)) |>
    dplyr::mutate(authorsR = if_else(authorsR == "Boettiger Carl", "Carl Boettiger", authorsR)) |>
    dplyr::mutate(authorsR = if_else(authorsR == "Sy Han Steven Chiou", "Sy Han Chiou", authorsR)) |>
    dplyr::mutate(authorsR = if_else(authorsR %in% c("R Core Team", "R Core team", "The R Core Team", "The R Core team"), "R Core Team", authorsR)) |>
    dplyr::mutate(authorsR = if_else(authorsR %in% c("Rstudio", "Rstudio Inc.", "RStudio Inc.", "RStudio PBC"), "RStudio", authorsR)) |>
    dplyr::mutate(authorsR = stringr::str_trim(stringr::str_to_title(authorsR))) |>
    dplyr::mutate(authorsR = iconv(authorsR, to = "ASCII//TRANSLIT"))
  bip
}
