## FUNCTIONS -------------------------------------------------------------------

# subset the DRAC reference list
# 'x' is the input table from use_DRAC()
get_DRAC_references <- function(x) {
  
  refs <- DRAC_refs()
  refs_names <- names(refs)
  
  used <- list(refs = NULL, desc = NULL)
  
  # TI:4 - Conversion factors
  ref_tmp <- unique(x$`TI:4`)
  for (i in 1:length(ref_tmp)) {
    if (ref_tmp[i] == "X") 
      next
    used$refs <- c(used$refs, refs[refs_names %in% ref_tmp[i]])
    used$desc <- c(used$desc, "Conversion factors")
  }
  
  # TI:13 - External Rubidium
  ref_tmp <- unique(x$`TI:13`)
  if (any(ref_tmp == "Y")) {
    used$refs <- c(used$refs, refs["Mejdahl1987"])
    used$desc <- c(used$desc, "External rubidium")
  }
  
  # TI:22 - Internal Rubidium
  ref_tmp <- unique(x$`TI:22`)
  if (any(ref_tmp == "Y")) {
    used$refs <- c(used$refs, refs["Mejdahl1987"])
    used$desc <- c(used$desc, "Internal rubidium")
  }
  
  # TI:31 - Gamma dose rate scaling
  ref_tmp <- unique(x$`TI:31`)
  if (any(ref_tmp == "Y")) {
    used$refs <- c(used$refs, refs["Aitken1985"])
    used$desc <- c(used$desc, "Gamma dose rate scaling")
  }
  
  # TI:34 - alpha grain size attenuation
  ref_tmp <- unique(x$`TI:34`)
  for (i in 1:length(ref_tmp)) {
    if (ref_tmp[i] == "X") 
      next
    used$refs <- c(used$refs, refs[refs_names %in% ref_tmp[i]])
    used$desc <- c(used$desc, "Alpha grain size attenuation factors")
  }
  
  # TI:35 - Beta grain size attenuation
  ref_tmp <- unique(x$`TI:35`)
  for (i in 1:length(ref_tmp)) {
    if (ref_tmp[i] == "X") 
      next
    used$refs <- c(used$refs, refs[refs_names %in% ref_tmp[i]])
    used$desc <- c(used$desc, "Beta grain size attenuation factors")
  }
  
  # TI:38 - beta etch attenuation factor
  ref_tmp <- unique(x$`TI:38`)
  for (i in 1:length(ref_tmp)) {
    if (ref_tmp[i] == "X") 
      next
    used$refs <- c(used$refs, refs[refs_names %in% ref_tmp[i]])
    used$desc <- c(used$desc, "Beta etch attenuation factor")
  }
  
  # TI:50 - Cosmic dose rate
  ref_tmp <- unique(x$`TI:50`)
  if (any(ref_tmp == "X")) {
    used$refs <- c(used$refs, refs[c("PrescottHutton1994", "PrescottStephan1982")])
    used$desc <- c(used$desc, c("Cosmic dose rate", "Cosmic dose rate"))
  }
 
  return(used)
}

## REFERENCE LIST --------------------------------------------------------------
DRAC_refs <- function() {
  
  list(
    Aitken1985 = bibentry(
      bibtype = "Book", 
      author = person("M.J.", "Aitken"), 
      title = "Thermoluminescence Dating", 
      year = "1985",
      publisher = "Academic Press",
      adress = "London"
    ),
    
    AitkenXie1990 = bibentry(
      bibtype = "Article", 
      author = c(
        person("M.J.", "Aitken"),
        person("J.", "Xie")
      ), 
      title = "Moisture correction for annual gamma dose", 
      year = "1990",
      journal = "Ancient TL",
      volume = "8",
      pages = "6-9"
    ),
    
    AdamiecAitken1998 = bibentry(
      bibtype = "Article", 
      author = c(
        person("G.", "Adamiec"),
        person("M.J.", "Aitken")
      ),
      title = "Dose-rate conversion factors: update", 
      year = "1998",
      journal = "Ancient TL",
      volume = "16",
      pages = "37-46"
    ),
    
    Guerinetal2011 = bibentry(
      bibtype = "Article", 
      author = c(
        person("G.", "Guerin"),
        person("N.", "Mercier"),
        person("G.", "Adamiec")
      ), 
      title = "Dose-rate conversion factors: update", 
      year = "2011",
      journal = "Ancient TL",
      volume = "29",
      pages = "5-8"
    ),
    
    Liritzisetal2013 = bibentry(
      bibtype = "Article", 
      author = c(
        person("I.", "Liritzis"),
        person("K.", "Stamoulis"),
        person("C.", "Papachristodoulou"),
        person("K.", "Ioannides")
      ), 
      title = "A re-evaluation of radiation dose-rate conversion factors. ", 
      year = "2013",
      journal = "Mediterranean Archaeology and Archaeometry",
      volume = "13",
      pages = "1-15"
    ),
    
    Bell1979 = bibentry(
      bibtype = "Article", 
      author = c(
        person("W.T.", "Bell")
      ), 
      title = "Attenuation factors for the absorbed radiation dose in quartz inclusions for thermoluminescence dating", 
      year = "1979",
      journal = "Ancient TL",
      volume = "8",
      pages = "1-12"
    ),
    
    Bell1980 = bibentry(
      bibtype = "Article", 
      author = c(
        person("W.T.", "Bell")
      ), 
      title = "Alpha attenuation in Quartz grains for Thermoluminescence Dating", 
      year = "1980",
      journal = "Ancient TL",
      volume = "12",
      pages = "4-8"
    ),
    
    Brennanetal1991 = bibentry(
      bibtype = "Article", 
      author = c(
        person("B.J.", "Brennan"),
        person("R.G.", "Lyons"),
        person("S.W.", "Phillips")
      ), 
      title = "Attenuation of alpha particle track dose for spherical grains", 
      year = "1991",
      journal = "International Journal of Radiation Applications and Instrumentation. Part D. Nuclear Tracks and Radiation Measurements",
      volume = "18",
      pages = "249-253"
    ),
    
    Mejdahl1979 = bibentry(
      bibtype = "Article", 
      author = c(
        person("V.", "Mejdahl")
      ), 
      title = "Thermoluminescence Dating: Beta-Dose Attenuation in Quartz Grains", 
      year = "1979",
      journal = "Archaeometry",
      volume = "21",
      pages = "61-72"
    ),
    
    Mejdahl1987 = bibentry(
      bibtype = "Article", 
      author = c(
        person("V.", "Mejdahl")
      ), 
      title = "Internal radioactivity in quartz and feldspar grains", 
      year = "1987",
      journal = "Ancient TL",
      volume = "5",
      pages = "10-17"
    ),
    
    Brennan2003 = bibentry(
      bibtype = "Article", 
      author = c(
        person("B.J.", "Brennan")
      ), 
      title = "Beta doses to spherical grains",
      year = "2003",
      journal = "Radiation Measurements",
      volume = "37",
      pages = "299-303"
    ),
    
    `Guerinetal2012-Q` = bibentry(
      bibtype = "Article", 
      author = c(
        person("G.", "Guerin"),
        person("N.", "Mercier"),
        person("R.", "Nathan"),
        person("G.", "Adamiec"),
        person("Y.", "Lefrais")
      ), 
      title = "On the use of the infinite matrix assumption and associated concepts: A critical review", 
      year = "2012",
      journal = "Radiation Measurements",
      volume = "47",
      pages = "778-785"
    ),
    
    `Guerinetal2012-F` = bibentry(
      bibtype = "Article", 
      author = c(
        person("G.", "Guerin"),
        person("N.", "Mercier"),
        person("R.", "Nathan"),
        person("G.", "Adamiec"),
        person("Y.", "Lefrais")
      ), 
      title = "On the use of the infinite matrix assumption and associated concepts: A critical review", 
      year = "2012",
      journal = "Radiation Measurements",
      volume = "47",
      pages = "778-785"
    ),
    
    PrescottHutton1994 = bibentry(
      bibtype = "Article", 
      author = c(
        person("J.R.", "Prescott"),
        person("J.T.", "Hutton")
      ), 
      title = "Cosmic ray contributions to dose rates for luminescence and ESR dating: Large depths and long-term time variations",
      year = "1994",
      journal = "Radiation Measurements",
      volume = "23",
      pages = "497-500"
    ),
    
    PrescottStephan1982 = bibentry(
      bibtype = "Article", 
      author = c(
        person("J.R.", "Prescott"),
        person("L.G.", "Stephan")
      ), 
      title = "The contribution of cosmic radiation to the environmental dose for thermoluminescence dating",
      year = "1982",
      journal = "PACT",
      volume = "6",
      pages = "17-25"
    ),
    
    Readhead2002 = bibentry(
      bibtype = "Article", 
      author = c(
        person("M.L.", "ReadHead")
      ), 
      title = "Absorbed dose fraction for 87Rb beta particles",
      year = "2002",
      journal = "Ancient TL",
      volume = "20",
      pages = "25-29"
    )
  )
  
  
}