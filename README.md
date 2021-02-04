Faculty\_Homework
================
Mike Hatfield
2/4/2021

``` r
library(rvest)
```

    ## Loading required package: xml2

``` r
library(stringr)

faculty_html <- read_html("https://guide.wisc.edu/faculty/")
chunks <- html_nodes(faculty_html, ".uw-people p")
strings <- rep("none", length(chunks))
for (i in 1:length(chunks)) {
  xml_find_all(chunks[i], ".//br") %>% xml_add_sibling("p", "\n")
  xml_find_all(chunks[i], ".//br") %>% xml_remove()
  strings[i] = html_text(chunks[i]) %>% str_trim()
}

firstname <- rep("none", length(strings))
lastname <- rep("none", length(strings))
role <- rep("none", length(strings))
department <- rep("none", length(strings))
education <- rep("none", length(strings))

for (i in 1:length(strings)) {
ps <- strsplit(strings[i], "\n")
name <- strsplit(ps[[1]][1], ",")
firstname[i] = str_trim(name[[1]][2])
lastname[i] = name[[1]][1]
role[i] = ps[[1]][2]
department[i] = ps[[1]][3]
education[i] = ps[[1]][4]
}

middlename <- rep(NA, length(firstname))

for (i in 1: length(firstname)) {
  if(is.na(str_locate(firstname[i], " ")[1])) {
    firstn = firstname[i]
    middlen = NA
  }
  else {
    firstn = substring(firstname[i], 1, str_locate(firstname[i], " ")[1]-1)
    middlen = substring(firstname[i], str_locate(firstname[i], " ")[1]+1, nchar(firstname[i]))
  }
  firstname[i] = firstn
  middlename[i] = middlen
}

faculty <- data.frame("First" = firstname, "Middle" = middlename,
                      "Last" = lastname,
                      "Position" = role, "Department" = department,
                      "Degree" = education)

faculty$Department <- as.factor(faculty$Department)
faculty$Position <- as.factor(faculty$Position)

#summary(faculty)

write.csv(faculty, file = "faculty.csv")
```

``` r
head(faculty)
```

    ##      First Middle        Last              Position                 Department
    ## 1     LISA   <NA>       AARLI              Lecturer      Counseling Psychology
    ## 2   DANIEL      E      ABBOTT Assoc Professor (CHS)                    Surgery
    ## 3    DAVID     H.      ABBOTT             Professor    Obstetrics & Gynecology
    ## 4 NICHOLAS   <NA>      ABBOTT             Professor Chemical & Biological Engr
    ## 5     ALAA      A ABD-ELSAYED  Asst Professor (CHS)             Anesthesiology
    ## 6   FAISAL   <NA>   ABDUALLAH   Associate Professor                        Art
    ##                                Degree
    ## 1  EDM 2000 Univ of Wisconsin-Madison
    ## 2    MD 2016 University of Washington
    ## 3    PHD 1979 University of Edinburgh
    ## 4 PHD 1991 Massachusetts Inst Of Tech
    ## 5                             MD 2000
    ## 6       PHD 2012 Royal College of Art
