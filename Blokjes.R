blok_a <- c("hop", "bin", "foo")
blok_b <- c("log", "jo", "bun")
blok_c <- c("tip", "top")

# extra element aan vector met: 
blok_metid <- c(blok_a, "ID")

# verwerken van meerdere vectoren met verschillende lengtes
# handigst met een 'list', daarin mogen allerlei objecten staan
blokjes <- list(blok_a, blok_b, blok_c)

# blokje ophalen uit list in originele vorm met:
blokjes[[1]]

for (i in seq_along(blokjes)) {
  blokjes[[i]] <- c(blokjes[[i]], "ID")
}

