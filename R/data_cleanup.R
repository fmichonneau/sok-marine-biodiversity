chordata_classes_to_rm <- function()
    c("actinopteri", "actinopterygii",
      "cephalaspidomorphi", "agnatha",
      "amphibia", "aves", "chondrichthyes",
      "chondrichthys", "elasmobranchii",
      "holocephali", "mammalia", "myxini",
      "osteichthyes", "osteichthyes",
      "petromyzonti", "pisces", "reptilia", "unknown")

chordata_families_to_rm <- function()
    c("anarhichantidae", "anthiidae", "belontiidae",
      "branchiostegidae", "denticipitidae", "doliolunidae", "echeneidae",
      "echeneididae",
      "eleotrididae", "fritillariidae", "gobioididae", "grammistidae",
      "icelidae", "idiacanthidae", "macrorhamphosidae", "pholididae",
      "zaniolepidae", "zaniolepididae")

arthropod_classes_to_rm <- function()
    c("arachnida", "myriapoda", "protura", "symphyla", "chilopoda",
      "diplopoda", "hexapoda", "insecta", "trilobita", "unknown")

gom_phyla <- function() {
    taxa <- names(gom_taxa_to_keep())
    taxa <- strsplit(taxa, "-")
    taxa <- vapply(taxa, function(x) x[2], character(1))
    tolower(unique(taxa))
}
