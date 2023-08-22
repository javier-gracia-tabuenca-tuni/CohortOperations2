# settings
testConfigFile <- "test_config_eunomia.yml"

cohortOperationsSettings <- yaml::read_yaml(testthat::test_path("config", testConfigFile))

message("************* Testing on ", testConfigFile, " *************")
