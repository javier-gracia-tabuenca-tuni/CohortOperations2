# settings
testConfigFile <- "test_config_eunomia.yml"

configurationList <- yaml::read_yaml(testthat::test_path("config", testConfigFile))

message("************* Testing on ", testConfigFile, " *************")
