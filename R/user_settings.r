# These should be set for each specific user
# TODO set up git smudge filter so the specifics are not committed
#  see https://stackoverflow.com/questions/6557467/can-git-ignore-a-specific-line
glopnet_dir = switch(system('hostname', intern=T),
                     "ouce39-131" = "E:/Documents - BTSync/Databases/GLOPNET/",
                     "Allie-PC" = "D:/Documents/Databases/GLOPNET/")
glopnet_file = paste0(glopnet_dir, "nature02403-s2.xls")

try_dir = switch(system('hostname', intern=T),
                 "ouce39-131" = "E:/Documents - BTSync/Databases/TRY/",
                 "Allie-PC" = "D:/Documents/Databases/TRY/")

try_traits_file = paste0(try_dir,"1431.txt")
try_site_file = paste0(try_dir, "TRY30_site_climate_soil_2015_02_18.csv")
try_categ_file = paste0(try_dir, "TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease.csv")

baad_dir = switch(system('hostname', intern=T),
                  "ouce39-131" = "E:/Documents - BTSync/Databases/baad archive/baad_data/",
                  "Allie-PC" = "D:/Documents/Databases/baad archive/baad_data/")
baad_file = paste0(baad_dir,"baad_data.csv")

gwdd_dir = switch(system('hostname', intern=T),
                  "ouce39-131" = "E:/Documents - BTSync/Databases/Global Wood Density Database/",
                  "Allie-PC" = "D:/Documents/Databases/Global Wood Density Database/")
gwdd_file = paste0(gwdd_dir, "GlobalWoodDensityDatabase.xls")
