library(data.table)

gdxtools::igdx(dirname(Sys.which('gams'))) # Please have gams in your PATH!

gdxfile = "/home/lolow/Seafile/WITCH/witch/input/build/data_mod_wind.gdx"
gbfile = "/home/lolow/Seafile/WITCH/witch/input/build/data_globiom.sqlite"
reg_id = "witch17"
time_id = "t30"

# Region mappings
region_mapping_files = Sys.glob(file.path(system.file("regions", package = "witchdata"),"*.inc"))
region_mappings <- lapply(region_mapping_files, load_region_mapping)
region_definitions <- lapply(region_mapping_files, load_region_definition)
names(region_mappings) <- names(region_definitions) <- stringr::str_sub(basename(region_mapping_files), 1, -5)

# Timescale mappings
time_mapping_files = Sys.glob(file.path(system.file("timescale", package = "witchdata"), "*.csv"))
time_mappings = lapply(time_mapping_files, load_timescale_mapping)
names(time_mappings) = stringr::str_sub(basename(time_mapping_files), 1, -5)

output_dir = '~/Seafile/WITCH/witch/data_witch17_test'

data_directory = normalizePath('~/Seafile/WITCH/witch-data')

weights = load_weights(data_directory, region_mappings)

default_meta_param = witch_default_meta_param()

guess_input_n = "witch17"
guess_input_t = "t30"
default_missing_values = "zero"

#lauch convert_gdx
