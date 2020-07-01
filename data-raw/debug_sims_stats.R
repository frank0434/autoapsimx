path_sims = "c:/Data/Master/03processed-data/apsimxFilesLayers/"
pattern = "^SKL.+.db$"
tableName = "Report"
pattern_trts = "(Ashley|Iversen).+SD\\d{1,2}"
pattern_split = "(.+)(SD\\d{1,2})"
col_treatment1 = "Experiment"
col_treatment2 = "SowingDate"
mode = "Manual"
keys =  c("Experiment", "SowingDate", "Depth")
DT_observation = SW_mean
keys = c("Experiment", "SowingDate", "Depth")
col_pred = "pred_VWC"
col_obs = "ob_VWC"
# library(data.table)
# l_stats_layerKL = sims_stats_multi(path_sims = "C:/Data/Master/03processed-data/apsimxFilesLayers/",
#                                                pattern = "^SKL.+.db$",
#                                                DT_observation = DT_observation,
#                                                mode = "Manual",
#                                                keys = c("Experiment", "SowingDate", "Depth"))
