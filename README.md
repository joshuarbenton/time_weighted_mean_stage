# time_weighted_mean_stage
A time weighted mean stage calculator for discharge measurements collected at NWIS hydrologic stations.

This app calculates the time-weighted mean stage for a user specified time-interval using instantaneous (15-min interval) NWIS stage data. Specifically, it is designed to calculate a representative stage value for a discharge measurement. The app applies a linear interpolation between NWIS data points to estimate the measurement start and ending stage (red points), calculates the mean stage (MS) and time-lag (TL) between successive datapoints, and then calculates a time weighted average of all MS values where each MS value is weighted by the corresponding time-lag.      
