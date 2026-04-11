import copernicusmarine


copernicusmarine.subset(
    dataset_id="cmems-IFREMER-ATL-SST-L4-REP-OBS_FULL_TIME_SERIE",
    variables=["analysed_sst"],
    minimum_longitude=-10,
    maximum_longitude=10,
    minimum_latitude=40,
    maximum_latitude=55,
    start_datetime="2010-01-10T00:00:00",
    end_datetime="2026-04-10T00:00:00",
    output_directory="./day11/data",
    output_filename="sst_subset.nc",
)
