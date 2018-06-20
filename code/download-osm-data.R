library (popdens)

message ("getting kathmandu streets ... ", appendLF = FALSE)
get_who_streets (city = "kathmandu", n = 1)
message ("\ndone; getting kathmandu buildings ... ", appendLF = FALSE)
get_who_buildings (city = "kathmandu", n = 2)

message ("\ndone; getting accra streets ... ", appendLF = FALSE)
get_who_streets (city = "accra", n = 1)
message ("\ndone; getting accra buildings ... ", appendLF = FALSE)
get_who_buildings (city = "accra", n = 1)

message ("\ndone; getting bristol streets ... ", appendLF = FALSE)
get_who_streets (city = "bristol", n = 1)
message ("\ndone; getting bristol buildings ... ", appendLF = FALSE)
get_who_buildings (city = "bristol", n = 2)
message ("\ndone")
