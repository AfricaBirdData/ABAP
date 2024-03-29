# Most things in this package depend on the birdmap API being reachable
skip_if_offline(host = "api.birdmap.africa")

# Selection of species to test
spp <- c(95, 10015, 91, 103, 1016, 96, 89, 88, 102, 92, 1342, 1340, 90, 94, 98, 99,
         97, 101, 104, 100, 274, 236, 233, 239, 237, 955, 238, 235, 245, 248, 246, 240,
         241, 273, 282, 281, 231, 228, 229, 288, 289, 287, 4136, 2058, 306, 292,
         294, 903, 290, 291, 300, 297, 299, 293, 296, 298, 305, 304, 269, 270, 230,
         254, 256, 255, 258, 251, 259, 262, 257, 264, 250, 253, 232, 267, 266, 265, 263,
         268, 52, 66, 67, 61, 58, 59, 4150, 60, 64, 55, 56, 63, 54, 57, 65, 62, 69, 70, 74,
         79, 73, 75, 77, 76, 42, 41, 49, 48, 51, 50, 47, 86, 87, 5, 4, 6, 72, 81, 83, 84, 85,
         396, 397, 394, 395, 400, 149, 167, 166, 172, 216, 214, 215, 213, 199, 202, 203, 205,
         207, 209, 210, 211, 197, 208, 212, 370, 361, 360)


# Selection of years to test
years <- 2008:2022
