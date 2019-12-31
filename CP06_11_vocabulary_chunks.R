library(data.table)

dataset_gram_2_aggregated <- fread("./../skims/merged_grams/dataset_gram_2_aggregated.txt")
dataset_gram_3_aggregated <- fread("./../skims/merged_grams/dataset_gram_3_aggregated.txt")

str(dataset_gram_2_aggregated)
# 7884566
str(dataset_gram_3_aggregated)
# 14923896

gram_2_voc_01 <- dataset_gram_2_aggregated[1:500000,]
gram_2_voc_02 <- dataset_gram_2_aggregated[500001:1000000,]
gram_2_voc_03 <- dataset_gram_2_aggregated[1000001:1500000,]
gram_2_voc_04 <- dataset_gram_2_aggregated[1500001:2000000,]
gram_2_voc_05 <- dataset_gram_2_aggregated[2000001:2500000,]
gram_2_voc_06 <- dataset_gram_2_aggregated[2500001:3000000,]
gram_2_voc_07 <- dataset_gram_2_aggregated[3000001:3500000,]
gram_2_voc_08 <- dataset_gram_2_aggregated[3500001:4000000,]
gram_2_voc_09 <- dataset_gram_2_aggregated[4000001:4500000,]
gram_2_voc_10 <- dataset_gram_2_aggregated[4500001:5000000,]
gram_2_voc_11 <- dataset_gram_2_aggregated[5000001:5500000,]
gram_2_voc_12 <- dataset_gram_2_aggregated[5500001:6000000,]
gram_2_voc_13 <- dataset_gram_2_aggregated[6000001:6500000,]
gram_2_voc_14 <- dataset_gram_2_aggregated[6500001:7000000,]
gram_2_voc_15 <- dataset_gram_2_aggregated[7000001:7500000,]
gram_2_voc_16 <- dataset_gram_2_aggregated[7500001:7884566,]

write.csv(gram_2_voc_01, "./../skims/merged_grams/gram_2_voc_01.txt")
write.csv(gram_2_voc_02, "./../skims/merged_grams/gram_2_voc_02.txt")
write.csv(gram_2_voc_03, "./../skims/merged_grams/gram_2_voc_03.txt")
write.csv(gram_2_voc_04, "./../skims/merged_grams/gram_2_voc_04.txt")
write.csv(gram_2_voc_05, "./../skims/merged_grams/gram_2_voc_05.txt")
write.csv(gram_2_voc_06, "./../skims/merged_grams/gram_2_voc_06.txt")
write.csv(gram_2_voc_07, "./../skims/merged_grams/gram_2_voc_07.txt")
write.csv(gram_2_voc_08, "./../skims/merged_grams/gram_2_voc_08.txt")
write.csv(gram_2_voc_09, "./../skims/merged_grams/gram_2_voc_09.txt")
write.csv(gram_2_voc_10, "./../skims/merged_grams/gram_2_voc_10.txt")
write.csv(gram_2_voc_11, "./../skims/merged_grams/gram_2_voc_11.txt")
write.csv(gram_2_voc_12, "./../skims/merged_grams/gram_2_voc_12.txt")
write.csv(gram_2_voc_13, "./../skims/merged_grams/gram_2_voc_13.txt")
write.csv(gram_2_voc_14, "./../skims/merged_grams/gram_2_voc_14.txt")
write.csv(gram_2_voc_15, "./../skims/merged_grams/gram_2_voc_15.txt")
write.csv(gram_2_voc_16, "./../skims/merged_grams/gram_2_voc_16.txt")


gram_3_voc_01 <- dataset_gram_3_aggregated[1:500000,]
gram_3_voc_02 <- dataset_gram_3_aggregated[500001:1000000,]
gram_3_voc_03 <- dataset_gram_3_aggregated[1000001:1500000,]
gram_3_voc_04 <- dataset_gram_3_aggregated[1500001:2000000,]
gram_3_voc_05 <- dataset_gram_3_aggregated[2000001:2500000,]
gram_3_voc_06 <- dataset_gram_3_aggregated[2500001:3000000,]
gram_3_voc_07 <- dataset_gram_3_aggregated[3000001:3500000,]
gram_3_voc_08 <- dataset_gram_3_aggregated[3500001:4000000,]
gram_3_voc_09 <- dataset_gram_3_aggregated[4000001:4500000,]
gram_3_voc_10 <- dataset_gram_3_aggregated[4500001:5000000,]
gram_3_voc_11 <- dataset_gram_3_aggregated[5000001:5500000,]
gram_3_voc_12 <- dataset_gram_3_aggregated[5500001:6000000,]
gram_3_voc_13 <- dataset_gram_3_aggregated[6000001:6500000,]
gram_3_voc_14 <- dataset_gram_3_aggregated[6500001:7000000,]
gram_3_voc_15 <- dataset_gram_3_aggregated[7000001:7500000,]
gram_3_voc_16 <- dataset_gram_3_aggregated[7500001:8000000,]
gram_3_voc_17 <- dataset_gram_3_aggregated[8000001:8500000,]
gram_3_voc_18 <- dataset_gram_3_aggregated[8500001:9000000,]
gram_3_voc_19 <- dataset_gram_3_aggregated[9000001:9500000,]
gram_3_voc_20 <- dataset_gram_3_aggregated[9500001:10000000,]
gram_3_voc_21 <- dataset_gram_3_aggregated[10000001:10500000,]
gram_3_voc_22 <- dataset_gram_3_aggregated[10500001:11000000,]
gram_3_voc_23 <- dataset_gram_3_aggregated[11000001:11500000,]
gram_3_voc_24 <- dataset_gram_3_aggregated[11500001:12000000,]
gram_3_voc_25 <- dataset_gram_3_aggregated[12000001:12500000,]
gram_3_voc_26 <- dataset_gram_3_aggregated[12500001:13000000,]
gram_3_voc_27 <- dataset_gram_3_aggregated[13000001:13500000,]
gram_3_voc_28 <- dataset_gram_3_aggregated[13500001:14000000,]
gram_3_voc_29 <- dataset_gram_3_aggregated[14000001:14500000,]
gram_3_voc_30 <- dataset_gram_3_aggregated[14500001:14923896,]




write.csv(gram_3_voc_01, "./../skims/merged_grams/gram_3_voc_01.txt")
write.csv(gram_3_voc_02, "./../skims/merged_grams/gram_3_voc_02.txt")
write.csv(gram_3_voc_03, "./../skims/merged_grams/gram_3_voc_03.txt")
write.csv(gram_3_voc_04, "./../skims/merged_grams/gram_3_voc_04.txt")
write.csv(gram_3_voc_05, "./../skims/merged_grams/gram_3_voc_05.txt")
write.csv(gram_3_voc_06, "./../skims/merged_grams/gram_3_voc_06.txt")
write.csv(gram_3_voc_07, "./../skims/merged_grams/gram_3_voc_07.txt")
write.csv(gram_3_voc_08, "./../skims/merged_grams/gram_3_voc_08.txt")
write.csv(gram_3_voc_09, "./../skims/merged_grams/gram_3_voc_09.txt")
write.csv(gram_3_voc_10, "./../skims/merged_grams/gram_3_voc_10.txt")
write.csv(gram_3_voc_11, "./../skims/merged_grams/gram_3_voc_11.txt")
write.csv(gram_3_voc_12, "./../skims/merged_grams/gram_3_voc_12.txt")
write.csv(gram_3_voc_13, "./../skims/merged_grams/gram_3_voc_13.txt")
write.csv(gram_3_voc_14, "./../skims/merged_grams/gram_3_voc_14.txt")
write.csv(gram_3_voc_15, "./../skims/merged_grams/gram_3_voc_15.txt")
write.csv(gram_3_voc_16, "./../skims/merged_grams/gram_3_voc_16.txt")
write.csv(gram_3_voc_17, "./../skims/merged_grams/gram_3_voc_17.txt")
write.csv(gram_3_voc_18, "./../skims/merged_grams/gram_3_voc_18.txt")
write.csv(gram_3_voc_19, "./../skims/merged_grams/gram_3_voc_19.txt")
write.csv(gram_3_voc_20, "./../skims/merged_grams/gram_3_voc_20.txt")
write.csv(gram_3_voc_21, "./../skims/merged_grams/gram_3_voc_21.txt")
write.csv(gram_3_voc_22, "./../skims/merged_grams/gram_3_voc_22.txt")
write.csv(gram_3_voc_23, "./../skims/merged_grams/gram_3_voc_23.txt")
write.csv(gram_3_voc_24, "./../skims/merged_grams/gram_3_voc_24.txt")
write.csv(gram_3_voc_25, "./../skims/merged_grams/gram_3_voc_25.txt")
write.csv(gram_3_voc_26, "./../skims/merged_grams/gram_3_voc_26.txt")
write.csv(gram_3_voc_27, "./../skims/merged_grams/gram_3_voc_27.txt")
write.csv(gram_3_voc_28, "./../skims/merged_grams/gram_3_voc_28.txt")
write.csv(gram_3_voc_29, "./../skims/merged_grams/gram_3_voc_29.txt")
write.csv(gram_3_voc_30, "./../skims/merged_grams/gram_3_voc_30.txt")



