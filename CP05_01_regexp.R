library(data.table)
library(tidyverse)

merged_gram_1 <- fread("./../skims/merged_grams/dataset_gram_1_aggregated.txt")
merged_gram_2 <- fread("./../skims/merged_grams/dataset_gram_2_aggregated.txt")
merged_gram_3 <- fread("./../skims/merged_grams/dataset_gram_3_aggregated.txt")

str(merged_gram_1)
#Classes ‘data.table’ and 'data.frame':	477305 obs. of  3 variables:
str(merged_gram_2)
#Classes ‘data.table’ and 'data.frame':	7884566 obs. of  3 variables:
str(merged_gram_3)
#Classes ‘data.table’ and 'data.frame':	14923896 obs. of  3 variables:

#   .{()\^$|?*+
#escape with \

#warm-up examples
filter(merged_gram_1, str_detect(word, "^aaa"))
filter(merged_gram_1, str_detect(word, "^hhtpwww")) 
filter(merged_gram_1, str_detect(word, "^www")) 

filter(merged_gram_1, str_detect(word, "aaa$"))
filter(merged_gram_1, str_detect(word, "yyy$"))

#or
filter(merged_gram_1, str_detect(word, "(aaa|yyy)$"))

#don't want to filter out words like "attribute"
filter(merged_gram_1, str_detect(word, "ttr"))

#  "[a-z]$"
#  "[a-zA-Z]$"

merged_gram_1[1:10]
merged_gram_1[1:10] %>% filter(str_detect(word, "[a-z]*$"))

merged_gram_1[1:20000] %>% filter(str_detect(word, "[a-z]{15,}"))

select_long_words <- merged_gram_1 %>% filter(str_detect(word, "[a-z]{16,}"))
str(select_long_words)
#'data.frame':	28271 obs. of  3 variables:

# b[^aeiouAEIOU\s][a-zA-Z]*[aeiouAEIOU]\b

merged_gram_1[1:20000] %>% filter(str_detect(word, "[aeiou]{3,}"))
merged_gram_1[1:20000] %>% filter(str_detect(word, "[aeiou]{4,}"))

select_many_vowels <- merged_gram_1 %>% filter(str_detect(word, "[aeiou]{5,}"))
str(select_many_vowels)
#'data.frame':	2400 obs. of  3 variables:

merged_gram_1[1:20000] %>% filter(str_detect(word, "[b-df-hj-np-tv-z]{3,}"))
merged_gram_1[1:20000] %>% filter(str_detect(word, "[b-df-hj-np-tv-z]{4,}"))
merged_gram_1[1:20000] %>% filter(str_detect(word, "[b-df-hj-np-tv-z]{5,}"))

# treat y as a vowel
select_many_vowels_y <- merged_gram_1 %>% filter(str_detect(word, "[aeiouy]{5,}"))
str(select_many_vowels_y)
#'data.frame':	3371 obs. of  3 variables:

merged_gram_1[1:20000] %>% filter(str_detect(word, "[b-df-hj-np-tv-xz]{5,}"))

# treat h as a vowel
select_many_vowels_yh <- merged_gram_1 %>% filter(str_detect(word, "[aeiouyh]{5,}"))
str(select_many_vowels_yh)
#'data.frame':	6422 obs. of  3 variables:

merged_gram_1[1:20000] %>% filter(str_detect(word, "[b-df-gj-np-tv-xz]{5,}"))

select_many_consonants <- merged_gram_1[1:40000] %>% 
  filter(str_detect(word, "[b-df-gj-np-tv-xz]{6,}"))

select_http_www <- merged_gram_1 %>% filter(str_detect(word, "(^http|^www)")) 
str(select_http_www)
#'data.frame':	5662 obs. of  3 variables:

#first round of outs
select_round_1  <- merged_gram_1 %>% filter(str_detect(word, 
                    "(^http|^www|[a-z]{16,}|[aeiouyh]{5,}|[b-df-gj-np-tv-xz]{6,})")) 
str(select_round_1)
#'data.frame':	36623 obs. of  3 variables:

merged_gram_1_round_2 <- merged_gram_1 %>% filter(!word %in% (select_round_1$word))
str(merged_gram_1)
#Classes ‘data.table’ and 'data.frame':	477305 obs. of  3 variables:
str(merged_gram_1_round_2)
#'data.frame':	440682 obs. of  3 variables:

found_once <- merged_gram_1_round_2 %>% filter(freq == 1) %>% select(word)

table(merged_gram_1_round_2$freq)
#1      2     3     4     5      6      7      8      9     10     11     12     13     14 
#285317 52248 21897 12543 8392   5930   4600   3681   2915   2369   2118   1900 1616  1368 

merged_gram_1_highfreq <- merged_gram_1_round_2 %>% filter(freq > 20)
str(merged_gram_1_highfreq)
plot(merged_gram_1_highfreq$freq)

merged_gram_1_vhighfreq <- merged_gram_1_round_2 %>% filter(freq > 1000)
str(merged_gram_1_vhighfreq)
#2921
plot(merged_gram_1_vhighfreq$freq)

merged_gram_1_vhighfreq$word[1:1000]


