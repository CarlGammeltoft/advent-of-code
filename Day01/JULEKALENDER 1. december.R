##### JULEKALENDER 1. december ##### 

# 01_mock_data -----------------------------------------------------------------
strings <- c("1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet")

numbers <- c(1:4)

df_mock <- data.frame(strings, numbers)

# Rigtigt data
library(readxl)
df <- read_excel("C:/Users/caga/OneDrive - Epinion/DATA_1_DECEMBER.xlsx")
View(df)

# 02_regex ----------------------------------------------------------------
# Laver ny string kun med tal
df <- df %>% 
  mutate(strings_trimmed = gsub("[a-z]", "", strings)) %>% 
  mutate(strings_trimmed_num = as.numeric(strings_trimmed))

# Bruger if-else til at finde "okay", "for mange" og "for få" tilfælde
df <- df %>% 
  mutate(strings_okay = ifelse(strings_trimmed_num >= 10 & strings_trimmed_num <= 99, 1, 0),
         string_too_many = ifelse(strings_trimmed_num >= 99, 1, 0),
         string_too_few = ifelse(strings_trimmed_num < 10, 1, 0))

# Løser problemet for tilfælde med for få tal
df <- df %>% 
  mutate(strings_trimmed_num2 = case_when(strings_trimmed_num == 1 ~ 11, strings_trimmed_num == 2 ~ 22,
    strings_trimmed_num == 3 ~ 33,
    strings_trimmed_num == 4 ~ 44,
    strings_trimmed_num == 5 ~ 55,
    strings_trimmed_num == 6 ~ 66,
    strings_trimmed_num == 7 ~ 77,
    strings_trimmed_num == 8 ~ 88,
    strings_trimmed_num == 9 ~ 99,
    .default = as.numeric(strings_trimmed_num)))

# Løser problemet for tilfælde med for mange tal - laver ny df
df_too_many <- df %>% 
  filter(string_too_many == 1) %>% 
  mutate(first = str_split_i(as.character(strings_trimmed), "", 1),
         last = str_split_i(as.character(strings_trimmed), "", -1),
         combined = paste(first, last, sep = "", collapse = NULL))

# Kombinerer den nye df med den gamle
df <- df %>% 
  left_join(df_too_many, by = "strings")

# Erstatter værdien på strings_trimmed_num2.x med combined, hvis den er for stor
df <- df %>% 
  mutate(final = ifelse(strings_trimmed_num2.x > 99, combined, strings_trimmed_num2.x))

# 03_summarizer til endeligt tal ---------------------------------------------

# Har nu alle til, så de skal bare kombineres
df$final

# Laver den nummerisk
df$final_num <- as.numeric(df$final)

# summerer 
df_final <- df %>% 
  summarise(resultat = sum(final_num))
