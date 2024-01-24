library("tidyverse")
library("lubridate")
df = read.csv(file = 'OnlineRetail.csv', header = TRUE, encoding='iso-8859-1')
head(df)
sapply(df, class)
# lapply(df, summary)
df$InvoiceNo = as.factor(df$InvoiceNo)
df$InvoiceDate=date(as.POSIXct(df$InvoiceDate, format="%m/%d/%Y %H:%M", tz=Sys.timezone()))
df$CustomerID = as.factor(df$CustomerID)
df$Monetary = df$Quantity * df$UnitPrice

df <- df[(df$Quantity > 0), ]
df <- df[(df$UnitPrice > 0), ]
#indices_to_remove_cancel <- grep("C", df$InvoiceNo)
#df = df[-indices_to_remove_cancel, ]

na_count <- sapply(df, function(x) sum(is.na(x)))

#na_count <- sapply(df, function(x) sum(is.nan(x)))

print(na_count)
df = df[-is.na(df$CustomerID), ]

df$Global_max_date <- max(df$InvoiceDate)

df_rfmt <- NULL

df_m_preprocess <- df %>% 
  group_by(CustomerID) %>% 
  filter(InvoiceDate != min(InvoiceDate)) %>%
  summarise(total_monetary_value = sum(Monetary))

df_rfmt <- df %>% 
  group_by(CustomerID) %>% 
  summarise(frequency = length(unique(InvoiceNo)) - 1, 
            recency = as.numeric(difftime(max(InvoiceDate), min(InvoiceDate), units = "days")), 
            T = max(as.numeric(difftime(Global_max_date, InvoiceDate, units = "days")))) %>% 
  left_join(df_m_preprocess) %>% 
  mutate(monetary_value = total_monetary_value / frequency)
