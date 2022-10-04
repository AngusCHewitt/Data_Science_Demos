# EG of create many mod forecast

library("lubridate")

data(bike_sales)

monthly_qty_by_cat2 <- bike_sales %>%
  mutate(order.month = as_date(zoo::as.yearmon(order.date))) %>%
  group_by(category.secondary, order.month) %>%
  summarise(total.qty = sum(quantity))
monthly_qty_by_cat2


monthly_qty_by_cat2_nest <- monthly_qty_by_cat2 %>%
  group_by(category.secondary) %>%
  nest(.key = "data.tbl")
monthly_qty_by_cat2_nest$data.tbl[1]


monthly_qty_by_cat2_ts <- monthly_qty_by_cat2_nest %>%
  mutate(data.ts = map(.x       = data.tbl, 
                       .f       = tk_ts, 
                       select   = -order.month, 
                       start    = 2011,
                       freq     = 12))
monthly_qty_by_cat2_ts