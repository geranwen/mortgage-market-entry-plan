load('df3.Rdata')
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

df3 %>%
  filter(ORIG_DTE < as.Date("2018-01-01"), ORIG_DTE > as.Date("2016-12-31")) %>%
  filter(LAST_STAT == 'P') %>%
  group_by(purpose) %>%
  summarize(count = n(),) %>%
  mutate(freq =  count / sum(count))


dff1.1 %>%
  group_by(zip_3) %>%
  summarise(n())


# sp_NE <- zctas(year = 2010, state = 'NE')
# save(sp_NE, file = 'sp_NE.Rdata')

class(test_map)
plot(test_map$geometry)

test_map1<- test_map %>%
  mutate(zip = as.numeric(ZCTA5CE10))
class(test_map1$zip)


ggplot()  + geom_point(data = sample, aes(x = lng, y = lat), size = 2, alpha = 0.5) + geom_sf(data = bind_kml, aes(alpha = 0.5))
summary(bind_kml)