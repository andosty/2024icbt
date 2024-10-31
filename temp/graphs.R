library(plotly)


regionalErrors<- errdata %>%
  group_by(RegionName) %>% summarise(totalErrors = n()) %>% arrange(-totalErrors) %>% mutate(stringsAsFactors = FALSE,
                                                                                             RegionName = as.character(RegionName)
                                                                                             )
regionErrorData <- data.frame(regionalErrors$RegionName,regionalErrors$totalErrors, stringsAsFactors = FALSE)

regionErrorData$regionalErrors.RegionName <- factor(regionErrorData$regionalErrors.RegionName, levels = unique(regionErrorData$regionalErrors.RegionName)[order(regionErrorData$regionalErrors.totalErrors, decreasing = F)])
# fig <- plot_ly(x = c(20, 14, 23), y = c('giraffes', 'orangutans', 'monkeys'), type = 'bar', orientation = 'h')

# regionalErrors$RegionName <- factor(regionalErrors$RegionName, levels = unique(regionalErrors)[order(regionalErrors$totalErrors, decreasing = TRUE)])

fig <- plot_ly(regionErrorData, x = ~regionalErrors.totalErrors, y = ~regionalErrors.RegionName, type = 'bar', orientation = 'h')  %>%
   layout(xaxis = list(categoryorder = "total descending"))

# fig.update_layout(xaxis={'categoryorder': 'total descending'})
# fig.update_layout(xaxis={'categoryorder': 'total descending'})
fig


names(regionErrorData)