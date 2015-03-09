  # weather data with missing values
weatherm <- weather
weatherm$outlook[1] <- NA
weatherm$humidity[1:2] <- NA

  # conditional attribute value probabilities given the class
  # with and without missing values
pcond(weather$outlook, weather$play)
pcond(weatherm$outlook, weatherm$play)
pcond(weather$humidity, weather$play)
pcond(weatherm$humidity, weatherm$play)
