install.packages("prophet")
library(prophet)

#Prophet model two years
JohnsonJohnson_ds <- zoo::as.yearmon(time(JohnsonJohnson))
JohnsonJohnson_y <- JohnsonJohnson
JohnsonJohnson_df <- data.frame(ds=JohnsonJohnson_ds, y=JohnsonJohnson_y)
model_prohet <- prophet::prophet(JohnsonJohnson_df)
next_two_years <- prophet::make_future_dataframe(model_prohet, periods = 8, freq = "quarter")
model_prediction <- predict(model_prohet, next_two_years)
plot(model_prohet, model_prediction)
prophet_plot_components(model_prohet, model_prediction)

#Prophet model today
JohnsonJohnson_ds <- zoo::as.yearmon(time(JohnsonJohnson))
JohnsonJohnson_y <- JohnsonJohnson
JohnsonJohnson_df <- data.frame(ds=JohnsonJohnson_ds, y=JohnsonJohnson_y)
model_prohet <- prophet::prophet(JohnsonJohnson_df)
today_forecast <- prophet::make_future_dataframe(model_prohet, periods = 176, freq = "quarter")
model_prediction <- predict(model_prohet, today_forecast)
plot(model_prohet, model_prediction)
tail(model_prediction$yhat, 4)
mean(tail(model_prediction$yhat, 4))

#Linear model
JohnsonJohnson_x <- time(JohnsonJohnson)
JohnsonJohnson_y <- JohnsonJohnson
model_linear <- lm(JohnsonJohnson_y~JohnsonJohnson_x)
summary(model_linear)
plot(model_linear)

#other plots
decomposed <- decompose(JohnsonJohnson)
plot(decomposed)

?JohnsonJohnson


