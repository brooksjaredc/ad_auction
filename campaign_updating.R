library(dplyr)
library(reshape2)

# Set up initial info

## Set data for advertisers

ad_info <- data.frame("advertiser" = c("buymystuff.com", "eyespyonyou.com", "commodityfetishismllc.com"),
                      "bid" = c(0.7, 0.5, 0.4), "views" = c(2000, 0,0), 
                      "clicks" = c(47,0,0), "ctr" = c(0.025, 0.012, 0.045))

## Set data for previous campaigns

past_ctrs <- rbeta(500, 2, 98, ncp = 0)

## Fit data from previous campaigns to beta distr

prior_est <- MASS::fitdistr(past_ctrs, dbeta, start = list(shape1 = 2, shape2 = 98))
alpha_prior <- prior_est$estimate[1]
beta_prior <- prior_est$estimate[2]

## Set posteriors

alpha_post <- c(1,1,1)*alpha_prior + ad_info$clicks
beta_post <- c(1,1,1)*beta_prior + (ad_info$views - ad_info$clicks)

## Set sampling probabilities

N <- 10000

ad_A <- ad_info$bid[1]*rbeta(N, alpha_post[1], beta_post[1])
ad_B <- ad_info$bid[2]*rbeta(N, alpha_post[2], beta_post[2])
ad_C <- ad_info$bid[3]*rbeta(N, alpha_post[3], beta_post[3])

ad_info$p_gt_A <- c(0.5, sum(ad_A < ad_B)/N, sum(ad_A < ad_C)/N)

ad_info$weighted_p_gt_A <- ad_info$p_gt_A/(sum(ad_info$p_gt_A))


samp_prop = data.frame(t(ad_info$weighted_p_gt_A))

# Using sampling probabilities and underlying CTRs to set new views and clicks

i <- 1

while (i<51) {
  
  winners_df <- sample(ad_info$advertiser,
                       prob = ad_info$weighted_p_gt_A,
                       replace = TRUE,
                       size=100) %>%  
    table() %>%  
    as.data.frame()
  
  ad_info$new_views <- winners_df$Freq
  ad_info$new_clicks <- rbinom(c(1,1,1),ad_info$new_views,ad_info$ctr)
  
  alpha_post <- alpha_post + ad_info$new_clicks
  beta_post <- beta_post + (ad_info$new_views - ad_info$new_clicks)
  
  ad_A <- ad_info$bid[1]*rbeta(N, alpha_post[1], beta_post[1])
  ad_B <- ad_info$bid[2]*rbeta(N, alpha_post[2], beta_post[2])
  ad_C <- ad_info$bid[3]*rbeta(N, alpha_post[3], beta_post[3])
  
  ad_info$p_gt_A <- c(0.5, sum(ad_A < ad_B)/N, sum(ad_A < ad_C)/N)
  ad_info$weighted_p_gt_A <- ad_info$p_gt_A/(sum(ad_info$p_gt_A))
  
  samp_prop[nrow(samp_prop)+1,] = t(ad_info$weighted_p_gt_A)
  
  i <- i+1
}
names(samp_prop) <- ad_info$advertiser
samp_prop$id = 1:nrow(samp_prop)
df_long <- melt(samp_prop, id.vars = 'id')
ggplot() + geom_line(data = df_long, aes(x = id, y = value, color = variable), size = 1)



