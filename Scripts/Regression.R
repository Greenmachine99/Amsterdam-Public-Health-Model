### Libraries
library(rio)
library(ggplot2)
library(broom)
library(car)
library(dplyr)

### Datasets

  ## Importing Data
health = import('Datasets/amsterdam_health.csv')
income = import('Datasets/amsterdam_income.csv')
population = import('Datasets/amsterdam_population.csv')
public_space = import('Datasets/amsterdam_publicspace.csv')
social = import('Datasets/amsterdam_social.csv')
housing = import('Datasets/amsterdam_housing.csv')

  ## Knitting Datasets
all_data = inner_join(health, income)
all_data = inner_join(all_data, population)
all_data = inner_join(all_data, public_space)
all_data = inner_join(all_data, social)
all_data = inner_join(all_data, housing)

  ## Mutating Datasets
all_data = all_data %>%
  mutate(avg_greenery = pub_area_green / (pub_area_land + pub_area_water))
all_data = all_data %>%
  mutate(tot_facilities = soc_fac_cultural + soc_fac_sports)
all_data = all_data %>%
  mutate(pop_elder = (pop_60_69 + pop_70_79 + pop_80_89 + pop_90) / pop_total)

### Regression Models

  ## Simple Models

  # Health & Greenery
  m_greenery = lm(hea_good ~ avg_greenery, data = all_data)
  summary(m_greenery)  
  a_greenery = augment(m_greenery)
  g_greenery = ggplot(all_data, aes(avg_greenery, hea_good)) + 
    geom_point() 
  g_greenery  + geom_line(data = a_greenery, aes(y = .fitted), color = 'red')
  
  # Health & Perceived Easthetics Environment 
  m_environment = lm(hea_good ~ pub_setup , data = all_data)
  summary(m_environment)
  a_environment = augment(m_environment) 
  g_environment = ggplot(all_data, aes(pub_setup, hea_good)) +
    geom_boxplot()
  g_environment  
  
  ggplot(a_environment, aes(.resid)) +
    geom_histogram()
  ggplot(a_environment, aes(.fitted, .resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = 'dashed')
  
  # Health & Wealth (Income) 
  m_income = lm(hea_good ~ inc_disposable, data = all_data)
  summary(m_income)
  a_income = augment(m_income)
  g_income = ggplot(all_data, aes(inc_disposable, hea_good)) +
    geom_point() 
  g_income + geom_line(data = a_income, aes(y = .fitted), color = 'red')
  
  m_income2 = lm(hea_good ~ poly(inc_disposable, 2), data = all_data)
  summary(m_income2)
  g_income2 = ggplot(augment(m_income2, newdata = all_data), aes(inc_disposable, hea_good)) +
    geom_point() 
  g_income2 + geom_line(aes(y = .fitted), color = 'red')
  
  # Health & Wealth (Housing Value)
  m_houvalue = lm(hea_good ~ hou_value, data = all_data)
  summary(m_houvalue)
  a_houvalue = augment(m_houvalue) 
  g_houvalue = ggplot(all_data, aes(hou_value, hea_good)) +
    geom_point() 
  g_houvalue + geom_line(data = a_houvalue, aes(y = .fitted), color = 'red')
  
  m_houvalue2 = lm(hea_good ~ poly(hou_value, 2), data = all_data)
  summary(m_houvalue2)
  g_houvalue2 = ggplot(augment(m_houvalue2, newdata = all_data), aes(hou_value, hea_good)) +
    geom_point() 
  g_houvalue2 + geom_line(aes(y = .fitted), color = 'red')
  
  # Health & Social Facilities
  m_social = lm(hea_good ~ tot_facilities, data = all_data)
  summary(m_social)  
  a_social = augment(m_social)
  g_social = ggplot(all_data, aes(tot_facilities, hea_good)) +
    geom_point() 
  g_social + geom_line(data = a_social, aes( y = .fitted), color = 'red')
  
  ggplot(a_social, aes(.resid)) + 
    geom_histogram()
  ggplot(a_social, aes(.fitted, .resid)) + 
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed")
  
  m_social_2 = lm(hea_good ~ poly(tot_facilities, 2), data = all_data)
  summary(m_social_2)
  a_social_2 = augment(m_social_2)
  g_social_2 = ggplot(augment(m_social_2, newdata = all_data), aes(tot_facilities, hea_good)) +
    geom_point() 
  g_social_2 + geom_line(aes(y = .fitted), color = 'red')
  
  ggplot(augment(m_social_2, newdata = all_data), aes(.fitted, .resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = 'dashed')
  
  # Health & Age
  m_age = lm(hea_good ~ pop_elder, data = all_data)
  summary(m_age)  
  a_age = augment(m_age)  
  g_age = ggplot(all_data, aes(pop_elder, hea_good)) +
    geom_point() 
  g_age + geom_line(data = a_age, aes(y = .fitted), color = 'red')

  # Health & District
  m_district = lm(hea_good ~ district, data = all_data)
  summary(m_district)  
  a_district = augment(m_district)
  g_district = ggplot(all_data, aes(district, hea_good)) + 
    geom_boxplot()
  g_district  

  ## Multiple Linear Regression
  
  # Health & Easthetics Environment & Wealth (Income)
  m_setup_income = lm(hea_good ~ pub_setup  + inc_disposable, data = all_data)
  summary(m_setup_income) 
  a_setup_income = augment(m_setup_income)
  g_setup_income = ggplot(all_data, aes(inc_disposable, hea_good)) +
    geom_point() 
  g_setup_income + geom_line(data = m_setup_income, aes(y = .fitted, color = pub_setup))
  
  m_setup_income2 = lm(hea_good ~ pub_setup + poly(inc_disposable, 2), data = all_data)
  summary(m_setup_income2)
  g_setup_income2 = ggplot(augment(m_setup_income2, newdata = all_data), aes(inc_disposable, hea_good)) + 
    geom_point() 
  g_setup_income2 + geom_line(aes(y = .fitted, color = pub_setup))
  
  # Health & Easthetics Environment & Wealth (Housing Value)
  m_setup_houvalue = lm(hea_good ~ pub_setup + hou_value, data = all_data)
  summary(m_setup_houvalue) 
  a_setup_houvalue = augment(m_setup_houvalue)
  g_setup_houvalue = ggplot(all_data, aes(hou_value, hea_good)) +
    geom_point() 
  g_setup_houvalue + geom_line(data = a_setup_houvalue, aes(y = .fitted, color = pub_setup))
  
  m_setup_houvalue2 = lm(hea_good ~ pub_setup  + poly(hou_value, 2), data = all_data)
  summary(m_setup_houvalue2)
  g_setup_houvalue2 = ggplot(augment(m_setup_houvalue2, newdata = all_data), aes(hou_value, hea_good)) +
    geom_point() 
  g_setup_houvalue2 + geom_line(aes(y = .fitted, color = pub_setup))
  
  # Health & Easthetics Environment & Social Facilities
  m_setup_social = lm(hea_good ~ pub_setup + poly(tot_facilities, 2), data = all_data)
  summary(m_setup_social)
  a_setup_social = augment(m_setup_social)
  g_setup_social = ggplot(augment(m_setup_social, newdata = all_data), aes(tot_facilities, hea_good)) +
    geom_point() 
  g_setup_social + geom_line(data = a_setup_social, aes(y = .fitted, color = pub_setup))
  
  # Health & Easthetics Environment & Wealth & Social Facilities
  m_setup_wealth_social = lm(hea_good ~ pub_setup + inc_disposable + hou_value + tot_facilities, data = all_data)
  summary(m_setup_wealth_social)  
  a_setup_wealth_social = augment(m_setup_wealth_social) 
  
  ggplot(a_setup_wealth_social, aes(.resid)) + 
    geom_histogram()
  
  ggplot(a_setup_wealth_social, aes(.fitted, .resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = 'dashed')
  
  m_setup_wealth_social_2 = lm(hea_good ~ (pub_setup) + (poly(inc_disposable, 2)) + (poly(hou_value, 2))  + poly(tot_facilities, 2), data = all_data)
  summary(m_setup_wealth_social_2)
  a_setup_wealth_social_2 = augment(m_setup_wealth_social_2)
  
  ggplot(augment(m_setup_wealth_social_2, newdata = all_data), aes(.resid)) + 
    geom_histogram()
  ggplot(augment(m_setup_wealth_social_2, newdata = all_data), aes(.fitted, .resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = 'dashed')
  
  
  ### Interaction Effects
  
  # Health & Easthetics Environment & Wealth (Income)
  im_setup_income = lm(hea_good ~ (pub_setup) * (poly(inc_disposable, 2)), data = all_data)
  summary(im_setup_income) 
  g_setup_income2 + geom_line(data = augment(im_setup_income, newdata = all_data), aes(y = .fitted, color = pub_setup))

  # Health & Easthetics Environment & Wealth (Housing Value)
  im_setup_houvalue = lm(hea_good ~ (pub_setup) * (poly(hou_value, 2)) , data = all_data)
  summary(im_setup_houvalue)
  g_setup_houvalue2 + geom_line(data = augment(im_setup_houvalue, newdata = all_data), aes(y = .fitted, color = pub_setup))    
  
  # Health & Easthetics Environment & Social Facilities
  im_setup_social = lm(hea_good ~ (pub_setup) * (poly(tot_facilities, 2)), data = all_data)
  summary(im_setup_social)
  g_setup_social + geom_line(data = augment(im_setup_social, newdata = all_data), aes(y = .fitted, color = pub_setup))    

  # Health & Easthetics Environment & Wealth & Social Facilities
  im_setup_wealth_social = lm(hea_good ~ ((pub_setup ) + (poly(inc_disposable, 2)) + (poly(hou_value, 2)) + (tot_facilities)) ^2, data = all_data)
  summary(im_setup_wealth_social)  
  
  ### ANOVA
  
  # Health & Easthetics Environment & Wealth & Social Facilities
  aim_setup_wealth_social = lm(hea_good ~ (pub_setup) * 
                                 (poly(inc_disposable, 2)) * 
                                 (poly(hou_value, 2)) * (tot_facilities) - 
                                 pub_setup:poly(inc_disposable, 2):tot_facilities -
                                 pub_setup:poly(hou_value, 2):tot_facilities -
                                 poly(hou_value, 2):tot_facilities - 
                                 poly(inc_disposable, 2):poly(hou_value, 2):tot_facilities -
                                 pub_setup:poly(inc_disposable, 2):poly(hou_value, 2) -
                                 pub_setup:poly(hou_value, 2) - 
                                 pub_setup:poly(inc_disposable, 2):poly(hou_value, 2):tot_facilities -
                                 pub_setup:tot_facilities - 
                                 poly(inc_disposable, 2):poly(hou_value, 2), 
                                 data = all_data)
  Anova(aim_setup_wealth_social, type = 2)
  summary(aim_setup_wealth_social)  

  gaim_setup_wealth_social = ggplot(augment(aim_setup_wealth_social, newdata = all_data), aes(inc_disposable, hea_good)) + 
    geom_point()
  gaim_setup_wealth_social + geom_line(aes(y = .fitted, color = pub_setup))
  
  ggplot(augment(aim_setup_wealth_social, newdata = all_data), aes(.resid)) + 
    geom_histogram()
  ggplot(augment(aim_setup_wealth_social, newdata = all_data), aes(.fitted, .resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = 'dashed')
    

    