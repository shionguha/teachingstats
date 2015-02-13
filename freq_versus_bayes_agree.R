#Primary Objective: To learn how to do a basic t-test comparing between 2 groups from both frequentist/Bayesian angles
#Secondary Objective: To see an instance of when frequentist and Bayesian statistics agree with each other. 
#Shion Guha, Cornell University 2015

#Let us load some required libraries here.
library(ggplot2)
library(BayesFactor)

#Let us set a seed a the quasi-random number generation in our data simulation
set.seed(666)
 
##Prepping variables for data simulation
a = 100 # Number of people wearing fancy hats
b = 100 # Number of people wearing no fancy hats
mu1 = 103 # Population mean of creativity for people wearing fancy hats
mu2 = 98 # Population mean of creativity for people wearing no fancy hats
sigma = 15 # Average population standard deviation of both groups
n = a + b # Total sample size

##Generate the simulated data
y1 = rnorm(a, mu1, sigma) # Data for people wearing fancy hats
y2 = rnorm(b, mu2, sigma) # Data for people wearing no fancy hats

#Let's look at what we have so far ...
mean(y1)
mean(y2)
sd(y1)
sd(y2)
mean(y1)-mean(y2) # Mean difference

## Let us generate a boxplot to investigate the data
y = c(y2, y1) # Aggregate both data sets
x = rep(c(1,0), c(a, b)) # Indicator for people wearing no fancy hats
boxplotframe = data.frame(Group=factor(x, labels = c("Group 1", "Group 2")), Measurement=y)
ggplot(boxplotframe) +
       geom_boxplot ((aes(y = Measurement, x = Group))) +
       labs(title= "Box Plot of Some Measurement") +
       theme(text = element_text(size=15),
             panel.background = element_rect(fill = 'white', colour = 'black'))

t.test(y1,y2, var.equal=TRUE) #Frequentist t-test. This is what the vast majority of us would do.

##Bayesian t-test: via Bayes factor
yx = data.frame(y,x) # Prepare data
bf = ttestBF(formula = y ~ x, data=yx) # Estimate Bayes factor
bf # Investigate the result

##Bayesian t-test: via MCMC; draw from posterior distribution
#Let's set a seed first for the quasi-random number generation in our data simulation.
set.seed(666)
chains = posterior(bf, iterations = 10000) # Save 10000 draws from posterior
beta = chains[,2] # Save draws for mean difference
##Visualize posterior distribution for group mean difference in creativity
df = data.frame(beta)
hi = ggplot(df, aes(x=var1)) + geom_histogram(binwidth = .5, color = "black", fill="white") +
labs(x = "Estimated Mean Difference",
     y = "Frequency",
     title = "Distribution of Difference Parameter") +
theme(text = element_text(size=15),
     panel.background = element_rect(fill = 'white', color = 'black'))
hi # show histogram

mean(beta) # mean difference as measured here

##Credibility interval
CredInt = quantile(beta,c(0.025,0.975)) #Credibility interval for the difference between groups
CredInt

##Visualize credibility interval in the histogram
hi + geom_vline(xintercept=CredInt[1],color ="green", linetype = "longdash", size = 2) + #line at lower limit of credibility interval
  geom_vline(xintercept=CredInt[2],color ="green", linetype = "longdash", size = 2) + #line at upper limit of credibility interval
  geom_vline(xintercept=0, color ="red", size = 2)  #line at zero difference



#Reference:
#http://blog.efpsa.org/2014/11/17/bayesian-statistics-what-is-it-and-why-do-we-need-it-2/
