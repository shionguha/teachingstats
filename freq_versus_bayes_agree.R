#First, set a seed first for the quasi-random number generation in our data simulation
#If you set the same number you will get exactly the same data as I
set.seed(666)
 
##Prepare variables for data simulation
n1fh = 100 # Number of people wearing fancy hats
n2nfh = 100 # Number of people wearing no fancy hats
mu1 = 103 # Population mean of creativity for people wearing fancy hats
mu2 = 98 # Population mean of creativity for people wearing no fancy hats
sigma = 15 # Average population standard deviation of both groups
n = n1fh+n2nfh # Total sample size
##Generate the simulated data
y1 = rnorm(n1fh, mu1, sigma) # Data for people wearing fancy hats
y2 = rnorm(n2nfh, mu2, sigma) # Data for people wearing no fancy hats

mean(y1)
mean(y2)
sd(y1)
sd(y2)
mean(y1)-mean(y2) # Mean difference

## Generate a boxplot to investigate the data
install.packages("ggplot2") # Install package for flexible graphics
library("ggplot2")
y = c(y2, y1) # Aggregate both data sets
x = rep(c(1,0), c(n1fh, n2nfh)) # Indicator for people wearing no fancy hats
boxplotframe = data.frame(Group=factor(x, labels = c("No Fancy Hat", "Fancy Hat")), Creativity=y)
ggplot(boxplotframe) +
       geom_boxplot ((aes(y = Creativity, x = Group))) +
       labs(title= "Box Plot of Creativity Values") +
       theme(text = element_text(size=15),
             panel.background = element_rect(fill = 'white', colour = 'black'))

t.test(y1,y2, var.equal=TRUE) #Frequentist t-test

##Bayesian t-test: via Bayes factor
install.packages("BayesFactor") # Install BayesFactor-package
library('BayesFactor') # Load BayesFactor-package
yx = data.frame(y,x) # Prepare data
bf = ttestBF(formula = y ~ x, data=yx) # Estimate Bayes factor
bf # Investigate the result

##Bayesian t-test: via MCMC; draw from posterior distribution
#Let's set a seed first for the quasi-random number generation in our data simulation.
#If you set the same number you will get the same data as I
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

mean(beta) # mean difference creativity score

##Credibility interval
CredInt = quantile(beta,c(0.025,0.975)) #Credibility interval for the difference between groups
CredInt
##Visualize credibility interval in the histogram
hi + geom_vline(xintercept=CredInt[1],color ="green", linetype = "longdash", size = 2) + #line at lower limit of credibility interval
  geom_vline(xintercept=CredInt[2],color ="green", linetype = "longdash", size = 2) + #line at upper limit of credibility interval
  geom_vline(xintercept=0, color ="red", size = 2)  #line at zero difference


