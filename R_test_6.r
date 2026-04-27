# Load the CSV file
data <- read.csv("test3.csv") # Adjust the path accordingly

# Convert all columns to factors, except 'PV_adoption' initially
data <- transform(data,
                  Single_familly_house = factor(Single_familly_house),
                  Education_level = factor(Education_level),
                  Interest_in_politcs = factor(Interest_in_politcs),
                  Homeownership_status = factor(Homeownership_status),
                  Electric_car_ownership = factor(Electric_car_ownership),
                  Heat_pump_ownership = factor(Heat_pump_ownership),
                  Age = factor(Age),
                  Trust_in_media = factor(Trust_in_media),
                  Perceived_economy = factor(Perceived_economy),
                  Personal_network_PV = factor(Personal_network_PV),
                  Number_of_people_exchanged = factor(Number_of_people_exchanged),	
                  source = factor(source))

# Convert 'PV_adoption' to a factor
data$PV_adoption <- factor(data$PV_adoption)


# Define a blacklist to ensure PV_adopt is the final child node

# Define a blacklist to ensure 'PV_adoption' is only a child node
blacklist <- data.frame(from = "PV_adoption", to = setdiff(names(data), "PV_adoption"))
whitelist <- data.frame(
    from = c( "source", "Perceived_economy","Electric_car_ownership","Single_familly_house","Heat_pump_ownership","Trust_in_media"),
    to = c("Perceived_economy","PV_adoption","PV_adoption","PV_adoption","PV_adoption","Perceived_economy" )
)
library(bnlearn) # make sure to load the bnlearn library



# Learn network structure without whitelist
bn_no_whitelist <- hc(data, blacklist = blacklist, score = "bic")

# Learn network structure with whitelist
bn_with_whitelist <- hc(data, blacklist = blacklist, whitelist = whitelist, score = "bic")

# Fit the Bayesian networks
fitted_bn_no_whitelist <- bn.fit(bn_no_whitelist, data)
fitted_bn_with_whitelist <- bn.fit(bn_with_whitelist, data)
library(Rgraphviz) # make sure to load the Rgraphviz library for plotting
graphviz.plot(fitted_bn_with_whitelist)
# Compute BIC scores
print(class(fitted_bn_no_whitelist))
print(class(fitted_bn_with_whitelist))
bic_no_whitelist <- score(bn_no_whitelist, data, type = "bic")
bic_with_whitelist <- score(bn_with_whitelist, data, type = "bic")

# Output the BIC scores to compare
cat("BIC score without whitelist: ", bic_no_whitelist, "\n")
cat("BIC score with whitelist: ", bic_with_whitelist, "\n")

# Print the comparison results


# Plot the network
library(Rgraphviz) # make sure to load the Rgraphviz library for plotting
graphviz.plot(bn_with_whitelist)

# Print the fitted Bayesian network
#print(fitted_bn_with_whitelist)
