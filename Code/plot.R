
# read in data
cluster0_emb <- read.csv("Data/Cluster_0_emb.csv", row.names = 1)
cluster1_emb <- read.csv("Data/Cluster_1_emb.csv", row.names = 1)
cluster2_emb <- read.csv("Data/Cluster_2_emb.csv", row.names = 1)
cluster3_emb <- read.csv("Data/Cluster_3_emb.csv", row.names = 1)






# load required library
library(ggplot2)
library(ggrepel)


##############
### Cluster 0
##############

# Prepare colors and labels based on your requirement
color_vector = c("black", "red", "blue1", "blue2", "blue3")
label_vector = c("Official", "Human", "ChatGPT1", "ChatGPT2", "ChatGPT3")

# Create the plot
# ggplot is used to create the basic plot structure
embedding_plot = ggplot(cluster0_emb, aes(x = D1, y = D2)) + 
  
  # geom_point is used to add the points to the plot
  # Here we color the points based on the 'label' column
  geom_point(aes(color = Label), size = 1.3) + 
  
  # set the color scheme to match the 'label' column to your color vector
  scale_color_manual(values = color_vector, labels = label_vector, breaks = levels(cluster0_emb$Label)) +
  
  # Add labels to the points using geom_text_repel
  # Here we filter out the data for points we want to label based on a condition
  geom_text_repel(aes(label = Diagnosis, point.size= 5, segment.size = 0.3, segment.alpha = 0.5), size = 2, max.overlaps = 35) +
  
  # Add labels to x and y axes
  xlab("Embedding 1") + 
  ylab("Embedding 2") +
  ggtitle("Cluster 0 ") +
  
  # Use a classic theme and modify it as per requirements
  theme_classic() + 
  theme(legend.title = element_blank(), text = element_text(size = 15))

# Print the plot
embedding_plot



##############
### Cluster 1
##############

# Prepare colors and labels based on your requirement
color_vector = c("black", "red", "blue1", "blue2", "blue3")
label_vector = c("Official", "Human", "ChatGPT1", "ChatGPT2", "ChatGPT3")

# Create the plot
embedding_plot1 = ggplot(cluster1_emb, aes(x = D1, y = D2)) + 
  geom_point(aes(color = Label), size = 1.3) + 
  scale_color_manual(values = color_vector, labels = label_vector, breaks = levels(cluster1_emb$Label)) +
  geom_text_repel(aes(label = Diagnosis, point.size= 5, segment.size = 0.3, segment.alpha = 0.5), size = 2, max.overlaps = 50) +
  xlab("Embedding 1") + 
  ylab("Embedding 2") +
  ggtitle("Cluster 1") +
  theme_classic() + 
  theme(legend.title = element_blank(), text = element_text(size = 15))

embedding_plot1



##############
### Cluster 2
##############

color_vector = c("black", "red", "blue1", "blue2", "blue3")
label_vector = c("Official", "Human", "ChatGPT1", "ChatGPT2", "ChatGPT3")

# Create the plot
embedding_plot2 = ggplot(cluster2_emb, aes(x = D1, y = D2)) + 
  geom_point(aes(color = Label), size = 1.3) + 
  scale_color_manual(values = color_vector, labels = label_vector, breaks = levels(cluster2_emb$Label)) +
  geom_text_repel(aes(label = Diagnosis, point.size= 5, segment.size = 0.3, segment.alpha = 0.5), size = 2, max.overlaps = 59) +
  xlab("Embedding 1") + 
  ylab("Embedding 2") +
  ggtitle("Cluster 2") +
  theme_classic() + 
  theme(legend.title = element_blank(), text = element_text(size = 15))

embedding_plot2





##############
### Cluster 3
##############


color_vector = c("black", "red", "blue1", "blue2", "blue3")
label_vector = c("Official", "Human", "ChatGPT1", "ChatGPT2", "ChatGPT3")

# Create the plot
embedding_plot3 = ggplot(cluster3_emb, aes(x = D1, y = D2)) + 
  geom_point(aes(color = Label), size = 1.3) + 
  scale_color_manual(values = color_vector, labels = label_vector, breaks = levels(cluster3_emb$Label)) +
  geom_text_repel(aes(label = Diagnosis, point.size= 5, segment.size = 0.3, segment.alpha = 0.5), size = 2, max.overlaps = 40) +
  xlab("Embedding 1") + 
  ylab("Embedding 2") +
  ggtitle("Cluster 3") +
  theme_classic() + 
  theme(legend.title = element_blank(), text = element_text(size = 15))

embedding_plot3
