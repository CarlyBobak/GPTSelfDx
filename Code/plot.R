setwd("/Users/carlybobak/Library/CloudStorage/GoogleDrive-carly.a.bobak@dartmouth.edu/My Drive/RC /Pacific Symposium of Biocomputing/2023/Patient_Symptom_Project")

# load required library
library(ggplot2)
library(ggrepel)
library(tidyverse)

embeddings<-read.csv("Data/vectors_pca.csv",row.names = 1)
embeddings$Disease<-embeddings$Official_DX
data<-read.csv("Data/blinded_patient_data_with_adjusted_noise.csv")
data$Official_DX[data$Official_DX=="Cold"]<-"Common Cold"


# Prepare colors and labels based on your requirement
color_vector = c("black", "red", "skyblue")
label_vector = c("Official", "Human", "ChatGPT")
names(color_vector) <- label_vector

off_dxs <- unique(data$Official_DX)
disease_embeddings <- list()
plot_list <- list()  # Store individual plots here

for (i in off_dxs) {
  sub <- data %>% filter(Official_DX == i) %>% select(Patient.ID, Official_DX, Rater_DX_Clean, ChatGPT_Clean_1, ChatGPT_Clean_2, ChatGPT_Clean_3) %>%
    pivot_longer(cols = c(Official_DX, Rater_DX_Clean, ChatGPT_Clean_1, ChatGPT_Clean_2, ChatGPT_Clean_3), names_to = "Rater", values_to = "Disease") %>%
    mutate(Disease = tolower(Disease)) %>%
    left_join(embeddings)
  
  disease_embeddings[[i]] <- sub
  
  sub$Rater_fmt <- "ChatGPT"
  sub$Rater_fmt[sub$Rater == "Official_DX"] <- "Official"
  sub$Rater_fmt[sub$Rater == "Rater_DX_Clean"] <- "Human"
  
  # Create the plot for each sub data frame
  sub_plot <- sub %>% select(-Patient.ID) %>% unique()
  sub_plot$Rater_fmt <- factor(sub_plot$Rater_fmt, levels = label_vector)
  
  # Create the plot with separate color mapping for geom_point and geom_text_repel
  embedding_plot <- ggplot(sub_plot, aes(x = PC1, y = PC2)) + 
    geom_point(aes(color = Rater_fmt), size = 3) +
    geom_text_repel(aes(label = Disease, point.size = 5, segment.size = 0.3, segment.alpha = 0.5, color = Rater_fmt), size = 5, max.overlaps = 35) +
    xlab("Embedding 1") + 
    ylab("Embedding 2") +
    ggtitle(paste("Official Diagnosis:", i)) +
    scale_color_manual(values = color_vector, labels = label_vector, breaks = levels(sub_plot$Rater_fmt)) +
    guides(color = guide_legend(title = NULL, override.aes = list(size = 5, segment.size = 0.3, segment.alpha = 0.5))) +  # Hide legend for geom_text_repel
    theme_classic() + 
    theme(legend.title = element_blank(), text = element_text(size = 15))
  
  plot_list[[i]] <- embedding_plot
}



# Print all the plots stored in the plot_list
pdf("testplots_bertpca.pdf")
for (i in off_dxs) {
  print(plot_list[[i]])
}
dev.off()

png("hypertensionplot.png",height=6,width=6,units="in",res=300)
print(plot_list[["Hypertension"]])
dev.off()

png("heartdiseaseplot.png",height=6,width=6,units="in",res=300)
print(plot_list[["Heart Disease"]])
dev.off()

png("commoncoldplot.png",height=6,width=6,units="in",res=300)
print(plot_list[["Common Cold"]])
dev.off()






### Calculate cosine similarity

cosine_similarity <- function(a, b) {
  return(sum(a*b) / (sqrt(sum(a^2)) * sqrt(sum(b^2))))
}

# Initialize a result dataframe
result <- data.frame()

#run over our disease embeddings
for(l in disease_embeddings){

  df<-l
  
  # Get unique patient IDs
  unique_patients <- unique(df$Patient.ID)

  # Loop through each patient
  for(patient in unique_patients){
  
    # Filter data for current patient
    patient_data <- df[df$Patient.ID == patient, ]
  
    # Compute cosine similarity for Official_DX vs Rater_DX_Clean
    official_dx <- patient_data[patient_data$Rater == "Official_DX", c("PC1", "PC2")]
    rater_dx_clean <- patient_data[patient_data$Rater == "Rater_DX_Clean", c("PC1", "PC2")]
  
    cosine_official_rater <- cosine_similarity(as.numeric(official_dx), as.numeric(rater_dx_clean))
  
    # Compute cosine similarity for Official_DX vs ChatGPT_Clean_X
    chatgpt_dx_clean <- patient_data[grep("ChatGPT_Clean", patient_data$Rater), c("PC1", "PC2")]
    cosine_official_chatgpt <- sapply(1:nrow(chatgpt_dx_clean), function(i) cosine_similarity(as.numeric(official_dx), as.numeric(chatgpt_dx_clean[i, ])))
  
    # Store result
    patient_result <- data.frame(Patient.ID = patient,
                                 Official_Dx = df$Disease[df$Patient.ID==patient & df$Rater=="Official_DX"],
                                 Cosine_Official_Rater = cosine_official_rater, 
                                 Cosine_Official_ChatGPT = cosine_official_chatgpt)
  
    result <- rbind(result, patient_result)
  }
}

cosine_sim_df <- result %>%
  distinct() %>%
  group_by(Official_Dx) %>%
  summarise(across(-Patient.ID, mean, na.rm = TRUE))

library(ggsignif)
library(reshape2) # for melting data
library(ggpubr)

# Reshape your data from wide to long
df_long <- reshape2::melt(result, id.vars = c("Patient.ID", "Official_Dx"))
df_long$variable<-ifelse(df_long$variable=="Cosine_Official_Rater","Human","ChatGPT")
df_summarized <- df_long %>% 
  group_by(Official_Dx, variable) %>% 
  summarise(Mean_Value = mean(value), Std_Error = sd(value) / sqrt(n()), .groups = "drop")

p <- ggbarplot(df_long %>% arrange(Official_Dx), x = "Official_Dx", y = "value", 
          add = "mean_se", 
          fill = "variable", 
          palette = color_vector,
          x.text.angle = 45,
          xlab = "Official Diagnosis",
          ylab = "Cosine Similarity",
          position = position_dodge(0.8)) +
  stat_compare_means(aes(group = variable), 
                     method = "wilcox.test",
                     label = "p.signif") + theme(legend.title = element_blank())

ggsave("cosine_sim.png", plot = p, width = 6, height = 6)

# misdiagnosis networks

library(igraph)

df<-data %>% select(Patient.ID,Official_DX,Rater_DX_Clean,ChatGPT_Clean_1,ChatGPT_Clean_2,ChatGPT_Clean_3)

# Separate the data for Rater and ChatGPT predictions
df_rater <- df %>%
  select(Patient.ID, Official_DX, Rater_DX = Rater_DX_Clean)

df_chatgpt <- df %>%
  pivot_longer(cols = starts_with("ChatGPT"), names_to = "Source", values_to = "ChatGPT_DX") %>%
  select(Patient.ID, Official_DX, ChatGPT_DX)

# Create edges and compute their frequencies for both datasets separately
df_edges_rater <- df_rater %>%
  group_by(Rater_DX, Official_DX) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  rename(From = Rater_DX, To = Official_DX) %>%
  mutate(Source = "Rater")  # Add new column "Source"

df_edges_chatgpt <- df_chatgpt %>%
  group_by(ChatGPT_DX, Official_DX) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  rename(From = ChatGPT_DX, To = Official_DX) %>%
  mutate(Source = "ChatGPT")  # Add new column "Source"

# Combine the data
df_edges <- bind_rows(df_edges_rater, df_edges_chatgpt)

# Create graph
graph <- graph_from_data_frame(df_edges, vertices = NULL, directed = TRUE)

# Check if the edges are correct
E(graph)

#Adjust for 3 raters
E(graph)$Frequency[E(graph)$Source!="Rater"]<-E(graph)$Frequency[E(graph)$Source!="Rater"]/3
E(graph)$Frequency<-E(graph)$Frequency+1
E(graph)$Frequency<-log(E(graph)$Frequency)



V(graph)$label <- as.character(V(graph)$name)


png("misdiagnosisgraph.png",height=6,width=6,units="in",res=300)
par(mar = c(3, 0.69, 3, 0.25))
# plot graph
plot(graph,
     layout = layout_with_kk(graph),
     edge.width = E(graph)$Frequency,
     edge.arrow.size = 0.15,
     edge.color = ifelse(E(graph)$Source == "Rater", "red", "skyblue"),
     vertex.color = "darkgrey",
     vertex.size = 5,
     vertex.frame.color = NA,
     vertex.label.dist = 2,
     vertex.label.cex = 0.6,
     vertex.label.color = "black")

legend("topleft",  # Position of the legend
       legend = c("Human Rater", "ChatGPT"),  # Legend labels
       col = c("red", "skyblue"),  # Colors of the legend labels
       lty = 1,  # Line type
       lwd = 2, bty="n", cex=0.6) 
dev.off()

