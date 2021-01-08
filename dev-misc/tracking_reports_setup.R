# Generate datasets that allow us to test the functionality of multi-ds tracking
# reports. 

library(crunchtabs)
login()

# # Delete datasets if they already exist ----
# with_consent(deleteDataset("Example dataset"))
# with_consent(deleteDataset("Example dataset W1"))
# with_consent(deleteDataset("Example dataset W2"))
# with_consent(deleteDataset("Example dataset W3"))
# 
# Create datasets -----
ds1 <- newExampleDataset()
name(ds1) <- "Example dataset W1"

ds2 <- newExampleDataset()
name(ds2) <- "Example dataset W2"

ds3 <- newExampleDataset()
name(ds3) <- "Example dataset W3"

# Setup weights
ds1$weight1 <- makeWeight(ds1$q1 ~ c(0.3,0.3,0.4,0), name = 'weight1')
ds2$weight1 <- makeWeight(ds2$q1 ~ c(0.4,0.4,0.1,0.1), name = 'weight1')
ds3$weight1 <- makeWeight(ds3$q1 ~ c(0.2,0.2,0.4,0.2), name = 'weight1')
# 
weight(ds1) <- ds1$weight1
weight(ds2) <- ds2$weight1
weight(ds3) <- ds3$weight1

ds1 <- loadDataset("Example dataset W1")
ds2 <- loadDataset("Example dataset W2")
ds3 <- loadDataset("Example dataset W3")
ds1$only_wave1 <- factor(sample(letters[1:5], 20, replace = T))
ds2$only_wave2 <- factor(sample(letters[1:5], 20, replace = T))
ds3$only_wave3 <- factor(sample(letters[1:5], 20, replace = T))
ds1$avail_wave13 <- factor(sample(letters[1:5], 20, replace = T))
ds3$avail_wave13 <- factor(sample(letters[1:5], 20, replace = T))


tema <- themeNew(default_theme = themeDefaultLatex(), latex_flip_grids = TRUE, one_per_sheet = FALSE)
ct <- tracking_report(list(ds1, ds2, ds3), vars = c("allpets", "q1", "only_wave2", "avail_wave13"))
writeLatex(ct, pdf = TRUE,theme = tema, title = "Data from 3 Example Datasets")


# Clean up

datasets() %>% 
  as.data.frame() %>% 
  filter(grepl("Example", name)) %>% 
  pull(id) %>% 
  lapply(function(x) 
    with_consent(deleteDataset(sprintf("https://app.crunch.io/datasets/%s",x))))