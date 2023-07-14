
library(tidyverse)
library(statsExpressions)
library(ggsignif)
library(patchwork)
library(ggbeeswarm)
library(ggprism)

# input <- readr::read_csv("./ATN_no_AD2.csv", show_col_types = FALSE)




# 创建sample ID
sample <- paste0("Sample", 1:100)
# 创建Group
Group <- sample(c("A", "B", "C"), 100, replace = TRUE)
# 创建特征列
features <- matrix(0, nrow = 100, ncol = 8)
# 给不同分组的特征赋予不同的均值
Group_A_mean <- c(5, 6, 7, 8, 9, 10, 11, 12)
Group_B_mean <- c(1, 2, 3, 4, 5, 6, 7, 8)
Group_C_mean <- c(2, 4, 6, 8, 10, 12, 14, 16)

for (i in 1:8) {
  if (Group[i] == "A") {
    features[Group == "A", i] <- rnorm(sum(Group == "A"), mean = Group_A_mean[i])
  } else if (Group[i] == "B") {
    features[Group == "B", i] <- rnorm(sum(Group == "B"), mean = Group_B_mean[i])
  } else {
    features[Group == "C", i] <- rnorm(sum(Group == "C"), mean = Group_C_mean[i])
  }
}

# 创建Dataframe
mydata3 <- data.frame(sample, Group, features)




Boxplot<- function(mydata=mydata3,width=NULL,height=NULL,out.name="output/"){
      dir.create(out.name,recursive = T)
      input=mydata3
      input$Group <- factor(input$Group, levels = c("A", "B", "C"))
      
      ## I just show one variable plot, but one can run a loop for whatever you like to make the plot
      # creating the base plot
      ## save the plots into a list in case you need to plot several plots in the same page with patchwork
      pst <- list()
      
      ## we could also save the comparison statistics into a dataframe.
      DF <- NULL
      
      mycols <- c("#E53935", "#1976D2", "#FF8F00", "#7C4DFF", "#647687")
      
      ##
      for (i in names(input)[-c(1:2)]) {
        print(i)
        ## boxbeeswarm plot
        p <- ggplot(input, aes_string(x = "Group", y = i)) +
          geom_boxplot(alpha = 1.0, width = 0.5, outlier.shape = NA) +
          #  ggbeeswarm::geom_quasirandom(width = 0.2, size = .8,  aes(color = Group)) +
          ggbeeswarm::geom_beeswarm(cex = 2, size = .8, corral = "random", corral.width = 0.5, aes(color = Group)) +
          xlab("") +
          ylab("") +
          scale_color_manual(values = mycols)
        
        # boxviolin plot
        # p <- ggplot(input, aes_string(x = "Group", y = i)) +
        #   geom_violin(aes(fill = Group), width = 1.0) +
        #   geom_boxplot(alpha = 1.0, fill = "white", width = 0.3, outlier.shape = NA) +
        #   xlab("") +
        #   ylab("") +
        #   scale_fill_manual(values = mycols)
        
        # using `pairwise_comparisons()` function to create a data frame with results
        set.seed(123)
        df <- pairwise_comparisons(input, Group, !!i, type = "nonparametric", p.adjust.method = "BH") %>%
          dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = base::c)) %>%
          dplyr::arrange(group1) %>%
          dplyr::mutate(asterisk_label = case_when(
            p.value > 0.05 ~ "ns",
            p.value < 0.05 & p.value > 0.01 ~ "*",
            p.value < 0.01 & p.value > 0.001 ~ "**",
            p.value < 0.001 & p.value > 0.0001 ~ "***",
            p.value < 0.0001 ~ "****"
          )) %>%
          dplyr::filter(asterisk_label != "ns")
        
        DF <- dplyr::bind_rows(DF, df)
        
        ### it is very likely that there are no siginicant comparison after fdr, then we just there is no need to
        ## show the asterisk, therefore we do two kinds of plots - one with asterisk if any, one without it.
        
        if (nrow(df) > 0) {
          # adding pairwise comparisons using `{ggsignif}` package
          p <- p +
            ggsignif::geom_signif(
              comparisons = df$groups,
              annotations = df$asterisk_label,
              test = NULL,
              step_increase = 0.1,
              na.rm = TRUE
            )
          p <- p + theme_prism(base_size = 14) + theme(legend.position = "none")
          pst[[i]] <- p
          ## assign each output figure
          ## assign(paste0(i, ".mriplot"), p)
          ggsave(filename = glue::glue( out.name,i, "_boxplot.pdf"), width = 4, height = 5)
        } else {
          p <- p + theme_prism(base_size = 14) + theme(legend.position = "none")
          pst[[i]] <- p
          ## assign each output figure
          ## assign(paste0(i, ".mriplot"), p)
          ggsave(filename = glue::glue( out.name,i, "_boxplot.pdf"), width = 4, height = 5)
        }
      }
      ## we need to remove one problematic column "expression" before save
      DF <- DF[, -9]
      openxlsx::write.xlsx(DF, file =  glue::glue( out.name, "group_comparison_statistics.xlsx"))
      
      ## you may plot several plots from the list on the same page
      wrap_plots(pst, nrow = 2)
      ggsave(filename = glue::glue( out.name, "merge_boxplot.pdf"), width = 14, height = 8)
}
Boxplot(mydata=mydata3,width=NULL,height=NULL,out.name="output/")





