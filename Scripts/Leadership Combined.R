library(binom)
library(reshape2)
library(forcats)
library(tidyr)
library(ggplot2)
library(here)
library(grid)

par(mfrow=c(1, 1), mar=c(4, 4, 4, 4))
output_csvs = list()
input_csvs = dir(here("Data/Leadership_Combined"), pattern = "*.csv")
major_totals = matrix(nrow=length(input_csvs), ncol=3)

for (i in 1:length(input_csvs)) {
  output_csvs[[i]] = read.csv(here("Data/Leadership_Combined", input_csvs[i]))
  output_csvs[[i]][is.na(output_csvs[[i]])] = 0
  output_csvs[[i]][, 1] = fct_rev(output_csvs[[i]][, 1])
  output_csvs[[i]] = output_csvs[[i]][dim(output_csvs[[i]])[1]:1,]
  colnames(output_csvs[[i]]) = c("major", "Actual Count", "Expected Count")
  data = melt(output_csvs[[i]][, c(1, 3, 2)], id.vars="major")
  confint = matrix(nrow=length(output_csvs[[i]][, 2]), ncol=2)
  for(j in 1:nrow(confint)){
    confint[j, 1] = binom.confint(x=output_csvs[[i]][j, 2], n=52, method='wilson')$lower * 52
    confint[j, 2] = binom.confint(x=output_csvs[[i]][j, 2], n=52, method='wilson')$upper * 52
  }
  label_colors = ifelse(output_csvs[[i]]$`Actual Count` == 0, "red", "black")
  bar_colors = ifelse(output_csvs[[i]]$`Expected Count` > confint[, 2], "#FF0000", "#000000")
  bar_colors = rep(bar_colors, 2)
  data$value = trunc(round(data$value))
  bar_text_colors = c("black", "white")
  bar_text_colors = rep(bar_text_colors, each=nrow(confint))
  transparent_bars = ifelse(data$variable == "Actual Count", 1, 0.25)
  pdf(here("Output/Leadership_Combined", paste(gsub(".csv", "", input_csvs[i]), ".pdf", "")), width=10, height=8)
  p = ggplot(data=data, aes(major, value, fill=variable, label=value, alpha=factor(transparent_bars))) + geom_col(position="dodge", fill=bar_colors) + geom_text(position=position_dodge(width=0.9), hjust=1.5, size=3, color=bar_text_colors) + theme_minimal() + theme(axis.text.y = element_text(colour = label_colors)) +
    xlab("Major") +
    ylab("Number of Students") +
    scale_y_discrete(limits=seq(0, max(data$value), 1)) +
    guides(alpha = guide_legend(reverse = TRUE)) +
    guides(fill=FALSE) +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values = bar_colors) +
    scale_alpha_manual(values=c("0.25"=0.25, "1"=1), labels=c("Expected Number of Students", "Actual Number of Students")) +
    coord_flip() +
    ggtitle(paste(gsub("-Leadership Combined.csv", "", input_csvs[i]), "Leadership Awardees\nAUT 2017 - WIN 2019")) +
    theme(plot.title = element_text(hjust=0.5))
  plot(p)
  dev.off()
  major_totals[i, 3] = sum(output_csvs[[i]]$`Expected Count`)
  major_totals[i, 2] = sum(output_csvs[[i]]$`Actual Count`)
  major_totals[i, 1] = gsub("-Leadership Combined.csv", "", input_csvs[i])
}

pdf(here("Output/Leadership_Combined", "Total_Major_Counts-Leadership-Combined.pdf"), width=7, height=5)
major_totals = as.data.frame(major_totals)
major_totals[, 1] = fct_rev(major_totals[, 1])
major_totals = major_totals[dim(major_totals)[1]:1,]
major_totals[, 2] = as.numeric(as.character(major_totals[, 2]))
major_totals[, 3] = as.numeric(as.character(major_totals[, 3]))
label_colors = ifelse(major_totals[, 2] == 0, "red", "black")
confint = matrix(nrow=length(label_colors), ncol=2)
for(k in 1:nrow(confint)) {
  confint[k, 1] = binom.confint(x=major_totals[k, 2], n=52, method='wilson')$lower * 52
  confint[k, 2] = binom.confint(x=major_totals[k, 2], n=52, method='wilson')$upper * 52
}
bar_colors = ifelse(major_totals[, 3] > confint[, 2], "#FF0000", "#000000")
bar_colors = rep(bar_colors, 2)
bar_text_colors = c("black", "white")
bar_text_colors = rep(bar_text_colors, each=nrow(confint))
colnames(major_totals) = c("Area of Study", "Actual Total", "Expected Total")
major_totals = melt(major_totals[, c(1, 3, 2)], id.vars="Area of Study")
major_totals[, 3] = trunc(round(major_totals[, 3]))
transparent_bars = ifelse(major_totals$variable == "Actual Total", 1, 0.25)
q = ggplot(major_totals, aes(major_totals$`Area of Study`, value, fill=variable, label=value, alpha=factor(transparent_bars))) + geom_col(position="dodge", fill=bar_colors) + geom_text(position=position_dodge(width=0.9), hjust=1.25, size=4, color=bar_text_colors) +
  theme_minimal() + theme(axis.text.y = element_text(colour = label_colors)) +
  scale_y_discrete(limits=seq(0, max(major_totals$value), 1)) +
  guides(alpha = guide_legend(reverse=TRUE)) + #theme(legend.title = element_blank()) +
  scale_fill_manual(values = bar_colors) +
  guides(fill=FALSE) +
  scale_alpha_manual(values=c("0.25"=0.25, "1"=1), labels=c("Expected Total", "Actual Total"), name="Overall Total = 52") +
  xlab("Area of Study") + ylab("Number of Students") + ggtitle(paste("Number of Leadership Awardees for Each Area of Study\nAUT 2017 - WIN 2019")) + #annotation_custom(grob=textGrob(label="*Red Bars Depict", vjust=8, hjust=-2.3, gp = gpar(cex=0.6))) + annotation_custom(grob=textGrob(label="Underrepresented", vjust=10, hjust=-2.23, gp = gpar(cex=0.6))) + annotation_custom(grob=textGrob(label="Departments", vjust=12, hjust=-3.12, gp = gpar(cex=0.6))) +
  annotation_custom(grob=textGrob(label="*Examples of Physical", vjust=16, hjust=-2.53, gp = gpar(cex=0.6))) +
  annotation_custom(grob=textGrob(label="Science Majors:", vjust=18, hjust=-3.56, gp = gpar(cex=0.6))) +
  annotation_custom(grob=textGrob(label="Chemistry, Physics, etc.", vjust=20, hjust=-2.395, gp = gpar(cex=0.6))) +
  annotation_custom(grob=textGrob(label="*Examples of Social", vjust=24, hjust=-2.805, gp = gpar(cex=0.6))) +
  annotation_custom(grob=textGrob(label="Science Majors:", vjust=26, hjust=-3.56, gp = gpar(cex=0.6))) +
  annotation_custom(grob=textGrob(label="Geography, History, etc.", vjust=28, hjust=-2.39, gp = gpar(cex=0.6))) +
  theme(plot.title = element_text(hjust=0.5)) + coord_flip()
gt = ggplot_gtable(ggplot_build(q))
gt$layout$clip[gt$layout$name == "panel"] = "off"
grid.draw(gt)
#plot(q)
dev.off()