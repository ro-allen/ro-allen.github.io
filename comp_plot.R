library(ggplot2)
library(cowplot)


theme_ro_legend = function(){
  theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12), 
          axis.title.y = element_text(size = 12))
}


rx = matrix(nrow = 10, ncol = 2)
rx[,1] = 10
rx[,2] = 10
rx[1,2] = 20
rd = as.data.frame(rx)
rd = lapply(rd, function(x) x/sum(x))
rv = cbind(rd$V1, rd$V2)
rv = as.vector(as.matrix(rv))
rvd = as.data.frame(rv)
rvd$Condition = c(rep("Control",10), rep("Treatment", 10))
rvd$species = c("ASV 1", "ASV 2", "ASV 3", "ASV 4", "ASV 5", "ASV 6", "ASV 7", "ASV 8", "ASV 9", "ASV 10", "ASV 1", "ASV 2", "ASV 3", "ASV 4", "ASV 5", "ASV 6", "ASV 7", "ASV 8", "ASV 9", "ASV 10")
rvd$species = factor(rvd$species, levels = c("ASV 1", "ASV 2", "ASV 3", "ASV 4", "ASV 5", "ASV 6", "ASV 7", "ASV 8", "ASV 9", "ASV 10"))

ra = matrix(nrow = 10, ncol = 2)
ra[,1] = 10
ra[,2] = 10
ra[1,2] = 20
ra = as.data.frame(ra)
rva = cbind(ra[1], ra[2])
rva = as.vector(as.matrix(rva))
rva = as.data.frame(rva)
rva$Condition = c(rep("Control",10), rep("Treatment", 10))
rva$species = c("ASV 1", "ASV 2", "ASV 3", "ASV 4", "ASV 5", "ASV 6", "ASV 7", "ASV 8", "ASV 9", "ASV 10", "ASV 1", "ASV 2", "ASV 3", "ASV 4", "ASV 5", "ASV 6", "ASV 7", "ASV 8", "ASV 9", "ASV 10")
rva$species = factor(rva$species, levels = c("ASV 1", "ASV 2", "ASV 3", "ASV 4", "ASV 5", "ASV 6", "ASV 7", "ASV 8", "ASV 9", "ASV 10"))
#55 tot sum, 10 = 18.1818, ASV 2-10 = 5 (n=9, tot = 45)
rq = matrix(nrow = 10, ncol = 2)
rq[,1] = 10
rq[,2] = 5
rq[1,2] = 10
rq = as.data.frame(rq)
rvq = lapply(rq, function(x) x/sum(x))
rvq = cbind(rvq$V1, rvq$V2)
rvq = as.vector(as.matrix(rvq))
rvq = as.data.frame(rvq)
rvq$Condition = c(rep("Control",10), rep("Treatment", 10))
rvq$species = c("ASV 1", "ASV 2", "ASV 3", "ASV 4", "ASV 5", "ASV 6", "ASV 7", "ASV 8", "ASV 9", "ASV 10", "ASV 1", "ASV 2", "ASV 3", "ASV 4", "ASV 5", "ASV 6", "ASV 7", "ASV 8", "ASV 9", "ASV 10")
rvq$species = factor(rvd$species, levels = c("ASV 1", "ASV 2", "ASV 3", "ASV 4", "ASV 5", "ASV 6", "ASV 7", "ASV 8", "ASV 9", "ASV 10"))

rw = matrix(nrow = 10, ncol = 2)
rw[,1] = 10
rw[,2] = 5
rw[1,2] = 10
rw = as.data.frame(rw)
rvw = cbind(rw[1], rw[2])
rvw = as.vector(as.matrix(rvw))
rvw = as.data.frame(rvw)
rvw$Condition = c(rep("Control",10), rep("Treatment", 10))
rvw$species = c("ASV 1", "ASV 2", "ASV 3", "ASV 4", "ASV 5", "ASV 6", "ASV 7", "ASV 8", "ASV 9", "ASV 10", "ASV 1", "ASV 2", "ASV 3", "ASV 4", "ASV 5", "ASV 6", "ASV 7", "ASV 8", "ASV 9", "ASV 10")
rvw$species = factor(rvw$species, levels = c("ASV 1", "ASV 2", "ASV 3", "ASV 4", "ASV 5", "ASV 6", "ASV 7", "ASV 8", "ASV 9", "ASV 10"))

#end
p1 = ggplot(rvd, aes(species, rv, fill = Condition)) + geom_bar(stat = "identity", position = "dodge") + theme_bw() + ylab("Relative Abundance") + xlab("Amplicon Sequence Variant") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.21)) + 
  scale_fill_manual(values = c("skyblue2", "darkblue")) +
  theme_ro_legend()

p2 = ggplot(rva, aes(species, rva, fill = Condition)) + geom_bar(stat = "identity", position = "dodge") + theme_bw() + ylab("True Abundance") + xlab("Amplicon Sequence Variant") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 21)) + 
  scale_fill_manual(values = c("skyblue2", "darkblue")) +
  theme_ro_legend()

p3 = ggplot(rvq, aes(species, rvq, fill = Condition)) + geom_bar(stat = "identity", position = "dodge") + theme_bw() + ylab("Relative Abundance") + xlab("Amplicon Sequence Variant") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.21)) + 
  scale_fill_manual(values = c("lightcoral", "darkred")) +
  theme_ro_legend()

p4 = ggplot(rvw, aes(species, rvw, fill = Condition)) + geom_bar(stat = "identity", position = "dodge") + theme_bw() + ylab("True Abundance") + xlab("Amplicon Sequence Variant") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 21)) + 
  scale_fill_manual(values = c("lightcoral", "darkred")) +
  theme_ro_legend()

quartz()

comp_plot  = plot_grid(p2, p1, p4, p3, labels = c('a', 'b', 'c', 'd'), ncol = 2)
save_plot("discussion_comp_plot.png", comp_plot, base_height = 9, base_width = 13.5)
?save_plot
