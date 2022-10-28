
rentrée <- 2020

# Style

k_style <- kpiesr_style(
  point_size = 20,
  line_size = 1,
  text_size = 6,
  yaxis_position = "left",
  evol_y_breaker = scale_100_breaker)

o_style <- kpiesr_style(
  point_size = 20,
  line_size = 0.7,
  text_size = 6,
  primaire_margin = 1.25,
  strip_labeller = lfc_dont_labeller,
  yaxis_position = "left",
  evol_y_breaker = scale_100_breaker)

lfc_pc_labeller_custom <- function(labels) {
  return(
    stringr::str_replace(lfc_pc_labeller(labels),"\\(","\n (")
  )
}

onorm_style <- kpiesr_style(
  point_size = 20,
  line_size = 0.7,
  text_size = 6,
  primaire_margin = 1.25,
  strip_labeller = lfc_pc_labeller_custom,
  label_wrap = 12,
  evol_y_breaker = scale_100_breaker
)


theme_set(ggcpesrthemes::theme_cpesr() + 
            theme(plot.title = element_text(hjust=1),
                  panel.spacing = unit(2,"lines"), 
                  plot.margin = margin(0,0,0,0),
                  strip.text = element_text(size=rel(1.3), 
                                            margin=margin(c(2,0,2,0)))))



axis_text_size_rel <- 0.8

rm_lt <- ggplot2::theme(legend.position = "none",
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        axis.ticks.x = element_blank())
rm_label <- ggplot2::theme(legend.position = "none",
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           strip.background = element_blank(),
                           strip.text.x = element_blank())
rm_xytext <- rm_lt + ggplot2::theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                                    axis.text.y = element_blank(), axis.ticks.y = element_blank())
rm_ytext <- rm_lt + ggplot2::theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
shrink_xtext <- ggplot2::theme(axis.text.x = element_text(margin = margin(-10,0,0,0)))
rm_ymingrid <- ggplot2::theme(panel.grid.minor.y = element_blank())
rm_ygrid <- ggplot2::theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())
rm_xgrid <- ggplot2::theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
sats <- ggplot2::theme(axis.text.x = element_text(size = rel(axis_text_size_rel)),
                       axis.text.y = element_text(size = rel(axis_text_size_rel)))
bottit <- ggplot2::theme(plot.caption = element_text(hjust = 0.5, size=rel(1.2)),
                         plot.title = element_text(hjust=0.5))
rl_margin <- ggplot2::theme(plot.margin = margin(l=10,r=10))
rle_margin <- ggplot2::theme(plot.margin = margin(l=3,r=10))
rL_margin <- ggplot2::theme(plot.margin = margin(l=10,r=35))
rm_facetmargins <- ggplot2::theme(panel.spacing = unit(0, "lines"))
small_facetmargins <- ggplot2::theme(panel.spacing = unit(10, "pt"))
rm_ytextmargin <- ggplot2::theme(axis.text.y=element_text(margin=margin(r=-5)))


th_abs = rm_lt + rl_margin + rm_xytext + rm_facetmargins + rm_xgrid
th_norm = rm_lt + rL_margin
th_evol <- rm_label + rL_margin
