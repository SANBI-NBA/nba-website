#####################################################################################
##
## Script name: custom_nba_plot_donut.R
##
## Purpose of script:
## customise the donut plot from nba_plot()
##
## Author: Jock Currie
##
## Notes:
##
##
#####################################################################################
### packages & functions

require(ggplot2)
# require(sf)
# require(terra)

#####################################################################################
### 
nba_plot_donut <- function(DF, GROUPS, COLS, NUM = FALSE, LAB, GRP = FALSE, SAVE = NULL, SCALE_TEXT = 1) {
  # if (CHRT == "donut") {
  if (GRP == FALSE) {
    dat <- DF %>% tidyr::pivot_longer({
      {
        COLS
      }
    }, names_to = "FILL", values_to = "COUNT") %>% 
      dplyr::summarise(COUNT = sum(COUNT, na.rm = T), .by = FILL) %>% 
      dplyr::mutate(FILL = factor(FILL, levels = nbaR::NBA_categories)) %>% 
      dplyr::mutate(ymax = cumsum(COUNT)) %>% 
      dplyr::mutate(ymin = ymax - COUNT) %>% 
      dplyr::ungroup()
    if (NUM == FALSE) {
      plot <- ggplot2::ggplot(dat, aes(ymax = ymax,ymin = ymin, xmax = 4, xmin = 3, fill = FILL)) +
        ggplot2::geom_rect() + ggplot2::coord_polar(theta = "y") +
        ggplot2::xlim(c(2, 4)) + ggplot2::scale_fill_manual(values = nbaR::NBA_colours) +
        ggplot2::labs(fill = "", title = LAB) + ggplot2::theme_void() +
        ggplot2::theme(panel.background = element_rect(fill = "transparent", color = NA), 
                       plot.background = element_rect(fill = "transparent", color = NA), 
                       title = element_text(size = 10*SCALE_TEXT), strip.text = element_blank())
    } else {
      plot <- ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = FILL)) +
        ggplot2::geom_rect() + ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax)/2, label = COUNT), color = "black", size = 5 * SCALE_TEXT) + 
        ggplot2::coord_polar(theta = "y") +
        ggplot2::xlim(c(2, 4)) + 
        ggplot2::scale_fill_manual(values = nbaR::NBA_colours) +
        ggplot2::labs(fill = "", title = LAB) + ggplot2::theme_void() +
        ggplot2::theme(panel.background = element_rect(fill = "transparent", color = NA), 
                       plot.background = element_rect(fill = "transparent", color = NA), 
                       title = element_text(size = 10), strip.text = element_blank())
    }
  } else {
    
    dat <- DF %>% tidyr::pivot_longer({
      {
        COLS
      }
    }, names_to = "FILL", values_to = "COUNT") %>% 
      dplyr::mutate(TOT = sum(COUNT, 
                              na.rm = T), .by = {
                                {
                                  GROUPS
                                }
                              }) %>% dplyr::mutate(PERCENTAGE = (COUNT/TOT) * 100) %>% 
      dplyr::mutate(FILL = factor(FILL, levels = nbaR::NBA_categories)) %>% 
      dplyr::mutate(ymax = cumsum(PERCENTAGE), .by = {
        {
          GROUPS
        }
      }) %>% 
      dplyr::mutate(ymin = ymax - PERCENTAGE)
    
    if (NUM == FALSE) {
      plot <- ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = FILL)) + 
        ggplot2::geom_rect() + ggplot2::facet_wrap(vars({
          {
            GROUPS
          }
        })) + 
        ggplot2::coord_polar(theta = "y") + ggplot2::xlim(c(2,4)) + 
        ggplot2::scale_fill_manual(values = nbaR::NBA_colours) + 
        ggplot2::labs(fill = "", title = "") + ggplot2::theme_void() + 
        ggplot2::theme(panel.background = element_rect(fill = "transparent", color = NA), 
                       plot.background = element_rect(fill = "transparent", color = NA), 
                       title = element_text(size = 10*SCALE_TEXT), strip.text = element_blank())
    } else {
      plot <- ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = FILL)) + 
        ggplot2::geom_rect() + ggplot2::facet_wrap(vars({
          {
            GROUPS
          }
        })) + 
        ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax)/2, label = COUNT), color = "black", size = 3*SCALE_TEXT) + 
        ggplot2::coord_polar(theta = "y") + 
        ggplot2::xlim(c(2, 4)) + ggplot2::scale_fill_manual(values = nbaR::NBA_colours) + 
        ggplot2::labs(fill = "", title = "") + 
        ggplot2::theme_void() + 
        ggplot2::theme(panel.background = element_rect(fill = "transparent", color = NA), 
                       plot.background = element_rect(fill = "transparent", color = NA), 
                       title = element_text(size = 10*SCALE_TEXT))
    }
  }
}

  #################################
  
  
  
  
  
  
#   else {
#     ord <- DF %>% dplyr::pull({
#       {
#         GROUPS
#       }
#     })
#     dat <- DF %>% tidyr::pivot_longer({
#       {
#         COLS
#       }
#     }, names_to = "FILL", values_to = "COUNT") %>% dplyr::mutate(TOT = sum(COUNT, 
#                                                                            na.rm = T), .by = {
#                                                                              {
#                                                                                GROUPS
#                                                                              }
#                                                                            }) %>% dplyr::mutate(PERCENTAGE = (COUNT/TOT) * 100) %>% 
#       dplyr::mutate(dplyr::across(COUNT, ~dplyr::na_if(., 
#                                                        0))) %>% dplyr::mutate(FILL = factor(FILL, levels = nbaR::NBA_categories))
#     if (NUM == TRUE) {
#       plot <- ggplot2::ggplot(dat, aes(y = PERCENTAGE, 
#                                        x = factor({
#                                          {
#                                            GROUPS
#                                          }
#                                        }, level = ord), fill = FILL)) + ggplot2::geom_bar(stat = "identity", 
#                                                                                           position = position_stack(reverse = TRUE), width = 0.5) + 
#         ggplot2::geom_text(aes(label = COUNT), position = position_stack(vjust = 0.5, 
#                                                                          reverse = TRUE), size = 3 * SCALE_TEXT, color = "black", 
#                            show.legend = FALSE) + ggplot2::scale_fill_manual(values = nbaR::NBA_colours) + 
#         ggplot2::ylab({
#           {
#             LAB
#           }
#         }) + ggplot2::xlab("") + ggplot2::guides(fill = guide_legend(reverse = F, 
#                                                                      nrow = 1, size = 0.5 * SCALE_TEXT)) + ggplot2::labs(fill = "") + 
#         ggplot2::scale_y_continuous(labels = function(x) paste0(x, 
#                                                                 "%"), breaks = c(0, 50, 100)) + ggplot2::theme_minimal() + 
#         ggplot2::theme(legend.position = "left", panel.grid.minor = element_blank(), 
#                        axis.line = element_blank(), panel.grid.major.y = element_blank(), 
#                        legend.text = element_text(size = 8 * SCALE_TEXT), 
#                        plot.margin = margin(10, 10, 10, 10), axis.text.x = element_text(size = 8 * 
#                                                                                           SCALE_TEXT), axis.text.y = element_text(size = 8 * 
#                                                                                                                                     SCALE_TEXT), axis.title.x = element_text(size = 10 * 
#                                                                                                                                                                                SCALE_TEXT), axis.title.y = element_text(size = 10 * 
#                                                                                                                                                                                                                           SCALE_TEXT), legend.key.size = unit(1 * SCALE_TEXT, 
#                                                                                                                                                                                                                                                               "lines"), legend.box.margin = margin()) + 
#         ggplot2::coord_flip()
#     }
#     else {
#       plot <- ggplot2::ggplot(dat, aes(y = PERCENTAGE, 
#                                        x = factor({
#                                          {
#                                            GROUPS
#                                          }
#                                        }, level = ord), fill = FILL)) + ggplot2::geom_bar(stat = "identity", 
#                                                                                           position = position_stack(reverse = TRUE), width = 0.5) + 
#         ggplot2::scale_fill_manual(values = nbaR::NBA_colours) + 
#         ggplot2::ylab({
#           {
#             LAB
#           }
#         }) + ggplot2::xlab("") + ggplot2::guides(fill = guide_legend(reverse = F, 
#                                                                      nrow = 1, size = 0.5 * SCALE_TEXT)) + ggplot2::labs(fill = "") + 
#         ggplot2::scale_y_continuous(labels = function(x) paste0(x, 
#                                                                 "%"), breaks = c(0, 50, 100)) + ggplot2::theme_minimal() + 
#         ggplot2::theme(legend.position = "bottom", panel.grid.minor = element_blank(), 
#                        axis.line = element_blank(), panel.grid.major.y = element_blank(), 
#                        legend.text = element_text(size = 8 * SCALE_TEXT), 
#                        plot.margin = margin(10, 10, 10, 10), axis.text.x = element_text(size = 8 * 
#                                                                                           SCALE_TEXT), axis.text.y = element_text(size = 8 * 
#                                                                                                                                     SCALE_TEXT), axis.title.x = element_text(size = 10 * 
#                                                                                                                                                                                SCALE_TEXT), axis.title.y = element_text(size = 10 * 
#                                                                                                                                                                                                                           SCALE_TEXT), legend.key.size = unit(1 * SCALE_TEXT, 
#                                                                                                                                                                                                                                                               "lines"), legend.box.margin = margin()) + 
#         ggplot2::coord_flip()
#     }
#   }
#   if (!is.null(SAVE)) {
#     plot_save <- plot + theme(legend.justification = "right")
#     ggsave(paste0("outputs/", SAVE, ".jpeg"), plot = plot_save, 
#            height = 8, width = 6, units = "cm", dpi = 300, create.dir = TRUE)
#   }
#   plot
# }

#####################################################################################
###

#####################################################################################

# detach("package:xxx", unload=TRUE)
