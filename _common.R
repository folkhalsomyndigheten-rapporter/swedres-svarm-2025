## Useful functions for Swedres-Svarm in Quarto =======================================
#' --- title: "Swedres-Svarm Quarto Helper Functions"
#' --- author: "Julius Lautenbach, Folkhälsomyndigheten"
#' --- date: "`r Sys.Date()`"
#' --- output: html_document

# Load required libraries
library(tidyverse)
library(DT)
library(ggiraph)
library(patchwork)
library(systemfonts)
library(showtext)
library(readxl)
library(knitr)
library(gt)
library(glue)
library(xml2)
library(downlit)
library(binom)
library(janitor)

## weblinks: {target="_blank"}

#if (!require(fohmR)) renv::install("H:/Projects/R-package-builds/fohmR_0.1.0.tar.gz")
#library(fohmr)

#install.packages("pak")
#pak::pkg_install("git::https://git.folkhalsomyndigheten.se/sb-za/fohmr.git")

#scale_fill_SweSva <-  function(){fohmR::scale_fill_fohm()}

scale_fill_SweSva <- function() {
  if ("fohmR" %in% installed.packages()) {
    fohmR::scale_fill_fohm()
  } else {
    ggplot2::scale_fill_brewer(palette = "Set1") # Example default, you can change the palette as needed
  }
}

#scale_color_SweSva <-  function(){fohmR::scale_color_fohm()}

scale_color_SweSva <- function() {
  if ("fohmR" %in% installed.packages()) {
    fohmR::scale_color_fohm()
  } else {
    ggplot2::scale_color_brewer(palette = "Set1") # Example default, you can change the palette as needed
  }
}

one_color_SweSva <- setNames("#0065AC","one_color_SweSva")
# =============================================================================
# 1. Data Table Function
# =============================================================================

make_table_data_tab_old <- function(data, file_name = "default") {
  out <- data %>% 
    ## rename column names to capital
    rename_with(~ str_replace_all(., "(^|_).", str_to_upper)) |> 
    ## remove tempoary columns
    dplyr::select(-contains(".tmp")) |> 
    DT::datatable(extensions = 'Buttons',
                  options = list(dom = 'Blfrtip',
                                 autoWidth = TRUE,
                                 buttons = list(
                                   list(extend = 'copy'),
                                   list(extend = 'csv', filename = file_name),
                                   list(extend = 'excel', filename = file_name)
                                   #list(extend = 'pdf', filename = file_name)
                                   ),
                                 text = "Download",
                                 pageLength = 5,
                                 lengthMenu = list(c(5, 10, 25, 50, -1), c(5, 10, 25, 50, "All")) 
                                 #lengthMenu = c(5,10,25,50,"All")
                                 ),
                  rownames = F
    ) 
  return(out)
}

## alternative with more fohm style
library(DT)
library(htmltools)
make_table_data_tab <- function(
    data,
    file_name = "default",
    source_note = NULL,
    page_length = 5
) {
  # Clean the data
  cleaned_data <- data %>%
    # Capitalize column names (replace underscores with spaces and capitalize)
    rename_with(~ str_replace_all(., "(^|_).", str_to_upper)) %>%
    # Remove temporary columns
    dplyr::select(-contains(".tmp")) |> 
    # remoce all markdown character
    mutate(across(everything(), ~ str_remove_all(., "\\\\|\\*|\\**|\\||<sub>|</sub>|~|<br>")))
  
  # Create the DT datatable with buttons and options
  dt_table <- datatable(
    cleaned_data,
    extensions = "Buttons",
    options = list(
      dom = "Blfrtip",  # Enable buttons, length changing, filtering, etc.
      autoWidth = TRUE,
      buttons = list(
        list(extend = "copy"),
        list(extend = "csv", filename = file_name),
        list(extend = "excel", filename = file_name)
      ),
      pageLength = page_length,
      lengthMenu = list(c(5, 10, 25, 50, -1), c(5, 10, 25, 50, "All")),
      rownames = FALSE
    ),
    rownames = FALSE
  )
  
  # Define the CSS for Folkhälsomyndigheten styling
  custom_css <- "
  <style>
    /* Table container */
    .dataTables_wrapper {
      font-family: Arial, Helvetica, sans-serif;
      font-size: 14px;
      margin-bottom: 20px;
    }

    /* Header styling: blue background, white text, bold, centered */
    table.dataTable thead th {
      background-color: #3764a0 !important;
      color: white !important;
      font-weight: bold !important;
      text-align: center !important;
      padding: 8px !important;
      border: 1px solid #dddddd !important;
    }

    /* First column header left-aligned */
    table.dataTable thead th:first-child {
      text-align: left !important;
    }

    /* Body styling: borders and padding */
    table.dataTable tbody td {
      padding: 8px !important;
      border: 1px solid #dddddd !important;
      text-align: center !important;
    }

    /* First column body left-aligned and bold */
    table.dataTable tbody td:first-child {
      text-align: left !important;
      font-weight: bold !important;
    }

    /* Alternating row colors */
    table.dataTable tbody tr:nth-child(even) {
      background-color: #f9f9f9 !important;
    }

    table.dataTable tbody tr:nth-child(odd) {
      background-color: white !important;
    }

    /* Hover effect (optional) */
    table.dataTable tbody tr:hover {
      background-color: #f0f0f0 !important;
    }

    /* Buttons styling */
    .buttons-copy, .buttons-csv, .buttons-excel {
      background-color: #3764a0 !important;
      color: white !important;
      border: none !important;
      padding: 6px 12px !important;
      margin-right: 5px !important;
      border-radius: 4px !important;
    }

    /* Pagination and length menu styling */
    .dataTables_length, .dataTables_filter, .dataTables_info, .dataTables_paginate {
      font-size: 12px !important;
      color: #666666 !important;
    }
  </style>
  "
  
  # Combine the CSS with the datatable
  styled_dt <- tagList(
    HTML(custom_css),
    dt_table
  )
  
  # Add source note if provided
  if (!is.null(source_note)) {
    styled_dt <- tagList(
      styled_dt,
      div(
        style = "font-size: 12px; color: #666666; text-align: center; margin-top: 10px;",
        source_note
      )
    )
  }
  
  return(styled_dt)
}

# =============================================================================
# 2. Dynamic Plot with ggiraph
# =============================================================================

# Vectorized Markdown → HTML conversion
md_to_html <- Vectorize(function(x) commonmark::markdown_html(x), USE.NAMES = FALSE)


tooltip_css <- "padding:5px;border-radius:3px;"

make_dynamic_plot <- function(plot, filename = "filename",width = 8, height = 5
                              ) {
  out <- ggiraph::girafe(
    ggobj = plot,
    width_svg = width, 
    height_svg = height,
    options = list(
      opts_hover_inv(css = "opacity:0.1;"),
      opts_hover(css = "stroke-width:1;"),
      opts_tooltip(use_fill = TRUE),#, tooltip_css),
      opts_zoom(max = 5),
      opts_sizing(rescale = TRUE),
      opts_toolbar(
        position = "top",
        saveaspng = TRUE,
        pngname = paste0("SwedresSvarm2025_figure_", filename),
        delay_mouseout = 2000
      )
    )
  )
  return(out)
}

# =============================================================================
# 3. ggplot2 Theme and Styling
# =============================================================================
# 3.1. Swedres-Svarm Theme (`theme_SweSva`)

theme_SweSva_old <- function(text_size = 11) {
  if ("fohmR" %in% installed.packages()) {
    fohmR::theme_fohm(text_size = text_size)
  } else {
    
    function(text_size=text_size,
                             selected_font = "Open Sans", 
                             gridline_x = FALSE,
                             gridline_y = TRUE){
      gdtools::register_gfont()
      showtext::showtext_auto()
      sysfonts::font_add_google(selected_font)
      gridline <- element_line(color = "#999999",linewidth = 0.25)
      gridline_x <- if (isTRUE(gridline_x)) gridline else element_blank()
      gridline_y <- if (isTRUE(gridline_y)) gridline else element_blank()
      
      out <- ggplot2::theme_bw(base_family = selected_font, base_size = text_size) %+replace%
        #ggplot2::theme_minimal() + 
        ggplot2::theme(
          text = ggplot2::element_text(size = text_size,family = selected_font),
          panel.grid.major.x = gridline_x,
          #panel.grid.minor.x = ggplot2::element_blank(),
          
          panel.grid.major.y = gridline_y,
          #panel.grid.minor.y = ggplot2::element_blank(),
          panel.background = element_blank(),
          ## uncomment for adjustment
          #panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          panel.border = ggplot2::element_blank(),
          
          axis.line = ggplot2::element_line(colour = "black"),
          axis.title.x = ggplot2::element_text(size = text_size + 1,
                                               vjust=-1,
                                               face = "bold"),
          axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size=text_size),
          axis.text.y = ggplot2::element_text(angle = 0, #hjust = 1, 
                                              size=text_size),
          
          axis.ticks.x = element_line(linetype = "solid",
                                      linewidth = 0.25, 
                                      color = "#999999"),
          axis.ticks.length.x = unit(4, units = "pt"),
          
          
          plot.title.position = "plot",
          plot.title = ggplot2::element_text(size = text_size + 2),
          plot.subtitle = element_text(hjust=.01, ## adjust y axis title HERE
                                       vjust=-6.5,
                                       face = "bold"
                                       #size = text_size + 1,
                                       # color = "#999999", 
                                       #margin = margin(l=0,b = 10) #top, right, bottom, left
          ), 
          
          plot.background = ggplot2::element_blank(),
          plot.margin = margin(t = 0.1, r = 0.1, b = 0.1,l = 0.1, "cm"),  # Increase bottom margin to accommodate legend and subtitle
          plot.caption.position = "plot",
          plot.caption = element_text(size = text_size - 2, 
                                      color = "#777777",
                                      margin = margin(t = 15),
                                      hjust = 0),
          
          legend.title = element_blank(),
          legend.text=ggplot2::element_text(size = text_size - 1),
          legend.direction = "horizontal",
          legend.justification = c(1, 1),  # Center the legend horizontally 0.5, 1
          legend.position = "top",#c(0.6, 1.3),  # Position legend above the subtitle
          
          legend.key.size = unit(.25, 'cm'), #change legend key size
          legend.key.height = unit(.25, 'cm'), #change legend key height
          legend.key.width = unit(.25, 'cm'), #change legend key width
          
          complete = TRUE
          
        )
      class(out) <- c("mytheme",class(ggplot2::theme_minimal()))
      return(out)
    }
    
    ggplot_add.mytheme <- function(object, plot, object_name) {
      
      # Conditional wrapping function
      wrapped_strings <- if_else(
        str_detect(plot$labels$y, regex("\\/100\\s+000", ignore_case = TRUE)),
        map2_chr(
          plot$labels$y,
          str_length(str_extract(plot$labels$y, "\\S+")),
          ~ str_wrap(.x, width = .y)
        ),
        str_wrap(plot$labels$y, width = 20)
      )
      
      # Update the plot
      plot$labels$subtitle <- wrapped_strings
      plot$labels$y <- ""
      plot$theme <- update_theme(plot$theme, object)
      plot
    }
    
    update_theme <- function(oldtheme, newtheme) {
      # If the newtheme is a complete one, don't bother searching
      # the default theme -- just replace everything with newtheme
      if (isTRUE(attr(newtheme, "complete", exact = TRUE)))
        return(newtheme)
      
      # These are elements in newtheme that aren't already set in oldtheme.
      # They will be pulled from the default theme.
      newitems <- !names(newtheme) %in% names(oldtheme)
      newitem_names <- names(newtheme)[newitems]
      oldtheme[newitem_names] <- theme_get()[newitem_names]
      
      # Update the theme elements with the things from newtheme
      # Turn the 'theme' list into a proper theme object first, and preserve
      # the 'complete' attribute. It's possible that oldtheme is an empty
      # list, and in that case, set complete to FALSE.
      old.validate <- isTRUE(attr(oldtheme, "validate"))
      new.validate <- isTRUE(attr(newtheme, "validate"))
      oldtheme <- do.call(theme, c(oldtheme,
                                   complete = isTRUE(attr(oldtheme, "complete")),
                                   validate = old.validate & new.validate))
      
      oldtheme + newtheme
    }  
    
  }
}

## allow markdown labels
## previously theme_SweSva_md
theme_SweSva <- function(text_size=11,
                            selected_font = "Open Sans", 
                            gridline_x = FALSE,
                            gridline_y = TRUE,
                            markdown = NULL){
  gdtools::register_gfont()
  showtext::showtext_auto()
  sysfonts::font_add_google(selected_font)
  gridline <- element_line(color = "#999999",linewidth = 0.25)
  gridline_x <- if (isTRUE(gridline_x)) gridline else element_blank()
  gridline_y <- if (isTRUE(gridline_y)) gridline else element_blank()
  
  # Helper function to apply markdown if requested
  use_markdown <- function(element_type) {
    if (is.null(markdown)) return(FALSE)
    element_type %in% markdown
  }
  
  out <- ggplot2::theme_bw(base_family = selected_font, base_size = text_size) %+replace%
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size,family = selected_font),
      panel.grid.major.x = gridline_x,
      panel.grid.major.y = gridline_y,
      panel.background = element_blank(),
      panel.border = ggplot2::element_blank(),
      
      axis.line = ggplot2::element_line(colour = "black"),
      axis.title.x = if (use_markdown("axis.title.x")) 
        ggtext::element_markdown(size = text_size + 1, vjust = -1, face = "bold") 
      else ggplot2::element_text(size = text_size + 1, vjust = -1, face = "bold"),
      axis.text.x = if (use_markdown("axis.text.x")) 
        ggtext::element_markdown(angle = 0, hjust = 0.5, size = text_size) 
      else ggplot2::element_text(angle = 0, hjust = 0.5, size = text_size),
      #axis.text.y = if (use_markdown("axis.text.y")) 
      #  ggtext::element_markdown(angle = 0, size = text_size,hjust=1) 
      #else ggplot2::element_text(angle = 0, size = text_size),
      
      axis.text.y = if (use_markdown("axis.text.y")) 
        ggtext::element_markdown(angle = 0, size = text_size, hjust = 1, margin = margin(r = 1, unit = "mm"))
      else ggplot2::element_text(angle = 0, size = text_size, hjust = 1, margin = margin(r = 1, unit = "mm")),
      
      axis.ticks.x = element_line(linetype = "solid",
                                  linewidth = 0.25, 
                                  color = "#999999"),
      axis.ticks.length.x = unit(4, units = "pt"),
      
      
      plot.title.position = "plot",
      plot.title = if (use_markdown("plot.title")) 
        ggtext::element_markdown(size = text_size + 2) 
      else ggplot2::element_text(size = text_size + 2),
      plot.subtitle = if (use_markdown("plot.subtitle")) 
        ggtext::element_markdown(hjust = 0.01, vjust = -6.5, face = "bold") 
      else element_text(hjust = 0.01, vjust = -6.5, face = "bold"),
      
      plot.background = ggplot2::element_blank(),
      plot.margin = margin(t = 0.1, r = 0.1, b = 0.1,l = 0.1, "cm"),
      plot.caption.position = "plot",
      plot.caption = if (use_markdown("plot.caption")) 
        ggtext::element_markdown(size = text_size - 2, color = "#777777", margin = margin(t = 15), hjust = 0) 
      else element_text(size = text_size - 2, color = "#777777", margin = margin(t = 15), hjust = 0),
      
      legend.title = if (use_markdown("legend.title")) 
        ggtext::element_markdown() 
      else element_blank(),
      legend.text = if (use_markdown("legend.text")) 
        ggtext::element_markdown(size = text_size - 1) 
      else ggplot2::element_text(size = text_size - 1),
      legend.direction = "horizontal",
      legend.justification = c(1, 1),
      legend.position = "top",
      
      legend.key.size = unit(.25, 'cm'),
      legend.key.height = unit(.25, 'cm'),
      legend.key.width = unit(.25, 'cm'),
      
      complete = TRUE
    )
  class(out) <- c("mytheme",class(ggplot2::theme_minimal()))
  return(out)
}

## usage: 
# Enable markdown for all text elements
#theme_SweSva(markdown = c("axis.title.x", "axis.text.x", "axis.text.y", 
#                          "plot.title", "plot.subtitle", "plot.caption",
#                          "legend.title", "legend.text"))



## make labels nicer

theme_SweSva_nicelabs <- function(text_size = 11) {
  if ("fohmR" %in% installed.packages()) {
    fohmR::theme_fohm(text_size = text_size)
  } else {
    
    function(text_size=text_size,
             selected_font = "Open Sans", 
             gridline_x = FALSE,
             gridline_y = TRUE){
      gdtools::register_gfont()
      showtext::showtext_auto()
      sysfonts::font_add_google(selected_font)
      gridline <- element_line(color = "#999999",linewidth = 0.25)
      gridline_x <- if (isTRUE(gridline_x)) gridline else element_blank()
      gridline_y <- if (isTRUE(gridline_y)) gridline else element_blank()
      
      out <- ggplot2::theme_bw(base_family = selected_font, base_size = text_size) %+replace%
        #ggplot2::theme_minimal() + 
        ggplot2::theme(
          text = ggplot2::element_text(size = text_size,family = selected_font),
          panel.grid.major.x = gridline_x,
          #panel.grid.minor.x = ggplot2::element_blank(),
          
          panel.grid.major.y = gridline_y,
          #panel.grid.minor.y = ggplot2::element_blank(),
          panel.background = element_blank(),
          ## uncomment for adjustment
          #panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          panel.border = ggplot2::element_blank(),
          
          axis.line = ggplot2::element_line(colour = "black"),
          axis.title.x = ggplot2::element_text(size = text_size + 1,
                                               vjust=-1,
                                               face = "bold"),
          axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size=text_size),
          axis.text.y = ggplot2::element_text(angle = 0, #hjust = 1, 
                                              size=text_size),
          
          axis.ticks.x = element_line(linetype = "solid",
                                      linewidth = 0.25, 
                                      color = "#999999"),
          axis.ticks.length.x = unit(4, units = "pt"),
          
          plot.title.position = "plot",
          plot.title = ggplot2::element_text(size = text_size + 2),
          plot.subtitle = element_text(hjust=.01, ## adjust y axis title HERE
                                       vjust=-6.5,
                                       face = "bold"
                                       #size = text_size + 1,
                                       # color = "#999999", 
                                       #margin = margin(l=0,b = 10) #top, right, bottom, left
          ),
          
          plot.background = ggplot2::element_blank(),
          plot.margin = margin(t = 0.1, r = 0.1, b = 0.1,l = 0.1, "cm"),  # Increase bottom margin to accommodate legend and subtitle
          plot.caption.position = "plot",
          plot.caption = element_text(size = text_size - 2, 
                                      color = "#777777",
                                      margin = margin(t = 15),
                                      hjust = 0),
          
          legend.title = element_blank(),
          legend.text=ggplot2::element_text(size = text_size - 1),
          legend.direction = "horizontal",
          legend.justification = c(1, 1),  # Center the legend horizontally 0.5, 1
          legend.position = "top",#c(0.6, 1.3),  # Position legend above the subtitle
          
          legend.key.size = unit(.25, 'cm'), #change legend key size
          legend.key.height = unit(.25, 'cm'), #change legend key height
          legend.key.width = unit(.25, 'cm'), #change legend key width
          
          complete = TRUE
          
        )
      class(out) <- c("mytheme",class(ggplot2::theme_minimal()))
      return(out)
    }
    
    ggplot_add.mytheme <- function(object, plot, object_name) {
      # Apply label_number() to y-axis if it's numeric continuous and has default labels
      if (requireNamespace("scales", quietly = TRUE) && !is.null(plot$scales)) {
        for (i in seq_along(plot$scales$scales)) {
          scale <- plot$scales$scales[[i]]
          if ("y" %in% scale$aesthetics && inherits(scale, "ScaleContinuousNumeric")) {
            if (is.null(scale$labels) || identical(scale$labels, ggplot2::waiver())) {
              plot$scales$scales[[i]]$labels <- scales::label_number()
            }
          }
        }
      }
      
      # Conditional wrapping function
      wrapped_strings <- if_else(
        str_detect(plot$labels$y, regex("\\/100\\s+000", ignore_case = TRUE)),
        map2_chr(
          plot$labels$y,
          str_length(str_extract(plot$labels$y, "\\S+")),
          ~ str_wrap(.x, width = .y)
        ),
        str_wrap(plot$labels$y, width = 20)
      )
      
      # Update the plot
      plot$labels$subtitle <- wrapped_strings
      plot$labels$y <- ""
      plot$theme <- update_theme(plot$theme, object)
      plot
    }
    
    update_theme <- function(oldtheme, newtheme) {
      # If the newtheme is a complete one, don't bother searching
      # the default theme -- just replace everything with newtheme
      if (isTRUE(attr(newtheme, "complete", exact = TRUE)))
        return(newtheme)
      
      # These are elements in newtheme that aren't already set in oldtheme.
      # They will be pulled from the default theme.
      newitems <- !names(newtheme) %in% names(oldtheme)
      newitem_names <- names(newtheme)[newitems]
      oldtheme[newitem_names] <- theme_get()[newitem_names]
      
      # Update the theme elements with the things from newtheme
      # Turn the 'theme' list into a proper theme object first, and preserve
      # the 'complete' attribute. It's possible that oldtheme is an empty
      # list, and in that case, set complete to FALSE.
      old.validate <- isTRUE(attr(oldtheme, "validate"))
      new.validate <- isTRUE(attr(newtheme, "validate"))
      oldtheme <- do.call(theme, c(oldtheme,
                                   complete = isTRUE(attr(oldtheme, "complete")),
                                   validate = old.validate & new.validate))
      
      oldtheme + newtheme
    }
    
  }
}


# =============================================================================
# 4. Captions and Status Messages
# =============================================================================

add_fohm_caption <- function(caption = "Source: The Public Health Agency of Sweden") {
  labs(caption = caption)
}

add_sva_caption <- function(caption = "Source: Swedish Veterinary Agency") {
  labs(caption = caption)
}

#status <- function(type) {
#  status <- switch(
#    type,
#    polishing = "should be readable but is currently undergoing final polishing",
#    restructuring = "is undergoing heavy restructuring and may be confusing or incomplete",
#    drafting = "is currently a dumping ground for ideas, and we don't recommend reading it",
#    complete = "is largely complete and just needs final proof reading",
#    stop("Invalid `type`", call. = FALSE)
#  )
#  class <- switch(
#    type,
#    polishing = "note",
#    restructuring = "important",
#    drafting = "important",
#    complete = "note"
#  )
#  cat(paste0(
#    "\n",
#    ":::: status\n",
#    "::: callout-", class, "\n",
#    "You are reading the work-in-progress version of the Swedres-Svarm report 2025. ",
#    "This part ", status, ". ",
#    "You can find last years report <https://www.sva.se/media/amupibfr/swedres-svarm-2024-webb.pdf>.\n",
#    ":::\n",
#    "::::\n"
#  ))
#}

status <- function(type) {
  if (type == "ready2publish") {
    return(invisible(NULL))
  }
  
  status <- switch(
    type,
    polishing = "should be readable but is currently undergoing final polishing",
    restructuring = "is undergoing heavy restructuring and may be confusing or incomplete",
    drafting = "is currently a dumping ground for ideas, and we don't recommend reading it",
    complete = "is largely complete and just needs final proof reading",
    stop("Invalid `type`", call. = FALSE)
  )
  class <- switch(
    type,
    polishing = "note",
    restructuring = "important",
    drafting = "important",
    complete = "note"
  )
  cat(paste0(
    "\n",
    ":::: status\n",
    "::: callout-", class, "\n",
    "You are reading the work-in-progress version of the Swedres-Svarm report 2025. ",
    "This part ", status, ". ",
    "You can find last years report <https://www.sva.se/media/amupibfr/swedres-svarm-2024-webb.pdf>.\n",
    ":::\n",
    "::::\n"
  ))
}

# =============================================================================
# 5. Load Data from Excel
# =============================================================================

#load_from_excel <- function(path_file) {
#  fig_info <- suppressMessages(readxl::read_excel(path = path_file, range = "A1:B2", col_names = FALSE) %>% setNames(c("cat", "info")))
#  fig_data <- suppressMessages(readxl::read_excel(path = path_file, skip = 2)) # sheet="data"
#  list(
#    fig_cap = fig_info %>% dplyr::filter(cat == "fig-cap") %>% dplyr::pull(info),
#    fig_alt = fig_info %>% dplyr::filter(cat == "fig-alt") %>% dplyr::pull(info),
#    fig_data = fig_data
#  )
#}
load_from_excel <- function(path_file, sheet = "data") {
  fig_info <- suppressMessages(readxl::read_excel(path = path_file, sheet = sheet, range = "A1:B2", col_names = FALSE) %>% setNames(c("cat", "info")))
  fig_data <- suppressMessages(readxl::read_excel(path = path_file, sheet = sheet, skip = 2))
  list(
    fig_cap = fig_info %>% dplyr::filter(cat == "fig-cap") %>% dplyr::pull(info),
    fig_alt = fig_info %>% dplyr::filter(cat == "fig-alt") %>% dplyr::pull(info),
    fig_data = fig_data
  )
}

# =============================================================================
# 6. Knitr and dplyr Options
# =============================================================================

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  fig.retina = 2,
  #fig.width = 6, #10
  #fig.asp = 2 / 3,
  fig.width = 6,
  fig.height = 6 * 0.618,  # Golden rectangles!
  fig.align = "center",
  out.width = "80%",
  fig.show = "hold"
)

options(
  dplyr.print_min = 6,
  dplyr.print_max = 6,
  pillar.max_footer_lines = 2,
  pillar.min_chars = 15,
  stringr.view_n = 6,
  cli.num_colors = 0,
  cli.hyperlink = FALSE,
  pillar.bold = TRUE,
  width = 77
)

# =============================================================================
# 6. Dummy plot
# =============================================================================

dummy_plot <- ggplot(data = data.frame()) +
  geom_point() + 
  xlim(0, 10) + ylim(0, 100)+
  annotate(geom="text", 
           x=5, y=50,
           xend=Inf, yend=Inf, 
           label='Dummy plot', 
           color='black',
           angle=25,
           fontface='bold', 
           size=18,
           alpha=0.5) +
  labs(x="X axis title",
       y="Y axis title")

# =============================================================================
# colors
# =============================================================================

grey_SewSva <- fohmR::fohm_pal(palette = "grey3")[3]
fohm_diagram_discrete <- fohmR::fohm_pal(palette = "diagram_discrete")

# ========================================
# additional functions - under development
# ========================================

## facet_grid extension
theme_SweSva_facet <- function(markdown = NULL){
  use_markdown <- function(element_type) {
    if (is.null(markdown)) return(FALSE)
    element_type %in% markdown
  }
  
  theme(
    strip.background = element_blank(),
    strip.text = if (use_markdown("strip.text")) 
      ggtext::element_markdown(margin = margin(.15, .15, .15, .15), size = 12)
    else element_text(margin = margin(.15, .15, .15, .15), size = 12),
    strip.clip = "off",
    panel.spacing.y = unit(0, "lines"),
    panel.spacing.x = unit(0.5, "lines"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
}

theme_SweSva_facet_old <- function(){ theme(strip.background = element_blank(),
                                      strip.text=element_text(margin=margin(.15,.15,.15,.15), size=12),
                                      strip.clip = "off",
                                      panel.spacing.y = unit(0, "lines"),
                                      panel.spacing.x = unit(0.5, "lines"), #0.1 before
                                      axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1))
}

## get_ci function
get_ci <- function(df){
  df |> 
    mutate(r_antal = R,
           tot_antal = R + S + I,
           r_perc = (r_antal/tot_antal*100)
    ) |> 
    group_by(antibiotikum_namn) %>%
    na.omit() |> 
    mutate(ci_low = as.numeric(formatC(binom::binom.confint(r_antal, tot_antal ,method="wilson")$lower*100, format = "f", digits = 1)),
           ci_high = as.numeric(formatC(binom::binom.confint(r_antal, tot_antal ,method="wilson")$upper*100, format = "f", digits = 1)),
           ci =  paste0("(",as.character(ci_low),"-",as.character(ci_high),")"),
           felstapel_low = round(r_perc - ci_low,1),
           felstapel_high = round(ci_high - r_perc,1)
    ) |> 
    ungroup()
}

theme_SweSva_rotate <- function(){
  theme(axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1))
}

theme_SweSva_full_legend <- function(){
 # guides(color=guide_legend(nrow=3)) +
  theme(plot.subtitle = element_text(vjust = -6)) 
}

get_wilson_ci <- function(df){
  df |> 
  mutate(ci_low = as.numeric(formatC(binom::binom.confint(r_antal, tot_antal ,method="wilson")$lower*100, format = "f", digits = 1)),
         ci_high = as.numeric(formatC(binom::binom.confint(r_antal, tot_antal ,method="wilson")$upper*100, format = "f", digits = 1)),
         ci =  paste0(as.character(ci_low),"-",as.character(ci_high)),
         felstapel_low = round(r_perc - ci_low,1),#, format='f', digits=1),
         felstapel_high = round(ci_high - r_perc,1)
  ) |> 
  ungroup()
}

## gt theme  ## gt_theme_SweSva
gt_theme_SweSva_old <- function(gt_obj){
  out <- gt_obj |> 
    tab_options(
      table.font.size = px(14),
      heading.title.font.size = px(16),
      heading.background.color = "lightgray"
    ) %>%
    fmt_markdown(columns = everything()) %>%
    # Left-align the first column (including label and rows)
    tab_style(
      style = cell_text(align = "left",
                        weight = "bold"),
      locations = cells_column_labels(columns = 1)
    ) %>%
    # Center-align the other columns (including labels and rows)
    tab_style(
      style = cell_text(weight = "bold",
                        #align = "center"
      ),
      locations = cells_column_labels(columns = -1)
    ) %>%
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_body(columns = -1)
    ) %>%
    opt_row_striping(row_striping = TRUE) |> 
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "--"
    ) 
  return(out)
}

## new FOHM gt theme
## gt_theme_fohm
gt_theme_SweSva <- function(gt_obj, source_note = NULL) {
  out <- gt_obj |>
    fmt_markdown(columns = everything(),
                 rows = everything(),
                 md_engine = c("markdown", "commonmark")) |> 
    
    # General table options
    tab_options(
      #table.font.family = "Arial, Helvetica, sans-serif",
      table.font.size = px(14),
      heading.title.font.size = px(16),
      heading.subtitle.font.size = px(14),
      row.striping.include_table_body = TRUE
    ) |>
    # Format missing values (optional)
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "--"
    ) |>
    # Style the column labels (header) with blue background and white text
    tab_style(
      style = cell_text(
        weight = "bold",
        color = "white",
        align = "center"
      ),
      locations = cells_column_labels()
    ) |>
    tab_style(
      style = cell_fill(color = "#3764a0"),# "#005EB8"),  # Blue background for header
      locations = cells_column_labels()
    ) |>
    # Left-align the header of the first column
    tab_style(
      style = cell_text(align = "left"),
      locations = cells_column_labels(columns = 1)
    ) |>
    # Style the first column (row labels) as bold and left-aligned
    tab_style(
      style = cell_text(
        weight = "bold",
        align = "left"
      ),
      locations = cells_body(columns = 1)
    ) |>
    # Center-align all other cells
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_body(columns = -1)
    ) |>
    # Add borders to cells
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom", "left", "right"),
        color = "#dddddd",
        weight = px(1)
      ),
      locations = cells_body()
    ) |>
    tab_style(
      style = cell_borders(
        sides = c("bottom"),
        color = "#dddddd",
        weight = px(2)
      ),
      locations = cells_column_labels()
    ) |>
    # Alternating row colors (white and light gray)
    tab_style(
      style = cell_fill(color = "#f9f9f9"),
      locations = cells_body(rows = seq(1, nrow(gt_obj$`_data`), by = 2))
    ) |> 
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "--"
    ) |> 
    cols_label_with(
      fn = function(x) {
        x |>  md()
      }
    )
  
  # Add source note if provided
  if (!is.null(source_note)) {
    out <- out |>
      tab_source_note(source_note = source_note) |>
      tab_style(
        style = cell_text(
          size = px(12),
          color = "#666666",
          align = "center"
        ),
        locations = cells_source_notes()
      )
  }
  
  return(out)
}




## fohm percentage breaks -> 0,25,50,100
fohm_perc_breaks <- c(0,25,50,75,100)

perc_25 <- c(0,5,10,15,20,25)
perc_100 <- c(0,25,50,75,100)

col_SweSva_9 <- c(fohmR::fohm_pal("diagram_discrete"),fohmR::fohm_pal("main")[3])

#### ==== FROM KRISTA ====

# =============================================================================
# 8. MIC distribution table from excel, uses load_from_excel()
# The function splits the data into pieces that will be assembled in the next
# function
# =============================================================================
make_mic_table_from_excel <- function(path_file,
                                      breakpoint_col = "Cut-off value (mg/L)") {
  x <- load_from_excel(path_file)
  df <- x$fig_data
  
  # Find breakpoint column in imported header names
  bp_idx <- match(breakpoint_col, names(df))
  if (is.na(bp_idx)) {
    stop(
      "Column '", breakpoint_col, "' not found.\nAvailable columns: ",
      paste(names(df), collapse = ", ")
    )
  }
  
  # MIC block starts immediately to the right of breakpoint column
  mic_start <- bp_idx + 1
  if (mic_start > ncol(df)) {
    stop("No columns found to the right of '", breakpoint_col, "'.")
  }
  
  # MIC labels are column names
  mic_cols <- trimws(names(df)[mic_start:ncol(df)])
  
  mic_cols <- vapply(mic_cols, function(x) {
    # preserve inequality signs
    prefix <- stringr::str_extract(x, "^[≤>=]+")
    
    # remove symbols/spaces and standardize decimal marker
    num_part <- x |>
      gsub("^[≤>=]+", "", x = _) |>
      trimws() |>
      gsub(",", ".", x = _)
    
    num <- suppressWarnings(as.numeric(num_part))
    
    if (!is.na(num)) {
      paste0(
        ifelse(is.na(prefix), "", prefix),
        format(signif(num, 10), scientific = FALSE, trim = TRUE)
      )
    } else {
      x
    }
  }, character(1))
  
  # Rename to cleaned MIC labels
  names(df)[mic_start:ncol(df)] <- mic_cols
  
  # attach metadata as attributes
  attr(df, "fig_cap") <- x$fig_cap
  attr(df, "fig_alt") <- x$fig_alt
  
  # do NOT drop first row in the new structure
  list(cap = x$fig_cap, alt = x$fig_alt, data = df, mic_cols = mic_cols)
}


# =============================================================================
# 9. Format a distribution table
# Handles also superscripts/footnotes like make_qt_table_from_excel()
# =============================================================================
make_mic_gt <- function(
    data,
    mic_cols,
    key_col = "Antibiotic",
    breakpoint_col = "Cut-off value (mg/L)",
    tested_range_col = "Tested range (mg/L)",
    resistance_col = "Resistance (%)",
    footnotes = NULL
) {
  stopifnot(is.data.frame(data))
  
  # clean messy table contents, handles:
  # em dashes, empty cells, different dash types, decimal commas, extra whitespace
  parse_num <- function(x) {
    x <- as.character(x)
    x[x %in% c("—", "-", "–", "", " ")] <- NA_character_
    x <- trimws(x)
    x <- gsub(",", ".", x)
    suppressWarnings(as.numeric(x))
  }
  
  # create numeric vector from the range column, handles also commas, different dash types
  # empty cells etc
  parse_range <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    if (is.na(x) || x == "" || x %in% c("—", "-", "–")) {
      return(c(NA_real_, NA_real_))
    }
    x <- gsub(",", ".", x)
    x <- gsub("[–—−]", "-", x)
    parts <- strsplit(x, "-", fixed = TRUE)[[1]]
    if (length(parts) != 2) {
      return(c(NA_real_, NA_real_))
    }
    c(parse_num(parts[1]), parse_num(parts[2]))
  }
  
  # Footnote handling
  
  # find footnote markers in column names
  orig_names <- names(data)
  header_footnote_ids <- stringr::str_match(orig_names, "<sup>([a-z]+)</sup>")[, 2]
  clean_names <- stringr::str_remove_all(orig_names, "<sup>[a-z]+</sup>")
  names(data) <- clean_names
  
  # update column-name arguments in case they contained <sup>...</sup>
  mic_cols <- stringr::str_remove_all(mic_cols, "<sup>[a-z]+</sup>")
  key_col <- stringr::str_remove_all(key_col, "<sup>[a-z]+</sup>")
  breakpoint_col <- stringr::str_remove_all(breakpoint_col, "<sup>[a-z]+</sup>")
  tested_range_col <- stringr::str_remove_all(tested_range_col, "<sup>[a-z]+</sup>")
  resistance_col <- stringr::str_remove_all(resistance_col, "<sup>[a-z]+</sup>")
  
  # find footnote markers in body cells
  body_footnotes <- list()
  
  for (j in seq_along(data)) {
    if (is.character(data[[j]])) {
      ids_list <- stringr::str_match_all(data[[j]], "<sup>([a-z]+)</sup>")
      
      for (i in seq_along(ids_list)) {
        ids <- ids_list[[i]][, 2]
        if (length(ids) > 0) {
          body_footnotes[[length(body_footnotes) + 1]] <- list(row = i, col = j, ids = ids)
        }
      }
      
      data[[j]] <- stringr::str_remove_all(data[[j]], "<sup>[a-z]+</sup>")
    }
  }
  
  # Building the actual mic table
  
  # MIC columns to numeric
  data[mic_cols] <- lapply(data[mic_cols], parse_num)
  
  # numeric version of MIC header labels for range/breakpoint matching
  # handles also commas
  mic_num <- mic_cols
  mic_num <- gsub("[≤>< ]", "", mic_num)
  mic_num <- gsub(",", ".", mic_num)
  mic_num <- suppressWarnings(as.numeric(mic_num))
  
  # save numeric breakpoint values before formatting column for display
  bp_num <- parse_num(data[[breakpoint_col]])
  bp_label <- as.character(data[[breakpoint_col]])
  
  # align each breakpoint value with mic column label
  # if breakpoint doesn't exist, skips the row
  # handles also decimals in case range has for example "8.0"
  for (i in seq_along(bp_num)) {
    if (is.na(bp_num[i])) next
    j <- which(!is.na(mic_num) & mic_num == bp_num[i])
    if (length(j) >= 1) {
      lab <- gsub("[≤>]", "", mic_cols[j[1]])
      bp_label[i] <- lab
    } else {
      bp_label[i] <- if (bp_num[i] >= 1) sprintf("%.0f", bp_num[i]) else format(bp_num[i], trim = TRUE)
    }
  }
  
  data[[breakpoint_col]] <- bp_label
  
  n_data <- nrow(data)
  data_disp <- data
  
  # make sure tested range column has . instead of , for decimals
  data_disp[[tested_range_col]] <- gsub(",", ".", data_disp[[tested_range_col]])
  
  # fill in a line to empty cells or those with 0 or 0.0. Only mic-columns.
  for (nm in mic_cols) {
    v <- data_disp[[nm]]
    data_disp[[nm]] <- ifelse(is.na(v) | v == 0, "—", sprintf("%.1f", v))
  }
  
  # markdown labels for headers
  col_labels <- stats::setNames(lapply(names(data_disp), gt::md), names(data_disp))
  
  # character columns for markdown formatting in body
  char_cols <- names(data_disp)[vapply(data_disp, is.character, logical(1))]
  
  # build gt table and add formatting
  gt_tbl <- gt::gt(data_disp, rowname_col = NULL) |>
    gt::cols_label(.list = col_labels) |>
    gt::fmt_missing(columns = gt::everything(), missing_text = "") |>
    gt::cols_align(align = "center", columns = -gt::all_of(key_col)) |>
    gt::tab_options(table.font.size = gt::px(12), table.width = gt::pct(100), data_row.padding = gt::px(3)) |>
    gt::tab_style(style = gt::cell_text(weight = "bold"), locations = gt::cells_column_labels())
  
  if (length(char_cols) > 0) {
    gt_tbl <- gt_tbl |>
      gt::fmt_markdown(columns = dplyr::all_of(char_cols))
  }
  
  if (resistance_col %in% names(data_disp)) {
    gt_tbl <- gt_tbl |>
      gt::fmt_number(columns = gt::all_of(resistance_col), decimals = 0)
  }
  
  # Include only horizontal borders in the table
  gt_tbl <- gt_tbl |>
    gt::tab_style(
      style = gt::cell_borders(sides = c("top", "bottom"), color = "#6f6f6f", weight = gt::px(1)),
      locations = gt::cells_body()
    )
  
  # Set default MIC background (blue), then override tested range to white
  gt_tbl <- gt_tbl |>
    gt::tab_style(style = gt::cell_fill(color = "#eaf4fb"), locations = gt::cells_body(columns = gt::all_of(mic_cols)))
  
  for (i in seq_len(n_data)) {
    r <- parse_range(data[[tested_range_col]][i])
    rmin <- r[1]
    rmax <- r[2]
    if (is.na(rmin) || is.na(rmax)) next
    
    in_range <- which(!is.na(mic_num) & mic_num >= rmin & mic_num <= rmax)
    if (length(in_range) == 0) next
    
    gt_tbl <- gt_tbl |>
      gt::tab_style(style = gt::cell_fill(color = "#ffffff"), locations = gt::cells_body(rows = i, columns = mic_cols[in_range]))
  }
  
  # Thick right border at breakpoint columns
  for (i in seq_len(n_data)) {
    if (is.na(bp_num[i])) next
    j <- which(!is.na(mic_num) & mic_num == bp_num[i])
    if (length(j) != 1) next
    
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = gt::cell_borders(sides = "right", color = "#000000", weight = gt::px(3)),
        locations = gt::cells_body(rows = i, columns = mic_cols[j])
      )
  }
  
  # Applying the footnotes
  
  if (!is.null(footnotes)) {
    # Header footnotes
    for (i in seq_along(header_footnote_ids)) {
      id <- match(tolower(header_footnote_ids[i]), letters)
      
      if (!is.na(id) && id <= length(footnotes)) {
        gt_tbl <- gt_tbl |>
          gt::tab_footnote(footnote = gt::md(footnotes[[id]]), locations = gt::cells_column_labels(columns = i))
      }
    }
    
    # Body cell footnotes
    for (k in seq_along(body_footnotes)) {
      cell_info <- body_footnotes[[k]]
      
      for (id_chr in cell_info$ids) {
        id <- match(tolower(id_chr), letters)
        
        if (!is.na(id) && id <= length(footnotes)) {
          gt_tbl <- gt_tbl |>
            gt::tab_footnote(
              footnote = gt::md(footnotes[[id]]),
              locations = gt::cells_body(columns = cell_info$col, rows = cell_info$row)
            )
        }
      }
    }
  }
  
  gt_tbl <- gt_tbl |>
    gt::tab_options(footnotes.marks = "letters") |>
    gt::tab_style(
      style = gt::cell_text(align = "left"),
      locations = gt::cells_footnotes()
    )
  
  cap <- attr(data, "fig_cap")[1]
  
  # fix missing spaces before parentheses
  cap <- gsub("([[:alnum:]])\\(", "\\1 (", cap)
  
  # fix missing spaces after commas
  cap <- gsub(",([[:alnum:]])", ", \\1", cap)
  
  gt_tbl <- gt_tbl |>
    gt::tab_caption(gt::md(cap))
  
  gt_tbl
}

# =============================================================================
# 10. Basic table from the load_from_excel object
# handles also footnotes
# =============================================================================
make_qt_table_from_excel_old <- function(path_file, sheet = "data", footnotes = NULL) {
  x <- load_from_excel(path_file, sheet = sheet)
  df <- x$fig_data
  
  df <- df |>
    dplyr::select(where(~ !all(is.na(.x)))) |>
    dplyr::filter(!if_all(dplyr::everything(), is.na))
  
  # find footnote markers in column names
  orig_names <- names(df)
  header_footnote_ids <- stringr::str_match(orig_names, "<sup>([a-z]+)</sup>")[, 2]
  clean_names <- stringr::str_remove_all(orig_names, "<sup>[a-z]+</sup>")
  names(df) <- clean_names
  
  # find footnote markers in body cells
  body_footnotes <- list()
  
  for (j in seq_along(df)) {
    if (is.character(df[[j]])) {
      ids_list <- stringr::str_match_all(df[[j]], "<sup>([a-z]+)</sup>")
      
      for (i in seq_along(ids_list)) {
        ids <- ids_list[[i]][, 2]
        if (length(ids) > 0) {
          body_footnotes[[length(body_footnotes) + 1]] <- list(row = i, col = j, ids = ids)
        }
      }
      
      df[[j]] <- stringr::str_remove_all(df[[j]], "<sup>[a-z]+</sup>")
    }
  }
  
  col_labels <- stats::setNames(lapply(names(df), gt::md), names(df))
  
  char_cols <- names(df)[vapply(df, is.character, logical(1))]
  
  gt_tbl <- gt::gt(df) |>
    gt::tab_caption(caption = gt::md(x$fig_cap)) |>
    gt::cols_label(.list = col_labels) |>
    gt::fmt_missing(columns = gt::everything(), missing_text = "") |>
    gt::cols_align(align = "left", columns = 1)
  
  if (length(char_cols) > 0) {
    gt_tbl <- gt_tbl |>
      gt::fmt_markdown(columns = dplyr::all_of(char_cols))
  }
  
  if (ncol(df) >= 2) {
    gt_tbl <- gt_tbl |>
      gt::cols_align(align = "center", columns = 2:ncol(df))
  }
  
  gt_tbl <- gt_tbl |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels(columns = gt::everything())
    )
  
  if (!is.null(footnotes)) {
    # Header footnotes
    for (i in seq_along(header_footnote_ids)) {
      id <- match(tolower(header_footnote_ids[i]), letters)
      
      if (!is.na(id) && id <= length(footnotes)) {
        gt_tbl <- gt_tbl |>
          gt::tab_footnote(
            footnote = gt::md(footnotes[[id]]),
            locations = gt::cells_column_labels(columns = i)
          )
      }
    }
    
    # Body cell footnotes
    for (k in seq_along(body_footnotes)) {
      cell_info <- body_footnotes[[k]]
      
      for (id_chr in cell_info$ids) {
        id <- match(tolower(id_chr), letters)
        
        if (!is.na(id) && id <= length(footnotes)) {
          gt_tbl <- gt_tbl |>
            gt::tab_footnote(
              footnote = gt::md(footnotes[[id]]),
              locations = gt::cells_body(columns = cell_info$col, rows = cell_info$row)
            )
        }
      }
    }
  }
  
  gt_tbl <- gt_tbl |>
    gt::tab_options(footnotes.marks = "letters") |>
    gt::tab_style(style = gt::cell_text(align = "left"), locations = gt::cells_footnotes())
  
  list(table = gt_tbl, fig_cap = x$fig_cap, fig_alt = x$fig_alt)
}


##############################
# 11. Footnotes to figures
##############################
make_fig_caption <- function(caption, footnotes = NULL) {
  if (is.null(footnotes)) {
    return(caption)
  }
  
  fn <- paste0("<br>", paste0("<sup>", letters[seq_along(footnotes)], "</sup> ", footnotes, collapse = "<br>"))
  paste0(caption, fn)
}

nb <- function(x) format(x, big.mark = "\u00A0", scientific = FALSE)


### modified by julius - based on kristas function

make_qt_table_from_excel  <- function(path_file, sheet = "data", footnotes = NULL, source_note = NULL) {
  x <- load_from_excel(path_file, sheet = sheet)
  df <- x$fig_data
  
  df <- df |>
    dplyr::select(where(~ !all(is.na(.x)))) |>
    dplyr::filter(!if_all(dplyr::everything(), is.na))
  
  # find footnote markers in column names
  orig_names <- names(df)
  header_footnote_ids <- stringr::str_match(orig_names, "<sup>([a-z]+)</sup>")[, 2]
  clean_names <- stringr::str_remove_all(orig_names, "<sup>[a-z]+</sup>") |>
    stringr::str_remove_all("\\*\\*")
  names(df) <- clean_names
  
  # find footnote markers in body cells
  body_footnotes <- list()
  
  for (j in seq_along(df)) {
    if (is.character(df[[j]])) {
      ids_list <- stringr::str_match_all(df[[j]], "<sup>([a-z]+)</sup>")
      
      for (i in seq_along(ids_list)) {
        ids <- ids_list[[i]][, 2]
        if (length(ids) > 0) {
          body_footnotes[[length(body_footnotes) + 1]] <- list(row = i, col = j, ids = ids)
        }
      }
      
      df[[j]] <- stringr::str_remove_all(df[[j]], "<sup>[a-z]+</sup>")
    }
  }
  
  col_labels <- stats::setNames(lapply(names(df), gt::md), names(df))
  
  char_cols <- names(df)[vapply(df, is.character, logical(1))]
  
  gt_tbl <- gt::gt(df) |>
    gt::tab_caption(caption = gt::md(x$fig_cap)) |>
    gt::cols_label(.list = col_labels) |>
    gt::fmt_missing(columns = gt::everything(), missing_text = "") |>
    gt::cols_align(align = "left", columns = 1) |>
    # General table options
    gt::tab_options(
      table.font.size = px(14),
      heading.title.font.size = px(16),
      heading.subtitle.font.size = px(14),
      row.striping.include_table_body = TRUE,
      footnotes.marks = "letters"
    ) |>
    # Format missing values
    gt::sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "--"
    ) |>
    # Style the column labels (header) with blue background and white text
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        color = "white",
        align = "center"
      ),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style = gt::cell_fill(color = "#3764a0"),
      locations = gt::cells_column_labels()
    ) |>
    # Left-align the header of the first column
    gt::tab_style(
      style = gt::cell_text(align = "left"),
      locations = gt::cells_column_labels(columns = 1)
    ) |>
    # Style the first column (row labels) as bold and left-aligned
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        align = "left"
      ),
      locations = gt::cells_body(columns = 1)
    ) |>
    # Center-align all other cells
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_body(columns = -1)
    ) |>
    # Add borders to cells
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("top", "bottom", "left", "right"),
        color = "#dddddd",
        weight = px(1)
      ),
      locations = gt::cells_body()
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("bottom"),
        color = "#dddddd",
        weight = px(2)
      ),
      locations = gt::cells_column_labels()
    ) |>
    # Alternating row colors (white and light gray)
    gt::tab_style(
      style = gt::cell_fill(color = "#f9f9f9"),
      locations = gt::cells_body(rows = seq(1, nrow(df), by = 2))
    ) |>
    # Left-align footnote cells
    gt::tab_style(
      style = gt::cell_text(align = "left"),
      locations = gt::cells_footnotes()
    )
  
  if (length(char_cols) > 0) {
    gt_tbl <- gt_tbl |>
      gt::fmt_markdown(columns = dplyr::all_of(char_cols))
  }
  
  if (ncol(df) >= 2) {
    gt_tbl <- gt_tbl |>
      gt::cols_align(align = "center", columns = 2:ncol(df))
  }
  
  # Header footnotes
  if (!is.null(footnotes)) {
    for (i in seq_along(header_footnote_ids)) {
      id <- match(tolower(header_footnote_ids[i]), letters)
      
      if (!is.null(id) && !is.na(id) && id <= length(footnotes)) {
        gt_tbl <- gt_tbl |>
          gt::tab_footnote(
            footnote = gt::md(footnotes[[id]]),
            locations = gt::cells_column_labels(columns = i)
          )
      }
    }
    
    # Body cell footnotes
    for (k in seq_along(body_footnotes)) {
      cell_info <- body_footnotes[[k]]
      
      for (id_chr in cell_info$ids) {
        id <- match(tolower(id_chr), letters)
        
        if (!is.null(id) && !is.na(id) && id <= length(footnotes)) {
          gt_tbl <- gt_tbl |>
            gt::tab_footnote(
              footnote = gt::md(footnotes[[id]]),
              locations = gt::cells_body(columns = cell_info$col, rows = cell_info$row)
            )
        }
      }
    }
  }
  
  # Add source note if provided
  if (!is.null(source_note)) {
    gt_tbl <- gt_tbl |>
      gt::tab_source_note(source_note = source_note) |>
      gt::tab_style(
        style = gt::cell_text(
          size = px(12),
          color = "#666666",
          align = "center"
        ),
        locations = gt::cells_source_notes()
      )
  }
  
  list(table = gt_tbl, fig_cap = x$fig_cap, fig_alt = x$fig_alt)
}



