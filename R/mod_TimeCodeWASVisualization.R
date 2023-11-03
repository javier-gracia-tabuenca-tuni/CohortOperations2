#
# UI
#

mod_timeCodeWASVisualization_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
       shiny::uiOutput(ns("visualization_ui")),
  )
}

.build_plot <- function(studyResult, values){
  req(studyResult)

  # get time_periods
  l <- unique(studyResult$timeRange)
  l_split <- lapply(l, function(x) {stringr::str_split(x, " ", simplify = TRUE)})
  time_periods <- as.data.frame(do.call(rbind, l_split)) |>
    dplyr::arrange(as.numeric(V2)) |>
    dplyr::mutate(period = paste(V1,V2,V3,V4)) |>
    dplyr::pull(period)

  values$time_periods <- time_periods

  studyResult <- studyResult |>
    dplyr::transmute(
      code = covariateId,
      time_period = factor(timeRange, levels = time_periods, labels = time_periods),
      name = covariateName,
      OR=OR,
      p=p,
      up_in=up_in,
      cases_per = n_cases_yes/n_cases,
      controls_per = n_controls_yes/n_controls,
      n_cases_yes = n_cases_yes,
      n_controls_yes = n_controls_yes
    ) |>
    tidyr::separate(name, c("domain", "name"), sep = ":", extra = "merge") |>
    dplyr::mutate(name = stringr::str_remove(name, "^[:blank:]")) |>
    dplyr::mutate(p = dplyr::if_else(p==0, 10^-323, p))

  studyResult_fig <- studyResult |>
    # dplyr::filter(p<0.00001) |>
    # dplyr::filter(p<0.05) |>
    dplyr::filter(p < values$p_limit) |>
    dplyr::arrange(time_period, name) |>
    dplyr::mutate_if(is.character, stringr::str_replace_na, "") |>
    dplyr::mutate(
      GROUP = time_period,
      label = stringr::str_c(code),
      label = stringr::str_remove(label, "[:blank:]+$"),
      label = stringr::str_c(domain, " : ", name,
                             "\n-log10(p)=", scales::number(-log10(p), accuracy = 0.1) ,
                             "\n log10(OR) = ", scales::number(log10(OR), accuracy = 0.1),
                             "\n cases:", n_cases_yes, " (", scales::percent(cases_per, accuracy = 0.01), ")",
                             "\n controls:", n_controls_yes, " (", scales::percent(controls_per, accuracy = 0.01), ")"
      ),
      link = paste0("https://atlas.app.finngen.fi/#/concept/", stringr::str_sub(code, 1, -4)),
      up_in = factor(domain) |> as.integer() |> as.character(),
      id = dplyr::row_number(),
      p_group = cut(-log10(p),
                    breaks = c(-1, 50, 100, 200, Inf ),
                    labels = c("-log10(p) [0,50]", "-log10(p) (50,100]", "-log10(p) (100,200]", "-log10(p) (200,Inf]"),
                    ordered_result = TRUE
      ),
      p_group_size = dplyr::case_when(
        as.integer(p_group)==1 ~ 1L,
        as.integer(p_group)==2 ~ 5L,
        as.integer(p_group)==3 ~ 10L,
        as.integer(p_group)==4 ~ 20L,
      ),
      log10_OR = dplyr::case_when(
        log10(OR) == -Inf ~ -2.5,
        log10(OR) == Inf ~ 5,
        TRUE ~ log10(OR) ,

      )
    )

  # View(studyResult_fig)
  # browser()
  return(studyResult_fig)
}

#
# server
#

mod_timeCodeWASVisualization_server <- function(id, r_studyResult) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    values <- shiny::reactiveValues(selection = NULL, time_periods = NULL, gg_data = NULL, gg_data_full = NULL, p_limit = 0.05)

    # View(r_studyResult)
    # browser()

    values$gg_data_full <- values$gg_data <- .build_plot(r_studyResult, values)

    #
    # handlers
    #

    shiny::observeEvent(input$redraw, {
      if(input$p_limit != values$p_limit){
        values$p_limit <- input$p_limit
        values$gg_data_full <- values$gg_data <- .build_plot(r_studyResult, values)
      }
      domains <- c()
      if(input$condition_occurrence == TRUE) domains <- c(domains, "condition_occurrence")
      if(input$drug_exposure == TRUE) domains <- c(domains, "drug_exposure")
      if(input$measurement == TRUE) domains <- c(domains, "measurement")
      if(input$procedure_occurrence == TRUE) domains <- c(domains, "procedure_occurrence")

      values$gg_data <- values$gg_data_full |>
        dplyr::filter(domain %in% domains)
    })

    observeEvent(input$unselect, {
      # remove the previous selection
      session$sendCustomMessage(type = 'codeWASplot_set', message = character(0))
      values$selection <- NULL
    }, ignoreInit = TRUE)

    #
    # mouse click handler
    #
    observeEvent(input$codeWASplot_selected, {
      selected_rows <- input$codeWASplot_selected

      # browser()

      values$selection <- values$gg_data |>
        dplyr::filter(code %in% selected_rows) |>
        dplyr::arrange(code, time_period) |>
        dplyr::mutate(position = match(time_period, values$time_periods)) |>
        dplyr::mutate(name = ifelse(!is.na(position), paste0("panel-1-", position), "NA")) |>
        dplyr::select(code, domain, name, cases_per, controls_per)

    }, ignoreInit = TRUE)

    #
    # renderUI ####
    #
    output$visualization_ui <- renderUI({
      req(r_studyResult)

      htmltools::tagList(
        shinyjs::useShinyjs(),
        ggiraph::girafeOutput(ns("codeWASplot"), width = "100%", height = "100%"),
        shiny::hr(style = "margin-top: -20px;"),
        shiny::column(3,
                      shinyWidgets::awesomeCheckbox(ns("condition_occurrence"), label = "Condition occurrence", value = TRUE),
                      shinyWidgets::awesomeCheckbox(ns("drug_exposure"), label = "Drug exposure", value = TRUE),
                      shinyWidgets::awesomeCheckbox(ns("measurement"), label = "Measurement", value = TRUE),
                      shinyWidgets::awesomeCheckbox(ns("procedure_occurrence"), label = "Procedure occurrence", value = TRUE),
        ),
        shiny::column(3,
                      shinyWidgets::awesomeCheckbox(ns("show_labels"), label = "Label outstanding", value = FALSE),
                      shiny::sliderInput(ns("cases_per"), label="Cases % must be at least",
                                         min = 20, max = 100, post  = " %", width = "200px",
                                         value = 30
                      ),
        ),
        shiny::column(3,
                      shiny::actionButton(ns("unselect"), label = "Unselect"),
                      shiny::sliderInput(ns("p_limit"), label="p limit",
                                         min = 0.00001, max = 1.00, pre  = "p < ", width = "400px",
                                         value = isolate(values$p_limit)
                      ),
        ),
        shiny::column(3,
                      shiny::actionButton(ns("redraw"), label = "Update CodeWAS")
        ),
        htmltools::br(),
      )

    })

    # simplify facet labels
    label_editor <- function(s){
      s <- stringr::str_remove(s, "from ")
      s <- stringr::str_replace(s, "to", " / ")
    }

    #
    # renderGirafe ####
    #

    output$codeWASplot <- ggiraph::renderGirafe({

      if(is.null(values$gg_data)) return()
      # adjust the label area according to facet width
      facet_max <- max(values$gg_data$controls_per, values$gg_data$cases_per, 0.03, na.rm = TRUE)
      #
      # browser()
      # message("plotting ", nrow(values$gg_data))
      #
      gg_fig <- ggplot2::ggplot(
        data = dplyr::arrange(values$gg_data, log10_OR),
        ggplot2::aes(
          y = cases_per, #log10_OR, # cases_per-controls_per,#log10_OR,# -log10(p), cases_per-controls_per,#
          x = controls_per, #-log10(p), # 1, # id, #log10_OR,
          color = "darkgray",
          fill = domain,
          tooltip = label,
          # size = ordered(p_group), # log10_OR
          data_id = code
          # onclick = paste0('window.open("', link , '")')
        ), alpha = 0.75)+
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, y = 0, xend = facet_max, yend = facet_max),
          color = "red", alpha = 0.5, linewidth = 0.2, linetype = "dashed") +
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, y = 0, xend = facet_max, yend = 0),
          color = "black", alpha = 0.5, linewidth = 0.2, linetype = "dashed") +
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, y = 0, xend = 0, yend = facet_max),
          color = "black", alpha = 0.5, linewidth = 0.2, linetype = "dashed") +
        ggiraph::geom_point_interactive(
          ggplot2::aes(size = p_group), show.legend=T, shape = 21) + #, position = position_dodge(width = 12))+
        ggplot2::scale_size_manual(values = c(2,4,6,8)) +
        {if(input$show_labels)
          ggrepel::geom_text_repel(
            data = values$gg_data |> dplyr::filter((is.infinite(OR) & cases_per > 0.05) | cases_per > (input$cases_per / 100)),
            ggplot2::aes(label = stringr::str_wrap(name, 8)),
            max.overlaps = Inf,
            size = 2.5,
            hjust = 0.1,
            xlim = c(facet_max / 4, NA),
            box.padding = 0.8
          )} +
        ggplot2::scale_x_continuous(
          breaks = seq(0, 0.8, 0.1),
          labels = seq(0, 80, 10),
          limits = c(-0.01, facet_max) #, expand = ggplot2::expansion(add = c(0.0, 0.05))
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq(0, 0.8, 0.1),
          labels = seq(0,80, 10),
          limits = c(-0.01, facet_max), expand = ggplot2::expansion(add = c(0, 0.01))
        ) +
        ggplot2::coord_fixed() +
        ggplot2::facet_grid(.~GROUP, drop = FALSE, scales = "fixed", labeller = ggplot2::labeller(GROUP = label_editor))+
        ggplot2::theme_minimal()+
        ggplot2::theme(
          legend.key.height = grid::unit(5, "mm"),
          legend.key.width = grid::unit(10, "mm"),
          legend.position = "bottom",
          legend.direction = "vertical"
        ) +
        ggplot2::scale_color_manual(values = c("darkgray")) +
        ggplot2::scale_fill_manual(values = c(
          "condition_occurrence" = "khaki",
          "drug_exposure" = "lightblue2",
          "measurement" = "palegreen",
          "procedure_occurrence" = "plum1"),
          labels = c(
            "condition_occurrence" = "Condition occurrence",
            "drug_exposure" = "Drug exposure",
            "measurement" ="Measurement",
            "procedure_occurrence" = "Procedure occurrence"
          )
        ) +
        ggplot2::guides(color = "none", fill = ggplot2::guide_legend(override.aes = list(size = 5))) +
        ggplot2::labs(size = "p value group", fill = "Domain", x = "\nControls %", y = "Cases %")

      gb <- ggplot2::ggplot_build(gg_fig)
      g <- ggplot2::ggplot_gtable(gb)

      # browser()

      if(!is.null(values$selection) && length(unique(values$selection))){
        # remove domains not in the current data
        selection <- values$selection |>
          dplyr::filter(domain %in% values$gg_data$domain)
        # check if we have lines to draw
        if(nrow(selection) > 1){
          z_val = 0
          ranges <- gb$layout$panel_params
          data2npc <- function(x, range) scales::rescale(c(range, x), c(0,1))[-c(1,2)]
          x_range <-  ranges[[1]][["x.range"]]
          y_range <- ranges[[1]][["y.range"]]

          selection <- dplyr::inner_join(selection, g$layout, by = "name") |>
            dplyr::mutate(controls_per = data2npc(controls_per, x_range)) |>
            dplyr::mutate(cases_per = data2npc(cases_per, y_range))
          selection$z <- 1
          selection$clip <- "off"

          # print(selection)

          # move to the beginning of selection
          g <- gtable::gtable_add_grob(
            g, grid::moveToGrob(selection[1,]$controls_per, selection[1,]$cases_per),
            t = selection[1,]$t, selection[1,]$l, z = z_val)
          # draw the lines
          for(i in 2:nrow(selection)){
            if(is.na(selection[i,]$t) || is.na(selection[i,]$l))
              next
            g <- gtable::gtable_add_grob(
              g, grid::lineToGrob(selection[i,]$controls_per, selection[i,]$cases_per, gp = grid::gpar(col = "red", alpha = 0.3, lwd = 2.5)),
              t = selection[i,]$t, selection[i,]$l, z = z_val)
          }

          # turn clip off to see the line across panels
          g$layout$clip <- "off"
        }
      }

      selected_items <- as.character(values$selection$code)
      if(!is.null(selected_items)) selected_items <- dplyr::first(selected_items)
      if(is.na(selected_items)) selected_items <- ""
      # message("selected_items: ", toString(selected_items))

      gg_girafe <- ggiraph::girafe(ggobj = ggplotify::as.ggplot(g), width_svg = 15)
      gg_girafe <- ggiraph::girafe_options(gg_girafe,
                                           ggiraph::opts_sizing(rescale = TRUE, width = 1.0),
                                           ggiraph::opts_hover(css = "fill-opacity:1;fill:red;stroke:black;"),
                                           ggiraph::opts_selection(
                                             type = c("single"),
                                             only_shiny = TRUE,
                                             selected = selected_items
                                           )

      )
    })

    shiny::observeEvent(input$show_table, {
      print("show_table")
    })

  })
}

