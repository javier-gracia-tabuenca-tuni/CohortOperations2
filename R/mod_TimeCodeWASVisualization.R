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
  shiny::req(studyResult)

  message(".build_plot")
  # start_time <- Sys.time()

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
    dplyr::filter(p<0.00001) |>
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
      ),
      data_id = paste0(code, "@", as.character(time_period)),
      data_id_class = code
    )

  # View(studyResult_fig)
  # browser()
  # print(Sys.time() - start_time)
  return(studyResult_fig)
}

#
# server
#

mod_timeCodeWASVisualization_server <- function(id, r_studyResult) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    values <- shiny::reactiveValues(
      selection = NULL, time_periods = NULL, gg_data = NULL, gg_data_saved = NULL, skip_selection = FALSE)

    # this is done once
    values$gg_data_saved <- .build_plot(r_studyResult, values)

    # these must be in sync with renderUI
    domains <- c("condition_occurrence", "drug_exposure")
    p_groups <- c(10,20)

    values$gg_data <- values$gg_data_saved |>
      dplyr::filter(domain %in% domains & p_group_size %in% p_groups)

    # View(r_studyResult)
    # browser()

    #
    # handlers
    #

    shiny::observeEvent(input$redraw, {
      # message("Update CodeWAS")

      domains <- c()
      if(input$condition_occurrence == TRUE) domains <- c("condition_occurrence")
      if(input$drug_exposure == TRUE) domains <- c(domains, "drug_exposure")
      if(input$measurement == TRUE) domains <- c(domains, "measurement")
      if(input$procedure_occurrence == TRUE) domains <- c(domains, "procedure_occurrence")
      if(input$observation == TRUE) domains <- c(domains, "observation")

      p_groups <- c()
      if(input$group_1 == TRUE) p_groups <- c(1)
      if(input$group_5 == TRUE) p_groups <- c(p_groups, 5)
      if(input$group_10 == TRUE) p_groups <- c(p_groups, 10)
      if(input$group_20 == TRUE) p_groups <- c(p_groups, 20)

      values$gg_data <- values$gg_data_saved |>
        dplyr::filter(domain %in% domains & p_group_size %in% p_groups)

    }, ignoreInit = TRUE)

    #
    # unselect ####
    #
    shiny::observeEvent(input$unselect, {
      # message("unselect")
      # remove the previous selection
      session$sendCustomMessage(type = 'codeWASplot_set', message = character(0))
      values$selection <- NULL
    }, ignoreInit = TRUE)

    #
    # mouse click handler ####
    #
    shiny::observeEvent(input$codeWASplot_selected, {
      selected_rows <- input$codeWASplot_selected
      if(length(selected_rows) == 1 && selected_rows == "") {
        message("empty selection, exiting")
        return()
      }

      message("codeWASplot_selected")
      message(paste("selection: ", toString(selected_rows)))

      # browser()

      if(values$skip_selection){
        message("skip_selection == TRUE")
        values$skip_selection <- FALSE
        return()
      }

      # browser()

      # get selected points from girafe
      selected_rows <- input$codeWASplot_selected
      # remove the current selection from the new one
      old_selection <- values$selection$data_id
      selected_rows <- setdiff(selected_rows, old_selection)
      values$selection <- NULL

      selected_rows <- selected_rows[selected_rows != ""]

      if(length(selected_rows) == 1 && selected_rows == "") {
        message("same selection as previously, exiting")
        return()
      }

      if(length(selected_rows) > 1){
        # marquee selection
        # message("marquee selection")
        # message(toString(selected_rows))
        df_lasso <- values$gg_data |>
          dplyr::filter(data_id %in% selected_rows) |>
          dplyr::mutate(up_in = ifelse(up_in == 1, "Case", "Ctrl")) |>
          dplyr::mutate(cases_per = scales::percent(cases_per, accuracy = 0.01)) |>
          dplyr::mutate(controls_per = scales::percent(controls_per, accuracy = 0.01)) |>
          dplyr::mutate(p = formatC(p, format = "e", digits = 2)) |>
          dplyr::select(name, up_in, n_cases_yes, n_controls_yes, cases_per, controls_per, GROUP, p)
        # dplyr::select(code, name, up_in, n_cases_yes, n_controls_yes, cases_per, controls_per, GROUP, p)
        # browser()
        values$selection <- NULL
        # show table
        shiny::showModal(
          shiny::modalDialog(
            DT::renderDataTable({
              DT::datatable(
                df_lasso,
                colnames = c(
                  # 'Covariate ID' = 'code',
                  'Covariate name' = 'name',
                  'Type' = 'up_in',
                  'Cases n' = 'n_cases_yes',
                  'Ctrls n' = 'n_controls_yes',
                  'Cases %' = 'cases_per',
                  'Ctrls %' = 'controls_per',
                  'Group' = 'GROUP',
                  'p' = 'p'
                )
              )
            }),
            size = "l",
            easyClose = FALSE,
            title = paste0("Entries (", nrow(df_lasso), ")"),
            footer = shiny::modalButton("Close"),
            options = list(
              autowidth = TRUE
            )
          )
        )

      } else {
        # single point
        selected_rows <- stringr::str_remove_all(selected_rows, "@.*")
        # message("single point selected")
        # message(toString(selected_rows))
        values$selection <- values$gg_data |>
          dplyr::filter(code %in% selected_rows) |>
          dplyr::arrange(code, time_period) |>
          dplyr::mutate(position = match(time_period, values$time_periods)) |>
          dplyr::mutate(name = ifelse(!is.na(position), paste0("panel-1-", position), "NA")) |>
          dplyr::select(code, domain, name, cases_per, controls_per, data_id)
      }
    }, ignoreInit = TRUE)

    #
    # renderUI ####
    #
    output$visualization_ui <- shiny::renderUI({
      shiny::req(r_studyResult)

      htmltools::tagList(
        shinyjs::useShinyjs(),
        shiny::fluidRow(
          # these must be in sync with server initialization
          shiny::column(3,
                        shiny::h5("Observation type"),
                        shinyWidgets::awesomeCheckbox(ns("condition_occurrence"), label = "Condition occurrence", value = TRUE),
                        shinyWidgets::awesomeCheckbox(ns("drug_exposure"), label = "Drug exposure", value = TRUE),
                        shinyWidgets::awesomeCheckbox(ns("measurement"), label = "Measurement", value = FALSE),
                        shinyWidgets::awesomeCheckbox(ns("procedure_occurrence"), label = "Procedure occurrence", value = FALSE),
                        shinyWidgets::awesomeCheckbox(ns("observation"), label = "Observation", value = FALSE),
          ),
          shiny::column(3, # c("-log10(p) [0,50]", "-log10(p) (50,100]", "-log10(p) (100,200]", "-log10(p) (200,Inf]")
                        shiny::h5("p-value groups"),
                        shinyWidgets::awesomeCheckbox(ns("group_1"), label = "-log10(p) [0,50]", value = FALSE),
                        shinyWidgets::awesomeCheckbox(ns("group_5"), label = "-log10(p) (50,100]", value = FALSE),
                        shinyWidgets::awesomeCheckbox(ns("group_10"), label = "-log10(p) (100,200]", value = TRUE),
                        shinyWidgets::awesomeCheckbox(ns("group_20"), label = "-log10(p) (200,Inf]", value = TRUE),
          ),
          shiny::column(3,
                        shinyWidgets::awesomeCheckbox(ns("show_labels"), label = "Show labels"),
                        shiny::hr(style = "margin-bottom: 0px;"),
                        shiny::sliderInput(ns("cases_per"), label="Filter cases% <",
                                           min = 0, max = 100, post  = " %", width = "200px",
                                           value = 50
                        ),
          ),
          shiny::column(3,
                        shiny::actionButton(ns("redraw"), label = "Update CodeWAS"),
                        shiny::hr(style = "margin-bottom: 20px;"),
                        shiny::actionButton(ns("unselect"), label = "Unselect"),
          )
        ),
        shiny::hr(style = "margin-bottom: 20px;"),
        shinycustomloader::withLoader(
          ggiraph::girafeOutput(ns("codeWASplot"), width = "100%", height = "100%"),
          type = "html",
          loader = "dnaspin",
        ),
        shiny::hr(style = "margin-bottom: 20px;"),
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
      message("renderGirafe")
      message("input size ==", paste(nrow(values$gg_data)))
      # take a reactive dependency on the following
      input$unselect
      input$redraw

      if(is.null(values$gg_data)) return()
      # adjust the label area according to facet width
      facet_max_x <- max(values$gg_data$controls_per, 0.03, na.rm = TRUE)
      facet_max_y <- max(values$gg_data$cases_per, 0.03, na.rm = TRUE)
      #
      # message("plotting ", nrow(values$gg_data))
      # browser()
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
          data_id = data_id
          # onclick = paste0('window.open("', link , '")')
        ), alpha = 0.75)+
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, y = 0,
                       xend = ifelse(facet_max_x > facet_max_y, facet_max_y, facet_max_x),
                       yend = ifelse(facet_max_x > facet_max_y, facet_max_y, facet_max_x)
          ),
          color = "red", alpha = 0.5, linewidth = 0.2, linetype = "dashed") +
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, y = 0, xend = facet_max_x, yend = 0),
          color = "black", alpha = 0.5, linewidth = 0.2, linetype = "dashed") +
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, y = 0, xend = 0, yend = facet_max_y),
          color = "black", alpha = 0.5, linewidth = 0.2, linetype = "dashed") +
        ggiraph::geom_point_interactive(
          ggplot2::aes(size = p_group), show.legend=T, shape = 21) + #, position = position_dodge(width = 12))+
        ggplot2::scale_size_manual(
          values = c(
            "-log10(p) [0,50]" = 1,
            "-log10(p) (50,100]" = 1.5,
            "-log10(p) (100,200]" = 2,
            "-log10(p) (200,Inf]" = 3
          )
        ) +
        {if(shiny::isolate(input$show_labels))
          ggrepel::geom_text_repel(
            data = values$gg_data |>
              dplyr::filter(cases_per > shiny::isolate(input$cases_per)/100),
            ggplot2::aes(label = stringr::str_wrap(stringr::str_trunc(name, 30), 15)),
            max.overlaps = Inf,
            size = 3,
            hjust = 0.1,
            xlim = c(facet_max_x / 4, NA),
            box.padding = 0.8
          )} +
        ggplot2::scale_x_continuous(
          breaks = c(0, 0.05, seq(0.1, 0.8, 0.1)),
          labels = c(0, 5, seq(10, 80, 10)),
          limits = c(-0.02 * facet_max_x, facet_max_x)
        ) +
        ggplot2::scale_y_continuous(
          breaks = c(0, 0.05, seq(0.1, 0.8, 0.1)),
          labels = c(0, 5, seq(10, 80, 10)),
          limits = c(-0.02 * facet_max_y, facet_max_y)
        ) +
        # ggplot2::coord_fixed() +
        ggplot2::facet_grid(
          .~GROUP, drop = FALSE, scales = "fixed",
          labeller = ggplot2::labeller(GROUP = label_editor)
        )+
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
          "procedure_occurrence" = "plum1",
          "observation" = "gray"),
      labels = c(
            "condition_occurrence" = "Condition occurrence",
            "drug_exposure" = "Drug exposure",
            "measurement" ="Measurement",
            "observation" = "Observation"
          )
        ) +
        ggplot2::guides(color = "none", fill = ggplot2::guide_legend(override.aes = list(size = 5))) +
        ggplot2::labs(size = "p value group", fill = "Domain", x = "\nControls %", y = "Cases %")

      # gb <- ggplot2::ggplot_build(gg_fig)
      # g <- ggplot2::ggplot_gtable(gb)

      # browser()
      selected_items <- ""

      if(!is.null(values$selection) && length(unique(values$selection$code)) == 1){
        # one point selected -> draw a line connecting the same code in each facet
        message("one point selected")
        gb <- ggplot2::ggplot_build(gg_fig)
        g <- ggplot2::ggplot_gtable(gb)
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

        if(!is.null(values$selection)){
          message("we have a selection")
          selected_items <- as.character(unique(values$selection$code))
          # extend selection to same code in all facets
          # browser()
          selected_items <- values$gg_data |>
            dplyr::filter(code == selected_items) |>
            dplyr::pull(data_id)
          # message("selected_items(values$selection$code): ", toString(selected_items))
          values$skip_selection <- TRUE
        } else {
          selected_items <- ""
          # message("selected_items: NULL")
        }

        # if(!is.null(selected_items)) selected_items <- dplyr::first(selected_items)
        # if(is.na(selected_items)) selected_items <- ""

        gg_plot <- ggplotify::as.ggplot(g)
      } else {
        gg_plot <- gg_fig
      }

      # message("renderGirafe selected_items: ", toString(selected_items))

      gg_girafe <- ggiraph::girafe(ggobj = gg_plot, width_svg = 15)
      gg_girafe <- ggiraph::girafe_options(gg_girafe,
                                           ggiraph::opts_sizing(rescale = TRUE, width = 1.0),
                                           ggiraph::opts_hover(
                                             css = "fill-opacity:1;fill:red;stroke:black;",
                                             reactive = FALSE
                                           ),
                                           ggiraph::opts_selection(
                                             type = c("multiple"),
                                             only_shiny = TRUE,
                                             selected = selected_items
                                           )

      )
      # print(Sys.time() - start_time)
      return(gg_girafe)
    })

  })
}

