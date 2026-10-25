library(shiny)
library(sf)
library(spdep)
library(spData)
library(leaflet)

# ================= DATA PREP =================
nf <- read.csv("netflix_titles.csv", stringsAsFactors = FALSE)
nf$date_added <- as.Date(trimws(nf$date_added), format = "%B %d, %Y")
nf$year_added <- as.integer(format(nf$date_added, "%Y"))

sp   <- strsplit(nf$country, ",")
long <- data.frame(show_id = rep(nf$show_id, lengths(sp)),
                   country = trimws(unlist(sp)))
long <- long[!is.na(long$country) & long$country != "", ]
long$year  <- nf$year_added[match(long$show_id, nf$show_id)]
long$genre <- nf$listed_in[match(long$show_id, nf$show_id)]

fix <- c("South Korea" = "Republic of Korea",
         "Russia" = "Russian Federation",
         "Soviet Union" = "Russian Federation",
         "West Germany" = "Germany",
         "East Germany" = "Germany")
hit <- long$country %in% names(fix)
long$country[hit] <- fix[long$country[hit]]

world  <- spData::world
cent   <- st_centroid(st_geometry(world))
coords <- st_coordinates(cent)
dmat   <- matrix(as.numeric(st_distance(cent)) / 1000, nrow = nrow(world))

maxv <- log1p(max(table(long$country)))
pal  <- colorNumeric(c("#f5f5f1", "#e50914", "#221f1f"),
                     domain = c(0, maxv))

make_lw <- function(k) {
  nb2listw(knn2nb(knearneigh(coords, k = k, longlat = TRUE)), style = "W")
}

genres <- c("Anime"         = "Anime",
            "Kids & family" = "Kids' TV|Children & Family",
            "Documentaries" = "Documentaries|Docuseries",
            "Comedy"        = "Comedies",
            "Horror"        = "Horror")

lisa_cols <- c("High-High (hotspot)" = "#b20710",
               "Low-Low (coldspot)"  = "#2c7bb6",
               "High-Low (outlier)"  = "#f4a582",
               "Low-High (outlier)"  = "#92c5de",
               "Not significant"     = "#e0e0e0")

role_cols <- c("Your country"                    = "#2c7bb6",
               "Where this title was made"       = "#e50914",
               "Nearest producers of this genre" = "#f4a582",
               "Other"                           = "#e0e0e0")

generic <- c("International TV Shows", "International Movies",
             "TV Shows", "Movies")

# ================= UI =================
ui <- navbarPage(
  "Netflix: a spatial view",
  inverse = TRUE,
  header = tags$style(HTML("
    .navbar-inverse { background-color: #221f1f; border-color: #221f1f; }
    .navbar-inverse .navbar-brand { color: #e50914 !important;
                                    font-weight: bold; }
    .navbar-inverse .navbar-nav > .active > a,
    .navbar-inverse .navbar-nav > .active > a:hover {
      background-color: #b20710 !important; color: white !important; }
    .result { background: #f5f5f1; border-left: 5px solid #e50914;
              padding: 15px; margin-top: 15px; font-size: 15px; }
    .result h4 { color: #221f1f; margin-top: 0; }
    .stats { color: #777; font-size: 13px; }
    .about { font-size: 16px; line-height: 1.6; }
    .about h4 { color: #b20710; margin-top: 25px; }
  ")),

  # ---------- About ----------
  tabPanel("About",
    fluidRow(column(8, offset = 2, class = "about",
      h2("Where does Netflix content come from?"),
      p("Netflix feels global, but the shows we watch are made in a small
         number of places. This dashboard maps about 8,800 Netflix titles
         by the country that produced them, and uses spatial statistics to
         ask whether content clusters geographically, and what that means
         for viewers in places like South Africa."),
      h4("Three questions, three methods"),
      tags$ol(
        tags$li(strong("How has Netflix's catalogue grown over time? "),
                "Tab 1 uses Global Moran's I to test whether countries
                 that produce lots of content sit next to each other."),
        tags$li(strong("Where are the hotspots for specific genres? "),
                "Tab 2 uses Local Moran's I to find clusters of countries
                 producing genres such as anime or kids' shows."),
        tags$li(strong("If I search for a show, how close is it? "),
                "Tab 3 uses nearest-neighbour distance to find the closest
                 countries producing the same kind of content.")
      ),
      h4("How to use it"),
      tags$ul(
        tags$li("Press play under the year slider in Tab 1 to watch the
                 catalogue grow from 2008 to 2021."),
        tags$li("Hover over any country to see its numbers."),
        tags$li("The 'number of neighbours' slider changes how many nearby
                 countries each country is compared with.")
      ),
      h4("About the data"),
      p("Netflix Movies and TV Shows dataset (Kaggle), covering titles
         added between 2008 and 2021. Country boundaries come from Natural
         Earth via the spData R package."),
      h4("Limitations"),
      tags$ul(
        tags$li(strong("Production, not availability: "),
                "the data shows where a title was made, not which
                 countries can stream it."),
        tags$li("About 1 in 10 titles has no production country recorded
                 (e.g. Squid Game), so these can't be mapped."),
        tags$li("Very small territories, including Hong Kong and
                 Singapore, are not drawn on the world map."),
        tags$li("Distances are measured between country centre points,
                 which can shift for countries with overseas territories,
                 such as France."),
        tags$li("Local Moran's I runs one test per country, so a few
                 'significant' results may occur by chance; use the 0.01
                 level for a stricter test.")
      ),
      br(), br()
    ))
  ),

  # ---------- Tab 1 ----------
  tabPanel("1. Growth over time",
    sidebarLayout(
      sidebarPanel(
        sliderInput("year", "Titles added up to:", min = 2008, max = 2021,
                    value = 2021, sep = "", animate = TRUE),
        sliderInput("k1", "Number of neighbours (k):", 3, 10, 5)
      ),
      mainPanel(
        leafletOutput("map1"),
        uiOutput("moran"),
        plotOutput("mplot")
      )
    )
  ),

  # ---------- Tab 2 ----------
  tabPanel("2. Genre hotspots",
    sidebarLayout(
      sidebarPanel(
        selectInput("genre", "Genre:", choices = names(genres)),
        sliderInput("k2", "Number of neighbours (k):", 3, 10, 5),
        radioButtons("sig", "Significance level:",
                     choices = c("0.05", "0.01"), inline = TRUE)
      ),
      mainPanel(
        leafletOutput("map2"),
        uiOutput("lisa_sum")
      )
    )
  ),

  # ---------- Tab 3 ----------
  tabPanel("3. Find a show",
    sidebarLayout(
      sidebarPanel(
        selectizeInput("title", "Search for a title:", choices = NULL,
                       options = list(placeholder = "Start typing...")),
        selectInput("home", "Your country:",
                    choices = sort(world$name_long),
                    selected = "South Africa")
      ),
      mainPanel(
        leafletOutput("map3"),
        uiOutput("t3_info")
      )
    )
  )
)

# ================= SERVER =================
server <- function(input, output, session) {

  # ===== Tab 1: Global Moran's I =====
  lw1 <- reactive(make_lw(input$k1))

  dat1 <- reactive({
    sub <- long[!is.na(long$year) & long$year <= input$year, ]
    w <- world
    w$titles <- as.numeric(table(factor(sub$country,
                                        levels = world$name_long)))
    w
  })

  output$map1 <- renderLeaflet({
    d <- dat1()
    leaflet(d) |> addTiles() |>
      addPolygons(fillColor = ~pal(log1p(titles)), fillOpacity = 0.85,
                  weight = 1, color = "white",
                  label = ~paste0(name_long, ": ", titles, " titles"))
  })

  output$moran <- renderUI({
    d  <- dat1()
    mt <- moran.test(log1p(d$titles), lw1())
    I  <- mt$estimate[1]
    p  <- mt$p.value
    msg <- if (p < 0.05 && I > 0) {
      "Countries that make a lot of Netflix content tend to be near other
       big producers. Content is <b>clustered</b>, not spread evenly."
    } else {
      "At this point there is <b>no clear geographic pattern</b>: big and
       small producers are mixed together."
    }
    div(class = "result",
      h4(paste0("By ", input$year, ", ", sum(d$titles > 0),
                " countries had produced Netflix titles")),
      p(HTML(msg)),
      p(class = "stats",
        sprintf("Global Moran's I = %.3f, p %s", I,
                if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)))
    )
  })

  output$mplot <- renderPlot({
    d <- dat1()
    moran.plot(log1p(d$titles), lw1(), labels = d$name_long,
               xlab = "log(titles + 1)", ylab = "Spatially lagged value")
  })

  # ===== Tab 2: Local Moran's I =====
  lw2 <- reactive(make_lw(input$k2))

  lisa_r <- reactive({
    sub <- long[grepl(genres[[input$genre]], long$genre), ]
    w <- world
    w$titles <- as.numeric(table(factor(sub$country,
                                        levels = world$name_long)))
    x   <- log1p(w$titles)
    lm  <- localmoran(x, lw2())
    z   <- x - mean(x)
    lag <- lag.listw(lw2(), z)
    sig <- lm[, 5] < as.numeric(input$sig)
    cl <- ifelse(!sig, "Not significant",
          ifelse(z > 0 & lag > 0, "High-High (hotspot)",
          ifelse(z < 0 & lag < 0, "Low-Low (coldspot)",
          ifelse(z > 0, "High-Low (outlier)", "Low-High (outlier)"))))
    w$cluster <- factor(cl, levels = names(lisa_cols))
    w
  })

  output$map2 <- renderLeaflet({
    d <- lisa_r()
    leaflet(d) |> addTiles() |>
      addPolygons(fillColor = unname(lisa_cols[as.character(d$cluster)]),
                  fillOpacity = 0.85, weight = 1, color = "white",
                  label = ~paste0(name_long, ": ", titles, " titles | ",
                                  cluster)) |>
      addLegend(colors = unname(lisa_cols), labels = names(lisa_cols),
                title = "Local Moran's I")
  })

  output$lisa_sum <- renderUI({
    d    <- lisa_r()
    hot  <- d$name_long[d$cluster == "High-High (hotspot)"]
    cold <- d$name_long[d$cluster == "Low-Low (coldspot)"]
    top  <- d$name_long[order(-d$titles)][1:3]
    hot_txt <- if (length(hot)) {
      paste0("<b>Hotspots</b> (big producers surrounded by other big
              producers): ", paste(hot, collapse = ", "), ".")
    } else {
      "No significant hotspots: production of this genre is too
       concentrated in one or two countries to form a regional cluster."
    }
    div(class = "result",
      h4(paste0(input$genre, ": top producers are ",
                paste(top, collapse = ", "))),
      p(HTML(hot_txt)),
      if (length(cold)) p(HTML(paste0("<b>Coldspots</b>: ",
                                      paste(cold, collapse = ", "), "."))),
      p(class = "stats",
        paste0(sum(d$cluster != "Not significant"), " of ", nrow(d),
               " countries significant at p < ", input$sig,
               " (Local Moran's I)."))
    )
  })

  # ===== Tab 3: nearest-neighbour distance =====
  updateSelectizeInput(session, "title",
                       choices = sort(unique(nf$title)),
                       selected = "Blood & Water", server = TRUE)

  t3 <- reactive({
    req(input$title)
    row  <- nf[nf$title == input$title, ][1, ]
    prod <- unique(long$country[long$show_id == row$show_id])
    gl   <- trimws(strsplit(row$listed_in, ",")[[1]])
    g1   <- setdiff(gl, generic)[1]
    if (is.na(g1)) g1 <- gl[1]
    sub  <- long[grepl(g1, long$genre, fixed = TRUE), ]
    cnt  <- as.numeric(table(factor(sub$country,
                                    levels = world$name_long)))
    h    <- match(input$home, world$name_long)
    d    <- dmat[h, ]
    cand <- which(cnt > 0 & seq_along(cnt) != h)
    near <- cand[order(d[cand])][seq_len(min(5, length(cand)))]
    list(row = row, prod = prod, genre = g1, cnt = cnt,
         h = h, d = d, near = near)
  })

  output$map3 <- renderLeaflet({
    x <- t3()
    role <- rep("Other", nrow(world))
    role[x$near] <- "Nearest producers of this genre"
    role[world$name_long %in% x$prod] <- "Where this title was made"
    role[x$h] <- "Your country"
    m <- leaflet(world) |> addTiles() |>
      addPolygons(fillColor = unname(role_cols[role]), fillOpacity = 0.85,
                  weight = 1, color = "white",
                  label = paste0(world$name_long, ": ", x$cnt, " ",
                                 x$genre, " titles | ",
                                 round(x$d), " km away"))
    for (j in x$near) {
      m <- m |> addPolylines(lng = c(coords[x$h, 1], coords[j, 1]),
                             lat = c(coords[x$h, 2], coords[j, 2]),
                             color = "#221f1f", weight = 2,
                             dashArray = "5,5")
    }
    m |> addLegend(colors = unname(role_cols), labels = names(role_cols))
  })

  output$t3_info <- renderUI({
    x    <- t3()
    made <- if (length(x$prod)) paste(x$prod, collapse = ", ") else
            "an unrecorded country"
    own  <- if (x$cnt[x$h] > 0) {
      paste0(input$home, " has <b>", x$cnt[x$h],
             "</b> titles of its own in this genre. ")
    } else {
      paste0(input$home, " has <b>no</b> titles of its own in this genre. ")
    }
    nearest <- ""
    if (length(x$near)) {
      n1 <- x$near[1]
      nearest <- paste0("The nearest other country making ", x$genre,
                        " is <b>", world$name_long[n1], "</b>, about <b>",
                        format(round(x$d[n1], -1), big.mark = ","),
                        " km</b> away.")
    }
    div(class = "result",
      h4(paste0(x$row$title, " (", x$row$type, ", ",
                x$row$release_year, ")")),
      p(paste0("Made in ", made, ". Main genre: ", x$genre, ".")),
      p(HTML(paste0(own, nearest))),
      tableOutput("t3_table")
    )
  })

  output$t3_table <- renderTable({
    x <- t3()
    data.frame(Country         = world$name_long[x$near],
               Titles          = x$cnt[x$near],
               `Distance (km)` = format(round(x$d[x$near]), big.mark = ","),
               check.names = FALSE)
  })
}

shinyApp(ui, server)