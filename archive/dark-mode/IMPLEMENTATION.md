# Dark Mode — Implementierungsarchiv

Ausgebaut wegen Komplexität (Bootstrap 3 + viele inline-Styles).
Kann reaktiviert werden wenn die App auf bslib/Bootstrap 5 migriert wird.

---

## Was war implementiert

### www/dark-mode.css
Vollständige CSS-Datei (liegt daneben). Übernimmt:
- Bootstrap 3 Navbar, Tabs, Panels, Wells
- Form Controls, Sliders, Buttons
- DT / reactable Tabellen
- Inline-Style-Overrides via `[style*="background-color:lavender"]` + `!important`
- Toggle-Button Styling (fester Kreis-Button oben rechts)

### ui.R — Änderungen

```r
# In fluidPage() einfügen:

# CSS einbinden
tags$link(rel = "stylesheet", type = "text/css", href = "dark-mode.css"),

# Toggle-Button (fixed top-right)
actionButton("toggle_dark_mode", label = NULL, icon = icon("moon"),
             class = "btn btn-default",
             title = "Toggle dark / light mode"),
```

### server.R — Am Anfang von shinyServer() einfügen

```r
# ── Dark mode toggle ────────────────────────────────────────────────────────
dark_mode_active <- reactiveVal(FALSE)

# ggplot2-Theme (+ dm_theme() an jeden ggplot anhängen)
dm_theme <- reactive({
  if (!dark_mode_active()) return(theme())
  theme(
    plot.background   = element_rect(fill = "#1e2235", colour = NA),
    panel.background  = element_rect(fill = "#16213e", colour = NA),
    panel.border      = element_blank(),
    panel.grid.major  = element_line(colour = "#2a3556"),
    panel.grid.minor  = element_line(colour = "#212d4a"),
    text              = element_text(colour = "#dde1f0"),
    axis.text         = element_text(colour = "#b0b8d0"),
    axis.title        = element_text(colour = "#dde1f0"),
    plot.title        = element_text(colour = "#dde1f0"),
    plot.subtitle     = element_text(colour = "#b0b8d0"),
    plot.caption      = element_text(colour = "#8090b0"),
    legend.background = element_rect(fill = "#16213e", colour = NA),
    legend.key        = element_rect(fill = "#16213e", colour = NA),
    legend.text       = element_text(colour = "#dde1f0"),
    legend.title      = element_text(colour = "#dde1f0"),
    strip.background  = element_rect(fill = "#0f3460"),
    strip.text        = element_text(colour = "#dde1f0")
  )
})

# Plotly-Layout-Helper (p %>% dm_plotly_layout() in renderPlotly)
dm_plotly_layout <- function(p) {
  if (!dark_mode_active()) return(p)
  p %>% plotly::layout(
    paper_bgcolor = "#1e2235",
    plot_bgcolor  = "#16213e",
    font          = list(color = "#dde1f0"),
    xaxis         = list(gridcolor = "#2a3556", zerolinecolor = "#4a6fa5",
                         tickfont  = list(color = "#b0b8d0")),
    yaxis         = list(gridcolor = "#2a3556", zerolinecolor = "#4a6fa5",
                         tickfont  = list(color = "#b0b8d0")),
    polar         = list(
      bgcolor      = "#16213e",
      radialaxis   = list(gridcolor = "#2a3556", linecolor  = "#4a6fa5",
                          tickfont  = list(color = "#b0b8d0")),
      angularaxis  = list(gridcolor = "#2a3556", linecolor  = "#4a6fa5",
                          tickfont  = list(color = "#b0b8d0", size = 12))
    ),
    legend = list(bgcolor = "#16213e", font = list(color = "#dde1f0"))
  )
}

observeEvent(input$toggle_dark_mode, {
  dark_mode_active(!dark_mode_active())
  if (dark_mode_active()) {
    shinyjs::addClass(selector = "body", class = "dark-mode")
    shinyjs::runjs('document.getElementById("toggle_dark_mode").innerHTML =
                     "<i class=\\"fa fa-sun\\"></i>";')
  } else {
    shinyjs::removeClass(selector = "body", class = "dark-mode")
    shinyjs::runjs('document.getElementById("toggle_dark_mode").innerHTML =
                     "<i class=\\"fa fa-moon\\"></i>";')
  }
})
```

### server.R — Bulk-Replacement (Python-Skript)

```python
import re
with open('server.R', 'r') as f:
    content = f.read()
# theme_minimal() → theme_minimal() + dm_theme()
content = re.sub(r'theme_minimal\(\)(?!\s*\+\s*dm_theme)', 'theme_minimal() + dm_theme()', content)
# theme_void() → theme_void() + dm_theme()
content = re.sub(r'theme_void\(\)(?!\s*\+\s*dm_theme)', 'theme_void() + dm_theme()', content)
with open('server.R', 'w') as f:
    f.write(content)
# Ergibt: 21× theme_minimal, 2× theme_void
```

### server.R — Spezialfälle manuell

```r
# 1. Waffle-Chart: plot.background konditionell
plot.background = element_rect(fill = if (dark_mode_active()) "#1e2235" else "white", color = NA)

# 2. Parliament-Plot (renderPlotly): dm_plotly_layout() in layout-Chain
interactive_parliament_plot <- interactive_parliament_plot %>%
  layout(...) %>%
  dm_plotly_layout() %>%
  event_register("plotly_click")

# 3. Radar-Chart (renderPlotly): am Ende
plot_ly(...) %>% layout(...) %>% dm_plotly_layout()
```

### parliament_local.R
```r
# fill = "white" → fill = NA in theme_ggparliament_local() und waffle_gg()
# (damit dm_theme() den Hintergrund übernehmen kann)
plot.background = element_rect(fill = NA, colour = NA)
```

---

## Warum ausgebaut

- Bootstrap 3 + viele hardcodierte inline-Styles (`style="color:black;background-color:lavender"`)
  machen konsistentes Dark Mode theming aufwendig
- Einfacherer Weg: Migration auf **bslib Bootstrap 5** (`bs_theme(preset="lumen")`)
  + `bslib::input_dark_mode()` Widget — dann ist Dark Mode 3 Zeilen statt 300
- Empfehlung: Dark Mode zusammen mit bslib-Migration umsetzen
