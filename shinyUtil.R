selectList <- function(x) {
  x <- unique(as.character(x))
  lapply(x, function(e) {
    e
  })
}

abbrev <- function(x, prefix) {
  if(is.na(x)) return(as.character(x))
  suffix <- ""
  if(x > 1e12){
    suffix <- 'T'
    x <- x/1e12
  } else if(x > 1e9){
    suffix <- 'B'
    x <- x/1e9
  } else if(x > 1e6){
    suffix <- 'M'
    x <- x/1e6
  } else if(x > 1000){
    suffix <- 'K'
    x <- x/1000
  } 
  paste0(prefix, round(x, 0), suffix)
}

abbrev_large <- function(d, prefix = "") {
  vapply(d, abbrev, vector('character', 1), prefix = prefix)
}

walk_nav <- function(x, brand='', level=1) {
  tagList(lapply(x, function(obj) {
    if (class(obj) == 'shiny.navbarmenu') {
      return(
        tags$li(class='dropdown',
                tags$a(class='dropdown-toggle', `data-toggle`='dropdown',
                       href='#', obj$title, tags$b(class='caret')
                ),
                tags$ul(class=ifelse(level == 1, 
                                     'dropdown-menu', 
                                     'dropdown-menu dropdown-submenu'),
                        walk_nav(obj$tabs, brand=brand, level=level + 1)
                )
        )
      )
    } else if (obj$attribs$class == 'tab-pane') {
      if (obj$attribs$id == brand) {
        a_class = 'brand'
        li_class = 'active'
      } else {
        a_class = NULL
        li_class = NULL
      }
      return(tags$li(class=li_class,
                     tags$a(class=a_class, `data-toggle`='tab', `data-value`=obj$attribs$id,
                            href=paste0('#', obj$attribs$id), obj$attribs$title
                     )
      ))
    } else {
      stop(paste0('Unexpected tag: ', obj))
    }
  }))
}

nav <- function(x, id=NULL, brand='') {
  tagList(
    tags$head(tags$script(src='js/navbar.js', type='text/javascript')),
    tags$div(class='navbar navbar-fixed-top navbar-inverse', id=id, 
             `data-nav-value`=brand,
             tags$div(class='navbar-inner col-sm-offset-1',
                      tags$div(class='container-fluid',
                               tags$ul(class='nav navbar-nav', walk_nav(x, brand=brand))
                      )
             )
    )
  )
}


tabs <- function(x, id=NULL, brand='') {
  result = list()
  queue = x
  
  # find all of the tab-panes
  while (length(queue)) {
    obj = queue[[1]]
    queue = queue[-1]
    
    if (class(obj) == 'shiny.navbarmenu') {
      queue = append(obj$tabs, queue)
    } else if (obj$attribs$class == 'tab-pane') {
      if (obj$attribs$id == brand) {
        obj$attribs$class = paste(obj$attribs$class, 'active')
      }
      result = append(result, list(obj))
    }
  }
  
  return(tags$div(class='tab-content', result))
}
