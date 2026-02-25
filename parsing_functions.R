# Regex to locate links in text
find_link <- regex("
  \\[   # Grab opening square bracket
  .+?   # Find smallest internal text as possible
  \\]   # Closing square bracket
  \\(   # Opening parenthesis
  .+?   # Link text, again as small as possible
  \\)   # Closing parenthesis
  ",
                   comments = TRUE)

# Function that removes links from text and replaces them with superscripts that are 
# referenced in an end-of-document list. 
sanitize_links <- function(text){
  if(PDF_EXPORT){
    str_extract_all(text, find_link) %>% 
      pluck(1) %>% 
      walk(function(link_from_text){
        title <- link_from_text %>% str_extract('\\[.+\\]') %>% str_remove_all('\\[|\\]') 
        link <- link_from_text %>% str_extract('\\(.+\\)') %>% str_remove_all('\\(|\\)')
        
        # add link to links array
        links <<- c(links, link)
        
        # Build replacement text
        new_text <- glue('{title}<sup>{length(links)}</sup>')
        
        # Replace text
        text <<- text %>% str_replace(fixed(link_from_text), new_text)
      })
  }
  text
}

# Take entire positions dataframe and removes the links 
# in descending order so links for the same position are
# right next to eachother in number. 
strip_links_from_cols <- function(dat, cols) {
  existing_cols <- intersect(cols, colnames(dat))

  if(length(existing_cols) == 0){
    return(dat)
  }
  for(col in existing_cols){
    dat[[col]] <- ifelse(is.na(dat[[col]]), 
                         NA, 
                         stringr::str_remove_all(dat[[col]], "\\\\[.*?\\\\]\\(.*?\\\\)"))
  }
  return(dat)
}

# Take a position dataframe and the section id desired
# and prints the section to markdown. 
print_section <- function(dat, section_id) {
  dat %>%
    filter(section == section_id) %>%
    arrange(desc(rank)) %>%
    mutate(
      description = ifelse(is.na(description), "", description),
      description_bullets = ifelse(is.na(description_bullets), "", description_bullets)
    ) %>%
    rowwise() %>%
    mutate(
      bullets = if(description_bullets == "") "" else {
        paste0(
          "<ul>",
          paste0("<li>", unlist(strsplit(description_bullets, ";")), "</li>", collapse=""),
          "</ul>"
        )
      }
    ) %>%
    glue::glue_data("
### {title}
**{institution}**, {loc}  
{start} - {end}<br>
{description}
{bullets}
")
}
# Construct a bar chart of skills
build_skill_bars <- function(skills, out_of = 5){
  bar_color <- "#969696"
  bar_background <- "#d9d9d9"
  skills %>% 
    mutate(width_percent = round(100*level/out_of)) %>% 
    glue_data(
      "<div class = 'skill-bar'",
      "style = \"background:linear-gradient(to right,",
      "{bar_color} {width_percent}%,",
      "{bar_background} {width_percent}% 100%)\" >",
      "{skill}",
      "</div>"
    )
}

