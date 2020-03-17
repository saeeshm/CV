# Author: Nick Strayer, adapted for use by Saeesh Mangwani
# Date: 2020-03-02
# Title: Parsing Functions for CV
# Desc: A set of functions to assist with parsing data from google sheets and turning it into a CV

# Loading libraries
library(tidyverse)
library(glue)

# Regex to locate links in text
find_link <- regex("
  \\[   # Grab opening square bracket
  .+?   # Find smallest internal text as possible
  \\]   # Closing square bracket
  \\(   # Opening parenthesis
  .+?   # Link text, again as small as possible
  \\)   # Closing parenthesis
  ", comments = TRUE)

# Function that removes links from text and replaces them with superscripts that are referenced in an end-of-document list. -- I don't use this
# function but I think its interesting and useful so I've left it in
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

# Take entire positions dataframe and removes the links in descending order so links for the same position are right next to each other in number. --
# I don't use this function but I think its interesting and useful so I've left it in
strip_links_from_cols <- function(data, cols_to_strip){
  for(i in 1:nrow(data)){
    for(col in cols_to_strip){
      data[i, col] <- sanitize_links(data[i, col])
    }
  }
  data
}

# Take a position dataframe and the section desired (selected using a section_id column) and prints the section to markdown. 
print_section <- function(position_data, section_id){
  position_data %>% 
    # filtering only the rows for the section we want
    filter(section == section_id) %>% 
    # Arrange in descending order of year completed
    arrange(desc(end)) %>% 
    # resetting the id varible to now assign ids counting from 1 to the length of the section 
    mutate(id = 1:n()) %>% 
    # Pivoting the 3 description columns down to 2, with one column specfiying the description number and another containing the description text
    pivot_longer(
      starts_with('description'),
      names_to = 'description_num',
      values_to = 'description',
      values_drop_na = TRUE
    ) %>% 
    # Grouping by id and then creating a list of all the descriptions relevant to that id. Storing that list in a new variable called descriptions for
    # each id
    group_by(id) %>% 
    mutate(
      descriptions = list(description)
    ) %>% 
    ungroup() %>% 
    # filter only the first descriptions from the descriptions column for now (to make the following step possible)
    filter(description_num == 'description_1') %>% 
    # creating a new variable 'timeline' that stores the markdown text required to make the timeline for that id
    mutate(
      # if the start is empty or the end and the start are the same year, set the text to just end. Else, set it to range from start to end
      timeline = ifelse(
        is.na(start) | start == end,
        end,
        glue('{start} - {end}')
      ),
      # Create a variable called description bullets which takes the list of descriptions and turns them into a single string formatted as markdown
      # bullet points
      description_bullets = map_chr(descriptions, ~paste('-', ., collapse = '\n')),
    ) %>% 
    # Remove the links for storage later from the following columns
    strip_links_from_cols(c('title', 'description_bullets')) %>% 
    # If there are any missing values, replacing them with the string N/A rather than have them break the code
    mutate_all(~ifelse(is.na(.), 'N/A', .)) %>% 
    # glueing the final markdown chunk for this section.
    glue_data(
      "### {title}",
      "\n\n",
      "{institution}",
      "\n\n",
      "{loc}",
      "\n\n",
      "{timeline}", 
      "\n\n",
      "{description_bullets}",
      "\n\n\n",
    )
}

# Construct a bar chart of skills -- I don't use this function but I think its interesting and useful so I've left it in
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

# Gets blocks of text for the intro and asides from text_blocks spreadsheet and returns it as a printable string.
print_text_block <- function(text_blocks, label){
  filter(text_blocks, loc == label)$text %>%
    sanitize_links() %>%
    cat()
}