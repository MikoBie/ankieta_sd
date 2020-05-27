recoder <- function(table, question = "qn_47"){
  ## Function to recode fields.
  ## Takes as an argument data frame in the long format
  ## and returns a vector.
  temp <- table %>%
    filter(qn == question)
  responses <- vector(mode = "character", length = nrow(temp))
  
  for (i in c(1:nrow(temp))){
    cat(paste0(i, "/", nrow(temp), "\n"))
    cat(paste0(temp$responses[i], "\n"))
    cat("Czy chcesz poprawić odpowiedź?\n")
    logic <- readline() %>% as.character()
    if (logic %in% c("yes", "Yes", "tak", "Tak", "t", "T", "Y", "y")){
      cat("Wpisz poprawioną odpowiedź. Oddziel średnikiem jeśli jest to lista.\n")
      res <- readline() %>% as.character()
      responses[i] <- res
    } else {
      responses[i] <- temp$responses[i]
    }
  }
  return(responses)
}

get_similar <- function(tokens, pattern, n = 20, ignore_case = TRUE, decreasing = TRUE) {
  ## Function to list similar strings.
  ## Takes as an argument a vector of strings and returns 
  ## strings consisting the pattern.
  tokens %>%
    keep(~str_detect(.x, regex(pattern, ignore_case = ignore_case))) %>%
    table %>%
    sort(decreasing = decreasing) %>%
    head(n = n)
}

get_png <- function(filename) {
  ## Function to read png files.
  ## Takes as an argument a file name.
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}



compute_number <- function(table = data_wide,
                           question = qn_3,
                           response = "Tak",
                           conditional = FALSE,
                           question2 = qn_3,
                           response2 = "Tak"){
  question <- enquo(question)
  question2 <- enquo(question2)
  table <- table %>%
    filter((!is.na(!! question)) & (!! question == response)) 
    
  if (conditional){
    number <- table %>%
      filter((!is.na(!! question2)) & (!! question2 == response2)) %>%
      nrow()
  }else{
    number <- table %>%
      nrow()
  }
  return(number)
}

filter_data_wide <- function(table = data_wide, question = qn_17, response = "Nie"){
  question <- enquo(question)
  table %>%
    filter(!is.na(!! question) & !! question == response)
}

plot_con_questions <- function(table = data_wide,
                               question = qn_3,
                               conditional = FALSE,
                               question2 = qn_3,
                               xmin = -3, xmax = 4, ymin = 85, ymax = 105,
                               legend_title = "",
                               plot_title = "",
                               reorder = FALSE,
                               legend = TRUE,
                               COLORS = COLORS[1:2]){
  plot_title <- str_wrap(plot_title)
  legend_title <- str_wrap(legend_title,60)
  question <- enquo(question)
  question2 <- enquo(question2)
  table <- table %>%
    filter(!is.na(!! question)) %>%
    mutate(label = !! question)
  if(reorder){
    order_question <- table %>%
      group_by(label) %>%
      summarise(count = n()) %>%
      arrange((count)) %>%
      pull(label)
    
    table <- table %>%
      mutate(label = factor(label, levels = order_question))
  }
  
  if (conditional){
     table <- table %>%
       filter(!is.na(!! question2)) %>%
       filter(!! question2 != "") %>%
       mutate(label2 = !! question2)
     
     plot_con <- table %>%
       ggplot(aes(x = label, fill = label2)) +
       geom_bar(show.legend = legend) +
       theme_classic() +
       labs(title = plot_title, x = "", y = "Liczebność")+ 
       #annotation_custom(l, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) + 
       #coord_cartesian(clip = "off") +
       coord_flip() +
       scale_y_continuous(expand = expansion(mult = c(0, .1))) +
       scale_fill_manual(legend_title,values=COLORS) +
       theme(legend.position="bottom")
      
  }else{
    plot_con <- table %>%
      ggplot(aes(x = label)) +
      geom_bar(fill = "#123e65") +
      theme_classic() +
      labs(title = plot_title, x = "", y = "Liczebność")+ 
      #annotation_custom(l, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) + 
      #coord_cartesian(clip = "off") +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, .1)))
  }
  print(plot_con)
}

make_table <- function(table = data_wide,
                       question = qn_3,
                       conditional = FALSE,
                       question2 = qn_3,
                       cap = ""){
  
  
  
  question <- enquo(question)
  question2 <- enquo(question2)
  question_label <- question_show(question = question)
  question_label2 <- question_show(question = question2)
  
  
  if(conditional){
    temp <- table %>%
      filter(!is.na(!! question)) %>%
      filter(!! question2 != "") %>%
      mutate(label = !! question,
             label2 = !! question2) %>%
      group_by(label, label2) %>%
      summarise(Frequency = n()) %>% 
      select(!! question_label := label,
             !! question_label2 := label2,
             Liczebność = Frequency) %>%
      kable(caption = cap, "html") %>%
      kable_styling(bootstrap_options = c("striped","hover"),
                    font_size = 10,
                    full_width = TRUE)
  } else {
    temp <- table %>%
      filter(!is.na(!! question)) %>%
      filter(!! question2 != "") %>%
      mutate(label = !! question) %>%
      group_by(label) %>%
      summarise(Frequency = n()) %>% 
      select(!! question_label2 := label,
             Liczebność  = Frequency) %>%
      kable(caption = cap, "html") %>%
      kable_styling(bootstrap_options = c("striped","hover"),
                    font_size = 10,
                    full_width = TRUE)
  }
  return(temp)
}

question_show <- function(table = QUESTION_LOOKUP, question = 'qn_1', lang = "polski"){
  table  %>%
    pivot_longer(cols = polski:english,
                 values_to = "value",
                 names_to = "language") %>%
    pivot_wider(names_from = qn,
                values_from = value) %>%
    filter(language == lang) %>%
    select(!! question) %>%
    pull()
}

make_open_table <- function(table = data_wide,
                            question = qn_4,
                            defualt_cap = TRUE,
                            custom_cap = "",
                            stupid_answers = ""
                            ){
  question <- enquo(question)
  if (defualt_cap){
    cap <- question_show(question = question) %>%
      str_remove(pattern = "_\\d")
      
  } else {
    cap <- custom_cap
  }
  
  table %>%
    filter(!is.na(!! question)) %>%
    filter(nchar(!! question) > 10) %>%
    filter(!(!! question) %in% stupid_answers) %>%
    select(Jednostka =  faculty_label, position, Odpowiedź = !! question) %>%
    ungroup(id) %>%
    mutate(id = 1:n(),
           Jednostka = as.factor(Jednostka),
           position = as.factor(position)) %>%
    rename("Rodzaj kształcenia" = position) %>%
    DT::datatable(caption = cap,
                  rownames = FALSE,
                  filter = "top",
                  options = list(sDom  = '<"top">lrt<"bottom">ip'))
}
