# COLOR
colorblind_pallet <- c("#888888", "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
                       "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#000000", "#E69F00", "#56B4E9", "#009E73",
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#009900", "#E00F00", "#50B0E9", "#999E73",
                       "#F9E992", "#3372B2", "#A33E00", "#EE79A7","#777888", "#44CCEE", "#CC1177", "#AACC00")

# PLOTs

# Data Summary
get_data_summary <- function(data, plot.type,  measure.var, group.vars){

  if(class(data) != 'data.frame'){
    data<-as.data.frame(data)
  }
  data.summary <- ddply(.data = data, .variables = group.vars,
                        .fun = function(xx, col) {
                          c(N    = sum(!is.na(xx[[col]])),
                            Mean = mean(xx[[col]], na.rm=T),
                            SD   = sd(xx[[col]], na.rm=T)
                          )},
                        measure.var)

  data.summary <- plyr::rename(data.summary, c("Mean" = measure.var))
  data.summary$SE <- data.summary$SD/sqrt(data.summary$N)  # Calculate standard error

  if (plot.type == "normal") {
    return(data.summary)
  }else{
    return(data)
  }
}
get_data_summary_study <- function(data, measure.var, group.vars){
  if(class(data) != 'data.frame'){
    data<-as.data.frame(data)
  }
  # make sure the colnames of indata including 'Arms', 'ID', 'Times', 'Volume' or 'BodyWeight', which guarantee normal running for the following code.

  need.cols <- c('Arms', 'ID', 'Times', 'Volume')
  judged.res <- is.element(need.cols,colnames(data))

  if ("FALSE" %in% judged.res)
    stop("The input data must include at least four columns, that are 'Arms', 'ID', 'Times' and 'Volume'. Please check the data whether includes these columns and ensure the colnames is consistent with 'Arms', 'ID', 'Times' and 'Volume'")

  data <- subset(data,measure.var>0)

  data.summary <- ddply(.data = data,.variables = group.vars,
                        .fun = function(xx, col) {
                          c(N    = sum(!is.na(xx[[col]])),
                            Mean = mean(xx[[col]], na.rm=T),
                            SD   = sd(xx[[col]], na.rm=T)
                          )},
                        measure.var)

  data.summary <- plyr::rename(data.summary, c("Mean" = measure.var))
  data.summary$SE <- data.summary$SD/sqrt(data.summary$N)  # Calculate standard error

  return(data.summary)
}

# Default
get_tv_plot <- function(data, level = c('Animal','Arm'), pattern = c('Treatment', 'Study'), orders = NULL, position.dodge, ...){

  if(class(data) != 'data.frame'){
    data<-as.data.frame(data)
  }

  data$Arms <- relevel(factor(data$Arms), 'Control')

  if( level == 'Arm'){
    s.data <- get_data_summary(data, plot.type="normal", measure.var = "Volume",
                          group.vars = c('Study', "Times", "Arms"))
    if(pattern == 'Study'){
      p <- ggplot(data = s.data, aes(x = Times, y = Volume, color = Arms)) +
                  geom_line(position = position_dodge2(position.dodge),cex = 1.2) +
                  geom_errorbar(aes(ymin = Volume - SE, ymax = Volume + SE),
                                width = 1,
                                position = position_dodge2(position.dodge)) +
                  geom_point(cex = 2,
                            position = position_dodge2(position.dodge)
                  )
    }
    if(pattern == 'Treatment'){

      p <- ggplot(data = s.data, aes(x = Times, y = Volume, color = Study)) +
            geom_line(position = position_dodge2(position.dodge),cex = 1.2) +
            geom_errorbar(aes(ymin = Volume - SE, ymax = Volume + SE),
                          width = 1,
                          position = position_dodge2(position.dodge)) +
            geom_point(cex = 2,
                      position = position_dodge2(position.dodge)
            )
    }


  }

  if( level == 'Animal') {
    if(pattern == 'Study'){
      p <- ggplot(data, aes(x = Times, y = Volume, group = ID, color = Arms)) +
        geom_line(size=0.8) +
        geom_point(cex=1.5,aes(colour = Arms))
    }
    if(pattern == 'Treatment'){
      p <- ggplot(data, aes(x = Times, y = Volume, group = ID, color = Study)) +
        geom_line(size=0.8) +
        geom_point(cex=1.5,aes(colour = Study))
    }
  }

  p <- p + xlab('Time (days)') + ylab('Tumor Volume (mm3)')

  p <- p + theme_bw() +
    theme(
      panel.background = element_rect(fill = "transparent"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background  = element_rect(fill = "transparent")
    )   #backgroud
  p <- p + theme(
    axis.title.x = element_text(face = "bold",size = 8),
    axis.text.x  = element_text(vjust = 0,size = 7),
    axis.title.y = element_text(face = "bold",size = 8),
    axis.text.y  = element_text(hjust = 1,size = 7)
  )

  if(pattern == 'Study'){
    p <- p + facet_wrap(~ Study, dir = 'h')+ labs(color = "Treatments") +
      theme(strip.text = element_text(colour = "black", face = "bold", size = rel(1)),
            strip.background = element_rect(fill = "#56b4e9", size = rel(1.05), linetype = 1)
      )
  }

  if(pattern == 'Treatment'){
    p <- p + facet_wrap(~ Arms, dir = 'h')+ labs(color = "Study") +
      theme(strip.text = element_text(colour = "black", face = "bold", size = rel(1)),
            strip.background = element_rect(fill = "#56b4e9", size = rel(1.05), linetype = 1)
      )
  }

  p <- p + scale_color_manual(values=colorblind_pallet)
  pdf(NULL)
  p
}

# Scaled - Moscow
get_plot_scaled <- function(data, orders = NULL, position.dodge, title = NULL, plot_on = TRUE, scale.factor, scale.by.volume = FALSE, level = 'Arm', pattern = "TAN", ...){

  if(inherits(data, "data.frame")){
    data<-as.data.frame(data)
  }

  #check the order
  if (!is.null(orders)){

    arms <- unique(data$Arms)
    judged.order <- is.element(orders,arms)

    if ("FALSE" %in% judged.order)
      stop("The input order is improper. Please ensure the input order is the order of 'Arms' among data.")

    data$Arms <- factor(data$Arms,levels=orders) # set the order
  }

  Volume <-  data[ , 'Volume']
  data <- subset(data,Volume >= 0)

  if ('Control'%in%unique(data$Arms)){
    data$Arms <- relevel(factor(data$Arms), 'Control')
  }

  if(scale.by.volume) {
    adjusted.data <- data %>%
      group_by(Study, Arms, Times) %>%
      mutate(TimeMean = mean(Volume)) %>%
      ungroup() %>%
      group_by(Study, Arms, ID) %>%
      mutate(zero_adjust = Volume - Volume[1L],
             EndPoint = scale.factor,
             ArmMean = mean(Volume),
             Moscow = ifelse(zero_adjust == 0, 0,
                             ifelse(zero_adjust < 0, ((((Volume - Volume[1L]) / Volume[1L]) * 100)),
                                    ifelse(Volume >= EndPoint, 100, (((Volume / EndPoint) * 100))))))
    caption_text = bquote('Endpoint Scale = ' ~ .(scale.factor) ~ 'mm'^3)
  } else {
    adjusted.data <- data %>%
      group_by(Study, Arms, Times) %>%
      mutate(TimeMean = mean(Volume)) %>%
      ungroup() %>%
      group_by(Study, Arms, ID) %>%
      mutate(zero_adjust = Volume - Volume[1L],
             EndPoint = TimeMean[1L] * scale.factor,
             ArmMean = mean(Volume),
             Moscow = ifelse(zero_adjust == 0, 0,
                             ifelse(zero_adjust < 0, ((((Volume - Volume[1L]) / Volume[1L]) * 100)),
                                    ifelse(Volume >= EndPoint, 100, (((Volume / EndPoint) * 100))))))
    caption_text = paste0('Endpoint Scale = ', scale.factor, 'x')
  }

  if( level == 'Animal') {
    if(pattern == 'Study'){
      p <- ggplot(data = adjusted.data, aes(x = Times, y = Moscow, group = ID, color = Arms)) +
        geom_line(position = position_dodge(position.dodge),cex = 1.2) +
        geom_point(cex = 2, position = position_dodge(position.dodge)) +
        ylim(-100, 100) + labs(caption = caption_text)
    }
    if(pattern == 'Treatment'){
      p <- ggplot(data = adjusted.data, aes(x = Times, y = Moscow, group = ID, color = Study)) +
        geom_line(position = position_dodge(position.dodge),cex = 1.2) +
        geom_point(cex = 2, position = position_dodge(position.dodge)) +
        ylim(-100, 100) + labs(caption = caption_text)
    }
  }

  if( level == 'Arm') {
    s.data <- adjusted.data %>%
              group_by(Study, Arms, Times) %>%
              summarise(N = n(), Mean = mean(Moscow), SD = sd(Moscow), SE = (SD / sqrt(N)), Study=Study, ID=ID, .groups = 'drop') %>%
              dplyr::rename(Volume = Mean)
    if(pattern == 'Study'){
      p <- ggplot(data = s.data, aes(x = Times, y = Volume, color = Arms)) +
            geom_line(position = position_dodge(position.dodge),cex = 1.2) +
            geom_errorbar(aes(ymin = Volume - SE, ymax = Volume + SE),
                          position = position_dodge(position.dodge)) +
            geom_point(cex = 2,position = position_dodge(position.dodge)) +
            ylim(-100, 100) + labs(caption = caption_text)
    }

    if(pattern == 'Treatment'){
      p <- ggplot(data = s.data, aes(x = Times, y = Volume, color = Study)) +
            geom_line(position = position_dodge(position.dodge),cex = 1.2) +
            geom_errorbar(aes(ymin = Volume - SE, ymax = Volume + SE),
                          position = position_dodge(position.dodge)) +
            geom_point(cex = 2,position = position_dodge(position.dodge)) +
            ylim(-100, 100) + labs(caption = caption_text)
    }

  }
  text <- "% progression / regression endpoint"

  p <- p + xlab("Time (d)") + ylab(text)

  p <- p + theme_bw() +
    theme(
      panel.background = element_rect(fill = "transparent"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background  = element_rect(fill = "transparent")
    )   #backgroud
  p <- p + theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.text.x  = element_text(vjust = 0,size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    axis.text.y  = element_text(hjust = 1,size = 12)
  )

  if(pattern == 'Study'){

    p <- p + facet_wrap(~ Study, dir = 'h')+ labs(color = "Treatments") +
      theme(strip.text = element_text(colour = "black", face = "bold", size = rel(1)),
            strip.background = element_rect(fill = "#56b4e9", size = rel(1.05), linetype = 1)
      )
  }

  if(pattern == 'Treatment'){

    p <- p + facet_wrap(~ Arms, dir = 'h')+ labs(color = "Study") +
      theme(strip.text = element_text(colour = "black", face = "bold", size = rel(1)),
            strip.background = element_rect(fill = "#56b4e9", size = rel(1.05), linetype = 1)
      )
  }

  p <- p + scale_color_manual(values=colorblind_pallet)

  if (! is.null(title)) {
    p <- p + ggtitle(title)
  }

  if (plot_on) {
    plot(p)
  } else {
    p
  }
}

get_plot_scaled_study <- function(data, orders = NULL, position.dodge, title = NULL, plot_on = TRUE, scale.factor, scale.by.volume = FALSE, ...){

  if(inherits(data, "data.frame")){
    data<-as.data.frame(data)
  }

  #check the order
  if (!is.null(orders)){

    arms <- unique(data$Arms)
    judged.order <- is.element(orders,arms)

    if ("FALSE" %in% judged.order)
      stop("The input order is improper. Please ensure the input order is the order of 'Arms' among data.")

    data$Arms <- factor(data$Arms,levels=orders) # set the order


  }

  Volume <-  data[ , 'Volume']
  data <- subset(data,Volume >= 0)

  if ('Control'%in%unique(data$Arms)){
    data$Arms <- relevel(factor(data$Arms), 'Control')
  }

  if(scale.by.volume) {
    adjusted.data <- data %>%
      group_by(Arms, Times) %>%
      mutate(TimeMean = mean(Volume)) %>%
      ungroup() %>%
      group_by(Arms, ID) %>%
      mutate(zero_adjust = Volume - Volume[1L],
             EndPoint = scale.factor,
             ArmMean = mean(Volume),
             Moscow = ifelse(zero_adjust == 0, 0,
                             ifelse(zero_adjust < 0, (( (Volume - Volume[1L]) / Volume[1L]) * 100),
                                    ifelse(Volume >= EndPoint, 100, (((Volume / EndPoint) * 100))))))
    caption_text = bquote('Endpoint Scale = ' ~ .(scale.factor) ~ 'mm'^3)
  } else {
    adjusted.data <- data %>%
      group_by(Arms, Times) %>%
      mutate(TimeMean = mean(Volume)) %>%
      ungroup() %>%
      group_by(Arms, ID) %>%
      mutate(zero_adjust = Volume - Volume[1L],
             EndPoint = TimeMean[1L] * scale.factor,
             ArmMean = mean(Volume),
             Moscow = ifelse(zero_adjust == 0, 0,
                             ifelse(zero_adjust < 0, (( (Volume - Volume[1L]) / Volume[1L]) * 100),
                                    ifelse(Volume >= EndPoint, 100, (((Volume / EndPoint) * 100))))))
    caption_text = paste0('Endpoint Scale = ', scale.factor, 'x')
  }

  s.data <- adjusted.data %>%
    group_by(Arms, Times) %>%
    summarise(N = n(), Mean = mean(Moscow), SD = sd(Moscow), SE = (SD / sqrt(N)), .groups = 'drop') %>%
    dplyr::rename( Volume = Mean)

  p <- ggplot(data = s.data, aes(x = Times, y = Volume, color = Arms)) +
    geom_line(position = position_dodge(position.dodge),cex = 1.2) +
    geom_errorbar(aes(ymin = Volume - SE, ymax = Volume + SE),
                  position = position_dodge(position.dodge)) +
    geom_point(cex = 2,position = position_dodge(position.dodge)) +
    ylim(-100, 100) +
    labs(caption = caption_text)


  text <- "% progression / regression endpoint"

  p <- p + xlab("Time (d)") + ylab(text)

  p <- p + theme_bw() +
    theme(
      panel.background = element_rect(fill = "transparent"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background  = element_rect(fill = "transparent")
    )   #backgroud
  p <- p + theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.text.x  = element_text(vjust = 0,size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    axis.text.y  = element_text(hjust = 1,size = 12)
  )

  p <- p + scale_color_manual(values=colorblind_pallet)

  if (! is.null(title)) {
    p <- p + ggtitle(title)
  }

  if (plot_on) {
    plot(p)
  } else {
    return(p)
  }
}

# Interpolated - Moscow
get_interpolated_pdx_data <- function(data){

  # Get Max Days for Each Group
  data.tmp <- data %>%
    dplyr::arrange(ID, Times) %>%
    dplyr::group_by(Tumor, Arms, ID) %>%
    dplyr::summarise(Day_max=max(Times), Day_min=min(Times), N=n(), .groups = 'drop') %>%
    dplyr::mutate(adjustment = case_when(Day_min == 0 ~ 1,
                                         Day_min == 1 ~ 0)) %>%
    dplyr::filter(N != 1)

  # Back fill the data with all day numbers
  data.tmp <- data.tmp %>%
    dplyr::group_by(ID) %>%
    do( data.frame(ID = rep(.$ID, each = .$Day_max+.$adjustment),
                   Tumor = rep(.$Tumor, each = .$Day_max+.$adjustment),
                   Arms = rep(.$Arms, each = .$Day_max+.$adjustment),
                   Times=.$Day_min:.$Day_max,   stringsAsFactors = FALSE) )

  # Add via Join all data we have for days that data were actually collected.
  data.tmp <- data %>%
    dplyr::full_join( data.tmp, by = c( "Tumor" = "Tumor" , "Arms" = "Arms" ,  "ID" = "ID" , "Times" = "Times"))%>%
    arrange( ID, Times)

  #print(data.tmp)

  # Add in missing measurements via na.approx from Zoo package.
  Interpolate.Data <- data.tmp %>%
    arrange( ID, Times) %>%
    dplyr::group_by(ID) %>%
    mutate(Interpolated_Volume = na.approx(Volume, na.rm=FALSE))

  # Add in missing metadata with na.locf
  Interpolate.Data <- arrange(Interpolate.Data, ID, Times)%>%
    dplyr::mutate(Type = na.locf(Type, na.rm = F),
                  Arms = na.locf(Arms, na.rm = F),
                  Study = na.locf(Study, na.rm = F),
                  Interpolated_Volume = na.locf(Interpolated_Volume, na.rm = F))

  return(Interpolate.Data)
}

get_plot_scaled_interpolated <- function(data, orders = NULL, position.dodge, title = NULL, plot_on = TRUE, scale.factor, scale.by.volume = FALSE, level = 'Arm', pattern = 'TAN', ...) {

  if(inherits(data, "data.frame")){
    data<-as.data.frame(data)
  }

  #check the order
  if (!is.null(orders)){

    arms <- unique(data$Arms)
    judged.order <- is.element(orders,arms)

    if ("FALSE" %in% judged.order)
      stop("The input order is improper. Please ensure the input order is the order of 'Arms' among data.")

    data$Arms <- factor(data$Arms,levels=orders) # set the order
  }

  if ('Control'%in%unique(data$Arms)){
    data$Arms <- relevel(factor(data$Arms), 'Control')
  }

  if(scale.by.volume) {

    adjusted.data <- data %>%
      group_by(Arms, Times) %>%
      mutate(TimeMean = mean(Interpolated_Volume)) %>%
      ungroup() %>%
      group_by(Arms, ID) %>%
      mutate(zero_adjust = Interpolated_Volume - Interpolated_Volume[1L],
             EndPoint = scale.factor,
             ArmMean = mean(Interpolated_Volume),
             Moscow = ifelse(zero_adjust == 0, 0,
                             ifelse(zero_adjust < 0, ((((Interpolated_Volume - Interpolated_Volume[1L]) / Interpolated_Volume[1L]) * 100)),
                                    ifelse(Interpolated_Volume >= EndPoint, 100, (((Interpolated_Volume / EndPoint) * 100))))))
    caption_text = bquote('Endpoint Scale = ' ~ .(scale.factor) ~ 'mm'^3)

  } else {

    adjusted.data <- data %>%
      group_by(Arms, Times) %>%
      mutate(TimeMean = mean(Interpolated_Volume)) %>%
      ungroup() %>%
      group_by(Arms, ID) %>%
      mutate(zero_adjust = Interpolated_Volume - Interpolated_Volume[1L],
             EndPoint = TimeMean[1L] * scale.factor,
             ArmMean = mean(Interpolated_Volume),
             Moscow = ifelse(zero_adjust == 0, 0,
                             ifelse(zero_adjust < 0, ((((Interpolated_Volume - Interpolated_Volume[1L]) / Interpolated_Volume[1L]) * 100)),
                                    ifelse(Interpolated_Volume >= EndPoint, 100, (((Interpolated_Volume / EndPoint) * 100))))))
    caption_text = paste0('Endpoint Scale = ', scale.factor, 'x')
  }

  if( level == 'Animal') {
    if(pattern == 'Study'){
      p <- ggplot(data = adjusted.data, aes(x = Times, y = Moscow, group = ID, color = Arms)) +
        geom_line(position = position_dodge(position.dodge),cex = 1.2) +
        geom_point(cex = 2, position = position_dodge(position.dodge)) +
        ylim(-100, 100) + labs(caption = caption_text)
    }
    if(pattern == 'Treatment'){
      p <- ggplot(data = adjusted.data, aes(x = Times, y = Moscow, group = ID, color = Study)) +
        geom_line(position = position_dodge(position.dodge),cex = 1.2) +
        geom_point(cex = 2, position = position_dodge(position.dodge)) +
        ylim(-100, 100) + labs(caption = caption_text)
    }
  }

  if( level == 'Arm') {
    s.data <- adjusted.data %>%
              group_by(Study, Arms, Times) %>%
              summarise(N = n(), Mean = mean(Moscow), SD = sd(Moscow), SE = (SD / sqrt(N)), Study=Study, ID=ID, .groups = 'drop') %>%
              dplyr::rename(Volume = Mean)
    if(pattern == 'Study'){
      p <- ggplot(data = s.data, aes(x = Times, y = Volume, color = Arms)) +
            geom_line(position = position_dodge(position.dodge),cex = 1.2) +
            geom_errorbar(aes(ymin = Volume - SE, ymax = Volume + SE),
                          position = position_dodge(position.dodge)) +
            geom_point(cex = 2,position = position_dodge(position.dodge)) +
            ylim(-100, 100) + labs(caption = caption_text)
    }

    if(pattern == 'Treatment'){
      p <- ggplot(data = s.data, aes(x = Times, y = Volume, color = Study)) +
            geom_line(position = position_dodge(position.dodge),cex = 1.2) +
            geom_errorbar(aes(ymin = Volume - SE, ymax = Volume + SE),
                          position = position_dodge(position.dodge)) +
            geom_point(cex = 2,position = position_dodge(position.dodge)) +
            ylim(-100, 100) + labs(caption = caption_text)
    }

  }
  text <- "% progression / regression endpoint"

  p <- p + xlab("Time (d)") + ylab(text)

  p <- p + theme_bw() +
    theme(
      panel.background = element_rect(fill = "transparent"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background  = element_rect(fill = "transparent")
    )   #backgroud
  p <- p + theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.text.x  = element_text(vjust = 0,size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    axis.text.y  = element_text(hjust = 1,size = 12)
  )

  if(pattern == 'Study'){

    p <- p + facet_wrap(~ Study, dir = 'h')+ labs(color = "Treatments") +
      theme(strip.text = element_text(colour = "black", face = "bold", size = rel(1)),
            strip.background = element_rect(fill = "#56b4e9", size = rel(1.05), linetype = 1)
      )
  }

  if(pattern == 'Treatment'){

    p <- p + facet_wrap(~ Arms, dir = 'h')+ labs(color = "Study") +
      theme(strip.text = element_text(colour = "black", face = "bold", size = rel(1)),
            strip.background = element_rect(fill = "#56b4e9", size = rel(1.05), linetype = 1)
      )
  }

  p <- p + scale_color_manual(values=colorblind_pallet)

  if (! is.null(title)) {
    p <- p + ggtitle(title)
  }

  if (plot_on) {
    plot(p)
  } else {
    p
  }
}

get_plot_interpolated <- function(data, orders = NULL, position.dodge, title = NULL, plot_on = TRUE, level = 'Arm', pattern = 'TAN', ...) {

  if(inherits(data, "data.frame")){
    data<-as.data.frame(data)
  }

  #check the order
  if (!is.null(orders)){
    arms <- unique(data$Arms)
    judged.order <- is.element(orders,arms)

    if ("FALSE" %in% judged.order)
      stop("The input order is improper. Please ensure the input order is the order of 'Arms' among data.")

    data$Arms <- factor(data$Arms,levels=orders) # set the order
  }

  Volume <-  data[ , 'Interpolated_Volume']
  data <- subset(data,Interpolated_Volume >= 0)

  # Order Control to the top
  if ('Control'%in%unique(data$Arms)){
    data$Arms <- relevel(factor(data$Arms), 'Control')
  }

  if( level == 'Animal'){


    p <- ggplot(data, aes(x = Times, y = Interpolated_Volume, group = ID,color = Arms)) +
      geom_line(size=0.8)+
      geom_point(cex=1.5,aes(colour = Arms))
  }
  if( level == 'Arm'){
    s.data <- get_data_summary_study(data,measure.var = "Interpolated_Volume", group.vars = c("Arms", "Times"))

    p <- ggplot(data = s.data, aes(x = Times, y = Interpolated_Volume, color = Arms)) +
      geom_line(position = position_dodge(position.dodge),cex = 1.2) +
      geom_errorbar(aes(ymin = Interpolated_Volume - SE, ymax = Interpolated_Volume + SE),
                    width = 1.2,
                    position = position_dodge(position.dodge)) +
      geom_point(cex = 2,position = position_dodge(position.dodge))
  }

  p <- p + xlab("Time (d)") + ylab('Tumor Volume mm^3')

  p <- p + theme_bw() +
    theme(
      panel.background = element_rect(fill = "transparent"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background  = element_rect(fill = "transparent")
    )   #backgroud
  p <- p + theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.text.x  = element_text(vjust = 0,size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    axis.text.y  = element_text(hjust = 1,size = 12)
  )

  if( level == 'Animal'){
    p <- p + theme(legend.position='none')
    p <- p + facet_wrap( ~ Arms,dir = 'h') +
      theme(strip.text = element_text(colour = "black", face = "bold", size = rel(1)),
            strip.background = element_rect(fill = "#E6AB02", size = rel(1.05), linetype = 1)
      )
  }

  p <- p + scale_color_manual(values=colorblind_pallet)

  if (plot_on) {
    if (! is.null(title)) {
      plot(p + ggtitle(title))
    } else {
      plot(p)
    }
  } else {
    if (! is.null(title)) {
      p <- p + ggtitle(title)
      return(p)
    } else {
      return(p)
    }
  }
}

# Other
get_DRLevel <- function(data, neg.control, rm.neg.control=TRUE, last.measure.day = NULL){
  if(inherits(data, "data.frame")){
    data<-as.data.frame(data)
  }

  Volume <- data[,'Volume']
  indata <- subset(data,Volume >= 0)

  drug.response.level <- get_response_level(indata, last.measure.day)

  return(drug.response.level)

}

get_plot_volumeGC_alt <- function(data, level = c('Animal','Arm'), orders = NULL,
                                  position.dodge, title = NULL, plot_on = TRUE, ...){

  if(inherits(data, "data.frame")){
    data<-as.data.frame(data)
  }

  #check the order
  if (!is.null(orders)){
    arms <- unique(data$Arms)
    judged.order <- is.element(orders,arms)

    if ("FALSE" %in% judged.order)
      stop("The input order is improper. Please ensure the input order is the order of 'Arms' among data.")

    data$Arms <- factor(data$Arms,levels=orders) # set the order
  }

  Volume <-  data[ , 'Volume']
  data <- subset(data,Volume >= 0)

  # Order Control to the top
  if ('Control'%in%unique(data$Arms)){
    data$Arms <- relevel(factor(data$Arms), 'Control')
  }

  if( level == 'Animal'){

    Volume <-  data[ , 'Volume']
    Times<- data[ , 'Times']
    ID <- data[ , 'ID']
    Arms <- data[ , 'Arms']

    p <- ggplot(data, aes(x = Times, y = Volume,group = ID,color = Arms)) +
      geom_line(size=0.8)+
      geom_point(cex=1.5,aes(colour = Arms))
  }
  if( level == 'Arm'){
    s.data <- get_data_summary_study(data,measure.var = "Volume", group.vars = c("Arms","Times"))

    Volume <-  s.data[ , 'Volume']
    Times<- s.data[ , 'Times']
    Arms <- s.data[ , 'Arms']
    SE <- s.data[ , 'SE']

    p <- ggplot(data = s.data, aes(x = Times, y = Volume, color = Arms)) +
      geom_line(position = position_dodge(position.dodge),cex = 1.2) +
      geom_errorbar(aes(ymin = Volume - SE, ymax = Volume + SE),
                    width = 1.2,
                    position = position_dodge(position.dodge)) +
      geom_point(cex = 2,position = position_dodge(position.dodge))
  }

  p <- p + xlab("Time (d)") + ylab(expression(bold(paste("Tumor Volume (",mm^3,")",sep = " "))))

  p <- p + theme_bw() +
    theme(
      panel.background = element_rect(fill = "transparent"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background  = element_rect(fill = "transparent")
    )   #backgroud
  p <- p + theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.text.x  = element_text(vjust = 0,size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    axis.text.y  = element_text(hjust = 1,size = 12)
  )

  if( level == 'Animal'){
    p <- p + theme(legend.position='none')
    p <- p + facet_wrap( ~ Arms,dir = 'h') +
      theme(strip.text = element_text(colour = "black", face = "bold", size = rel(1)),
            strip.background = element_rect(fill = "#E6AB02", size = rel(1.05), linetype = 1)
      )
  }

  p <- p + scale_color_manual(values=colorblind_pallet)

  if (plot_on) {
    if (! is.null(title)) {
      plot(p + ggtitle(title))
    } else {
      plot(p)
    }
  } else {
    if (! is.null(title)) {
      p <- p + ggtitle(title)
      return(p)
    } else {
      return(p)
    }
  }
}
get_response_level <- function(data, last.measure.day){

  if(inherits(data, "data.frame")){
    data<-as.data.frame(data)
  }

  Response.Level <- vector()
  mouse.info <- vector()
  Arms <- unique(data$Arms)

  for ( i in 1:length(Arms)){
    data.id.i <- data[data$Arms == Arms[i],]
    ID <- unique(data.id.i$ID)
    rc.change <- vector()

    for ( a in 1:length(ID)) {
      data.id.a <- data.id.i[data.id.i$ID == ID[a],]
      data.id.a <- data.id.a[order(data.id.a$Times),]

      if (! is.null(last.measure.day)) {
        end_day_index = match(last.measure.day,data.id.a$Times)

        if (is.na(end_day_index)) {
          end_day_index = which.min(abs(data.id.a$Times - last.measure.day))
        }
      } else {
        end_day_index <-length(data.id.a$Volume)
      }

      volume.change.rate <- (((data.id.a$Volume[end_day_index] - data.id.a$Volume[1])/data.id.a$Volume[1]) * 100)
      rc.change <- append(rc.change, volume.change.rate)
      mouse.info.a <- data.frame(Arms = Arms[i],
                                 ID = data.id.a$ID[a],
                                 Nearest.Measure.Day.Avail = data.id.a$Times[end_day_index])
      mouse.info <- rbind(mouse.info, mouse.info.a)

    }

    best.response <- min(rc.change)
    avg.response <- mean(rc.change)

    if (best.response < -95 & avg.response < -40) {
      response.level <- "CR"
    } else if (best.response < -50 & avg.response < -20) {
      response.level <- "PR"
    } else if (best.response < 30 & avg.response < 35) {
      response.level <- "SD"
    } else {
      response.level <- "PD"
    }

    response.level.id.i <- data.frame(Arms = Arms[i],
                                      Best.Response = best.response,
                                      Avg.Response = avg.response,
                                      Response.Level = response.level,
                                      #last_measure_day_present = data.id.a$Times[end_day_index], # this is by mouse not by the max available.
                                      stringsAsFactors = F)

    Response.Level <- rbind(Response.Level,response.level.id.i)
  }

  return(list(Response.Level = Response.Level, mouse.info = mouse.info))
}

EFSplot <- function(data, PercChange_EventSize = 100, plot_on = TRUE) {

  if(inherits(data, "data.frame")){
    data<-as.data.frame(data)
  }

  Response.Level <- vector()

  #data <- PDX_interpolate(data)

  data <- data %>%
    dplyr::arrange(ID, Times) %>%
    dplyr::group_by(Tumor, Arms, ID) %>%
    dplyr::mutate(dVt = (((Volume - Volume[1]) / Volume[1] ) * 100),
                  log2.Fold.Change = log2(Volume / Volume[1])) %>%
    dplyr::mutate(Outcome = ifelse(dVt >= PercChange_EventSize, 1, 0)) %>%
    dplyr::slice(ifelse(any(Outcome==1), which(Outcome==1)[1], which(Outcome==0)[n()]))

  ## Make 'Event' flexible to use volume or growth factor change.

 
  data <- droplevels(data)

  data$Arms <- relevel(as.factor(data$Arms), 'Control')

  fit <- survfit(Surv(Times, Outcome) ~ Arms, data = data, error = "greenwood")

  p <- ggsurvplot(fit,
                  data = data,
                  size = 1,                 # change line size
                  palette = colorblind_pallet,
                  pval = TRUE,              # Add p-value
                  risk.table = TRUE,        # Add risk table
                  risk.table.col = "strata",# Risk table color by groups
                  legend.labs = levels(relevel(as.factor(data$Arms), 'Control')),    # Change legend labels
                  xlab = 'Time in Days',
                  risk.table.y.text = TRUE,
                  surv.median.line = "hv",
                  risk.table.height = 0.25,
                  legend.title="Arms",
                  risk.table.title=''
  )

  return(p)
  
}

auc <-
  function(x, y, from = min(x, na.rm=TRUE), to = max(x, na.rm=TRUE), type=c("linear", "spline"), absolutearea=FALSE, subdivisions = 100, ...)
  {
    type <- match.arg(type)
    
    # Sanity checks
    stopifnot(length(x) == length(y))
    stopifnot(!is.na(from))
    
    if (length(unique(x)) < 2)
      return(NA)
    
    if (type=="linear") {
      
      ## Default option
      if (absolutearea==FALSE) {
        values <- approx(x, y, xout = sort(unique(c(from, to, x[x > from & x < to]))), ...)
        res <- 0.5 * sum(diff(values$x) * (values$y[-1] + values$y[-length(values$y)]))
      } else { ## Absolute areas
        ## This is done by adding artificial dummy points on the x axis
        o <- order(x)
        ox <- x[o]
        oy <- y[o]
        
        idx <- which(diff(oy >= 0)!=0)
        newx <- c(x, x[idx] - oy[idx]*(x[idx+1]-x[idx]) / (y[idx+1]-y[idx]))
        newy <- c(y, rep(0, length(idx)))
        values <- approx(newx, newy, xout = sort(unique(c(from, to, newx[newx > from & newx < to]))), ...)
        res <- 0.5 * sum(diff(values$x) * (abs(values$y[-1]) + abs(values$y[-length(values$y)])))
      }
      
    } else { ## If it is not a linear approximation
      if (absolutearea)
        myfunction <- function(z) { abs(splinefun(x, y, method="natural")(z)) }
      else
        myfunction <- splinefun(x, y, method="natural")
      
      
      res <- integrate(myfunction, lower=from, upper=to, subdivisions=subdivisions)$value
    }
    
    res
  }


IndividualMouseReponse <- function(data, last.measure.day = NULL) {

  if(inherits(data, "data.frame")){
    data<-as.data.frame(data)
  }

  Response.Level <- vector()

  for (d in levels(as.factor(data$Tumor))) {

    data.d <- data[data$Tumor == d,]

    Volume <- data.d[,'Volume']
    indata <- subset(data.d, Volume >= 0)

    ID <- unique(indata$ID)

    for ( i in 1:length(ID)){

      data.id.i <- indata[indata$ID == ID[i],]

      if (!is.null(last.measure.day)) {

        end_day_index = match(last.measure.day,data.id.i$Times)

        if (is.na(end_day_index)) {
          end_day_index = which.min(abs(data.id.i$Times - last.measure.day))
        }

        data.id.sub <- data.id.i[end_day_index,]

      } else {
        end_day_index <-length(data.id.i$Volume)
      }

      t.max <- max(data.id.i$Times)

      data.id.i <- data.id.i[order(data.id.i$Times),]

      last.avail.day <- data.id.i[end_day_index,]$Times

      data.id.i <- data.id.i %>%
        dplyr::arrange(ID, Times) %>%
        dplyr::group_by(ID) %>%
        dplyr::mutate(dVt = (((Volume - Volume[1]) / Volume[1] ) * 100),
                      log2.Fold.Change = log2(Volume / Volume[1]))

      data.id.i <- data.id.i %>%
        dplyr::group_by(ID) %>%
        dplyr::mutate(AUC.All.Measures = auc(Times, dVt, type = 'spline') / max(Times))

      data.id.sub <- data.id.i %>%
        dplyr::group_by(ID) %>%
        dplyr::filter(Times <= last.avail.day) %>%
        dplyr::mutate(AUC.Filtered.Measures = auc(Times, dVt, type = 'spline') / max(Times)) %>%
        dplyr::select(ID, Times, AUC.Filtered.Measures) %>%
        dplyr::full_join(data.id.i, by = c('ID', 'Times')) %>%
        dplyr::filter(Times == last.avail.day) %>%
        dplyr::select(c('ID', 'Times',  'Arms', 'Tumor', 'Volume', 'Type', 'dVt', 'log2.Fold.Change', 'AUC.Filtered.Measures', 'AUC.All.Measures'))


      data.id.sub <- as.data.frame(data.id.sub)

      Response.Level <- rbind(Response.Level,data.id.sub)

    }
  }
  Response.Level <- Response.Level %>% dplyr::rename(VC.LastDay = Times)
  return(Response.Level)
}

WaterfallPlot_PDX <- function(data,
                              plot_measure = c('dVt', 'AUC.Filtered.Measures', 'AUC.All.Measures'),
                              caption_text_on = TRUE,
                              plot_on = TRUE, ...) {

  data$Arms <- relevel(as.factor(data$Arms), 'Control')

  data <- data %>%
    dplyr::arrange(Arms) %>%
    dplyr::group_by(Tumor, Arms) %>%
    dplyr::mutate(vc.rate = sort(!!as.name(plot_measure), decreasing = TRUE)) %>%
    dplyr::group_by(Tumor) %>%
    dplyr::mutate(orders = 1:n())

  levels <- levels(relevel(as.factor(data$Arms), 'Control'))

  if (plot_measure == 'dVt') {
    ylab_text <- 'Change in Tumor Volume (%)'
  } else if (plot_measure == 'AUC.Filtered.Measures') {
    ylab_text <- 'AUC (Through Specific Date)'
  } else if (plot_measure == 'AUC.All.Measures') {
    ylab_text <- 'AUC (All Measures)'
  }

  p <- ggplot(data, aes(x = orders, y = vc.rate, fill = as.factor(Arms))) +
    scale_fill_manual(name = "Treatment Arms", limits = levels, values = colorblind_pallet) +
    scale_color_discrete(guide = "none") +
    ylab(ylab_text) +
    xlab('Animals') +
    theme_classic() %+replace%
    theme(axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y  = element_text(hjust = 1,vjust = 0.5,size = 12),
          axis.title.y = element_text(face = "bold",angle = 90,size = 12)) +
    coord_cartesian(ylim = c(NA,max(data$vc.rate)))

  p <- p + geom_abline(intercept = 0, slope = 0,size = 0.5,colour = 'black') +
    geom_bar(stat = "identity", width = 0.8, position = position_dodge(width = 0.8)) #+
  #geom_abline(intercept = 50, slope = 0, size = 0.5, colour = 'black', linetype="dotdash") +
  #geom_abline(intercept = 90, slope = 0, size = 0.5, colour = 'black', linetype="dashed")

  p <- p + theme(legend.background = element_rect(fill = 'white', colour = 'black'),
                 legend.title = element_text( size = 12, face = "bold"),
                 legend.text = element_text( size = 12))

  # + theme(legend.position = c(0.95,0.95),legend.justification = c(0.95,0.95))

  if(length(levels(as.factor(data$Tumor))) > 1) {
    p <- p + facet_wrap(~ Tumor,dir = 'v')
  }

  if(caption_text_on){
    caption_text = paste(unique(paste0(data$Arms, ': date of measure ', data$VC.LastDay)), collapse = ', ')
    p <- p + labs(caption = caption_text)
  }

  if (plot_on) {
    plot(p)
  } else {
    return(p)
  }
}



### CHANGE PLOT TYPE NAME TO BE MORE INTUATIVE

### NEED TO INCLUDE T/C PLOTS.

### NEED TO INCLUDE ANOVA STATS OR OTHER.

### WHY ARE TV STATS PLOTS TRUNCATED AT 30 DAYS? 'DOC 20' is a good example to check 

### NEED TO INCLUDE HYBRID PLOTS. THIS SHOULD BE ANOTHER TAB IN THE 'Matched Tumor Volume - Visual Analytics'

### NEED TO INCLUDE LOG2 FOLD PLOTS. THIS SHOULD BE ANOTHER TAB IN THE 'Matched Tumor Volume - Visual Analytics'

### NEED TO INCLUDE 'SEMI-LOG' BUTTON.
