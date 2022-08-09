
number_ticks <- function(n) 
{function(limits) pretty(limits, n)}

scaleFUN <- function(x)  {
  sprintf("%.2f", x) }

compPlot <- function(x = "propVacant") {
  
  major <- input %>%
    select(RC_ID, x) %>%
    distinct() 
  major <- major[order(major[, 2]),]
  major <- major[962:1068, ]
  
  minor <- input %>%
    select(RC_ID, x) %>%
    distinct() 
  minor <- minor[order(minor[, 2]),]
  minor <- minor[1:106, ]
  
  datMajor <- input %>%
    select(RC_ID, Date, Ai, x) %>%
    filter(RC_ID %in% major$RC_ID)
  datMinor <- input %>%
    select(RC_ID, Date, Ai, x) %>%
    filter(RC_ID %in% minor$RC_ID)
  
  f1 <- datMajor %>%
    ggplot(aes(x = as.Date(Date), y = Ai)) +
    geom_smooth(inherit.aes = T, color = "black") +
    scale_y_continuous(labels = scaleFUN, n.breaks = 4) +
    ylab(bquote("A"["it"])) +
    xlab("Date") +
    labs(title = "i)", subtitle = paste0("Top 10% retail centres with highest", " ", x)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    geom_vline(xintercept = as.Date("2021-11-27"), color = "red", lwd = 2) +
    geom_vline(xintercept = as.Date("2022-02-14"), color = "red", lwd = 2) +
    geom_vline(xintercept = as.Date("2022-05-20"), color = "red", lwd = 2) +
    theme_bw() +
    theme(text = element_text(family = "Times"),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold"))
  
  f2 <- datMinor %>%
    ggplot(aes(x = as.Date(Date), y = Ai)) +
    geom_smooth(inherit.aes = T, color = "black") +
    scale_y_continuous(labels = scaleFUN, n.breaks = 4) +
    ylab(bquote("A"["it"])) +
    xlab("Date") +
    labs(title = "ii)", subtitle = paste0("Bottom 10% retail centres with lowest", " ", x)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    geom_vline(xintercept = as.Date("2021-11-27"), color = "red", lwd = 2) +
    geom_vline(xintercept = as.Date("2022-02-14"), color = "red", lwd = 2) +
    geom_vline(xintercept = as.Date("2022-05-20"), color = "red", lwd = 2) +
    theme_bw() +
    theme(text = element_text(family = "Times"),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold"))
  
   r <- ggarrange(f1, f2, ncol = 2)
   r
   
}

getMajor <- function(x) {
  
  major <- input %>%
    select(RC_ID, x) %>%
    distinct() 
  major <- major[order(major[, 2]),]
  major <- major[962:1068, ]
  
  datMajor <- input %>%
    select(RC_ID, Date, Ai, x) %>%
    filter(RC_ID %in% major$RC_ID)
  return(datMajor)
  
}

getMinor <- function(x) {
  
  
  minor <- input %>%
    select(RC_ID, x) %>%
    distinct() 
  minor <- minor[order(minor[, 2]),]
  minor <- minor[1:106, ]
  
  datMinor <- input %>%
    select(RC_ID, Date, Ai, x) %>%
    filter(RC_ID %in% minor$RC_ID)
  return(datMinor)
}