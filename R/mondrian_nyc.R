
#' mondrian_nyc
#'
#' @param df dataframe
#' @import tidyverse
#' @return df
#' @export
#'
#' @examples
#' df=data.frame(x=sample(18,5),y=sample(18,5))
#' mondrian_nyc(df)
mondrian_nyc <- function(df) {

  pal <- c("#255293","#db0a16","#f8c72d")

  df_add = data.frame(z=runif(6,0,max(df$x))) %>%
    mutate(color = sample(3, nrow(.), replace=T))
  #find where line crosses in order to color them
  df_cross <- expand.grid(x=df$x, y=df$y)
  #find small squares
  find_small_squares <- function(df, n) {
    df_x <- df %>%
      arrange(x) %>%
      mutate(x_lead=lead(x,1), x_gap = x_lead-x) %>%
      arrange(x_gap) %>%
      filter(x_gap!=0) %>%
      head(n) %>%
      select(x, x_lead)

    df_y <- df %>%
      arrange(y) %>%
      mutate(y_lead=lead(y,1), y_gap = y_lead-y) %>%
      arrange(y_gap) %>%
      filter(y_gap!=0) %>%
      head(n) %>%
      select(y, y_lead)

    df_rect <- df_x %>%
      cbind(df_y) %>%
      mutate(color = sample(3, nrow(.), replace=T))

    #to ensure no NA datapoint in df_rect, need to make sure n<nrow(df)
    return(df_rect)
  }
  df_rect <- find_small_squares(df, 3)
  set.seed(1023)
  df %>%
    ggplot()+
    geom_vline(xintercept = df$x, size=5, color='#f8c72d')+
    geom_hline(yintercept = df$y, size=5, color='#f8c72d') +
    geom_segment(data=df_add, aes(y=df_add$z, yend=df_add$z,
                                  x=sample(df$x,nrow(df_add), replace=T),
                                  xend=sample(df$x,nrow(df_add), replace=T)),
                 size=5, color='#f8c72d') +
    geom_rect(data = df_rect, aes(xmin=x +.2,
                                  xmax=x_lead -.2,
                                  ymin=y + .065,
                                  ymax=y_lead - .065,
                                  fill=as.factor(color))) +

    geom_rect(data = df_rect, aes(xmin=x +.045 + (x_lead-x)/3,
                                  xmax=x_lead -.045 - (x_lead-x)/3,
                                  ymin=y + .065 + (y_lead-y)/3,
                                  ymax=y_lead - .065 - (y_lead-y)/3,
                                  fill=as.factor(lead(color,1)))) +
    geom_point(data=df_cross,aes(x=df_cross$x ,y=df_cross$y,
                                 color = as.factor(sample(3, nrow(df_cross), replace=T))), shape=15, size=4)+
    geom_point(data=df_add, aes(x=sample(df$x,nrow(df_add), replace=T),y=df_add$z,
                                color=as.factor(color)), shape=15, size=4) +
    geom_point(data=df_add, aes(y=sample(df$y,nrow(df_add), replace=T),x=df_add$z,
                                color=as.factor(color)), shape=15, size=4) +
    scale_fill_manual(values=pal) +
    scale_color_manual(values=pal) +
    theme_minimal() +
    theme(axis.title.y = element_blank(),axis.title.x = element_blank(),
          plot.title = element_text(face="bold", size=16),
          axis.text = element_blank(),
          plot.background = element_rect(fill = 'ghostwhite',color='white'),
          panel.grid = element_blank(),
          legend.position = 'None', legend.title = element_blank())
}
