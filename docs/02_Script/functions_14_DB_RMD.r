
is_na_null <-
function(x) {
  y=case_when(x %>% is.na()~0,
              x %>% is.nan()~0,
              x=="Inf"~0,
              T~x)
  return(y)
}

left_join_UserID <-
function(Data_Main, Data_Join){
  
  
  Data_Main %>% 
  left_join(
    
    dplyr::select(Data_Join, -c(Erfasser, Abteilung)), by=c("UserID"="UserID")
  )
}

ist_end <-
function(date){
  if(today() %>% year()==date %>% year() && today() %>% month() == date %>% month()){
  (today()-days(1)) %>% ymd()
} else 
  date %>% ymd()
  
}



plot_schachtel <-
function(Data){
   #p <- 
     Data %>%  
      plot_ly(
        type="treemap", 
        values=~Anzahl,
        parents=~parent,
        labels=~Schachtel, 
        domain=list(column=0),
        textinfo="label+value+percent parent"
      )
   #ggplotly(p)
  }


plot_bar_overlay_group <-
function(Data, group){
  Data %>% 
  plot_ly( y = {{group}}, orientation = 'h') %>%
  
        add_trace(
        x = ~Plan, name = 'Plan',  type = 'bar' ,width=0.8,
        marker = list(color = 'rgb(255,255,255)',
                      line = list(color = 'rgba(0,126,211, 1.0)', 
                                  width = 3))
      )  %>%
      
      add_trace(
        x = ~Forecast, name = 'Forecast',   type = 'bar' , width= 0.8,
        marker = list(color = 'rgba(226,72,73, 0.6)',
                      line = list(color = 'rgba(226,72,73, 1.0)',
                                  width = 3))

      ) %>%

      add_trace(
        x = ~IST, type = 'bar', name = 'IST',width= 0.65,
        marker = list(color =  'rgba(77, 189, 171,0.8)' , #0,255,0
                      line = list(color = 'rgba(77, 189, 171 ,1.0)', #0,255,0
                                  width = 3))

      ) %>%
      layout(barmode = 'overlay',
             xaxis = list(title = ""),
             yaxis = list(title =""),
             legend = list(orientation = "h", x = 0.4, y = -0.2))
}

