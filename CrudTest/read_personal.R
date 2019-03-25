# Reads a File from the "Stellen nach Finanzierungsart" export

read_personal <- function(){

  #get our people
  personen <- read_csv("data/STAMM_personalnummern.csv", col_types = "ccc")
  #import file
  employee_data <- read_csv2("data/WHRP_008_SPFP.csv", skip = 5)
  here::here()

  #names(employee_data)
  df <- employee_data %>% 
    inner_join(personen) 
  
  names(df) <- c("Planstelle_Stamm", "Planstelle_Stamm2", "Personalnummer", "Vorname",
                 "Nachname", "Tarifgruppe", "Planstelle_Begin", "Planstelle_Ende", "Zuordnungsbeginn",
                 "Zuordnungsende", "Planstelle", "Planstelle2", "PSP-Element", "PSP-Element2", "Planstelle_Tarif",
                 "Finanzierung_Stunden", "Prozentsatz")
  df <- df %>% 
    map_at(dmy, .at = c("Planstelle_Begin", "Planstelle_Ende", "Zuordnungsbeginn", "Zuordnungsende")) %>% 
    as_tibble() %>% 
    mutate(`Zuordnungsbeginn` = as_date(ifelse(year(`Zuordnungsbeginn`)==9999, dmy("31.12.2032"),`Zuordnungsbeginn`))) %>% 
    mutate(`Zuordnungsende` = as_date(ifelse(year(`Zuordnungsende`)==9999, dmy("31.12.2032"),`Zuordnungsende`))) %>% 
    mutate(Prozentsatz = as.numeric(Prozentsatz))
  df
}


plot_personal <- function(df, date_range, filter_lastname = ""){
  p <- df %>% 
    filter(Nachname %in% filter_lastname) %>% 
    ggplot() +
    aes(x=`Zuordnungsbeginn`, 
        xend=`Zuordnungsende`, 
        y=Nachname, 
        yend=Nachname, 
        color=Nachname,
        alpha = Finanzierung_Stunden/40/100) +
    scale_x_date(date_breaks = "1 year" , date_labels = "%y") +
    geom_segment(size=2, lineend = "butt" ) +
    geom_point(size=5, shape=5) +
    geom_point(size=5, aes(x=`Zuordnungsende`), shape=3) +
    geom_vline(xintercept = today(), color="lightgreen") +
    coord_cartesian(clip="off", xlim = c(date_range[1], date_range[2])) +
    guides(color=FALSE, alpha=FALSE)
  p
}

if(interactive()){
  plot_personal(df, c(ymd("2016-1-1"),ymd("2022-1-1")), c("Calero Valdez","Ziefle"))
}

df %>% filter(Nachname %in% c("Calero Valdez","Ziefle")) %>% select(Nachname, Prozentsatz)
