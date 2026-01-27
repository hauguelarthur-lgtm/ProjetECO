setwd("C:/Users/boufarsi/Documents/thèse/Enseignement/Geomarketing/New/")
pacman::p_load(dplyr, ggplot2, sf,readr,readxl,tmap,tmaptools,opentripplanner)
iris_shp<-st_read("georef-france-iris-millesime.shp")
sirene_shp<-st_read("dataseed-sirene-2.shp")
iris_data <- read_excel("iris_isere.xlsx")

iris_metro<-iris_shp%>%filter(com_name%in%c("Grenoble","Meylan","La Tronche","Corenc","Saint-Martin-le-Vinoux","Saint-�gr�ve","Sassenage",
                                               "Fontaine","Seyssinet-Pariset","Seyssins","�chirolles","Eybens","Saint-Martin-d'H�res","Gi�res"))

sirene_metro<-sirene_shp%>%filter(libellecomm%in%c("GRENOBLE","MEYLAN","LA TRONCHE", "CORENC","SAINT-MARTIN-LE-VINOUX","SAINT-EGREVE",
                                      "SASSENAGE","FONTAINE","SEYSSINET-PARISET","SEYSSINS","ECHIROLLES","EYBENS","SAINT MARTIN D'HERES","GIERES")&etatadminis=="Actif")

iris_metro$groupes<-ifelse(iris_metro$com_name%in%c("Meylan","La Tronche","Corenc","Saint-Martin-le-Vinoux","Saint-�gr�ve"),"Nord",
                           ifelse(iris_metro$com_name%in%c("Sassenage","Fontaine","Seyssinet-Pariset","Seyssins"),"Ouest",
                                  ifelse(iris_metro$com_name%in%c("�chirolles","Eybens","Saint-Martin-d'H�res","Gi�res"),"Sud-Est","Grenoble")))

tmap_mode("view")
map<-tm_shape(iris_metro) +
  tm_polygons(col="groupes", alpha=0.5,
              style="pretty", id="com_name",
              title="Groupes")
 

tmap_save(map, "Group_Report.html")
