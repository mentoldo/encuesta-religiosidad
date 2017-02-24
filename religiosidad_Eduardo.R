religiosidad <- read.delim("religiosidad.txt", fileEncoding = "latin1")
names(religiosidad)

## corregir niveles
religiosidad$Categoría.CONICET=factor(religiosidad$Categoría.CONICET,
                   levels(religiosidad$Categoría.CONICET) [c(2,1,3,4,5)])
religiosidad$X.Cree.Usted.en.Dios=factor(religiosidad$X.Cree.Usted.en.Dios,
                                      levels(religiosidad$X.Cree.Usted.en.Dios) [c(3,2,1)])
religiosidad$Categoría.de.incentivo.docente=factor(religiosidad$Categoría.de.incentivo.docente,
                                         levels(religiosidad$Categoría.de.incentivo.docente) [c(5,4,3,2,1)])
religiosidad$Según.su.opinión.personal...cuál.es=factor(religiosidad$Según.su.opinión.personal...cuál.es,
                                                   levels(religiosidad$Según.su.opinión.personal...cuál.es) [c(2,3,5,4,1)])

cree=table(religiosidad$X.Cree.Usted.en.Dios)
cree
cree1=as.data.frame(cree)
cree.en.dios=c("si","no","duda/no esta seguro")
casos=c(188,135,46)
porcentaje=c(50.95,36.58,12.47)
para.grafico.1=as.data.frame(cbind(cree.en.dios,casos,porcentaje))
levels(para.grafico.1$cree.en.dios)
para.grafico.1$cree.en.dios=factor(para.grafico.1$cree.en.dios,
                                         levels(para.grafico.1$cree.en.dios) [c(3,2,1)])

library(ggplot2)
library(ggthemes)
(grafico.1=ggplot(para.grafico.1,aes(x=cree.en.dios,y=porcentaje))+
    geom_bar(fill="skyblue",stat="identity")+theme_tufte())+xlab("Cree usted en Dios?")

milagros=table(religiosidad$Respecto.de.los.milagros....)
milagros
porcentaje.milagros=round(100*prop.table(milagros),2)
para.grafico.2=as.data.frame(porcentaje.milagros)
para.grafico.2
names(para.grafico.2)=c("milagros...", "porcentaje")
(grafico.2=ggplot(para.grafico.2)+geom_bar(aes(x=milagros...,y=porcentaje),
                                          fill="skyblue",stat="identity")+
  theme_tufte()+xlab("Respecto de los milagros..."))
  



tabla=table(religiosidad$X.Cree.Usted.en.Dios,religiosidad$Categoría.CONICET)
prop.table(tabla,2)#baja la proporcion de creyentes con la categoria
table(religiosidad$Según.su.opinión.personal...cuál.es)
table(religiosidad$Categoría.de.incentivo.docente)
table(religiosidad$Categoría.CONICET)


(cree.dios=ggplot(subset(religiosidad,!is.na(X.Cree.Usted.en.Dios)))+
  geom_bar(aes(X.Cree.Usted.en.Dios),fill="skyblue")+xlab("Cree usted en Dios")+ylab("casos")+
geom_text(stat='count',aes(x=X.Cree.Usted.en.Dios,label=..count..),
            vjust=-1)+theme_tufte())

(cree.milagros=ggplot(subset(religiosidad,!is.na(Respecto.de.los.milagros....) & !is.na(Sexo)))+
    geom_bar(aes(Respecto.de.los.milagros....),fill="skyblue")+
    xlab("Respecto de los milagros")+ylab("casos")+
    geom_text(stat='count',aes(x=Respecto.de.los.milagros....,label=..count..),
              vjust=-1)+theme_tufte())

(cree.segun.cat.incentivos=ggplot(subset(religiosidad,!is.na(X.Cree.Usted.en.Dios)))+
geom_bar(aes(Categoría.de.incentivo.docente,fill=X.Cree.Usted.en.Dios.),
                 position="fill")+xlab("Categoría en el programa de incentivos docentes")+
    ylab("Porcentaje en cada categoría")+
    scale_fill_manual(name="¿Cree usted\n en Dios?",breaks=c("Si","No","Duda/No está seguro"),values=rev(grey.colors(3)))+
    theme_tufte())

(cree.segun.sexo=ggplot(subset(religiosidad,!is.na(X.Cree.Usted.en.Dios)&!is.na(Sexo)))+
    geom_bar(aes(Sexo,fill=X.Cree.Usted.en.Dios.),
             position="fill")+xlab("Sexo")+
    ylab("Porcentaje en cada categoría")+
    scale_fill_manual(name="¿Cree usted\n en Dios?",breaks=c("Si","No","Duda/No está seguro"),values=rev(grey.colors(3)))+
    theme_tufte())

(cree.segun.cat.conicet=ggplot(subset(religiosidad,!is.na(X.Cree.Usted.en.Dios) & !is.na(Categoría.CONICET)))+
    geom_bar(aes(Categoría.CONICET,fill=X.Cree.Usted.en.Dios.),
             position="fill")+xlab("Categoría CONICET")+
    ylab("Porcentaje en cada categoría")+
    scale_fill_manual(name="¿Cree usted\n en Dios?",breaks=c("Si","No","Duda/No está seguro"),values=rev(grey.colors(3)))+
    theme_tufte())


opinion=table(religiosidad$Según.su.opinión.personal...cuál.es)
(para.grafico.3=round(100*prop.table(opinion),2))
(para.grafico.3=as.data.frame(para.grafico.3))
(grafico.3=ggplot(para.grafico.3)+geom_bar(aes(x=Var1,y=Freq),stat="identity",
                                              fill="skyblue")+
  xlab("Según su opinión personal, ¿cuál es la relación\n entre el conocimiento científico y la fe?")
+ylab("porcentaje")+theme_tufte())

(grafico.3=ggplot(para.grafico.3)+geom_bar(aes(x=Var1,y=Freq),stat="identity",
                                           fill="skyblue")+
    xlab("Según su opinión personal, ¿cuál es la relación\n entre el conocimiento científico y la fe?")
  +ylab("porcentaje")+theme_tufte())

(relacion.ciencia.fe=ggplot(subset(religiosidad,!is.na(Según.su.opinión.personal...cuál.es)))+
    geom_bar(aes(Según.su.opinión.personal...cuál.es),fill="skyblue")+
  xlab("Según su opinión personal, ¿cuál es la relación entre el conocimiento científico y la fe?")
  +ylab("casos")+theme_tufte()+theme(axis.title.x = element_text(size=20)))
  


  relacion.ciencia.fe.sexo=ggplot(subset(religiosidad,
                          !is.na(Según.su.opinión.personal...cuál.es)&!is.na(Sexo)))+
    geom_bar(aes(Sexo,fill=Según.su.opinión.personal...cuál.es), position="fill")

relacion.ciencia.fe.sexo+scale_fill_manual(name="",
                                           values = grey.colors(5))+
  ylab("proporción")+ggtitle("Según su opinión personal, ¿Cuál es la relación entre\n el conocimiento científico y la fe?")+ theme_tufte()+
  theme(plot.title = element_text(size=40))
  
  
  (relacion.ciencia.fe.segun.cat.incentivos=ggplot(subset(religiosidad,!is.na(Según.su.opinión.personal...cuál.es)))+
    geom_bar(aes(Categoría.de.incentivo.docente,fill=Según.su.opinión.personal...cuál.es),
             position="fill")+xlab("Categoría en el programa de incentivos docentes")+
    ylab("Porcentaje en cada categoría")+
      scale_fill_manual(name="",
                        values = grey.colors(5))+
      ggtitle("Según su opinión personal, ¿Cuál es la relación entre\n el conocimiento científico y la fe?")+ theme_tufte()+
      theme(plot.title = element_text(size=40)))


##algo con la edad, que se ocurre???
table(religiosidad$Edad)
(edades=ggplot(religiosidad)+geom_histogram(aes(Edad)))
religiosidad$Edad[72]=88
religiosidad$Edad[408]=37

library(Hmisc)
religiosidad$edad3=cut2(religiosidad$Edad,c(40,50,60))
(cree.por.edad=ggplot(subset(religiosidad, !is.na(X.Cree.Usted.en.Dios)))+
    geom_bar(aes(edad3,fill=X.Cree.Usted.en.Dios),position="fill")+
    xlab("Grupos de edad")+ylab("Proporción en cada categoría")+
    scale_fill_manual(name="¿Cree usted\n en Dios?",breaks=c("Si","No","Duda/No está seguro"),
                      values=grey.colors(3))+
    theme_tufte())
### P10
table(religiosidad$Por.favor..elija.de.las.siguientes)
################

religiosidad$basicas=religiosidad$Área.de.desempeño.académico..Cienci
religiosidad$naturales=religiosidad$Área.de.desempeño.académico..Cienc1
religiosidad$aplicadas=religiosidad$Área.de.desempeño.académico..Cienc2
religiosidad$salud=religiosidad$Área.de.desempeño.académico..Cienc3
religiosidad$sociales=religiosidad$Área.de.desempeño.académico..Cienc4
religiosidad$humanas=religiosidad$Área.de.desempeño.académico..Cienc5

##prueba que esté todo OK
table(religiosidad$basicas,religiosidad$Área.de.desempeño.académico..Cienci)
table(religiosidad$naturales,religiosidad$Área.de.desempeño.académico..Cienc1)
table(religiosidad$aplicadas,religiosidad$Área.de.desempeño.académico..Cienc2)
table(religiosidad$salud,religiosidad$Área.de.desempeño.académico..Cienc3)
table(religiosidad$sociales,religiosidad$Área.de.desempeño.académico..Cienc4)
table(religiosidad$humanas,religiosidad$Área.de.desempeño.académico..Cienc5)

##los NA de cada una son ceros
levels(religiosidad$basicas)=c(0,0,1)
levels(religiosidad$naturales)=c(0,0,1)
levels(religiosidad$aplicadas)=c(0,0,1)
levels(religiosidad$salud)=c(0,0,1)
levels(religiosidad$sociales)=c(0,0,1)
levels(religiosidad$humanas)=c(0,0,1)

##construyo la nueva variable
religiosidad$area=names(religiosidad[35:40])[max.col(religiosidad[35:40])]

##defino NA de area como los que tienen cero (o NA) en todas
religiosidad$area[religiosidad$basicas==0 & religiosidad$naturales==0 &
                  religiosidad$aplicadas==0 &
                    religiosidad$salud==0 & religiosidad$sociales==0 &
                    religiosidad$humanas==0]=NA

table(religiosidad$area)## quedaron 370 válidos}


(cree.segun.area=ggplot(subset(religiosidad,!is.na(X.Cree.Usted.en.Dios)&!is.na(area)))+
    geom_bar(aes(area,fill=X.Cree.Usted.en.Dios),
             position="fill")+xlab("Área de desempeño académico")+
    ylab("Porcentaje en cada categoría")+
    scale_fill_discrete(name="¿Cree usted\n en Dios?",breaks=c("Si","No","Duda/No está seguro")))


(relacion.ciencia.fe.segun.area=ggplot(subset(religiosidad,!is.na(Según.su.opinión.personal...cuál.es) & !is.na(area)))+
    geom_bar(aes(area,fill=Según.su.opinión.personal...cuál.es),
             position="fill")+xlab("Área de desempeño académico")+
    ylab("Porcentaje en cada categoría")+
    scale_fill_discrete(name="¿Cómo se relaciona el\nconocimiento científico\n y la fe?",
                        breaks=c("Se complementan","Se contradicen","Son independientes",
                                 "Se potencian","Otro")))
##aca falta camiar de color, para que se distinga mejor

(ciencia.fe.area=ggplot(subset(religiosidad,!is.na(Según.su.opinión.personal...cuál.es) & !is.na(area)))+
  geom_bar(aes(area,fill=Según.su.opinión.personal...cuál.es),
           position="fill")+xlab("Área de desempeño académico")+
  ylab("Proporción en cada categoría") + scale_fill_manual(name="¿Cómo se relaciona el\nconocimiento científico\n y la fe?",
                   values = c("Se complementan"="pink","Se contradicen"="orange",
                                                              "Son independientes"="skyblue","Se potencian"="green","Otro"="white")))


(ciencia.fe.area=ggplot(subset(religiosidad,!is.na(Según.su.opinión.personal...cuál.es) & !is.na(area)))+
    geom_bar(aes(area,fill=Según.su.opinión.personal...cuál.es),
             position="fill")+xlab("Área de desempeño académico")+
    ylab("Proporción en cada categoría") + scale_fill_manual(name="",
                                                             values = grey.colors(5))+ggtitle("Según su opinión personal, ¿Cuál es la relación entre\n el conocimiento científico y la fe?")+ theme_tufte()+
    theme(plot.title = element_text(size=40)))






cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
scale_fill_manual(values=cdbPalette)
##en https://www.r-bloggers.com/choosing-colour-palettes-part-i-introduction/
##estan las paleta básicas
##desastroso, no encuentro modo de poner los % en las barras

##se pueden mejorarlos colores: una ràpida revisada a las recomendaciones sobre combinaciones

ciencia.fe.por.area=table(religiosidad$area,religiosidad$Según.su.opinión.personal...cuál.es)
porcentajes=as.data.frame(prop.table(ciencia.fe.por.area,1))
ciencia.fe.area+geom_text(aes(x = Var1, y = Freq,label = round(Freq, 2)),
                          data = porcentajes) 


(cree.dios.area=ggplot(subset(religiosidad,!is.na(X.Cree.Usted.en.Dios) & !is.na(area)))+
    geom_bar(aes(area,fill=X.Cree.Usted.en.Dios),
             position="fill")+xlab("Área de desempeño académico")+
    ylab("Proporción en cada categoría") + scale_fill_manual(name="¿Cree usted en Dios?",
                                                             values = grey.colors(3))+
    theme_tufte())
cree.dios.area
