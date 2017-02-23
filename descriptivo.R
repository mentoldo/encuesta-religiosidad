classes <- c(ID = "integer", fechaenvio = "POSIXct", ultima_pagina = "factor", leng_inicial= "factor",
             contrasenna = "character", facultad = "character", cat_inc_doc = "factor", cat_Conicet = "factor",
             desemp_cs_basicas = "factor", desemp_cs_nat = "factor", desemp_cs_aplicadas = "factor", desemp_cs_salud = "factor", 
             desemp_cs_sociales= "factor", desemp_cs_humanas = "factor",
             sexo = "factor", edad = "integer", cree_en_dios = "factor", respecto_milagros = "factor", relacion_cs_fe = "factor",
             cual_es2 = "character", frases = "factor", se_considera = "factor", relacion_creencia_trebajo = "factor", practica_religion = "factor",
             que_religion = "factor", otra_religion = "character", frec_misas = "factor",
             frec_orientacion = "factor", frecuencia_lee = "factor", frecuencia_reza = "factor", 
             frec_participa = "factor", comentario = "character")

religiosidad <- read.delim("religiosidad.txt", fileEncoding = "latin1", colClasses = unname(classes))
religiosidad <- religiosidad[-447,]
names(religiosidad) <- names(classes)
str(religiosidad)
summary(religiosidad)

# Clean an tidy data.frame
religiosidad$edad[religiosidad$edad > 100] <- NA
religiosidad$se_considera <- factor(religiosidad$se_considera,
                                    levels = c("Nada religiosa", "Poco religiosa", "Algo religiosa", "Bastante religiosa", "Muy religiosa"),
                                    ordered = TRUE)
religiosidad$respecto_milagros <- factor(religiosidad$respecto_milagros,
                                         levels = c("No cree definitivamente", "Ha creído alguna vez", "Cree en ellos"),
                                         ordered = TRUE)

names_frec <- c("Nunca", "Aproximadamente una vez cada año", "Aproximadamente una vez cada mes", "Aproximadamente una vez cada semana", "Aproximadamente una vez cada día")
religiosidad$frec_misas <- ordered(religiosidad$frec_misas, levels = names_frec)
religiosidad$frec_orientacion <- ordered(religiosidad$frec_orientacion, levels = names_frec)
religiosidad$frec_participa <- ordered(religiosidad$frec_participa, levels = names_frec)
religiosidad$frecuencia_lee <- ordered(religiosidad$frecuencia_lee, levels = names_frec)
religiosidad$frecuencia_reza <- ordered(religiosidad$frecuencia_reza, levels = names_frec)

# Descriptivo
summary(religiosidad)
resumen_tables <- apply(religiosidad, 2, table)
str(resumen_tables)

barplot(resumen_tables$sexo)
hist(religiosidad$edad)

barplot(resumen_tables$cree_en_dios)
barplot(table(religiosidad$respecto_milagros))
barplot(resumen_tables$relacion_cs_fe)

texto_frases <- c("La vida en la tierra, incluyendo la vida humana, fue creada por un Dios y siempre ha existido como la conocemos ahora",
                  "La vida en la tierra, incluyendo la vida humana, evolucionó a lo largo del tiempo en un proceso guiado por un Dios", 
                  "La vida en la tierra, incluyendo la vida humana, evolucionó a lo largo del tiempo como resultado de selección natural, en el que ningún Diós participó",
                  "Tiene una concepción del origen de las especies y el desarrollo de la vida en la tierra que no está incluido entre los anteriores", 
                  "La vida en la tierra, incluyendo la vida humana, evolucionó a lo largo del tiempo en un proceso en el que Dios intervino directamente",
                  "No sabe / no tiene posición tomada")
resumen_frases <- c("Creada por Dios\n, siempre =", "Evolución\n guiada \npor Dios", "Evolución \nselec. natural", "Otra \nconcepción", "Evolución \nintrevenida \npor dios", "NS/NC" )

referencia_frases <- cbind(names(resumen_tables$frases), texto_frases, resumen_frases)
barplot(resumen_tables$frases, names = resumen_frases)

barplot(table(religiosidad$se_considera))

barplot(table(religiosidad$relacion_creencia_trebajo))

barplot(table(religiosidad$practica_religion))

barplot(table(religiosidad$que_religion))

# Frecuencias
cat(levels(religiosidad$frec_misas))
names_frec_show <- c("Nunca", "Aproximadamente\nuna vez\n cada año", "Aproximadamente\n una vez\n cada mes", "Aproximadamente\n una vez\n cada semana", "Aproximadamente\n una vez \ncada día")
barplot(table(religiosidad$frec_misas), names = names_frec_show)
barplot(table(religiosidad$frec_orientacion), names = names_frec_show)
barplot(table(religiosidad$frecuencia_lee), names = names_frec_show)
barplot(table(religiosidad$frec_participa), names = names_frec_show)
barplot(table(religiosidad$frecuencia_reza), names = names_frec_show)


# Cruce de variables

barplot(table(religiosidad$sexo, religiosidad$cree_en_dios),
        beside = TRUE, legend.text = TRUE, 
        args.legend = list(x ='topright')
)

barplot(table(religiosidad$desemp_cs_basicas, religiosidad$respecto_milagros),
        beside = TRUE, legend.text = TRUE)
barplot(prop.table(table(religiosidad$desemp_cs_basicas, religiosidad$respecto_milagros), 1),
                                                  beside = TRUE, legend.text = TRUE
)

barplot(table(religiosidad$desemp_cs_aplicadas, religiosidad$respecto_milagros),
        beside = TRUE, legend.text = TRUE)
barplot(prop.table(table(religiosidad$desemp_cs_aplicadas, religiosidad$respecto_milagros), 1),
        beside = TRUE, legend.text = TRUE
)

barplot(table(religiosidad$desemp_cs_humanas, religiosidad$respecto_milagros),
        beside = TRUE, legend.text = TRUE)
barplot(prop.table(table(religiosidad$desemp_cs_humanas, religiosidad$respecto_milagros), 1),
        beside = TRUE, legend.text = TRUE
)

barplot(table(religiosidad$desemp_cs_nat, religiosidad$respecto_milagros),
        beside = TRUE, legend.text = TRUE)
barplot(prop.table(table(religiosidad$desemp_cs_nat, religiosidad$respecto_milagros), 1),
        beside = TRUE, legend.text = TRUE
)

barplot(table(religiosidad$desemp_cs_salud, religiosidad$respecto_milagros),
        beside = TRUE, legend.text = TRUE)
barplot(prop.table(table(religiosidad$desemp_cs_salud, religiosidad$respecto_milagros), 1),
        beside = TRUE, legend.text = TRUE
)

barplot(table(religiosidad$desemp_cs_sociales, religiosidad$respecto_milagros),
        beside = TRUE, legend.text = TRUE)
barplot(prop.table(table(religiosidad$desemp_cs_sociales, religiosidad$respecto_milagros), 1),
        beside = TRUE, legend.text = TRUE
)




# Cruce con edad
library(plyr)
resumen_sexo <- ddply (religiosidad, "sexo", summarise,
                      MEDIA = round (mean(edad, na.rm = TRUE), 2),
                      SD = round (sd(edad, na.rm = TRUE),2),
                      N = length(edad),
                      SE = round (sd(edad, na.rm = TRUE)/sqrt(length(edad)), 2)
)

resumen_cree_en_dios <- ddply (religiosidad, "cree_en_dios", summarise,
                               MEDIA = round (mean(edad, na.rm = TRUE), 2),
                               SD = round (sd(edad, na.rm = TRUE),2),
                               N = length(edad),
                               SE = round (sd(edad, na.rm = TRUE)/sqrt(length(edad)), 2)
)

library(ggplot2)
ggplot(resumen_cree_en_dios, aes(x=cree_en_dios, y=MEDIA)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=MEDIA-SE, ymax=MEDIA+SE),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))
   
resumen_respecto_milagros <- ddply (religiosidad, c("respecto_milagros", "sexo"), summarise,
                                   MEDIA = round (mean(edad, na.rm = TRUE), 2),
                                   SD = round (sd(edad, na.rm = TRUE),2),
                                   N = length(edad),
                                   SE = round (sd(edad, na.rm = TRUE)/sqrt(length(edad)), 2)
)

                                   
ggplot(resumen_respecto_milagros, aes(x=respecto_milagros, y=MEDIA, fill=sexo)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=MEDIA-SE, ymax=MEDIA+SE),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) 


