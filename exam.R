#XYLELLA FASTIDIOSA IN SALENTO: ANALYSIS OF AN ECOLOGICAL COLLAPSE

##########INTRO##########

#recall all the packages needed for the project
library(terra) #for the rast and crop functions
library(viridis)#to have a color ramp palette inclusive for color blind people
library(imageRy)#for the function im.classify
library(ggplot2)#for plotting the data in a proper way

#to set our working directory from where R will take the data
setwd("C:/Users/lenovo/Desktop/esame_salento")


#to set the color ramp palette inclusive for color blind people
cl<-colorRampPalette(viridis(7))(255) 


##########PART 1: NDVI CALCULATION AND COMPARISON##########

#let's import data OF 2020 from copernicus browser data coming from sentinel-2
#we have 3 bands inside our first file (false colour combination)
#band 1=NIR [B8] (near infrared)
#band 2=RED [B4]
#band 3=GREEN [B3] (non serve per ndvi)
list.files()
im_2020<-rast("Salento_2020_False_color.jpg")
plot(im_2020)


#let's calculate the NDVI=NIR-RED/NIR+RED 2020
NDVI_2020<-(im_2020[[1]]-im_2020[[2]])/(im_2020[[1]]+im_2020[[2]])
plot(NDVI_2020,col=cl)

#data import from 2024
im_2024<-rast("Salento_2024_False_color.jpg")
plot(im_2024)
#NDVI calculation 2024
NDVI_2024<-(im_2024[[1]]-im_2024[[2]])/(im_2024[[1]]+im_2024[[2]])
plot(NDVI_2024, col=cl)
dev.off()

#create a stack of the two years
NDVI_stack<-c(NDVI_2020, NDVI_2024)
plot(NDVI_stack, col=cl)
names(NDVI_stack)<-c("SALENTO.NDVI.2020","SALENTO.NDVI.2024")
dev.off()

#calculate the difference between the two years
NDVI_diff<-NDVI_2020-NDVI_2024
plot(NDVI_diff, col=cl)
dev.off()

#divide the pixels by two classes to see where the NDVI did not change and where it did
# # Valori sotto lo 0 (Classe 1) = Situazione Stabile / Nessun Deficit
# Valori sopra lo 0 (Classe 2) = Perdita di Vegetazione / Deficit Idrico
matrice_regole_ndvi <- matrix(c(-Inf, 0, 1,
                                0, Inf, 2), ncol = 3, byrow = TRUE)

NDVI_class <- classify(NDVI_diff, matrice_regole_ndvi)

# 2.  ritagliamo via il mare usando la sagoma del Salento
NDVI_class <- mask(NDVI_class, NDVI_diff)

par(mfrow=c(1,2))
plot(NDVI_diff, col=cl, main="Differenza NDVI")
plot(NDVI_class, col=cl, main="Classi NDVI (Pulito)")
dev.off()

##########PART 2: NDMI CALCULATION AND COMPARISON##########

#Normalized Difference Moisture Index-->depends on the vegetation 
#this time we have single band images, so we have to make the difference and the sum directly with the images
#NDMI=NIR-SWIR/NIR+SWIR
#with sentinel-2, NIR=BAND 8A, SWIR=B11

#2020 data import
B8A.2020<-rast("B8A.2020.tiff")  
B11.2020<-rast("B11.2020.tiff")
#2015 NDMI calculation
NDMI_2020=(B8A.2020-B11.2020)/(B8A.2020+B11.2020)
plot(NDMI_2020, col=cl)

#2024 data import
B8A.2024<-rast("B8A.2024.tiff")
B11.2024<-rast("B11.2024.tiff")
#2024 NDMI calculation
NDMI_2024=(B8A.2024-B11.2024)/(B8A.2024+B11.2024)
plot(NDMI_2024, col=cl)

dev.off()

#create a stack of the two years
# Allineiamo perfettamente l'immagine del 2024 alla griglia di quella del 2020
NDMI_2024_allineato <- resample(NDMI_2024, NDMI_2020, method = "bilinear")
NDMI_stack<-c(NDMI_2020, NDMI_2024_allineato)
plot(NDMI_stack, col=cl)
names(NDMI_stack)<-c("SALENTO.NDMI.2020","SALENTO.NDMI.2024")
dev.off()

#calculate the difference
NDMI_diff=NDMI_2020-NDMI_2024_allineato
plot(NDMI_diff, col=cl)

#divide the pixels in two classes: where NDMI did not change and where it did change
# 1. Creiamo la regola fissa: sotto lo 0 è deficit (Classe 1), sopra è stabile (Classe 2)
matrice_regole_ndmi <- matrix(c(-Inf, 0, 1,
                                0, Inf, 2), ncol = 3, byrow = TRUE)
NDMI_diff_class <- classify(NDMI_diff, matrice_regole_ndmi)

# 2. Ritagliamo via il mare usando la sagoma originale!
NDMI_diff_class <- mask(NDMI_diff_class, NDMI_diff)

# 3. Disegniamo il grafico pulito
plot(NDMI_diff_class, col=cl, main="Classi NDMI (Pulito)")       
dev.off()

#plot them together
par(mfrow=c(1,2))
plot(NDMI_diff, col=cl)
plot(NDMI_diff_class, col=cl)





########## PART 3: PERCENTUALI E GRAFICI FINALI CON GGPLOT2 ##########

# --- 1. Calcolo e Grafico per NDVI (Salute della Vegetazione) ---

# La funzione freq() conta esattamente quanti pixel ricadono in Classe 1 e Classe 2
freq_ndvi <- freq(NDVI_class)

# Sommiamo tutti i pixel per ottenere il totale
tot_ndvi <- sum(freq_ndvi$count)

# Calcoliamo la percentuale matematica
perc_ndvi <- (freq_ndvi$count / tot_ndvi) * 100

# Creiamo una tabella (dataframe) con i nomi chiari per il grafico
Classi_NDVI <- c("Stabile / In recupero", "Deficit di Vegetazione")
tab_ndvi <- data.frame(Classe = Classi_NDVI, Percentuale = perc_ndvi)

# Creazione del grafico a barre con ggplot2
grafico_ndvi <- ggplot(tab_ndvi, aes(x = Classe, y = Percentuale, fill = Classe)) +
  geom_bar(stat = "identity", color = "black") +
  # Scegliamo colori intuitivi: verde per la stabilità, giallo per il deficit
  scale_fill_manual(values = c("Stabile / In recupero" = "forestgreen", "Deficit di Vegetazione" = "gold")) +
  theme_minimal() +
  labs(title = "Impatto della Siccità sulla Vegetazione (NDVI)",
       subtitle = "Salento: Confronto 2020 - 2024",
       y = "Percentuale del territorio (%)", 
       x = "")

# Stampiamo il grafico a schermo
grafico_ndvi




# --- 2. Calcolo e Grafico per NDMI (Stress Idrico) ---

# Stesso identico procedimento per lo stress idrico
freq_ndmi <- freq(NDMI_diff_class)
tot_ndmi <- sum(freq_ndmi$count)
perc_ndmi <- (freq_ndmi$count / tot_ndmi) * 100

Classi_NDMI <- c("Umidità Stabile", "Deficit Idrico")
tab_ndmi <- data.frame(Classe = Classi_NDMI, Percentuale = perc_ndmi)

grafico_ndmi <- ggplot(tab_ndmi, aes(x = Classe, y = Percentuale, fill = Classe)) +
  geom_bar(stat = "identity", color = "black") +
  # Colori: blu per l'acqua, arancione per la siccità
  scale_fill_manual(values = c("Umidità Stabile" = "dodgerblue", "Deficit Idrico" = "darkorange")) +
  theme_minimal() +
  labs(title = "Stress Idrico nel Salento (NDMI)",
       subtitle = "Confronto 2020 - 2024",
       y = "Percentuale del territorio (%)", 
       x = "")

# Stampiamo il grafico a schermo
grafico_ndmi

























