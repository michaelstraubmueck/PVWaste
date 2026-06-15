rm(list=ls())
library(readxl)
library(dplyr)

setwd("G:/Meine Ablage/PVVAR/Journal/Code")
source("filter_rows_by_reference_rownames.R")
source("calculate_pv_waste_repowering.R")

#define parameter
region="Welt" #{Deutschland, Welt} die option zwischen welt und deutschland zu ändern kann entfernt werden. Also auch im weiteren verlauf schleifen die dadurch bedient werden
pv_forecast="IEA_NetZero" #{IRENA, Science, Technologieoffen, Effizienz, Beharrung, Robust,IEA_Current, IEA_Stated, IEA_NetZero} oder growth für fixe wachstumsrate #Hier brauche ich nur noch die Szenarien IEA_Current, IEA_Stated, IEA_NetZero
methodology<-"Weckend" #{Weckend, Dirr} #Weckend soll Weibull heißen und Dirr Gaussian, auch wieder in den schleifen dann anpassen
pv_growth<-1.05
first_year<-2003
observation_year<-2025
last_year<-2035
shape_value<-5.3759  #Regular-Loss: 5.3759  Early-Loss: 2.4928
fix_scale_value=FALSE #für Vergleich mit Weckend auf TRUE setzen -> Skalenparameter = 30
silver_only=TRUE # auf TRUE wenn für Kapitel 5 der Silberanteil in PV berechnet werden soll # die option silver only entfernen es ist immer FALSE und dementsprechend können schleifen, die zeiehn bei TRUE entfernt werden
recycling_rate_szenario<- "optimistisch" #optimistisch oder bedacht #diese auswahl kann entfernt werden
ag_share_scenario<-"high" # low, average, high #diese auswahl kann entfernt werden auch jeden code der damit zusammenhängt

if(region=="Deutschland"){
  deg_rate_Dach_cSi = 0.0074
  deg_rate_Dach_CdTe = 0.0187
  deg_rate_Freifläche_cSi = 0.0105
  deg_rate_Freifläche_CdTe = 0.0218
} else if(region=="Welt"){
  deg_rate_Dach_cSi = 0.0103
  deg_rate_Dach_CdTe = 0.0216
  deg_rate_Freifläche_cSi = 0.0134 
  deg_rate_Freifläche_CdTe = 0.0246
}else {
  print("Region falsch definiert")
}

lebensdauer_Dach_cSi<-0.20/deg_rate_Dach_cSi
scale_value_Dach_cSi<- lebensdauer_Dach_cSi/(log(2)^(1/shape_value))
lebensdauer_Dach_CdTe<-0.20/deg_rate_Dach_CdTe
scale_value_Dach_CdTe<- lebensdauer_Dach_CdTe/(log(2)^(1/shape_value))

lebensdauer_Freifläche_cSi<-0.20/deg_rate_Freifläche_cSi
scale_value_Freifläche_cSi<- lebensdauer_Freifläche_cSi/(log(2)^(1/shape_value))
lebensdauer_Freifläche_CdTe<-0.20/deg_rate_Freifläche_CdTe
scale_value_Freifläche_CdTe<- lebensdauer_Freifläche_CdTe/(log(2)^(1/shape_value))

if (fix_scale_value==TRUE){
  scale_value_Dach_cSi=30
  scale_value_Dach_CdTe =30
  scale_value_Freifläche_cSi =30
  scale_value_Freifläche_CdTe =30
}

#load share Dach/Freifläche
share_roof_ground<-read_excel("pv_roof_ground.xlsx", sheet= pv_forecast, col_names = FALSE, range = paste0("B", first_year-1948,":C",last_year-1948))
rownames(share_roof_ground)<-c(seq(first_year,last_year,1))
colnames(share_roof_ground)<-c("Dach","Freifläche")

#load share cSi thinfilm(CdTe)
share_cSi_CdTe<-read_excel("pv_cSi_thinfilm.xlsx", col_names = FALSE, range = paste0("B", first_year-1948,":C",last_year-1948))
rownames(share_cSi_CdTe)<-c(seq(first_year,last_year,1))
colnames(share_cSi_CdTe)<-c("cSi","CdTe")

#load installed capacity
installed_capacity<-read_excel("pv_capacity_IEA.xlsx", sheet= region, col_names = FALSE, range = paste0("B", first_year-1948,":B",observation_year-1948))
rownames(installed_capacity)<-c(seq(first_year,observation_year,1))
colnames(installed_capacity)<-"MW"

#load silver share in pv modules based on scenarios of Peeters et al. 2017
if (silver_only == TRUE){
Ag_share_table<-read_excel("G:/Meine Ablage/PVVAR/Journal/Code/Ag_share.xlsx", col_names = FALSE,range = paste0("A", first_year-1987,":D",last_year-1988))
colnames(Ag_share_table)<-c("Year","scenario_low","scenario_average","scenario_high")

ag_share <- data.frame(
  Year= Ag_share_table[["Year"]],
  Ag_share= Ag_share_table[[paste0("scenario_", ag_share_scenario)]])
} 

#select or calculate the future installed capacity

if (pv_forecast=="IRENA"){
  if(last_year>2050){
    print("no data past 2050")
    stop()
    }
installed_new_capacity<-diff(installed_capacity$MW)
installed_capacity_forecast<-data.frame(read_excel("pv_forecast.xlsx", col_names = FALSE, sheet="IRENA",range = paste0("D", observation_year-2017,":D",last_year-2018)))
installed_new_capacity<-c(installed_new_capacity,installed_capacity_forecast[,1])
installed_new_capacity<-as.data.frame(installed_new_capacity)                                 
} else if (pv_forecast=="growth"){
installed_new_capacity<-diff(installed_capacity$MW)
for (i in 1:(last_year-observation_year)) { 
  new_value <- installed_new_capacity[length(installed_new_capacity)] * pv_growth
  installed_new_capacity<- c(installed_new_capacity, new_value)
}
installed_new_capacity<-as.data.frame(installed_new_capacity)  
} else if (pv_forecast=="Science"){
  if(last_year>2060){
    print("no data past 2060")
    stop()
  }
  installed_new_capacity<-diff(installed_capacity$MW)
  installed_capacity_forecast<-data.frame(read_excel("pv_forecast.xlsx", col_names = FALSE, sheet="Science",range = paste0("D", observation_year-2017,":D",last_year-2018)))
  installed_new_capacity<-c(installed_new_capacity,installed_capacity_forecast[,1])
  installed_new_capacity<-as.data.frame(installed_new_capacity) 
}  else if (pv_forecast %in% c("Technologieoffen", "Effizienz", "Beharrung", "Robust")){
    if(last_year>2045){
      print("no data past 2045")
      stop()
    }
    installed_new_capacity<-diff(installed_capacity$MW)
    ISE_forecasts<-data.frame(read_excel("pv_forecast.xlsx", col_names = TRUE, sheet="ISE"))
    installed_capacity_forecast<-data.frame(ISE_forecasts[,pv_forecast])
    installed_new_capacity<-c(installed_new_capacity,installed_capacity_forecast[,1]) 
    installed_new_capacity<-as.data.frame(installed_new_capacity)
} else if (pv_forecast %in% c("IEA_Current", "IEA_Stated", "IEA_NetZero")){
  if(last_year>2050){
    print("no data past 2050")
    stop()
  }
  installed_new_capacity<-diff(installed_capacity$MW)
  IEA_forecasts<-data.frame(read_excel("pv_forecast_IEA.xlsx", col_names = FALSE, sheet="IEA",range = paste0("B", observation_year-2022,":D",last_year-2023)))
  colnames(IEA_forecasts) <- c("IEA_Current", "IEA_Stated", "IEA_NetZero")
  installed_capacity_forecast<-data.frame(IEA_forecasts[,pv_forecast])
  installed_new_capacity<-c(installed_new_capacity,installed_capacity_forecast[,1]) 
  installed_new_capacity<-as.data.frame(installed_new_capacity)
}

rownames(installed_new_capacity)<-c(seq(first_year+1,last_year,1))
colnames(installed_new_capacity)<-"MW"

#calculate the t_MW based on Weckend et al. data
Jahre<- c(1980,1990,1995,2000,2005,2010,2012,2015,2020,2025,2030,2050)
t_MW<-c(169.000,148.000,122.000,110.000,98.900,93.300,78.800,65.700,65.700,65.700,60.100,43.700)
#Jahre_ganz<-data.frame(seq(1990,2050,1))#NÖTIG???

t_MW<-data.frame(cbind(Jahre,t_MW))

###t/GW Exponentialfunktion auf die einzelen Punkte fitten und t/GW forecasten
model <- lm(formula=log(t_MW)~ Jahre, data = t_MW)
predict_t_MW<-as.data.frame(exp(predict(model,data.frame(Jahre=seq(1990,2050,1)))))
predict_t_MW<-cbind(seq(1990,2050,1),predict_t_MW)
colnames(predict_t_MW)<-c("Jahre","t_MW")
rownames(predict_t_MW)<-c(seq(1990,2050,1))


#bring dataframe to same length
predict_t_MW <- filter_rows_by_reference_rownames(predict_t_MW, installed_new_capacity)


share_roof_ground<-share_roof_ground[-1,] # erstes Jahr löschen, selbe länge
rownames(share_roof_ground)<-c(seq(first_year+1,last_year,1))
colnames(share_roof_ground)<-c("Dach","Freifläche")

share_cSi_CdTe<-share_cSi_CdTe[-1,]
rownames(share_cSi_CdTe)<-c(seq(first_year+1,last_year,1))
colnames(share_cSi_CdTe)<-c("cSi","CdTe")

installed_new_capacity_Dach_cSi <- share_roof_ground[,1] * share_cSi_CdTe[,1] * installed_new_capacity[,1]
installed_new_capacity_Dach_CdTe <- share_roof_ground[,1] * share_cSi_CdTe[,2] * installed_new_capacity[,1]
installed_new_capacity_Freifläche_cSi <- share_roof_ground[,2] * share_cSi_CdTe[,1] * installed_new_capacity[,1]
installed_new_capacity_Freifläche_CdTe <- share_roof_ground[,2] * share_cSi_CdTe[,2] * installed_new_capacity[,1]



if (methodology=="Weckend"){
  # >>> CHANGE START: Länge der Verteilungen direkt aus installed_new_capacity ableiten
  probability_function_Dach_cSi<-pweibull(0:nrow(installed_new_capacity), shape = shape_value, scale = scale_value_Dach_cSi, log = FALSE)
  pof_t_Dach_cSi<-diff(probability_function_Dach_cSi)
  
  probability_function_Dach_CdTe<-pweibull(0:nrow(installed_new_capacity), shape = shape_value, scale = scale_value_Dach_CdTe, log = FALSE)
  pof_t_Dach_CdTe<-diff(probability_function_Dach_CdTe)
  
  probability_function_Freifläche_cSi<-pweibull(0:nrow(installed_new_capacity), shape = shape_value, scale = scale_value_Freifläche_cSi, log = FALSE)
  pof_t_Freifläche_cSi<-diff(probability_function_Freifläche_cSi)
  
  probability_function_Freifläche_CdTe<-pweibull(0:nrow(installed_new_capacity), shape = shape_value, scale = scale_value_Freifläche_CdTe, log = FALSE)
  pof_t_Freifläche_CdTe<-diff(probability_function_Freifläche_CdTe)

} else if (methodology=="Dirr"){
#probability_function<-pnorm(0.8, mean=(1*(1-deg_rate)^1), sd=0.0167*(1+0.05*1))
  Dirr_initial<-c(0.013,0.004,0.004,0.004,0.004)
  # >>> CHANGE START: Länge der Verteilungen direkt aus installed_new_capacity ableiten
  probability_function_Dach_cSi<-pnorm(0.8, mean=(1*(1-deg_rate_Dach_cSi*(0:nrow(installed_new_capacity)))), sd=0.0167*(1+0.05*(0:nrow(installed_new_capacity))))
  pof_t_Dach_cSi<-diff(probability_function_Dach_cSi) 
  pof_t_Dach_cSi[1:5]<-Dirr_initial
  
  probability_function_Dach_CdTe<-pnorm(0.8, mean=(1*(1-deg_rate_Dach_CdTe*(0:nrow(installed_new_capacity)))), sd=0.0167*(1+0.05*(0:nrow(installed_new_capacity))))
  pof_t_Dach_CdTe<-diff(probability_function_Dach_CdTe) 
  pof_t_Dach_CdTe[1:5]<-Dirr_initial 
  
  probability_function_Freifläche_cSi<-pnorm(0.8, mean=(1*(1-deg_rate_Freifläche_cSi*(0:nrow(installed_new_capacity)))), sd=0.0167*(1+0.05*(0:nrow(installed_new_capacity))))
  pof_t_Freifläche_cSi<-diff(probability_function_Freifläche_cSi) 
  pof_t_Freifläche_cSi[1:5]<-Dirr_initial
  
  probability_function_Freifläche_CdTe<-pnorm(0.8, mean=(1*(1-deg_rate_Freifläche_CdTe*(0:nrow(installed_new_capacity)))), sd=0.0167*(1+0.05*(0:nrow(installed_new_capacity))))
  pof_t_Freifläche_CdTe<-diff(probability_function_Freifläche_CdTe) 
  pof_t_Freifläche_CdTe[1:5]<-Dirr_initial
  # <<< CHANGE END
}
#F(t)-F(t-1) probability of failure in year t

# >>> CHANGE START: Iterativen Repowering-Bedarf je Segment berechnen und danach jahresspezifisch in Masse umrechnen
repowering_Dach_cSi <- calculate_repowering_capacity(pof_t_Dach_cSi, installed_new_capacity_Dach_cSi)
repowering_Dach_CdTe <- calculate_repowering_capacity(pof_t_Dach_CdTe, installed_new_capacity_Dach_CdTe)
repowering_Freifläche_cSi <- calculate_repowering_capacity(pof_t_Freifläche_cSi, installed_new_capacity_Freifläche_cSi)
repowering_Freifläche_CdTe <- calculate_repowering_capacity(pof_t_Freifläche_CdTe, installed_new_capacity_Freifläche_CdTe)
repowering_sum <- data.frame(
  Year = seq(first_year + 1, last_year, 1),
  
  net_capacity =
    repowering_Dach_cSi$net_capacity +
    repowering_Dach_CdTe$net_capacity +
    repowering_Freifläche_cSi$net_capacity +
    repowering_Freifläche_CdTe$net_capacity,
  
  repowering_capacity =
    repowering_Dach_cSi$repowering_capacity +
    repowering_Dach_CdTe$repowering_capacity +
    repowering_Freifläche_cSi$repowering_capacity +
    repowering_Freifläche_CdTe$repowering_capacity,
  
  gross_capacity =
    repowering_Dach_cSi$gross_capacity +
    repowering_Dach_CdTe$gross_capacity +
    repowering_Freifläche_cSi$gross_capacity +
    repowering_Freifläche_CdTe$gross_capacity
)

installed_amount_Dach_cSi <- data.frame(amount = predict_t_MW[,2] * repowering_Dach_cSi$gross_capacity)
installed_amount_Dach_CdTe <- data.frame(amount = predict_t_MW[,2] * repowering_Dach_CdTe$gross_capacity)
installed_amount_Freifläche_cSi <- data.frame(amount = predict_t_MW[,2] * repowering_Freifläche_cSi$gross_capacity)
installed_amount_Freifläche_CdTe <- data.frame(amount = predict_t_MW[,2] * repowering_Freifläche_CdTe$gross_capacity)

rownames(installed_amount_Dach_cSi)<-c(seq(first_year+1,last_year,1))
rownames(installed_amount_Dach_CdTe)<-c(seq(first_year+1,last_year,1))
rownames(installed_amount_Freifläche_cSi)<-c(seq(first_year+1,last_year,1))
rownames(installed_amount_Freifläche_CdTe)<-c(seq(first_year+1,last_year,1))

if (silver_only == TRUE){
  installed_amount_Dach_cSi<-installed_amount_Dach_cSi*ag_share[,2]
  installed_amount_Dach_CdTe<-installed_amount_Dach_CdTe*ag_share[,2]
  installed_amount_Freifläche_cSi<-installed_amount_Freifläche_cSi*ag_share[,2]
  installed_amount_Freifläche_CdTe<-installed_amount_Freifläche_CdTe*ag_share[,2]
}

installed_amount <- data.frame(installed_amount_Dach_cSi[,1]+installed_amount_Dach_CdTe[,1]+installed_amount_Freifläche_cSi[,1]+installed_amount_Freifläche_CdTe[,1])
colnames(installed_amount)<-"amount"
rownames(installed_amount)<-c(seq(first_year+1,last_year,1))

amount_vector_Dach_cSi<-installed_amount_Dach_cSi[,1]
amount_vector_Dach_CdTe<-installed_amount_Dach_CdTe[,1]
amount_vector_Freifläche_cSi<-installed_amount_Freifläche_cSi[,1]
amount_vector_Freifläche_CdTe<-installed_amount_Freifläche_CdTe[,1]
amount<-installed_amount[,1]
# <<< CHANGE END

#berechnet Schrottmenge
waste_amount_Dach_cSi <- calculate_pv_waste(pof_t_Dach_cSi, amount_vector_Dach_cSi, first_year, last_year)
waste_amount_Dach_cSi <- waste_amount_Dach_cSi[-nrow(waste_amount_Dach_cSi), ]

waste_amount_Dach_CdTe <- calculate_pv_waste(pof_t_Dach_CdTe, amount_vector_Dach_CdTe, first_year, last_year)
waste_amount_Dach_CdTe <- waste_amount_Dach_CdTe[-nrow(waste_amount_Dach_CdTe), ]

waste_amount_Freifläche_cSi <- calculate_pv_waste(pof_t_Freifläche_cSi, amount_vector_Freifläche_cSi, first_year, last_year)
waste_amount_Freifläche_cSi <- waste_amount_Freifläche_cSi[-nrow(waste_amount_Freifläche_cSi), ]

waste_amount_Freifläche_CdTe <- calculate_pv_waste(pof_t_Freifläche_CdTe, amount_vector_Freifläche_CdTe, first_year, last_year)
waste_amount_Freifläche_CdTe <- waste_amount_Freifläche_CdTe[-nrow(waste_amount_Freifläche_CdTe), ]


#addiere die Abfallmenge aus Dach und Freiflächenanlagen cSi und CdTe
waste_amount<-data.frame(waste_amount_Dach_cSi[,2]+waste_amount_Freifläche_cSi[,2]+waste_amount_Dach_CdTe[,2]+waste_amount_Freifläche_CdTe[,2])
colnames(waste_amount) <- "Gesamtschrottmenge"
rownames(waste_amount)<-c(seq(first_year+2,last_year,1))

kumulierter_waste_amount <- data.frame(Kumulierte_Schrottmenge = cumsum(waste_amount$Gesamtschrottmenge))
colnames(kumulierter_waste_amount) <- "kumulierte Gesamtschrottmenge"
rownames(kumulierter_waste_amount)<-c(seq(first_year+2,last_year,1))


##################################################################################################################################
if (silver_only == TRUE){
#Definieren basierend auf Silver Survey wie viel Silber ohne PV benötigt wird
#Ag_secondary_production_ex_pv<-5556 #SS2024 recycling 2023 umgerechnet in t 1t=32,151 troy oz
#Ag_secondary_production_ex_pv<-6031 #SS2025
Ag_secondary_production_ex_pv<-6145 #SS2026 

#Anteil Silber in c-Si Schrott berechnen, dann wann der Schrott anfällt 

Ag_recycling_amount_pv<-data.frame(waste_amount_Dach_cSi[,2]+waste_amount_Freifläche_cSi[,2])
#Ag_recycling_amount_pv<-data.frame(waste_amount[-nrow(Ag_recycling_amount_pv),]) 
colnames(Ag_recycling_amount_pv) <- "Recycelbares Silber aus PV"
rownames(Ag_recycling_amount_pv)<-c(seq(first_year+2,last_year,1))



#Einlesen der recycling rate
ag_recycling_rate<-data.frame(read_excel("recycling_rate.xlsx", col_names = FALSE, sheet=recycling_rate_szenario,range = paste0("B", first_year-1946,":B",last_year-1948)))
colnames(ag_recycling_rate) <- "Recycling Rate Silber in PV"
rownames(ag_recycling_rate)<-c(seq(first_year+2,last_year,1))
#multiply waste amount with recycling rate
Ag_recycling_amount_pv<- data.frame(
  Year = seq(first_year + 2, last_year, 1),
  Ag_pv_recycling = Ag_recycling_amount_pv*ag_recycling_rate
)

#addiere den letzt bekannten non pv recycling amount zum prognostizierten pv recycling amount
Ag_recycling_amount<- data.frame(
  Year = seq(first_year + 2, last_year, 1),
  Ag_recycling = Ag_recycling_amount_pv[,2]+Ag_secondary_production_ex_pv
)

#calculate log return of recycling amount
log_Ag_recycling_amount<-log(Ag_recycling_amount[,2])
log_return_Ag_recycling_amount <- data.frame(
  Year = seq(first_year + 3, last_year, 1),
  Ag_recycling = diff(log_Ag_recycling_amount))
############################################################
share_cSi_CdTe<- data.frame(
  Year = seq(first_year + 1, last_year, 1),
  cSi = share_cSi_CdTe[,1],
  CdTe = share_cSi_CdTe[,2])
#calculate Ag content in new installations (consumption) based on ag share in pv mit Start 1 Jahr vor observation, da noch log genommen wird
Ag_consumption_amount_pv<-(installed_amount[rownames(installed_amount) >= observation_year-1 & rownames(installed_amount) <= last_year, ])*(share_cSi_CdTe$cSi[share_cSi_CdTe$Year >= observation_year-1 & share_cSi_CdTe$Year <= last_year])

#2023 Ag consumption excluding pv von feinunze in t
#Ag_consumption_amount_ex_pv<-23211 #aus SS2024 total consumption - hedging demand - pv- coin&bar
#Ag_consumption_amount_ex_pv<-29928 #aus SS2025 total consumption - hedging demand - pv
Ag_consumption_amount_ex_pv<-21349 #aus SS2026 total consumption - hedging demand - pv - coin&bar

#Ag consumption adding the consumption exluding pv in 2023 to the future Ag pv consumption
Ag_consumption_amount<-Ag_consumption_amount_pv+Ag_consumption_amount_ex_pv

#calculate log return of consumption amount
log_Ag_consumption_amount<-log(Ag_consumption_amount)
log_return_Ag_consumption_amount <- data.frame(
  Year = seq(observation_year, last_year, 1),
  Ag_consumption = diff(log_Ag_consumption_amount))

if (region == "Welt"){
  Ag_supply_demand <- data.frame(
    Year = (observation_year-1):last_year,
    Supply = Ag_recycling_amount_pv[Ag_recycling_amount_pv$Year >= observation_year-1 & 
                                      Ag_recycling_amount_pv$Year <= last_year, 2],
    Demand = Ag_consumption_amount_pv)
  Ag_supply_demand <- Ag_supply_demand[-c(1, 2), ]
} else if (region == "Deutschland"){
  Ag_supply_demand <- data.frame(
    Supply = Ag_recycling_amount_pv[Ag_recycling_amount_pv$Year >= observation_year-1 & 
                                      Ag_recycling_amount_pv$Year <= last_year, 2],
    Demand = Ag_consumption_amount_pv,
    Year = (observation_year-1):last_year)
}

}
