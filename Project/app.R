library(shiny)
library(tidyverse)
library(data.table)

carmarket <- fread('https://raw.githubusercontent.com/pjournal/boun01g-data-mine-r-s/gh-pages/data.table/turkey_car_market.csv')

#Translate TR -> EN
col_names = c('Date', 'Brand', 'Vehicle_Type_Group', 'Vehicle_Type', 'Model_Year', 'Fuel_Type', 'Gear', 'CCM', 'Horse_Power', 'Color', 'Body_Type', 'Seller', 'Seller_Status', 'Kilometers', 'Price')
names(carmarket) = col_names

carmarket <- carmarket %>%
    #Brand type
    mutate(Brand = as.character(Brand),
           Brand = if_else(Brand == 'Tofaş', 'Tofas', Brand),
           
           Brand = as.factor(Brand))%>%
    #Seller
    mutate(Seller = as.character(Seller),
           Seller = if_else(Seller == 'Galeriden', 'Galery', if_else(Seller == 'Sahibinden', 'Owner', 'Authority')),
           Seller = as.factor(Seller)) %>%  #unique(carmarket$Seller) #Turkish seller names are changed by English version
    
    #fuel type
    mutate(Fuel_Type = as.character(Fuel_Type),
           Fuel_Type = if_else(Fuel_Type == 'Dizel', 'Diesel', 
                               if_else(Fuel_Type == 'Benzin/LPG', 'Gasoline', 
                                       if_else(Fuel_Type == 'Hibrit', 'Hybrid', 'Electricity'))),
           Fuel_Type = as.factor(Fuel_Type)) %>%
    
    #Gear
    mutate(Gear = as.character(Gear),
           Gear = if_else(Gear == 'Otomatik Vites', 'Automatic', 
                          if_else(Gear == 'Yarı Otomatik Vites', 'Semi Automatic', 'Manual')),
           Gear = as.factor(Gear))%>%
    
    #Seller Status
    mutate(Seller_Status = as.character(Seller_Status),
           Seller_Status = if_else(Seller_Status == '2. El', '2nd Hand', 
                                   if_else(Seller_Status == 'Klasik', 'Classic', 
                                           if_else(Seller_Status == "0 km", "0 km", "Damaged"))),
           Seller_Status = as.factor(Seller_Status))%>%
    
    #Vehicle Type Group
    mutate(Vehicle_Type_Group = as.character(Vehicle_Type_Group),
           Vehicle_Type_Group = case_when(Vehicle_Type_Group == 'Diğer' ~ 'Other',
                                          Vehicle_Type_Group == 'A Serisi' ~ 'A Series',
                                          Vehicle_Type_Group == '100 Serisi' ~ '100 Series',
                                          Vehicle_Type_Group == '5 Serisi' ~ '5 Series',
                                          Vehicle_Type_Group == 'S Serisi' ~ 'S Series',
                                          Vehicle_Type_Group == '7 Serisi' ~ '7 Series',
                                          Vehicle_Type_Group == '3 Serisi' ~ '3 Series',
                                          Vehicle_Type_Group == '1 Serisi' ~ '1 Series',
                                          Vehicle_Type_Group == '4 Serisi' ~ '4 Series',
                                          Vehicle_Type_Group == '2 Serisi' ~ '2 Series',
                                          Vehicle_Type_Group == '6 Serisi' ~ '6 Series',
                                          Vehicle_Type_Group == 'Z Serisi' ~ 'Z Series',
                                          Vehicle_Type_Group == 'M Serisi' ~ 'M Series',
                                          Vehicle_Type_Group == '500 Ailesi' ~ '500 Family',
                                          Vehicle_Type_Group == 'E Serisi' ~ 'E Series',
                                          Vehicle_Type_Group == 'B Serisi' ~ 'B Series',
                                          Vehicle_Type_Group == 'G Serisi' ~ 'G Series',
                                          Vehicle_Type_Group == 'Serçe' ~ 'Serce',
                                          Vehicle_Type_Group == 'Doğan' ~ 'Dogan',
                                          TRUE ~ Vehicle_Type_Group),
           Vehicle_Type_Group = as.factor(Vehicle_Type_Group)) %>%
    
    #Vehicle Type
    mutate(Vehicle_Type = as.character(Vehicle_Type),
           Vehicle_Type = case_when(Vehicle_Type == 'Diğer' ~ 'Other',
                                    Vehicle_Type == '1.6 BlueHDI 130HP INTENSIVE EAT6 7 Kişilik' ~ '1.6 BlueHDI 130HP INTENSIVE EAT6 7 Seats',
                                    Vehicle_Type == '1.3 CDTI Enjoy 111.Yıl' ~ '1.3 CDTI Enjoy 111',
                                    Vehicle_Type == '1.3 GLS Otm.' ~ '1.3 GLS Auto',
                                    Vehicle_Type == '1.4 120.Yıl' ~ '1.4 120',
                                    Vehicle_Type == '1.4 CVVT Select Otm.' ~ '1.4 CVVT Select Auto',
                                    Vehicle_Type == '1.4 CVVT Style Otm.' ~ '1.4 CVVT Style Auto',
                                    Vehicle_Type == '0.9\n' ~ '0.9',
                                    Vehicle_Type == '1.0 TFSI Sportback\n' ~ '1.0 TFSI Sportback',
                                    Vehicle_Type == '1.0\n' ~ '1.0',
                                    Vehicle_Type == '1.2 D-CVVT\n' ~ '1.2 D-CVVT',
                                    Vehicle_Type == '1.2 DOHC\n' ~ '1.2 DOHC',
                                    Vehicle_Type == '1.2 TSI Sport Tourer\n' ~ '1.2 TSI Sport Tourer',
                                    Vehicle_Type == '1.2 TSI\n' ~ '1.2 TSI',
                                    Vehicle_Type == '1.2 VTi\n' ~ '1.2 VTi',
                                    Vehicle_Type == '1.2\n' ~ '1.2',
                                    Vehicle_Type == '1.3 CDTI \n' ~ '1.3 CDTI',
                                    Vehicle_Type == '1.3 Mjet\n' ~ '1.3 Mjet',
                                    Vehicle_Type == '1.3 Multijet Active Plus\n' ~ '1.3 Multijet Active Plus',
                                    Vehicle_Type == '1.3 Multijet Emotion Plus\n' ~ '1.3 Multijet Emotion Plus',
                                    Vehicle_Type == '1.3 Multijet\n' ~ '1.3 Multijet',
                                    Vehicle_Type == '1.3\n' ~ '1.3',
                                    Vehicle_Type == '1.4 CRDi' ~ '1.4 CRDi',
                                    Vehicle_Type == '1.4 CVVT\n' ~ '1.4 CVVT',
                                    Vehicle_Type == '1.4 Fire\n' ~ '1.4 Fire',
                                    Vehicle_Type == '1.4 HDi X\n' ~ '1.4 HDi X',
                                    Vehicle_Type == '1.4 HDi\n' ~ '1.4 HDI',
                                    Vehicle_Type == '1.4 ie SX\n' ~ '1.4 ie SX',
                                    Vehicle_Type == '1.4 PopStar\n' ~ '1.4 PopStar',
                                    Vehicle_Type == '1.4 T\n' ~ '1.4 T',
                                    Vehicle_Type == '1.4 TDCi\n' ~ '1.4 TDCi',
                                    Vehicle_Type == '1.4 TDI\n' ~ '1.4 TDI',
                                    Vehicle_Type == '1.4 TSI Sport Coupe Cupra\n' ~ '1.4 TSI Sport Coupe Cupra',
                                    Vehicle_Type == '1.4 Twinport\n' ~ '1.4 Twinport',
                                    Vehicle_Type == '1.4\n' ~ '1.4',
                                    Vehicle_Type == '1.5 CRDi \n' ~ '1.5 CRDi',
                                    Vehicle_Type == '1.5 DCI Ambiance' ~ '1.5 Dci Ambiance',
                                    Vehicle_Type == '1.5 dCi Ambiance\n' ~ '1.5 dCi Ambiance',
                                    Vehicle_Type == '1.5 dCi Authentique' ~ '1.5 DCi Authentique',
                                    Vehicle_Type == '1.5 DCi Extreme' ~ '1.5 dCi Extreme',
                                    Vehicle_Type == '1.5 dCi Grandtour Authentique\n' ~ '1.5 dCi Grandtour Authentique Edition',
                                    Vehicle_Type == '1.5 DCi Expression' ~ '1.5 dCi Expression',
                                    Vehicle_Type == '1.5 dCi Grandtour Executive\n' ~ '1.5 dCi Grandtour Executive',
                                    Vehicle_Type == '1.5 dCi GT-Line\n' ~ '1.5 dCi GT Line',
                                    Vehicle_Type == '1.5 DCi Privilege' ~ '1.5 dCi Privilege',
                                    Vehicle_Type == '1.5 DCI Tekna' ~ '1.5 dCi Tekna',
                                    Vehicle_Type == '1.5 DCI Visia' ~ '1.5 dCi Visia',
                                    Vehicle_Type == '1.5 dCi\n' ~ '1.5 dCi',
                                    Vehicle_Type == '1.5 TDCi Titanium' ~ '1.5 TDCI Titanium',
                                    Vehicle_Type == '1.5\n' ~ '1.5',
                                    Vehicle_Type == '1.6 BlueHDI 130HP INTENSIVE EAT6 7 Kişilik' ~ '1.6 BlueHDI 130HP INTENSIVE EAT6 7 Seats',
                                    Vehicle_Type == '1.6 BlueHDI Active' ~ '1.6 BlueHDi Active',
                                    Vehicle_Type == '1.6 CDTI  Grand Sport 120.Yıl' ~ '1.6 CDTI  Grand Sport 120',
                                    Vehicle_Type == '1.6 CDTI  Cosmo' ~ '1.6 CDTI Cosmo',
                                    Vehicle_Type == '1.6 CDTI  Business' ~ '1.6 CDTIBusiness',
                                    Vehicle_Type == '1.6 CDTI  Design' ~ '1.6 CDTI Design',
                                    Vehicle_Type == '1.6 CDTI  Elite' ~ '1.6 CDTI Elite',
                                    Vehicle_Type == '1.6 CDTI  Enjoy' ~ '1.6 CDTI Enjoy',
                                    Vehicle_Type == '1.6 CDTI  Sport' ~ '1.6 CDTI Sport',
                                    Vehicle_Type == '1.6 CDTI\n' ~ '1.6 CDTI',
                                    Vehicle_Type == '1.6 CR TDI Elegance\n' ~ '1.6 CR TDI Elegance',
                                    Vehicle_Type == '1.6 CRDI Concept Plus' ~ '1.6 CRDi Concept Plus',
                                    Vehicle_Type == '1.6 CRDi Elite\n' ~ '1.6 CRDi Elite',
                                    Vehicle_Type == '1.6 D Türkiye Paketi' ~ '1.6 D Turkey Package',
                                    Vehicle_Type == '1.6 dCi Tekna Sky Pack\n' ~ '1.6 dCi Tekna Sky Pack',
                                    Vehicle_Type == '1.6 e-HDi\n' ~ '1.6 e-HDi',
                                    Vehicle_Type == '1.6 Enjoy 111. Yıl' ~ '1.6 Enjoy 111',
                                    Vehicle_Type == '1.6 GDI\n' ~ '1.6 GDI',
                                    Vehicle_Type == '1.6 HDi \n' ~ '1.6 HDi',
                                    Vehicle_Type == '1.6 HDi\n' ~ '1.6 HDi',
                                    Vehicle_Type == '1.6 HDi Confort' ~ '1.6 HDi Comfort',
                                    Vehicle_Type == '1.6 HDI SX PK' ~ '1.6 HDi SX PK',
                                    Vehicle_Type == '1.6 Invite Otm' ~ '1.6 Invite Auto',
                                    Vehicle_Type == '1.6 LT\n' ~ '1.6 LT',
                                    Vehicle_Type == '1.6 Mjet\n' ~ '1.6 Mjet',
                                    Vehicle_Type == '1.6 Multijet Emotion Plus\n' ~ '1.6 Multijet Emotion Plus',
                                    Vehicle_Type == '1.6 Multijet\n' ~ '1.6 Multijet',
                                    Vehicle_Type == '1.6 Privelege' ~ '1.6 Privilege',
                                    Vehicle_Type == '1.6 Türkiye Paketi' ~ '1.6 Turkey Package',
                                    Vehicle_Type == '1.6 TDCİ Ghia' ~ '1.6 TDCi Ghia',
                                    Vehicle_Type == '1.6 TDCi\n' ~ '1.6 TDCi',
                                    Vehicle_Type == '1.6 TDI \n' ~ '1.6 TDI',
                                    Vehicle_Type == '1.6 TDI Ambiente' ~ '1.6 TDi Ambiente',
                                    Vehicle_Type == '1.6 TDI Ambition Sportback\n' ~ '1.6 TDI Ambition Sportback',
                                    Vehicle_Type == '1.6 TDI Attraction Sportback\n' ~ '1.6 TDI Attraction Sportback',
                                    Vehicle_Type == '1.6 TDI Elegance' ~ '1.6 TDi Elegance',
                                    Vehicle_Type == '1.6 TDI GreenLine Elegance\n' ~ '1.6 TDI GreenLine Elegance',
                                    Vehicle_Type == '1.6 TDI Sportback\n' ~ '1.6 TDI Sportback',
                                    Vehicle_Type == '1.6 TDI\n' ~ '1.6 TDI',
                                    Vehicle_Type == '1.6 Ti-VCT\n' ~ '1.6 Ti-VCT',
                                    Vehicle_Type == '1.6 VTES ES' ~ '1.6 VTEC ES',
                                    Vehicle_Type == '1.6 XHT Cosmo\n' ~ '1.6 XHT Cosmo',
                                    Vehicle_Type == '1.6\n' ~ '1.6',
                                    Vehicle_Type == '1.7\n' ~ '1.7',
                                    Vehicle_Type == '1.8\n' ~ '1.8',
                                    Vehicle_Type == 'Serçe' ~ 'Serce',
                                    Vehicle_Type == 'Kartal 5 Vites' ~ 'Kartal 5 Transmissions',
                                    Vehicle_Type == 'EVO 1.4 Fire Active' ~ 'EVO 1.4 Active Fire',
                                    Vehicle_Type == 'Country 4x2\n' ~ 'Country 4x2',
                                    Vehicle_Type == '4x2\n' ~ '4x2',
                                    Vehicle_Type == '3.0 SDV6\n' ~ '3.0 SDV6',
                                    Vehicle_Type == '200 BlueTEC AMG\n' ~ '200 BlueTEC AMG',
                                    Vehicle_Type == '200 BlueEfficiency Avantgarde\n' ~ '200 BlueEfficiency Avantgarde',
                                    Vehicle_Type == '2.5 Çift Kabin 4x4' ~ '2.5 Double Cabinet 4x4',
                                    Vehicle_Type == '2.0i sDrive\n' ~ '2.0i sDrive',
                                    Vehicle_Type == '2.0\n' ~ '2.0',
                                    Vehicle_Type == '2.0 TDI\n' ~ '2.0 TDI',
                                    Vehicle_Type == '2.0 TDCi\n' ~ '2.0 TDCi',
                                    Vehicle_Type == '2.0 SRTi \n' ~ '2.0 SRTi',
                                    Vehicle_Type == '2.0 Quattro\n' ~ '2.0 Quattro',
                                    Vehicle_Type == '2.0 Ghia Otm.' ~ '2.0 Ghia Auto',
                                    Vehicle_Type == '2.0 CRDi GLS\n' ~ '2.0 CRDi GLS',
                                    Vehicle_Type == '2.0 24V LT Otm.' ~ '2.0 24V LT Auto',
                                    Vehicle_Type == '180 CDI AMG\n' ~ '180 CDI AMG',
                                    Vehicle_Type == '180 BlueEfficiency AMG Sport\n' ~ '180 BlueEfficiency AMG Sport',
                                    Vehicle_Type == '180 BlueEfficiency Fascination\n' ~ '180 BlueEfficiency Fascination',
                                    Vehicle_Type == '150 Otm.' ~ '150 Auto',
                                    Vehicle_Type == '1.9 TDI\n' ~ '1.9 TDI',
                                    Vehicle_Type == '1.8\n' ~ '1.8',
                                    Vehicle_Type == '1.7\n' ~ '1.7',
                                    Vehicle_Type == '1.6\n' ~ '1.6',
                                    TRUE ~ Vehicle_Type),
           Vehicle_Type = as.factor(Vehicle_Type)) %>%
    
    #CCM
    mutate(CCM = as.character(CCM),
           CCM = case_when(CCM == '1300 cc ve altı' ~ '1300 cc and below',
                           CCM == '6001 cc ve üzeri' ~ '6001 cc and above',
                           CCM == 'Bilmiyorum' | CCM == '-' ~ 'Don\'t Know',
                           TRUE ~ CCM),
           CCM = factor(CCM, levels = c('Don\'t Know', '1300 cc and below', '1301-1600 cc', '1601-1800 cc', '1801-2000 cc', '2001-2500 cc', '2501-3000 cc', '3001-3500 cc', '3501-4000 cc', '4001-4500 cc', '4501-5000 cc', '5001-5500 cc', '5501-6000 cc', '6001 cc and above'))) %>%
    
    #Horse Power
    mutate(Horse_Power = as.character(Horse_Power),
           Horse_Power = case_when(Horse_Power == '100 BG ve altı' ~ '100 HP and below',
                                   Horse_Power == '50 BG ve altı' ~ '100 HP and below',
                                   Horse_Power == '51-75 BG' ~ '100 HP and below',
                                   Horse_Power == '601 BG ve üzeri' ~ '601 HP and above',
                                   Horse_Power == '101-125 BG' ~ '101-125 HP',
                                   Horse_Power == '126-150 BG' ~ '126-150 HP',
                                   Horse_Power == '151-175 BG' ~ '151-175 HP',
                                   Horse_Power == '176-200 BG' ~ '176-200 HP',
                                   Horse_Power == '201-225 BG' ~ '201-225 HP',
                                   Horse_Power == '226-250 BG' ~ '226-250 HP',
                                   Horse_Power == '251-275 BG' ~ '251-275 HP',
                                   Horse_Power == '276-300 BG' ~ '276-300 HP',
                                   Horse_Power == '301-325 BG' ~ '301-325 HP',
                                   Horse_Power == '326-350 BG' ~ '326-350 HP',
                                   Horse_Power == '376-400 BG' ~ '376-400 HP',
                                   Horse_Power == '451-475 BG' ~ '451-475 HP',
                                   Horse_Power == '76-100 BG' ~ '100 HP and below',
                                   Horse_Power == 'Bilmiyorum' | Horse_Power == '-' ~ 'Don\'t Know',
                                   TRUE ~ Horse_Power),
           Horse_Power = factor(Horse_Power, levels = c('Don\'t Know', '100 HP and below', '101-125 HP', '126-150 HP', '151-175 HP', '176-200 HP', '201-225 HP', '226-250 HP', '251-275 HP', '276-300 HP', '301-325 HP', '326-350 HP', '376-400 HP', '451-475 HP', '601 HP and above'))) %>%
    
    #Color
    mutate(Color = as.character(Color),
           Color = case_when(Color == 'Altın' ~ 'Gold',
                             Color == 'Amarant' ~ 'Amaranth',
                             Color == 'Bal Rengi' ~ 'Honey',
                             Color == 'Bej' ~ 'Beige',
                             Color == 'Beyaz' ~ 'White',
                             Color == 'Bordo' ~ 'Burgundy',
                             Color == 'Diğer' ~ 'Other',
                             Color == 'Füme' ~ 'Smoked',
                             Color == 'Gümüş' ~ 'Silver',
                             Color == 'Gümüş Gri' ~ 'Silver Gray',
                             Color == 'Gri' ~ 'Gray',
                             Color == 'Ihlamur' ~ 'Linden',
                             Color == 'Kırmızı' ~ 'Red',
                             Color == 'Kahverengi' ~ 'Brown',
                             Color == 'Krem' ~ 'Cream',
                             Color == 'Kum Rengi' ~ 'Sand Color',
                             Color == 'Lacivert' ~ 'Dark Blue',
                             Color == 'Mavi' ~ 'Blue',
                             Color == 'Mor' ~ 'Purple',
                             Color == 'Pembe' ~ 'Pink',
                             Color == 'Sarı' ~ 'Yellow',
                             Color == 'Siyah' ~ 'Black',
                             Color == 'Turkuaz' ~ 'Turquoise',
                             Color == 'Turuncu' ~ 'Orange',
                             Color == 'Yeşil' ~ 'Green',
                             Color == 'Zeytin Gri' ~ 'Olive Gray',
                             Color == 'Eflatun' ~ 'Magenta',
                             TRUE ~ Color),
           Color = as.factor(Color)) %>%
    
    #Body Type
    mutate(Body_Type = as.character(Body_Type),
           Body_Type = case_when(Body_Type == 'Üstü Açık / Cabriolet' ~ 'Open Top / Cabriolet',
                                 Body_Type == 'Arazi Aracı' ~ 'Off-road Vehicle',
                                 Body_Type == 'Camlı Van' ~ 'Glass Van',
                                 Body_Type == 'Diğer' ~ 'Other',
                                 Body_Type == 'Hatchback 3 Kapı' ~ 'Hatchback 3 Doors',
                                 Body_Type == 'Hatchback 5 Kapı' ~ 'Hatchback 5 Doors',
                                 Body_Type == 'Spor / Coupe' ~ 'Sport / Coupe',
                                 TRUE ~ Body_Type),
           Body_Type = as.factor(Body_Type)) 

#Preprocessing
keys <- c("Brand", "Model_Year", "Fuel_Type")
carmarket.CCM.DK <- data.table(carmarket[CCM == 'Don\'t Know'], key = keys)
carmarket.CCM.FULL <- data.table(carmarket[CCM != 'Don\'t Know'], key = keys)
calculate_mode = function(x){
    uniques = unique(x)
    uniques[which.max(tabulate(match(x, uniques)))]
}
for (i in 1: nrow(carmarket.CCM.DK)){
    brand = carmarket.CCM.DK[i, Brand]
    model_year = carmarket.CCM.DK[i, Model_Year]
    fuel_type = carmarket.CCM.DK[i, Fuel_Type]
    carmarket.CCM.DK[i, CCM := calculate_mode(carmarket.CCM.FULL[Brand == brand & Model_Year == model_year & Fuel_Type == fuel_type, .(Brand, Model_Year, Fuel_Type, CCM)])$CCM]
}
carmarket <- rbind(na.omit(carmarket.CCM.DK, 'CCM'), carmarket.CCM.FULL)

keys <- c("Brand", "CCM")
carmarket.HP.DK <- data.table(carmarket[Horse_Power == 'Don\'t Know'], key = keys)
carmarket.HP.FULL <- data.table(carmarket[Horse_Power != 'Don\'t Know'], key = keys)
for (i in 1: nrow(carmarket.HP.DK)){
    brand <- carmarket.HP.DK[i, Brand]
    ccm <- carmarket.HP.DK[i, CCM]
    carmarket.HP.DK[i, Horse_Power := calculate_mode(carmarket.HP.FULL[Brand == brand & CCM == ccm, .(Brand, CCM, Horse_Power)])$Horse_Power]
}
carmarket <- rbind(na.omit(carmarket.HP.DK, 'Horse_Power'), carmarket.HP.FULL)

carmarket <- carmarket[Price != 5086500,]

#Price Quantiles
quant <- quantile(carmarket$Price, seq(0, 1, 0.2))
carmarket <- carmarket %>%
    mutate(Price_Group = case_when(
        Price < quant[2] ~ "Very Low",
        Price < quant[3] ~ "Low",
        Price < quant[4] ~ "Medium",
        Price < quant[5] ~ "High",
        TRUE ~ "Very High"
    )) %>%
    mutate(Price_Group = factor(Price_Group, levels = c("Very Low", "Low", "Medium", "High", "Very High")))

#Get Price_Group list
Price_Group <- carmarket %>% 
    distinct(Price_Group) %>% 
    unlist(.)
names(Price_Group) <- NULL
Price_Group <- as.character(Price_Group)

#Get CCM list
CCM <- carmarket %>% 
    distinct(CCM) %>% 
    unlist(.)
names(CCM) <- NULL
CCM <- as.character(CCM)

#Get Horse_Power list
Horse_Power <- carmarket %>% 
    distinct(Horse_Power) %>% 
    unlist(.)
names(Horse_Power) <- NULL
Horse_Power <- as.character(Horse_Power)

#Get brand list
brand <- carmarket %>% 
    distinct(Brand) %>% 
    unlist(.)
names(brand) <- NULL
brand <- as.character(brand)

#Get color list
color <- carmarket %>% 
    distinct(Color) %>% 
    unlist(.)
names(color) <- NULL
color <- as.character(color)

#Get gear list
gear <- carmarket %>% 
    distinct(Gear) %>% 
    unlist(.)
names(gear) <- NULL
gear <- as.character(gear)

#Get fuel list
fuel <- carmarket %>% 
    distinct(Fuel_Type) %>% 
    unlist(.)
names(fuel) <- NULL
fuel <- as.character(fuel)

#Get body list
body <- carmarket %>% 
    distinct(Body_Type) %>% 
    unlist(.)
names(body) <- NULL
body <- as.character(body)

#Get status list
status <- carmarket %>% 
    distinct(Seller_Status) %>% 
    unlist(.)
names(status) <- NULL
status <- as.character(status)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Overview of Car Market 2020"),
    
    h5("Group 2: Can AYTORE, Taha BAYAZ, Ebru GECICI, Nezihe Nazli GUL, Talha UNLU, Mustafa KESER\n"),
    
    sidebarLayout(
        sidebarPanel(

            selectInput("brand", 
                        label = "Car Brand:", 
                        choices = c("All", brand), 
                        selected = c("Audi","BMW","Mercedes"),
                        multiple = TRUE),            
            
            sliderInput("price", 
                        label = "Price:",
                        min = min(carmarket$Price), 
                        max = max(carmarket$Price), 
                        value = c(min,max),
                        ticks = TRUE,
                        sep = ""),
            
            selectInput("Price_Group", 
                        label = "Price Group:", 
                        choices = c("All", Price_Group), 
                        selected = "All", 
                        multiple = TRUE), 
            
            sliderInput("age", 
                        label = "Model Year:",
                        min = min(carmarket$Model_Year), 
                        max = max(carmarket$Model_Year), 
                        value = c(2000,2020),
                        ticks = FALSE,
                        sep = ""),
            
            selectInput("color", 
                        label = "Color:", 
                        choices = c("All", color), 
                        selected = "All", 
                        multiple = TRUE),      
            
            selectInput("gear", 
                        label = "Gear Type:", 
                        choices = c("All", gear), 
                        selected = "All", 
                        multiple = TRUE), 
            
            selectInput("fuel", 
                        label = "Fuel Type:", 
                        choices = c("All", fuel), 
                        selected = "All", 
                        multiple = TRUE), 
            
            selectInput("body", 
                        label = "Body Type:", 
                        choices = c("All", body), 
                        selected = "All", 
                        multiple = TRUE), 
            
            selectInput("CCM", 
                        label = "CCM:", 
                        choices = c("All", CCM), 
                        selected = "All", 
                        multiple = TRUE), 
            
            selectInput("Horse_Power", 
                        label = "Horse Power:", 
                        choices = c("All", Horse_Power), 
                        selected = "All", 
                        multiple = TRUE), 
            
            sliderInput("Kilometers", 
                        label = "Kilometers:",
                        min = min(carmarket$Kilometers), 
                        max = max(carmarket$Kilometers), 
                        value = c(min(carmarket$Kilometers), median(carmarket$Kilometers)),
                        ticks = FALSE,
                        sep = ""),
            
            selectInput("status", 
                        label = "Status:", 
                        choices = c("All", status), 
                        selected = "All", 
                        multiple = TRUE),
            
            actionButton("show_options", 
                         label = "Show/Update Number of Options")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Graph", plotOutput("plot"), textOutput("options")),
                tabPanel("Table", dataTableOutput("table")),
                tabPanel("Density", plotOutput("density"))
            )
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    rv_options <- eventReactive(input$show_options, {
        
        option <- carmarket %>% 
            filter(Model_Year >= input$age[1] & Model_Year <= input$age[2]) %>%
            filter(Kilometers >= input$Kilometers[1] & Kilometers <= input$Kilometers[2]) %>%
            filter(Price >= input$price[1] & Price <= input$price[2])
        
        if(!("All" %in% input$Price_Group)){
            option <- option %>% filter(Price_Group %in% input$Price_Group)
        }
        if(!("All" %in% input$CCM)){
            option <- option %>% filter(CCM %in% input$CCM)
        }
        if(!("All" %in% input$Horse_Power)){
            option <- option %>% filter(Horse_Power %in% input$Horse_Power)
        }
        if(!("All" %in% input$brand)){
            option <- option %>% filter(Brand %in% input$brand)
        }
        if(!("All" %in% input$color)){
            option <- option %>% filter(Color %in% input$color)
        }
        if(!("All" %in% input$gear)){
            option <- option %>% filter(Gear %in% input$gear)
        }
        if(!("All" %in% input$fuel)){
            option <- option %>% filter(Fuel_Type %in% input$fuel)
        }
        if(!("All" %in% input$body)){
            option <- option %>% filter(Body_Type %in% input$body)
        }
        if(!("All" %in% input$status)){
            option <- option %>% filter(Seller_Status %in% input$status)
        }
        
        paste("You have", option %>% summarise(count=n()), "options.")
    })
    
    output$options <- renderText({
        rv_options() 
    })
    
    output$plot <- renderPlot({
        
        plot_df <- carmarket %>% 
            filter(Model_Year >= input$age[1] & Model_Year <= input$age[2]) %>%
            filter(Kilometers >= input$Kilometers[1] & Kilometers <= input$Kilometers[2]) %>%
            filter(Price >= input$price[1] & Price <= input$price[2])
        
        if(!("All" %in% input$Price_Group)){
            plot_df <- plot_df %>% filter(Price_Group %in% input$Price_Group)
        }
        if(!("All" %in% input$CCM)){
            plot_df <- plot_df %>% filter(CCM %in% input$CCM)
        }
        if(!("All" %in% input$Horse_Power)){
            plot_df <- plot_df %>% filter(Horse_Power %in% input$Horse_Power)
        }
        if(!("All" %in% input$brand)){
            plot_df <- plot_df %>% filter(Brand %in% input$brand)
        }
        if(!("All" %in% input$color)){
            plot_df <- plot_df %>% filter(Color %in% input$color)
        }
        if(!("All" %in% input$gear)){
            plot_df <- plot_df %>% filter(Gear %in% input$gear)
        }
        if(!("All" %in% input$fuel)){
            plot_df <- plot_df %>% filter(Fuel_Type %in% input$fuel)
        }
        if(!("All" %in% input$body)){
            plot_df <- plot_df %>% filter(Body_Type %in% input$body)
        }
        if(!("All" %in% input$status)){
            plot_df <- plot_df %>% filter(Seller_Status %in% input$status)
        }
        
        plot_df <- plot_df %>%
            mutate(avg_price = mean(Price))
        
        ggplot(plot_df, aes(x = Seller, y = avg_price, fill = Seller)) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            theme(legend.position = "bottom", 
                  legend.title = element_text(colour="red", size=13, face="bold"),
                  legend.text = element_text(size=12, face="italic"),
                  plot.title = element_text(hjust = 0.5, size=24),
                  axis.title=element_text(size=13,face="bold"),
                  strip.text.x = element_text(size = 12, colour = "blue3")) +
            scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ".", decimal.mark = ",")) +
            facet_wrap(vars(Brand), scales = "free") +
            labs(title = "Average Price by Seller", x = "Seller", y = "Average Price", fill = "Seller:") 
    })
    
    output$table <- renderDataTable({
        
        table_df <- carmarket %>% 
            filter(Model_Year >= input$age[1] & Model_Year <= input$age[2]) %>%
            filter(Kilometers >= input$Kilometers[1] & Kilometers <= input$Kilometers[2]) %>%
            filter(Price >= input$price[1] & Price <= input$price[2])
        
        if(!("All" %in% input$Price_Group)){
            table_df <- table_df %>% filter(Price_Group %in% input$Price_Group)
        }
        if(!("All" %in% input$CCM)){
            table_df <- table_df %>% filter(CCM %in% input$CCM)
        }
        if(!("All" %in% input$Horse_Power)){
            table_df <- table_df %>% filter(Horse_Power %in% input$Horse_Power)
        }
        if(!("All" %in% input$brand)){
            table_df <- table_df %>% filter(Brand %in% input$brand)
        }
        if(!("All" %in% input$color)){
            table_df <- table_df %>% filter(Color %in% input$color)
        }
        if(!("All" %in% input$gear)){
            table_df <- table_df %>% filter(Gear %in% input$gear)
        }
        if(!("All" %in% input$fuel)){
            table_df <- table_df %>% filter(Fuel_Type %in% input$fuel)
        }
        if(!("All" %in% input$body)){
            table_df <- table_df %>% filter(Body_Type %in% input$body)
        }
        if(!("All" %in% input$status)){
            table_df <- table_df %>% filter(Seller_Status %in% input$status)
        }
        
        table_df %>%
            arrange(Price)
    })
    
    output$density <- renderPlot({
        
        price_density <- carmarket %>% 
            filter(Model_Year >= input$age[1] & Model_Year <= input$age[2]) %>%
            filter(Kilometers >= input$Kilometers[1] & Kilometers <= input$Kilometers[2]) %>%
            filter(Price >= input$price[1] & Price <= input$price[2])
        
        if(!("All" %in% input$Price_Group)){
            price_density <- price_density %>% filter(Price_Group %in% input$Price_Group)
        }
        if(!("All" %in% input$CCM)){
            price_density <- price_density %>% filter(CCM %in% input$CCM)
        }
        if(!("All" %in% input$Horse_Power)){
            price_density <- price_density %>% filter(Horse_Power %in% input$Horse_Power)
        }
        if(!("All" %in% input$brand)){
            price_density <- price_density %>% filter(Brand %in% input$brand)
        }
        if(!("All" %in% input$color)){
            price_density <- price_density %>% filter(Color %in% input$color)
        }
        if(!("All" %in% input$gear)){
            price_density <- price_density %>% filter(Gear %in% input$gear)
        }
        if(!("All" %in% input$fuel)){
            price_density <- price_density %>% filter(Fuel_Type %in% input$fuel)
        }
        if(!("All" %in% input$body)){
            price_density <- price_density %>% filter(Body_Type %in% input$body)
        }
        if(!("All" %in% input$status)){
            price_density <- price_density %>% filter(Seller_Status %in% input$status)
        }
        
        ggplot(price_density, aes(x = Price, fill = Brand)) +
            geom_density(alpha=0.5) +
            theme_minimal() +
            theme(legend.position = "bottom", 
                  legend.title = element_text(colour="red", size=13, face="bold"),
                  legend.text = element_text(size=12, face="italic"),
                  plot.title = element_text(hjust = 0.5, size=24),
                  axis.title=element_text(size=13,face="bold"),
                  strip.text.x = element_text(size = 12, colour = "blue3")) +
            labs(title = "Price Density", x = "Price", y = "Density", fill = "Brand") +
            scale_x_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ".", decimal.mark = ","))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

