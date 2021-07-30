library(tidyverse)
`%notin%` <- Negate(`%in%`)

#New Data

#Directly from SECOP Portal (included)
raw <- read_csv("SECOP_Integrado_2018-21.csv")

df_new <- raw %>% 
  mutate(ca_year=str_extract(`Fecha de Firma del Contrato`,"20[:digit:]{2}"),
         tender_country="CO",
         bid_priceCurrency="COP") %>% 
  rename(
    tender_id=`Numero de Proceso`,
    bidder_name=`Nom Raz Social Contratista`,
    buyer_name=`Nombre de la Entidad`,
    source=`URL Contrato`,
    bid_price=`Valor Contrato`,
    ca_id=`Numero del Contrato`,
    tender_desc=`Objeto a Contratar`,
    buyer_id=`NIT de la Entidad`,
    buyer_buyertype=`Nivel Entidad`,
    tender_supplytype=`Tipo de Contrato`,
    ten_status=`Estado del Proceso`,
    ca_date_signed=`Fecha de Firma del Contrato`,
    #Important note: it is unclear if "Modalidad de Contratacion" corresponds to "tender_proceduretype"
    tender_proceduretype=`Modalidad de Contratación`
    ) %>% 
  mutate(
    ca_date_signed=str_extract(ca_date_signed,"[:digit:]+/[:digit:]+/[:digit:]+"),
    buyer_id=ifelse(buyer_id=="No registra",NA,buyer_id),
    tender_supplytype=case_when(
      tender_supplytype=="Acuerdo Marco"~"SERVICES",
      tender_supplytype=="Agregación de Demanda"~"SUPPLIES",
      tender_supplytype=="Arrendamiento"~"SERVICES",
      tender_supplytype=="Comodato "~"SERVICES",
      tender_supplytype=="Compraventa"~"SUPPLIES",
      tender_supplytype=="Concesión"~"SERVICES",
      tender_supplytype=="Consultoría"~"SERVICES",
      tender_supplytype=="Crédito"~"SERVICES",
      tender_supplytype=="Fiducia"~"SUPPLIES",
      tender_supplytype=="Interventoría"~"SERVICES",
      tender_supplytype=="No definido"~"",
      tender_supplytype=="Obra"~"WORKS",
      tender_supplytype=="Otro Tipo de Contrato"~"SERVICES",
      tender_supplytype=="Suministro"~"SUPPLIES",
      TRUE~""),
    ten_status=case_when(
      ten_status=="Adjudicado"~"Planned",
      ten_status=="Borrador"~"Planning",
      ten_status=="Celebrado"~"Active",
      ten_status=="Convocado"~"Planned",
      ten_status=="Liquidado"~"Complete",
      ten_status=="Terminado Anormalmente después de Convocado"~"Cancelled",
      ten_status=="Terminado sin Liquidar"~"Unsuccessful",
      TRUE~""),
    buyer_buyertype=case_when(
      buyer_buyertype=="NACIONAL"~"NATIONAL_AGENCY",
      buyer_buyertype=="TERRITORIAL"~"REGIONAL_AGENCY",
      TRUE~"")
  )

df_new[df_new==""]<-NA

#Old Data

#From GTI FTP Sever 0:/08akkiGH/WB_portals_2020/data/portal_datasets/CO/
df_old <- readr::read_delim("CO_mod_ind.csv", 
                     ";", escape_double = FALSE, trim_ws = TRUE)


#Row bind New and Old

df_new <- df_new %>% 
  filter(tender_id%notin%unique(df_old$tender_id))

a<-names(df_new)[names(df_new)%notin%names(df_old)]
df_old[a]<-NA

b<-names(df_old)[names(df_old)%notin%names(df_new)]
df_new[b]<-NA

df_new <- df_new %>% 
  select(names(df_old))

table(names(df_new)==names(df_old))

df_update <- rbind(df_old,df_new)

#Save File
write_csv(df_update, "CO_secop_update.csv")

