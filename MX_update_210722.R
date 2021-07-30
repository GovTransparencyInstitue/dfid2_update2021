library(tidyverse)
`%notin%` = Negate(`%in%`)
today<-Sys.Date()

#--------load compranet data
raw<-readRDS("compranet_clean.rds")

df_compranet <- raw %>% 
  mutate(
    ca_year=str_extract(FECHA_CELEBRACION, "[:digit:]{4}")) %>% 
  filter(ca_year%in%c("2018","2019","2020","2021")) %>% 
  rename(
    anb_name=DEPENDENCIA,
    anb_type=GOBIERNO,
    ca_procedure_int=CARACTER,
    tender_id=CODIGO_EXPEDIENTE,
    w_country=SIGLAS_PAIS,
    anb_id_detail=CLAVEUC,
    #tender_id=CODIGO_EXPEDIENTE,
    id_tender=NUMERO_PROCEDIMIENTO,
    ca_procedure=TIPO_PROCEDIMIENTO,
    ca_procedure_form=FORMA_PROCEDIMIENTO,
    #ten_title=TITULO_EXPEDIENTE,
    ca_procedure_nat=PLANTILLA_EXPEDIENTE,
    cft_date=FECHA_APERTURA_PROPOSICIONES,
    ca_type=TIPO_CONTRATACION,
    #rec_com_ca_id=CODIGO_CONTRATO,
    ca_start_date=FECHA_INICIO,
    ca_end_date=FECHA_FIN,
    ca_contract_value=IMPORTE_CONTRATO,
    aw_curr=MONEDA,
    ca_status=ESTATUS_CONTRATO,
    ca_sign_date=FECHA_CELEBRACION,
    #rec_com_aw_sup_id=FOLIO_RUPC,
    w_name=PROVEEDOR_CONTRATISTA,
    w_id= RFC,
    ten_title=TITULO_CONTRATO,
    ca_sign_date=FECHA_CELEBRACION,
    ca_start_date=FECHA_INICIO
    ) %>% 
  mutate (
    ca_procedure=tolower(ca_procedure),
    ca_procedure=str_remove_all(ca_procedure, "[:punct:]"),
    ca_procedure=stringi::stri_trans_general(ca_procedure, "Latin-ASCII"),
    #General string cleaning protocol MX names: START
    w_name=str_remove_all(w_name, "S.C."),
    w_name=tolower(w_name),
    w_name=stringi::stri_trans_general(w_name, "Latin-ASCII"),
    w_name=str_remove_all(w_name, "[:punct:]"),
    w_name=str_remove_all(w_name, "[:digit:]"),
    w_name=str_remove_all(w_name, "[:blank:]"),
    w_name=str_remove_all(w_name, "sderldecv|sadecv|srl|sderlmi|sprderi|snc"),
    anb_name=str_remove_all(anb_name, "S.C."),
    anb_name=tolower(anb_name),
    anb_name=stringi::stri_trans_general(anb_name, "Latin-ASCII"),
    anb_name=str_remove_all(anb_name, "[:punct:]"),
    anb_name=str_remove_all(anb_name, "[:digit:]"),
    anb_name=str_remove_all(anb_name, "[:blank:]"),
    anb_name=str_remove_all(anb_name, "sderldecv|sadecv|srl|sderlmi|sprderi|snc"),
    #General string cleaning protocol MX names: END
    country = "Mexico"
  ) 

df_compranet <- df_compranet %>% 
  mutate(
  ca_status=case_when(
    ca_status=="Activo"~"active",
    ca_status=="Expirado"~"terminated",
    ca_status=="Terminado"~"terminated",
    TRUE~""
  ),
  ca_type=case_when(
    ca_type=="Adquisiciones"~ "goods",
    ca_type=="ADQUISICIONES"~ "goods",
    ca_type=="Arrendamientos"~ "services",
    ca_type=="Internacional"~ "", 
    ca_type=="Nacional"~ "",
    ca_type=="Servicios relacionados con la OP"~ "services",
    ca_type=="Obra Pública"~ "works",
    ca_type=="Servicios"~ "services",
    ca_type=="Servicios Relacionados con la OP"~ "services",
    TRUE~""
  ),
  ca_procedure = case_when(
    ca_procedure=="invitacion a cuando menos 3 personas"~ "invitation (3 entities)",
    ca_procedure=="adjudicacion directa federal"~ "direct contracting",
    ca_procedure=="licitacion publica"~ "open auction",
    ca_procedure=="adjudicacion directa"~ "direct contracting",
    ca_procedure=="proyecto de convocatoria"~ "other",
    ca_procedure=="otro"~ "other",
    ca_procedure=="otras contrataciones"~ "other",
    ca_procedure=="contrato entre entes publicos"~ "between public entities",
    TRUE~""
  ),
  ca_procedure_ocds=case_when(
    grepl("*Directa*", df_compranet$ca_procedure_nat)~"direct",
    grepl("*Invitac*", df_compranet$ca_procedure_nat)~"selective",
    grepl("*Publica*", df_compranet$ca_procedure_nat)~ "open",
    grepl("*Licitaci*", df_compranet$ca_procedure_nat)~ "open",
    grepl("*Privada*", df_compranet$ca_procedure_nat)~ "direct",
    grepl("*externo*", df_compranet$ca_procedure_nat)~ "limited",
    TRUE~""
  ))

#---------Add Ghost Companies
df_ghost <- readRDS("ghost_mx.rds")

df_new <- left_join(df_compranet, df_ghost)

#---------Add Sanctioned Companies
df_sanct <- readRDS("sanctioned_mx.rds")

df_new <- left_join(df_new, df_sanct)


#---------Add N Bidders

df_bidders <- read_csv("Input/extracted.csv") %>% 
  mutate(tender_id=as.character(tender_id)) %>% 
  rename(ca_nrbid=numberOfTenderers,
         aw_item_class_id=item_classification_id) %>% 
  select(tender_id,ca_nrbid,aw_item_class_id)

#How many are string matched?
table(unique(df_bidders$tender_id)%in%unique(df_compranet$tender_id))

df_new <- left_join(df_new,df_bidders)
#Important note tender_id and id_tender are different! 

#-------------------------------------------------
#missing: calculate corr_ghost and corr_sanct 
#-------------------------------------------------

#----------Row Bind DF New to DF Old

#From GTI Server in SESNA_WB
df_old<- read_csv("Input/wb_mx_sesna_191125_csv.csv")
#Drop Non Overlapping Variables
df_new<-df_new[,names(df_new)[names(df_new)%in%names(df_old)]]
#Drop Duplicated Tender IDs
df_new <- df_new %>% filter(id_tender%notin%unique(df_old$id_tender))
#Add empty variables
a<-names(df_old)[names(df_old)%notin%names(df_new)]
df_new[a]<-NA
df_new <- df_new %>% 
  select(names(df_old))
#Check if names are equal
table(names(df_new)==names(df_old))
#Row Bind old and new
df_updated <- rbind(df_new,df_old)

#save as CSV
write_csv(df_updated,sprintf("MX_update_%s.csv",today))
