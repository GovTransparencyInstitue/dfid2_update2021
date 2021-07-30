#SECOP_II

#set directory
setwd("C:/Users/Bertold/Downloads/columbia-extra")
getwd()

#checking current memory limit
memory.limit()
#setting a bigger one
memory.limit(30000)

install.packages('readr')
library(readr)

#SECOP_II Procesos
df0 <- read_csv('SECOP_II_-_Procesos.csv')

#rename column names as the ones we have are impossible to use for further processing
names(df0) <- c('buyer_name', 'buyer_id','buyer_addid','drop1','drop2', 'ocid', 'ten_title', 'ten_descr', 'drop3', 'publ_date', 'drop4', 'drop5','drop6','drop7','drop8','drop9', 'ten_vamount', 'ten_proc_method','ten_durInDays', 'drop10', 'ten_endDate', 'drop11', 'aw_startDate', 'drop12','drop13','drop14','drop15','drop16','drop17','drop18','drop19','drop20', 'nr_ofTenderers', 'drop21','ten_status', 'drop22', 'drop23','aw_id', 'aw_sup_addid', 'aw_date', 'aw_vamount', 'drop24','aw_sup_name','aw_sup_id', 'ten_item_class_id','drop25', 'ten_proc_cat', 'drop26', 'ten_item_addid', 'drop27', 'drop28','drop29', 'drop30','ten_doc_url')


#drop unnessecary 
df0$drop1 <- NULL
df0$drop2 <- NULL
df0$drop3 <- NULL
df0$drop4 <- NULL
df0$drop5 <- NULL
df0$drop6 <- NULL
df0$drop7 <- NULL
df0$drop8 <- NULL
df0$drop9 <- NULL
df0$drop10 <- NULL
df0$drop11 <- NULL
df0$drop12 <- NULL
df0$drop13 <- NULL
df0$drop14 <- NULL
df0$drop15 <- NULL
df0$drop16 <- NULL
df0$drop17 <- NULL
df0$drop18 <- NULL
df0$drop19 <- NULL
df0$drop20 <- NULL
df0$drop21 <- NULL
df0$drop22 <- NULL
df0$drop23 <- NULL
df0$drop24 <- NULL
df0$drop25 <- NULL
df0$drop26 <- NULL
df0$drop27 <- NULL
df0$drop28 <- NULL
df0$drop29 <- NULL
df0$drop30 <- NULL

names(df0)

#basic information on the database 
class(df0)
dim(df0)

#replace ND in database with NA
df0[df0 == "ND"] <- NA
df0[df0 == "Adjudicado"] <- "awarded"
df0[df0 == "No Adjudicado"] <- "not awarded"
df0[df0 == 0] <- NA


#missing values
sapply(df0, function(df0) sum(is.na(df0)))

#check and set types of variables 
sapply(df0, class)
#change variable types and format where necessary

df0$buyer_id <- as.integer(df0$buyer_id)

#dates are converted to characters, remove patterns
#ten_endDate
df0$publ_date_x <- df0$publ_date
df0$publ_date_x <- gsub(df0$publ_date_x, pattern = " 12:00:00 AM", replacement = "")
df0$publ_date_x <- gsub(df0$publ_date_x, pattern = "/", replacement = "-")
df0$publ_date_x <- as.Date.factor(df0$publ_date_x, "%m-%d-%Y")
typeof(df0$publ_date_x)
df0$publ_date_x <- as.Date(df0$publ_date_x)
class(df0$publ_date_x)


#ten_endDate
df0$ten_endDate_x <- df0$ten_endDate
df0$ten_endDate_x <- gsub(df0$ten_endDate_x, pattern = " 12:00:00 AM", replacement = "")
df0$ten_endDate_x <- gsub(df0$ten_endDate_x, pattern = "/", replacement = "-")
df0$ten_endDate_x <- as.Date.factor(df0$ten_endDate_x, "%m-%d-%Y")
typeof(df0$ten_endDate_x)
df0$ten_endDate_x <- as.Date(df0$ten_endDate_x)
class(df0$ten_endDate_x)


#aw_startDate
df0$aw_startDate_x <- df0$aw_startDate
df0$aw_startDate_x <- gsub(df0$aw_startDate_x, pattern = " 12:00:00 AM", replacement = "")
df0$aw_startDate_x <- gsub(df0$aw_startDate_x, pattern = "/", replacement = "-")
df0$aw_startDate_x <- as.Date.factor(df0$aw_startDate_x, "%m-%d-%Y")
typeof(df0$aw_startDate_x)
df0$aw_startDate_x <- as.Date(df0$aw_startDate_x)
class(df0$aw_startDate_x)
#now it looks good


#aw_date
df0$aw_date_x <- df0$aw_date
df0$aw_date_x <- gsub(df0$aw_date_x, pattern = " 12:00:00 AM", replacement = "")
df0$aw_date_x <- gsub(df0$aw_date_x, pattern = "/", replacement = "-")
df0$aw_date_x <- as.Date.factor(df0$aw_date_x, "%m-%d-%Y")
typeof(df0$aw_date_x)
df0$aw_date_x <- as.Date(df0$aw_date_x)
class(df0$aw_date_x)
#now it looks good


#remove outliers
summary(df0$ten_vamount)
#45648 missing

df0$ten_vamount[df0$ten_vamount < 1055406] <- NA
df0$ten_vamount[df0$ten_vamount > 35165654708313] <- NA

sum(is.na(df0$ten_vamount))
#48172 
mean(is.na(df0$ten_vamount))
#33%

summary(df0$aw_vamount)
#105100
df0$aw_vamount[df0$aw_vamount < 1055406] <- NA
df0$aw_vamount[df0$aw_vamount > 35165654708313] <- NA

sum(is.na(df0$aw_vamount))
#106662
mean(is.na(df0$aw_vamount))
#72.4%

#date outliers (if contract start date is later than end date)
df0$ca_startDate[df0$ca_startDate > df0$ca_endDate] <- NA
sum(is.na(df0$ca_startDate))
mean(is.na(df0$ca_startDate))
#5 cases

#remove completely duplicated rows
df0 <- df0[!duplicated(df0),]

#recheck missing values
sapply(df0, function(df0) sum(is.na(df0)))
mean(is.na(df0$buyer_id))
#5.1%
mean(is.na(df0$aw_startDate))
#62.3%
mean(is.na(df0$aw_date))
#71.2%
mean(is.na(df0$ten_endDate))
#61.5%
mean(is.na(df0$aw_vamount))
#72.4%
mean(is.na(df0$ten_vamount))
#33%
mean(is.na(df0$nr_ofTenderers))
#69%
length(which(df0$aw_id != "not awarded"))
#42322; 29%
length(which(df0$aw_sup_id != "not awarded"))
#42320; 29%

#recode values of status, method, category
table(df0$ten_proc_method)

df0$ten_proc_method[grepl("*Concurso de m閞itos*", df0$ten_proc_method)] <- "selective"
df0$ten_proc_method[grepl("*irecta*", df0$ten_proc_method)] <- "direct"
df0$ten_proc_method[grepl("*r間imen especial*", df0$ten_proc_method)] <- "selective"
df0$ten_proc_method[grepl("*subasta*", df0$ten_proc_method)] <- "open"
df0$ten_proc_method[grepl("*Acuerdo Marco de Precios*", df0$ten_proc_method)] <- "limited"
df0$ten_proc_method[grepl("*M韓ima Cuant韆*", df0$ten_proc_method)] <- "selective"
df0$ten_proc_method[grepl("*M韓ima cuant韆*", df0$ten_proc_method)] <- "selective"
df0$ten_proc_method[grepl("*Menor Cuant*", df0$ten_proc_method)] <- "selective"
df0$ten_proc_method[grepl("*Solicitud*", df0$ten_proc_method)] <- "selective"
df0$ten_proc_method[grepl("*con sobre cerrado*", df0$ten_proc_method)] <- "direct"
df0$ten_proc_method[grepl("*p鷅lica*", df0$ten_proc_method)] <- "open"

table(df0$ten_proc_method)

#ten_status
table(df0$ten_status)
df0$ten_status[df0$ten_status=="awarded"] <- "planned"
df0$ten_status[df0$ten_status=="Abierto"] <- "planned"
df0$ten_status[df0$ten_status=="Evaluaci髇"] <- "active"
df0$ten_status[df0$ten_status=="Publicado"] <- "planning"
df0$ten_status[df0$ten_status=="Seleccionado"] <- "active"
df0$ten_status[df0$ten_status=="Suspendido"] <- "unsuccessful"

#ten_proc_cat
table(df0$ten_proc_cat)
df0$ten_proc_cat[df0$ten_proc_cat=="27 - Otros servicios"] <- "service"
df0$ten_proc_cat[df0$ten_proc_cat=="Arrendamiento Muebles"] <- "service"
df0$ten_proc_cat[df0$ten_proc_cat=="Concesi髇"] <- "service"
df0$ten_proc_cat[df0$ten_proc_cat=="Interventor韆"] <- "service"
df0$ten_proc_cat[df0$ten_proc_cat=="Servicios de aprovisionamiento"] <- "service"
df0$ten_proc_cat[df0$ten_proc_cat=="Acuerdo Marco"] <- "service"
df0$ten_proc_cat[df0$ten_proc_cat=="Comisi髇"] <- "service"
df0$ten_proc_cat[df0$ten_proc_cat=="Consultor韆"] <- "service"
df0$ten_proc_cat[df0$ten_proc_cat=="Obra"] <- "work"
df0$ten_proc_cat[df0$ten_proc_cat=="Suministros"] <- "goods"
df0$ten_proc_cat[df0$ten_proc_cat=="Alquiler de edificios"] <- "service"
df0$ten_proc_cat[df0$ten_proc_cat=="Compraventa"] <- "goods"
df0$ten_proc_cat[df0$ten_proc_cat=="Empr閟tito"] <- "service"
df0$ten_proc_cat[df0$ten_proc_cat=="Seguros"] <- "service"
df0$ten_proc_cat[df0$ten_proc_cat=="Ventas de muebles"] <- "goods"


df0$publ_date <- NULL
df0$ten_endDate <- NULL
df0$aw_startDate <- NULL
df0$aw_date <- NULL

#check unique ids
length(unique(df0$ocid))
sum(is.na(df0$ocid))

#check common variables
names(df0)
names(secopII_c)

#save file
write.csv(df0, file = "secopII_proc_c.csv", row.names = FALSE)


#############################################
#SECOP II Contratos
df1 <- read_csv('SECOP_II_-_Contratos.csv')
View(df1)

#rename variables
names(df1) <- c('buyer_name', 'buyer_id','ocid','drop1','drop2', 'ca_id','ca_startDate','ca_endDate', 'ca_impl_startDate','date', 'drop3', 'aw_sup_name', 'drop4', 'ca_status', 'drop5','drop6', 'drop7','drop8', 'drop9', 'ca_vamount', 'drop10', 'drop11', 'drop12', 'drop13', 'drop14', 'drop15', 'drop16', 'drop17', 'ten_proc_method', 'ca_date_signed', 'drop18', 'drop19', 'drop20')


#drop unnessecary 
df1$drop1 <- NULL
df1$drop2 <- NULL
df1$drop3 <- NULL
df1$drop4 <- NULL
df1$drop5 <- NULL
df1$drop6 <- NULL
df1$drop7 <- NULL
df1$drop8 <- NULL
df1$drop9 <- NULL
df1$drop10 <- NULL
df1$drop11 <- NULL
df1$drop12 <- NULL
df1$drop13 <- NULL
df1$drop14 <- NULL
df1$drop15 <- NULL
df1$drop16 <- NULL
df1$drop17 <- NULL
df1$drop18 <- NULL
df1$drop19 <- NULL
df1$drop20 <- NULL

names(df1)

#basic information on the database 
class(df1)
dim(df1)

#missing values
sapply(df1, function(df1) sum(is.na(df1)))

#check and set types of variables 
sapply(df1, class)
#change variable types and format where necessary


#dates are converted to characters, remove patterns
#ca_startDate
df1$ca_startDate <- gsub(df1$ca_startDate, pattern = " 12:00:00 AM", replacement = "")
df1$ca_startDate <- gsub(df1$ca_startDate, pattern = "/", replacement = "-")
df1$ca_startDate <- as.Date.factor(df1$ca_startDate, "%m-%d-%Y")
typeof(df1$ca_startDate)
df1$ca_startDate <- as.Date(df1$ca_startDate)
class(df1$ca_startDate)
#now it looks good

#ca_endDate
df1$ca_endDate <- gsub(df1$ca_endDate, pattern = " 12:00:00 AM", replacement = "")
df1$ca_endDate <- gsub(df1$ca_endDate, pattern = "/", replacement = "-")
df1$ca_endDate <- as.Date.factor(df1$ca_endDate, "%m-%d-%Y")
typeof(df1$ca_endDate)
df1$ca_endDate <- as.Date(df1$ca_endDate)
class(df1$ca_endDate)

#ca_impl_startDate
df1$ca_impl_startDate <- gsub(df1$ca_impl_startDate, pattern = " 12:00:00 AM", replacement = "")
df1$ca_impl_startDate <- gsub(df1$ca_impl_startDate, pattern = "/", replacement = "-")
df1$ca_impl_startDate <- as.Date.factor(df1$ca_impl_startDate, "%m-%d-%Y")
typeof(df1$ca_impl_startDate)
df1$ca_impl_startDate <- as.Date(df1$ca_impl_startDate)
class(df1$ca_impl_startDate)

#date
df1$date <- gsub(df1$date, pattern = " 12:00:00 AM", replacement = "")
df1$date <- gsub(df1$date, pattern = "/", replacement = "-")
df1$date <- as.Date.factor(df1$date, "%m-%d-%Y")
typeof(df1$date)
df1$date <- as.Date(df1$date)
class(df1$date)


#ca_date_signed
df1$ca_date_signed <- gsub(df1$ca_date_signed, pattern = " 12:00:00 AM", replacement = "")
df1$ca_date_signed <- gsub(df1$ca_date_signed, pattern = "/", replacement = "-")
df1$ca_date_signed <- as.Date.factor(df1$ca_date_signed, "%m-%d-%Y")
typeof(df1$ca_date_signed)
df1$ca_date_signed <- as.Date(df1$ca_date_signed)
class(df1$ca_date_signed)


df1$buyer_id <- as.integer(df1$buyer_id)

#check missing values
sapply(df1, function(df1) sum(is.na(df1)))
#missings mainly in date variables, some in id and name

#remove outliers
summary(df1$ca_vamount)

df1$ca_vamount[df1$ca_vamount < 1055406] <- NA
df1$ca_vamount[df1$ca_vamount > 35165654708313] <- NA

sum(is.na(df1$ca_vamount))
#11975
mean(is.na(df1$ca_vamount))
#8%

#date outliers (if contract start date is later than end date)
df1$ca_startDate[df1$ca_startDate > df1$ca_endDate] <- NA
sum(is.na(df1$ca_startDate))
mean(is.na(df1$ca_startDate))
#5 cases

#remove completely duplicated rows
df1 <- df1[!duplicated(df1),]

#recheck missing values
sapply(df1, function(df1) sum(is.na(df1)))
mean(is.na(df1$buyer_id))
#5.1%
mean(is.na(df1$ca_startDate))
#10%
mean(is.na(df1$ca_endDate))
#10%
mean(is.na(df1$ca_impl_startDate))
#34%
mean(is.na(df1$date))
#34%
mean(is.na(df1$aw_sup_name))
#5%
mean(is.na(df1$ca_date_signed))
#25.5%


#translate contract status into OCDS terms
table(df1$ca_status)

df1$ca_status[df1$ca_status=="Borrador"] <- "pending"
df1$ca_status[df1$ca_status=="Cancelado"] <- "cancelled"
df1$ca_status[df1$ca_status=="cedido"] <- "pending"
df1$ca_status[df1$ca_status=="Cerrado"] <- "terminated"
df1$ca_status[df1$ca_status=="confirmado"] <- "pending"
df1$ca_status[df1$ca_status=="En aprobaci髇"] <- "pending"
df1$ca_status[df1$ca_status=="enviado Proveedor"] <- "pending"
df1$ca_status[df1$ca_status=="Firmado"] <- "active"
df1$ca_status[df1$ca_status=="Modificado"] <- "pending"
df1$ca_status[df1$ca_status=="Prorrogado"] <- "active"
df1$ca_status[df1$ca_status=="Suspendido"] <- "terminated"
df1$ca_status[df1$ca_status=="terminado"] <- "terminated"

table(df1$ca_status)


#check if there is a unique id
length(unique(df1$ocid))
#not unique

length(unique(df1$ca_id))
#unique


#save .csv
write.csv(df1, file = "secopII_c.csv", row.names = FALSE)
#################################

#merging SECOP II sub files
#ocid is the common variable by which files can be merged


#select only necessary variables from secop II procesos
secopIIp_sel <- secopII_proc_c[c("ocid", "aw_sup_id", "aw_id", "aw_vamount", "nr_ofTenderers", "ten_vamount", "aw_startDate_x", "ten_endDate_x")]
       
secopII_final <- merge(secopII_c, secopIIp_sel, by = c("ocid"), all.x = TRUE)

sapply(secopII_final, function(secopII_final) sum(is.na(secopII_final)))
mean(is.na(secopII_final$ten_endDate_x))
#55%
mean(is.na(secopII_final$aw_startDate_x))
#55%
mean(is.na(secopII_final$aw_sup_id))
#0.3%
mean(is.na(secopII_final$aw_sup_name))
#4.2%
mean(is.na(secopII_final$aw_id))
#0.3%
mean(is.na(secopII_final$aw_vamount))
#65%
mean(is.na(secopII_final$ca_vamount))
#10%
mean(is.na(secopII_final$buyer_id))
#4.7%
mean(is.na(secopII_final$buyer_name))
#0%
mean(is.na(secopII_final$nr_ofTenderers))
#60.4%
mean(is.na(secopII_final$ten_proc_method))
#0%
mean(is.na(secopII_final$ten_vamount))
#31.4%
mean(is.na(secopII_final$ca_status))
#0%

secopII_c <- read_csv('secopII_c.csv')

#rename common column names
secopII_c$ca_startDate_x <- secopII_c$ca_startDate
secopII_c$ca_endDate_x <- secopII_c$ca_endDate
secopII_c$ca_dateSigned_x <- secopII_c$ca_date_signed

secopII_c$ca_startDate <- NULL
secopII_c$ca_endDate <- NULL
secopII_c$ca_date_signed <- NULL

#recode ten_procMethod
table(secopII_c$ten_proc_method)
secopII_c$ten_proc_method[grepl("*abierto*", secopII_c$ten_proc_method)] <- "open"
secopII_c$ten_proc_method[grepl("*irecta*", secopII_c$ten_proc_method)] <- "direct"
secopII_c$ten_proc_method[grepl("*especial*", secopII_c$ten_proc_method)] <- "selective"
secopII_c$ten_proc_method[grepl("*subasta*", secopII_c$ten_proc_method)] <- "open"
secopII_c$ten_proc_method[grepl("*Acuerdo Marco de Precios*", secopII_c$ten_proc_method)] <- "limited"
secopII_c$ten_proc_method[grepl("*Abreviada*", secopII_c$ten_proc_method)] <- "selective"
secopII_c$ten_proc_method[grepl("*abreviada subasta*", secopII_c$ten_proc_method)] <- "open"
secopII_c$ten_proc_method[grepl("*con sobre cerrado*", secopII_c$ten_proc_method)] <- "direct"
secopII_c$ten_proc_method[grepl("*blica*", secopII_c$ten_proc_method)] <- "open"
secopII_c$ten_proc_method[grepl("*cuant*", secopII_c$ten_proc_method)] <- "selective"

write.csv(secopII_c, file = "secopII_c.csv", row.names = FALSE)

#save .csv
write.csv(secopII_final, file = "secopII_final.csv", row.names = FALSE)

#########################################################
#SECOP I

install.packages('data.table')
library(data.table)

#SECOP I is a very huge file to import at once, it is more advisable to process data in chunks, to do it properly count the number of rows without opening the whole file
length(count.fields("C:/Users/Bertold/Downloads/columbia-extra/SECOP_I-real.csv", sep = ","))
#6508707

#it does not read the ten_item_class_id properly, so it read it as a character variable
df2 <- fread("C:/Users/Bertold/Downloads/columbia-extra/SECOP_I-real.csv",  header = TRUE, sep = ",", nrows = 200000)
View(df2)

names(df2) <- c('UID', 'drop1', 'drop2', 'drop3', 'buyer_level', 'buyer_name', 'buyer_id', 'buyer_addid', 'drop4', 'ten_proc_method', 'ten_status', 'drop5', 'drop6', 'drop7', 'ten_item_class_id', 'drop8', 'ten_item_class_descr', 'ten_proc_cat', 'drop9', 'drop10', 'drop11', 'drop12', 'record_nr', 'ocid', 'ca_id', 'ten_vamount', 'drop13', 'ten_item_descr_l1', 'drop14', 'drop15', 'drop16', 'drop17', 'aw_id', 'aw_sup_ident_scheme', 'aw_sup_ident_id', 'aw_sup_name', 'drop18', 'drop19', 'drop20', 'drop21', 'ca_dateSigned', 'ca_startDate', 'ca_durInDays', 'drop22', 'drop23', 'drop24', 'ca_endDate', 'drop25', 'ca_vamount', 'drop26', 'drop27', 'drop28', 'plan_budg_projID', 'plan_budg_proj', 'drop29', 'drop30', 'drop31', 'drop32', 'drop33', 'ca_vcurr', 'drop34', 'drop35')


#drop unnecessary variables
df2$drop1 <- NULL
df2$drop2 <- NULL
df2$drop3 <- NULL
df2$drop4 <- NULL
df2$drop5 <- NULL
df2$drop6 <- NULL
df2$drop7 <- NULL
df2$drop8 <- NULL
df2$drop9 <- NULL
df2$drop10 <- NULL
df2$drop11 <- NULL
df2$drop12 <- NULL
df2$drop13 <- NULL
df2$drop14 <- NULL
df2$drop15 <- NULL
df2$drop16 <- NULL
df2$drop17 <- NULL
df2$drop18 <- NULL
df2$drop19 <- NULL
df2$drop20 <- NULL
df2$drop21 <- NULL
df2$drop22 <- NULL
df2$drop23 <- NULL
df2$drop24 <- NULL
df2$drop25 <- NULL
df2$drop26 <- NULL
df2$drop27 <- NULL
df2$drop28 <- NULL
df2$drop29 <- NULL
df2$drop30 <- NULL
df2$drop31 <- NULL
df2$drop32 <- NULL
df2$drop33 <- NULL
df2$drop34 <- NULL
df2$drop35 <- NULL


#check and set types of variables 
sapply(df2, class)

#change ten_item_class_id as it displays ids incorrectly
df2$ten_item_class_id <- as.character(df2$ten_item_class_id)

#change 0, 'No registra', 'No definido' and empty cells to NA
df2[df2 == "No registra"] <- NA
df2[df2 == "No Definido"] <- NA
df2[df2 == "No definido"] <- NA
df2[df2 == "No Definida"] <- NA
df2[df2 == 0] <- NA
df2[df2 == ""] <- NA

#dates are converted to factor, and loses many values with simple as-Date function
#ca_startDate
df2$ca_startDate_x <- df2$ca_startDate

df2$ca_startDate_x <- as.character(df2$ca_startDate_x)
df2$ca_startDate_x <- as.Date(df2$ca_startDate_x, format = "%d/%m/%Y")

class(df2$ca_startDate_x)
#now it looks good
df2$ca_startDate <- NULL
sum(is.na(df2$ca_startDate_x))


#ca_endDate

df2$ca_endDate_x <- df2$ca_endDate

df2$ca_endDate_x <- as.character(df2$ca_endDate_x)
df2$ca_endDate_x <- as.Date(df2$ca_endDate_x, format = "%d/%m/%Y")

class(df2$ca_endDate_x)
#now it looks good
sum(is.na(df2$ca_endDate_x))

df2$ca_endDate <-NULL


#ca_dateSigned

df2$ca_dateSigned_x <- df2$ca_dateSigned

df2$ca_dateSigned_x <- as.character(df2$ca_dateSigned_x)
df2$ca_dateSigned_x <- as.Date(df2$ca_dateSigned_x, format = "%d/%m/%Y")

class(df2$ca_dateSigned_x)
#now it looks good
sum(is.na(df2$ca_dateSigned_x))

df2$ca_dateSigned <- NULL


#recode ten_status
table(df2$ten_status)
df2$ten_status[df2$ten_status=="Adjudicado"] <- "planned"
df2$ten_status[df2$ten_status=="Celebrado"] <- "active"
df2$ten_status[df2$ten_status=="Descartado"] <- "cancelled"
df2$ten_status[df2$ten_status=="Finalizado el plazo para manifestaciones de inter茅s"] <- "withdrawn"
df2$ten_status[df2$ten_status=="Lista Corta"] <- "planning"
df2$ten_status[df2$ten_status=="Terminado Anormalmente despu閟 de Convocado"] <- "cancelled"
df2$ten_status[df2$ten_status=="Borrador"] <- "planning"
df2$ten_status[df2$ten_status=="Convocado"] <- "planned"
df2$ten_status[df2$ten_status=="Expresi髇 de Inter閟"] <- "planning"
df2$ten_status[df2$ten_status=="Liquidado"] <- "complete"
df2$ten_status[df2$ten_status=="Publicaci贸n para manifestaciones de inter茅s"] <- "planning"
df2$ten_status[df2$ten_status=="Terminado sin Liquidar"] <- "unsuccessful"

#recode ten_proc_cat
table(df2$ten_proc_method)
df2$ten_proc_method[df2$ten_proc_method=="Asociaci髇 P鷅lico Privada"] <- "direct"
df2$ten_proc_method[df2$ten_proc_method=="Concurso de M閞itos Abierto"] <- "selective"
df2$ten_proc_method[df2$ten_proc_method=="Concurso de M閞itos con Lista Corta"] <- "selective"
df2$ten_proc_method[df2$ten_proc_method=="Concurso de M閞itos con Lista Multiusos"] <- "selective"
df2$ten_proc_method[df2$ten_proc_method=="Contrataci髇 Directa (Ley 1150 de 2007)"] <- "direct"
df2$ten_proc_method[df2$ten_proc_method=="Contrataci髇 M韓ima Cuant韆"] <- "selective"
df2$ten_proc_method[df2$ten_proc_method=="Contratos y convenios con m醩 de dos partes"] <- "limited"
df2$ten_proc_method[df2$ten_proc_method=="Licitaci髇 obra p鷅lica"] <- "open"
df2$ten_proc_method[df2$ten_proc_method=="Licitaci髇 P鷅lica"] <- "open"
df2$ten_proc_method[df2$ten_proc_method=="R間imen Especial"] <- "selective"
df2$ten_proc_method[df2$ten_proc_method=="Selecci髇 Abreviada de Menor Cuant韆 (Ley 1150 de 2007)"] <- "selective"
df2$ten_proc_method[df2$ten_proc_method=="Selecci髇 Abreviada del literal h del numeral 2 del art韈ulo 2 de la Ley 1150 de 2007"] <- "selective"
df2$ten_proc_method[df2$ten_proc_method=="Selecci髇 Abreviada servicios de Salud"] <- "selective"
df2$ten_proc_method[df2$ten_proc_method=="Subasta"] <- "open"

table(part2$ten_proc_method)


#recode ten_proc_cat
table(df2$ten_proc_cat)
df2$ten_proc_cat[df2$ten_proc_cat=="Acuerdo Marco"] <- "service"
df2$ten_proc_cat[df2$ten_proc_cat=="Compraventa"] <- "goods"
df2$ten_proc_cat[df2$ten_proc_cat=="Fiducia"] <- "goods"
df2$ten_proc_cat[df2$ten_proc_cat=="Otro Tipo de Contrato"] <- "service"
df2$ten_proc_cat[df2$ten_proc_cat=="Agregaci髇 de Demanda"] <- "goods"
df2$ten_proc_cat[df2$ten_proc_cat=="Concesi髇"] <- "service"
df2$ten_proc_cat[df2$ten_proc_cat=="Interventor韆"] <- "service"
df2$ten_proc_cat[df2$ten_proc_cat=="Prestaci髇 de Servicios"] <- "service"
df2$ten_proc_cat[df2$ten_proc_cat=="Arrendamiento"] <- "service"
df2$ten_proc_cat[df2$ten_proc_cat=="Consultor韆"] <- "service"
df2$ten_proc_cat[df2$ten_proc_cat=="Suministro"] <- "goods"
df2$ten_proc_cat[df2$ten_proc_cat=="Comodato"] <- "service"
df2$ten_proc_cat[df2$ten_proc_cat=="Cr閐ito"] <- "service"
df2$ten_proc_cat[df2$ten_proc_cat=="Obra"] <- "work"


#ten_item_descr_l1
table(df2$ten_item_descr_l1)
df2$ten_item_descr_l1[grepl("*Material Vivo Animal y Vegetal*", df2$ten_item_descr_l1)] <- "goods"
df2$ten_item_descr_l1[grepl("*Maquinaria, Herramientas, Equipo Industrial*", df2$ten_item_descr_l1)] <- "goods"
df2$ten_item_descr_l1[grepl("*Productos de Uso Final*", df2$ten_item_descr_l1)] <- "goods"
df2$ten_item_descr_l1[grepl("*Terrenos, Edificios, Estructuras*", df2$ten_item_descr_l1)] <- "work"
df2$ten_item_descr_l1[grepl("*Materias Primas*", df2$ten_item_descr_l1)] <- "goods"
df2$ten_item_descr_l1[grepl("*Componentes y Suministros*", df2$ten_item_descr_l1)] <- "goods"
df2$ten_item_descr_l1[grepl("*Servicios*", df2$ten_item_descr_l1)] <- "services"


#check missing values
sapply(df2, function(df2) sum(is.na(df2)))
mean(is.na(df2$buyer_level))
#close to 0
mean(is.na(df2$buyer_id))
#15%
mean(is.na(df2$ca_id))
#16%
mean(is.na(df2$ten_vamount))
#0.5%
mean(is.na(df2$aw_id))
#16%
mean(is.na(df2$aw_sup_ident_scheme)) 
#16%
mean(is.na(df2$aw_sup_ident_id))
#16%
mean(is.na(df2$aw_sup_name))
#16%
mean(is.na(df2$ca_dateSigned_x))
#16%
mean(is.na(df2$ca_startDate_x))
#16%
mean(is.na(df2$ca_durInDays))
#16%
mean(is.na(df2$ca_endDate_x))
#16%
mean(is.na(df2$ca_vamount))
#24.3%
mean(is.na(df2$plan_budg_projID))
#almost completely missing (83%) -> drop
mean(is.na(df2$plan_budg_proj))
#almost completely missing (83%) -> drop
mean(is.na(df2$ca_vcurr))
#16%

df2$plan_budg_projID <- NULL
df2$plan_budg_proj <- NULL

#check overlaps in ca_id and ocid, they look very similar, some are identical
idoverlap <- df2$ocid %in% df2$ca_id
table(idoverlap)
#51% overlap in this sample

#remove outliers
summary(df2$ca_vamount)
#16512 missings
df2$ca_vamount[df2$ca_vamount < 1055406] <- NA
df2$ca_vamount[df2$ca_vamount > 35165654708313] <- NA

sum(is.na(df2$ca_vamount))
#48689
mean(is.na(df2$ca_vamount))
#24.3%

df2$ten_vamount[df2$ten_vamount < 1055406] <- NA
df2$ten_vamount[df2$ten_vamount > 35165654708313] <- NA
sum(is.na(df2$ten_vamount))
#19887
mean(is.na(df2$ten_vamount))
#10%

#date outliers (if contract start date is later than end date)

df2$ca_startDate_x[df2$ca_startDate_x > df2$ca_endDate_x] <- NA
sum(is.na(df2$ca_startDate_x))
#69091 missings
mean(is.na(df2$ca_startDate_x))
#34.5% -> bit more than double

#check unique ids
length(unique(df2$UID))
#unique

length(unique(df2$ocid))
length(unique(df2$ca_id))
#not unique, not reliable

df2 <- df2[c("UID", "aw_id", "aw_sup_ident_id", "aw_sup_ident_scheme", "aw_sup_name", "buyer_addid", "buyer_id", "buyer_level", "buyer_name", "ca_dateSigned_x", "ca_durInDays", "ca_startDate_x", "ca_endDate_x", "ca_id", "ca_vamount", "ca_vcurr", "ocid", "record_nr", "ten_item_class_descr", "ten_item_class_id", "ten_item_descr_l1", "ten_proc_cat", "ten_proc_method", "ten_status", "ten_vamount")]


#save files
write.csv(df2, file = "secopI_200_c.csv", row.names = FALSE)
################################
#secop I chunk2 (201-1.2e6)


install.packages("LaF")
library('LaF')
huge_file <- 'C:/Users/Bertold/Downloads/columbia-extra/SECOP_I-real.csv'

#detect a data model for the file:
model <- detect_dm_csv(huge_file, sep=",", header=F, fill = T)
df.laf <- laf_open(model)
goto(df.laf, 2e5)
part2 <- next_block(df.laf,nrows=1e6)


names(part2) <- c('UID', 'drop1', 'drop2', 'drop3', 'buyer_level', 'buyer_name', 'buyer_id', 'buyer_addid', 'drop4', 'ten_proc_method', 'ten_status', 'drop5', 'drop6', 'drop7', 'ten_item_class_id', 'drop8', 'ten_item_class_descr', 'ten_proc_cat', 'drop9', 'drop10', 'drop11', 'drop12', 'record_nr', 'ocid', 'ca_id', 'ten_vamount', 'drop13', 'ten_item_descr_l1', 'drop14', 'drop15', 'drop16', 'drop17', 'aw_id', 'aw_sup_ident_scheme', 'aw_sup_ident_id', 'aw_sup_name', 'drop18', 'drop19', 'drop20', 'drop21', 'ca_dateSigned', 'ca_startDate', 'ca_durInDays', 'drop22', 'drop23', 'drop24', 'ca_endDate', 'drop25', 'ca_vamount', 'drop26', 'drop27', 'drop28', 'plan_budg_projID', 'plan_budg_proj', 'drop29', 'drop30', 'drop31', 'drop32', 'drop33', 'ca_vcurr', 'drop34', 'drop35')


#drop unnecessary variables
part2$drop1 <- NULL
part2$drop2 <- NULL
part2$drop3 <- NULL
part2$drop4 <- NULL
part2$drop5 <- NULL
part2$drop6 <- NULL
part2$drop7 <- NULL
part2$drop8 <- NULL
part2$drop9 <- NULL
part2$drop10 <- NULL
part2$drop11 <- NULL
part2$drop12 <- NULL
part2$drop13 <- NULL
part2$drop14 <- NULL
part2$drop15 <- NULL
part2$drop16 <- NULL
part2$drop17 <- NULL
part2$drop18 <- NULL
part2$drop19 <- NULL
part2$drop20 <- NULL
part2$drop21 <- NULL
part2$drop22 <- NULL
part2$drop23 <- NULL
part2$drop24 <- NULL
part2$drop25 <- NULL
part2$drop26 <- NULL
part2$drop27 <- NULL
part2$drop28 <- NULL
part2$drop29 <- NULL
part2$drop30 <- NULL
part2$drop31 <- NULL
part2$drop32 <- NULL
part2$drop33 <- NULL
part2$drop34 <- NULL
part2$drop35 <- NULL

#delete first two rows (as they are the last two rows of the previous part)
part2 <- part2[-(1:2),]


#change 0, 'No registra', 'No definido' and empty cells to NA
part2[part2 == "No registra"] <- NA
part2[part2 == "No Definido"] <- NA
part2[part2 == "No definido"] <- NA
part2[part2 == "No Definida"] <- NA
part2[part2 == 0] <- NA
part2[part2 == ""] <- NA

#check missing values
sapply(part2, function(part2) sum(is.na(part2)))

#check and set types of variables 
sapply(part2, class)
#change variable types 

#correct types
cols <- c(2,6,7,8,10,11,15,17,25,26,27)
part2[,cols] = lapply(part2[,cols], as.character)


cols <- c(14,24)
part2[,cols] = lapply(part2[,cols], as.numeric)

part2$ca_durInDays <- as.integer(part2$ca_durInDays)

#dates are converted to factor, and loses many values with simple as-Date function
#ca_startDate
part2$ca_startDate_x <- part2$ca_startDate

part2$ca_startDate_x <- as.character(part2$ca_startDate_x)
part2$ca_startDate_x <- as.Date(part2$ca_startDate_x, format = "%d/%m/%Y")

class(part2$ca_startDate_x)
#now it looks good
part2$ca_startDate <- part2$ca_startDate_x
sum(is.na(part2$ca_startDate_x))




#ca_endDate

part2$ca_endDate_x <- part2$ca_endDate

part2$ca_endDate_x <- as.character(part2$ca_endDate_x)
part2$ca_endDate_x <- as.Date(part2$ca_endDate_x, format = "%d/%m/%Y")

class(part2$ca_endDate_x)
#now it looks good
sum(is.na(part2$ca_endDate_x))

part2$ca_endDate <-part2$ca_endDate_X


#ca_dateSigned
part2$ca_dateSigned_x <- part2$ca_dateSigned

part2$ca_dateSigned_x <- as.character(part2$ca_dateSigned_x)
part2$ca_dateSigned_x <- as.Date(part2$ca_dateSigned_x, format = "%d/%m/%Y")


class(part2$ca_dateSigned_x)
#now it looks good
sum(is.na(part2$ca_dateSigned_x))

part2$ca_dateSigned <-part2$ca_dateSigned_X



#recode ten_status
table(part2$ten_status)
part2$ten_status[part2$ten_status=="Adjudicado"] <- "planned"
part2$ten_status[part2$ten_status=="Celebrado"] <- "active"
part2$ten_status[part2$ten_status=="Descartado"] <- "cancelled"
part2$ten_status[part2$ten_status=="Finalizado el plazo para manifestaciones de inter茅s"] <- "withdrawn"
part2$ten_status[part2$ten_status=="Lista Corta"] <- "planning"
part2$ten_status[part2$ten_status=="Terminado Anormalmente despu茅s de Convocado"] <- "cancelled"
part2$ten_status[part2$ten_status=="Borrador"] <- "planning"
part2$ten_status[part2$ten_status=="Convocado"] <- "planned"
part2$ten_status[part2$ten_status=="Expresi贸n de Inter茅s"] <- "planning"
part2$ten_status[part2$ten_status=="Liquidado"] <- "complete"
part2$ten_status[part2$ten_status=="Publicaci贸n para manifestaciones de inter茅s"] <- "planning"
part2$ten_status[part2$ten_status=="Terminado sin Liquidar"] <- "unsuccessful"

#recode ten_proc_cat
table(part2$ten_proc_method)
part2$ten_proc_method[part2$ten_proc_method=="Asociaci贸n P煤blico Privada"] <- "direct"
part2$ten_proc_method[part2$ten_proc_method=="Concurso de M茅ritos Abierto"] <- "selective"
part2$ten_proc_method[part2$ten_proc_method=="Concurso de M茅ritos con Lista Corta"] <- "selective"
part2$ten_proc_method[part2$ten_proc_method=="Contrataci贸n Directa (Ley 1150 de 2007)"] <- "direct"
part2$ten_proc_method[part2$ten_proc_method=="Iniciativa Privada sin recursos p煤blicos"] <- "direct"
part2$ten_proc_method[part2$ten_proc_method=="Licitaci贸n P煤blica"] <- "open"
part2$ten_proc_method[part2$ten_proc_method=="R茅gimen Especial"] <- "selective"
part2$ten_proc_method[part2$ten_proc_method=="Selecci贸n Abreviada servicios de Salud"] <- "selective"
part2$ten_proc_method[part2$ten_proc_method=="Subasta"] <- "open"

part2$ten_proc_method1 <- part2$ten_proc_method
part2$ten_proc_method1[grepl("*nima Cuant*", part2$ten_proc_method1)] <- "limited"
part2$ten_proc_method1[grepl("*Abreviada de Menor*", part2$ten_proc_method1)] <- "limited"
part2$ten_proc_method1[grepl("*Abreviada del literal h del*", part2$ten_proc_method1)] <- "limited"

part2$ten_proc_method <- part2$ten_proc_method1

table(part2$ten_proc_method)



#recode ten_proc_cat
table(part2$ten_proc_cat)
part2$ten_proc_cat[part2$ten_proc_cat=="Acuerdo Marco"] <- "service"
part2$ten_proc_cat[part2$ten_proc_cat=="Compraventa"] <- "goods"
part2$ten_proc_cat[part2$ten_proc_cat=="Fiducia"] <- "goods"
part2$ten_proc_cat[part2$ten_proc_cat=="Otro Tipo de Contrato"] <- "service"
part2$ten_proc_cat[part2$ten_proc_cat=="Agregaci贸n de Demanda"] <- "goods"
part2$ten_proc_cat[part2$ten_proc_cat=="Concesi贸n"] <- "service"
part2$ten_proc_cat[part2$ten_proc_cat=="Interventor胊"] <- "service"
part2$ten_proc_cat[part2$ten_proc_cat=="Prestaci贸n de Servicios"] <- "service"
part2$ten_proc_cat[part2$ten_proc_cat=="Arrendamiento"] <- "service"
part2$ten_proc_cat[part2$ten_proc_cat=="Consultor胊"] <- "service"
part2$ten_proc_cat[part2$ten_proc_cat=="Suministro"] <- "goods"
part2$ten_proc_cat[part2$ten_proc_cat=="Comodato"] <- "service"
part2$ten_proc_cat[part2$ten_proc_cat=="Cr茅dito"] <- "service"
part2$ten_proc_cat[part2$ten_proc_cat=="Obra"] <- "work"

part2$ten_proc_cat1 <- part2$ten_proc_cat
part2$ten_proc_cat1[grepl("*Interventor*", part2$ten_proc_cat1)] <- "service"
part2$ten_proc_cat1[grepl("*Consultor*", part2$ten_proc_cat1)] <- "service"
table(part2$ten_proc_cat1)

part2$ten_proc_cat <- part2$ten_proc_cat1


#ten_item_descr_l1
table(part2$ten_item_descr_l1)
part2$ten_item_descr_l1[grepl("*Material Vivo Animal y Vegetal*", part2$ten_item_descr_l1)] <- "goods"
part2$ten_item_descr_l1[grepl("*Maquinaria, Herramientas, Equipo Industrial*", part2$ten_item_descr_l1)] <- "goods"
part2$ten_item_descr_l1[grepl("*Productos de Uso Final*", part2$ten_item_descr_l1)] <- "goods"
part2$ten_item_descr_l1[grepl("*Terrenos, Edificios, Estructuras*", part2$ten_item_descr_l1)] <- "work"
part2$ten_item_descr_l1[grepl("*Materias Primas*", part2$ten_item_descr_l1)] <- "goods"
part2$ten_item_descr_l1[grepl("*Componentes y Suministros*", part2$ten_item_descr_l1)] <- "goods"
part2$ten_item_descr_l1[grepl("*Servicios*", part2$ten_item_descr_l1)] <- "services"



#check missing values
sapply(part2, function(part2) sum(is.na(part2)))
mean(is.na(part2$buyer_level))
#close to 0
mean(is.na(part2$buyer_id))
#17%
mean(is.na(part2$ca_id))
#15.3%
mean(is.na(part2$ten_vamount))
#0%
mean(is.na(part2$aw_id))
#15.3%
mean(is.na(part2$aw_sup_ident_scheme)) 
#15.3%
mean(is.na(part2$aw_sup_ident_id))
#15.3%
mean(is.na(part2$aw_sup_name))
#15.3%
mean(is.na(part2$ca_dateSigned_x))
#64%
mean(is.na(part2$ca_startDate_x))
#16%
mean(is.na(part2$ca_durInDays))
#16%
mean(is.na(part2$ca_endDate_x))
#16%
mean(is.na(part2$ca_vamount))
#24.3%
mean(is.na(part2$ca_vcurr))
#16%

mean(is.na(part2$plan_budg_projID))
#almost completely missing (83%) -> drop
mean(is.na(part2$plan_budg_proj))
#almost completely missing (83%) -> drop

part2$plan_budg_projID <- NULL
part2$plan_budg_proj <- NULL

#check overlaps in ca_id and ocid, they look very similar, some are identical
idoverlap <- part2$ocid %in% part2$ca_id
table(idoverlap)
#51% overlap in this sample

#remove outliers
summary(part2$ca_vamount)
#16512 missings
part2$ca_vamount[part2$ca_vamount < 1055406] <- NA
part2$ca_vamount[part2$ca_vamount > 35165654708313] <- NA

sum(is.na(part2$ca_vamount))
#48689
mean(is.na(part2$ca_vamount))
#24.3%

part2$ten_vamount[part2$ten_vamount < 1055406] <- NA
part2$ten_vamount[part2$ten_vamount > 35165654708313] <- NA
sum(is.na(part2$ten_vamount))
#19887
mean(is.na(part2$ten_vamount))
#10%

#date outliers (if contract start date is later than end date)

part2$ca_startDate_x[part2$ca_startDate_x > part2$ca_endDate_x] <- NA
sum(is.na(part2$ca_startDate_x))
#153559 
mean(is.na(part2$ca_startDate_x))
#15.3%

#check unique ids
length(unique(part2$UID))
#unique

length(unique(part2$ocid))
length(unique(part2$ca_id))
#not unique, not reliable

part2 <- part2[c("UID", "aw_id", "aw_sup_ident_id", "aw_sup_ident_scheme", "aw_sup_name", "buyer_addid", "buyer_id", "buyer_level", "buyer_name", "ca_dateSigned_x", "ca_durInDays", "ca_startDate_x", "ca_endDate_x", "ca_id", "ca_vamount", "ca_vcurr", "ocid", "record_nr", "ten_item_class_descr", "ten_item_class_id", "ten_item_descr_l1", "ten_proc_cat", "ten_proc_method", "ten_status", "ten_vamount")]


#save files
write.csv(part2, file = "secopI_part2_c.csv", row.names = FALSE)

################################
#secop I chunk3 (1.2e6-2.2e6)


huge_file <- 'C:/Users/Bertold/Downloads/columbia-extra/SECOP_I-real.csv'

#detect a data model for the file:
model <- detect_dm_csv(huge_file, sep=",", header=F, fill = T)
df.laf <- laf_open(model)
goto(df.laf, 1.2e6)
part3 <- next_block(df.laf,nrows=1e6)


names(part3) <- c('UID', 'drop1', 'drop2', 'drop3', 'buyer_level', 'buyer_name', 'buyer_id', 'buyer_addid', 'drop4', 'ten_proc_method', 'ten_status', 'drop5', 'drop6', 'drop7', 'ten_item_class_id', 'drop8', 'ten_item_class_descr', 'ten_proc_cat', 'drop9', 'drop10', 'drop11', 'drop12', 'record_nr', 'ocid', 'ca_id', 'ten_vamount', 'drop13', 'ten_item_descr_l1', 'drop14', 'drop15', 'drop16', 'drop17', 'aw_id', 'aw_sup_ident_scheme', 'aw_sup_ident_id', 'aw_sup_name', 'drop18', 'drop19', 'drop20', 'drop21', 'ca_dateSigned', 'ca_startDate', 'ca_durInDays', 'drop22', 'drop23', 'drop24', 'ca_endDate', 'drop25', 'ca_vamount', 'drop26', 'drop27', 'drop28', 'plan_budg_projID', 'plan_budg_proj', 'drop29', 'drop30', 'drop31', 'drop32', 'drop33', 'ca_vcurr', 'drop34', 'drop35')


#drop unnecessary variables
part3$drop1 <- NULL
part3$drop2 <- NULL
part3$drop3 <- NULL
part3$drop4 <- NULL
part3$drop5 <- NULL
part3$drop6 <- NULL
part3$drop7 <- NULL
part3$drop8 <- NULL
part3$drop9 <- NULL
part3$drop10 <- NULL
part3$drop11 <- NULL
part3$drop12 <- NULL
part3$drop13 <- NULL
part3$drop14 <- NULL
part3$drop15 <- NULL
part3$drop16 <- NULL
part3$drop17 <- NULL
part3$drop18 <- NULL
part3$drop19 <- NULL
part3$drop20 <- NULL
part3$drop21 <- NULL
part3$drop22 <- NULL
part3$drop23 <- NULL
part3$drop24 <- NULL
part3$drop25 <- NULL
part3$drop26 <- NULL
part3$drop27 <- NULL
part3$drop28 <- NULL
part3$drop29 <- NULL
part3$drop30 <- NULL
part3$drop31 <- NULL
part3$drop32 <- NULL
part3$drop33 <- NULL
part3$drop34 <- NULL
part3$drop35 <- NULL

#delete first two rows (as they are the last two rows of the previous part)
part3 <- part3[-(1:2),]

#change 0, 'No registra', 'No definido' and empty cells to NA
part3[part3 == "No registra"] <- NA
part3[part3 == "No Definido"] <- NA
part3[part3 == "No definido"] <- NA
part3[part3 == "No Definida"] <- NA
part3[part3 == 0] <- NA
part3[part3 == ""] <- NA



#check and set types of variables 
sapply(part3, class)
#it read all non-string data as factor 

#correct types
cols <- c(2,6,7,8,10,11,15,17,25,26,27)
part3[,cols] = lapply(part3[,cols], as.character)


cols <- c(14,24)
part3[,cols] = lapply(part3[,cols], as.numeric)

part3$ca_durInDays <- as.integer(part3$ca_durInDays)


#dates are converted to factor, and loses many values with simple as-Date function
#ca_startDate
part3$ca_startDate_x <- part3$ca_startDate

part3$ca_startDate_x <- as.character(part3$ca_startDate_x)
part3$ca_startDate_x <- as.Date(part3$ca_startDate_x, format = "%d/%m/%Y")

class(part3$ca_startDate_x)
#now it looks good
sum(is.na(part3$ca_startDate_x))

part3$ca_startDate <-NULL


#ca_endDate
part3$ca_endDate_x <- part3$ca_endDate
part3$ca_endDate_x <- as.character(part3$ca_endDate_x)
part3$ca_endDate_x <- as.Date(part3$ca_endDate_x, format = "%d/%m/%Y")

class(part3$ca_endDate_x)
#now it looks good
sum(is.na(part3$ca_endDate_x))

part3$ca_endDate <-part3$ca_endDate_X


#ca_dateSigned
part3$ca_dateSigned_x <- part3$ca_dateSigned
part3$ca_dateSigned_x <- as.character(part3$ca_dateSigned_x)
part3$ca_dateSigned_x <- as.Date(part3$ca_dateSigned_x, format = "%d/%m/%Y")

class(part3$ca_dateSigned_x)
#now it looks good
sum(is.na(part3$ca_dateSigned_x))

part3$ca_dateSigned <-part3$ca_dateSigned_X


#recode ten_status
table(part3$ten_status)
part3$ten_status[part3$ten_status=="Adjudicado"] <- "planned"
part3$ten_status[part3$ten_status=="Celebrado"] <- "active"
part3$ten_status[part3$ten_status=="Descartado"] <- "cancelled"
part3$ten_status[part3$ten_status=="Finalizado el plazo para manifestaciones de inter茅s"] <- "withdrawn"
part3$ten_status[part3$ten_status=="Lista Corta"] <- "planning"
part3$ten_status[part3$ten_status=="Terminado Anormalmente despu茅s de Convocado"] <- "cancelled"
part3$ten_status[part3$ten_status=="Borrador"] <- "planning"
part3$ten_status[part3$ten_status=="Convocado"] <- "planned"
part3$ten_status[part3$ten_status=="Expresi贸n de Inter茅s"] <- "planning"
part3$ten_status[part3$ten_status=="Liquidado"] <- "complete"
part3$ten_status[part3$ten_status=="Publicaci贸n para manifestaciones de inter茅s"] <- "planning"
part3$ten_status[part3$ten_status=="Terminado sin Liquidar"] <- "unsuccessful"

#recode ten_proc_cat
table(part3$ten_proc_method)
part3$ten_proc_method[part3$ten_proc_method=="Asociaci贸n P煤blico Privada"] <- "direct"
part3$ten_proc_method[part3$ten_proc_method=="Concurso de M茅ritos Abierto"] <- "selective"
part3$ten_proc_method[part3$ten_proc_method=="Concurso de M茅ritos con Lista Corta"] <- "selective"
part3$ten_proc_method[part3$ten_proc_method=="Contrataci贸n Directa (Ley 1150 de 2007)"] <- "direct"
part3$ten_proc_method[part3$ten_proc_method=="Iniciativa Privada sin recursos p煤blicos"] <- "direct"
part3$ten_proc_method[part3$ten_proc_method=="Licitaci贸n P煤blica"] <- "open"
part3$ten_proc_method[part3$ten_proc_method=="R茅gimen Especial"] <- "selective"
part3$ten_proc_method[part3$ten_proc_method=="Selecci贸n Abreviada servicios de Salud"] <- "selective"
part3$ten_proc_method[part3$ten_proc_method=="Subasta"] <- "open"

part3$ten_proc_method1 <- part3$ten_proc_method
part3$ten_proc_method1[grepl("*nima Cuant*", part3$ten_proc_method1)] <- "limited"
part3$ten_proc_method1[grepl("*Abreviada de Menor*", part3$ten_proc_method1)] <- "limited"
part3$ten_proc_method1[grepl("*Abreviada del literal h del*", part3$ten_proc_method1)] <- "limited"

part3$ten_proc_method <- part3$ten_proc_method1
part3$ten_proc_method1 <- NULL

table(part3$ten_proc_method)


#recode ten_proc_cat
table(part3$ten_proc_cat)
part3$ten_proc_cat[part3$ten_proc_cat=="Acuerdo Marco"] <- "services"
part3$ten_proc_cat[part3$ten_proc_cat=="Compraventa"] <- "goods"
part3$ten_proc_cat[part3$ten_proc_cat=="Fiducia"] <- "goods"
part3$ten_proc_cat[part3$ten_proc_cat=="Otro Tipo de Contrato"] <- "service"
part3$ten_proc_cat[part3$ten_proc_cat=="Agregaci贸n de Demanda"] <- "goods"
part3$ten_proc_cat[part3$ten_proc_cat=="Concesi贸n"] <- "service"
part3$ten_proc_cat[part3$ten_proc_cat=="Interventor胊"] <- "service"
part3$ten_proc_cat[part3$ten_proc_cat=="Prestaci贸n de Servicios"] <- "service"
part3$ten_proc_cat[part3$ten_proc_cat=="Arrendamiento"] <- "service"
part3$ten_proc_cat[part3$ten_proc_cat=="Consultor胊"] <- "service"
part3$ten_proc_cat[part3$ten_proc_cat=="Suministro"] <- "goods"
part3$ten_proc_cat[part3$ten_proc_cat=="Comodato"] <- "service"
part3$ten_proc_cat[part3$ten_proc_cat=="Cr茅dito"] <- "service"
part3$ten_proc_cat[part3$ten_proc_cat=="Obra"] <- "work"

part3$ten_proc_cat1 <- part3$ten_proc_cat
part3$ten_proc_cat1[grepl("*Interventor*", part3$ten_proc_cat1)] <- "service"
part3$ten_proc_cat1[grepl("*Consultor*", part3$ten_proc_cat1)] <- "service"
table(part3$ten_proc_cat1)

part3$ten_proc_cat <- part3$ten_proc_cat1
part3$ten_proc_cat1 <- NULL

#ten_item_descr_l1
table(part3$ten_item_descr_l1)
part3$ten_item_descr_l1[grepl("*Material Vivo Animal y Vegetal*", part3$ten_item_descr_l1)] <- "goods"
part3$ten_item_descr_l1[grepl("*Maquinaria, Herramientas, Equipo Industrial*", part3$ten_item_descr_l1)] <- "goods"
part3$ten_item_descr_l1[grepl("*Productos de Uso Final*", part3$ten_item_descr_l1)] <- "goods"
part3$ten_item_descr_l1[grepl("*Terrenos, Edificios, Estructuras*", part3$ten_item_descr_l1)] <- "work"
part3$ten_item_descr_l1[grepl("*Materias Primas*", part3$ten_item_descr_l1)] <- "goods"
part3$ten_item_descr_l1[grepl("*Componentes y Suministros*", part3$ten_item_descr_l1)] <- "goods"
part3$ten_item_descr_l1[grepl("*Servicios*", part3$ten_item_descr_l1)] <- "services"


#check missing values
sapply(part3, function(part3) sum(is.na(part3)))
mean(is.na(part3$buyer_level))
#close to 0
mean(is.na(part3$buyer_id))
#18.4%
mean(is.na(part3$ca_id))
#10.5%
mean(is.na(part3$ten_vamount))
#0.5%
mean(is.na(part3$aw_id))
#10.5%
mean(is.na(part3$aw_sup_ident_scheme)) 
#10.5%
mean(is.na(part3$aw_sup_ident_id))
#10.5%
mean(is.na(part3$aw_sup_name))
#10.5%
mean(is.na(part3$ca_dateSigned_x))
#10.5%
mean(is.na(part3$ca_startDate_x))
#10.5%
mean(is.na(part3$ca_durInDays))
#10.6%
mean(is.na(part3$ca_endDate_x))
#11%
mean(is.na(part3$ca_vamount))
#15%
mean(is.na(part3$ca_vcurr))
#10.6%

mean(is.na(part3$plan_budg_projID))
#drop
mean(is.na(part3$plan_budg_proj))
#drop

part3$plan_budg_projID <- NULL
part3$plan_budg_proj <- NULL


#remove outliers in contract vamount
summary(part3$ca_vamount)
#111538 missings
part3$ca_vamount[part3$ca_vamount < 1055406] <- NA
part3$ca_vamount[part3$ca_vamount > 35165654708313] <- NA

sum(is.na(part3$ca_vamount))
#150927
mean(is.na(part3$ca_vamount))
#15.1%

#ten_vamount
summary(part3$ten_vamount)
part3$ten_vamount[part3$ten_vamount < 1055406] <- NA
part3$ten_vamount[part3$ten_vamount > 35165654708313] <- NA
sum(is.na(part3$ten_vamount))
#52650
mean(is.na(part3$ten_vamount))
#no change

#date outliers (if contract start date is later than end date)

part3$ca_startDate_x[part3$ca_startDate_x > part3$ca_endDate_x] <- NA
sum(is.na(part3$ca_startDate_x))
#104754 
mean(is.na(part3$ca_startDate_x))
#10.5%

#check unique ids
length(unique(part3$UID))
#unique

part3<- part3[c("UID", "aw_id", "aw_sup_ident_id", "aw_sup_ident_scheme", "aw_sup_name", "buyer_addid", "buyer_id", "buyer_level", "buyer_name", "ca_dateSigned_x", "ca_durInDays", "ca_startDate_x", "ca_endDate_x", "ca_id", "ca_vamount", "ca_vcurr", "ocid", "record_nr", "ten_item_class_descr", "ten_item_class_id", "ten_item_descr_l1", "ten_proc_cat", "ten_proc_method", "ten_status", "ten_vamount")]


#save files
write.csv(part3, file = "secopI_part3_c.csv", row.names = FALSE)
##############################################
#secop I chunk4 (2.2e6-3.2e6)


huge_file <- 'C:/Users/Bertold/Downloads/columbia-extra/SECOP_I-real.csv'

#detect a data model for the file:
model <- detect_dm_csv(huge_file, sep=",", header=F, fill = T)
df.laf <- laf_open(model)
goto(df.laf, 2.2e6)
part4 <- next_block(df.laf,nrows=1e6)


names(part4) <- c('UID', 'drop1', 'drop2', 'drop3', 'buyer_level', 'buyer_name', 'buyer_id', 'buyer_addid', 'drop4', 'ten_proc_method', 'ten_status', 'drop5', 'drop6', 'drop7', 'ten_item_class_id', 'drop8', 'ten_item_class_descr', 'ten_proc_cat', 'drop9', 'drop10', 'drop11', 'drop12', 'record_nr', 'ocid', 'ca_id', 'ten_vamount', 'drop13', 'ten_item_descr_l1', 'drop14', 'drop15', 'drop16', 'drop17', 'aw_id', 'aw_sup_ident_scheme', 'aw_sup_ident_id', 'aw_sup_name', 'drop18', 'drop19', 'drop20', 'drop21', 'ca_dateSigned', 'ca_startDate', 'ca_durInDays', 'drop22', 'drop23', 'drop24', 'ca_endDate', 'drop25', 'ca_vamount', 'drop26', 'drop27', 'drop28', 'plan_budg_projID', 'plan_budg_proj', 'drop29', 'drop30', 'drop31', 'drop32', 'drop33', 'ca_vcurr', 'drop34', 'drop35')


#drop unnecessary variables
part4$drop1 <- NULL
part4$drop2 <- NULL
part4$drop3 <- NULL
part4$drop4 <- NULL
part4$drop5 <- NULL
part4$drop6 <- NULL
part4$drop7 <- NULL
part4$drop8 <- NULL
part4$drop9 <- NULL
part4$drop10 <- NULL
part4$drop11 <- NULL
part4$drop12 <- NULL
part4$drop13 <- NULL
part4$drop14 <- NULL
part4$drop15 <- NULL
part4$drop16 <- NULL
part4$drop17 <- NULL
part4$drop18 <- NULL
part4$drop19 <- NULL
part4$drop20 <- NULL
part4$drop21 <- NULL
part4$drop22 <- NULL
part4$drop23 <- NULL
part4$drop24 <- NULL
part4$drop25 <- NULL
part4$drop26 <- NULL
part4$drop27 <- NULL
part4$drop28 <- NULL
part4$drop29 <- NULL
part4$drop30 <- NULL
part4$drop31 <- NULL
part4$drop32 <- NULL
part4$drop33 <- NULL
part4$drop34 <- NULL
part4$drop35 <- NULL

####(remove duplicates in final file) delete first two rows (as they are the last two rows of the previous part)
part4 <- part4[-(1:2),]

#change 0, 'No registra', 'No definido' and empty cells to NA
part4[part4 == "No registra"] <- NA
part4[part4 == "No Definido"] <- NA
part4[part4 == "No definido"] <- NA
part4[part4 == "No Definida"] <- NA
part4[part4 == 0] <- NA
part4[part4 == ""] <- NA

#check and set types of variables 
sapply(part4, class)
#it read all non-string data as factor 

#correct types
cols <- c(2,6,7,8,10,11,15,17,25,26,27)
part4[,cols] = lapply(part4[,cols], as.character)


cols <- c(14,24)
part4[,cols] = lapply(part4[,cols], as.numeric)

part4$ca_durInDays <- as.integer(part4$ca_durInDays)


#dates are converted to factor, and loses many values with simple as-Date function
#ca_startDate

part4$ca_startDate_x <- part4$ca_startDate

part4$ca_startDate_x <- as.character(part4$ca_startDate_x)
part4$ca_startDate_x <- as.Date(part4$ca_startDate_x, format = "%d/%m/%Y")

class(part4$ca_startDate_x)
#now it looks good
sum(is.na(part4$ca_startDate_x))

part4$ca_startDate <- NULL


#ca_endDate
part4$ca_endDate_x <- part4$ca_endDate
part4$ca_endDate_x <- as.character(part4$ca_endDate_x)
part4$ca_endDate_x <- as.Date(part4$ca_endDate_x, format = "%d/%m/%Y")

class(part4$ca_endDate_x)
#now it looks good
sum(is.na(part4$ca_endDate_x))

part4$ca_endDate <-part4$ca_endDate_X


#ca_dateSigned
part4$ca_dateSigned_x <- part4$ca_dateSigned
part4$ca_dateSigned_x <- as.character(part4$ca_dateSigned_x)
part4$ca_dateSigned_x <- as.Date(part4$ca_dateSigned_x, format = "%d/%m/%Y")

class(part4$ca_dateSigned_x)
#now it looks good
sum(is.na(part4$ca_dateSigned_x))

part4$ca_dateSigned <-part4$ca_dateSigned_X



#recode ten_status
table(part4$ten_status)
part4$ten_status[part4$ten_status=="Adjudicado"] <- "planned"
part4$ten_status[part4$ten_status=="Celebrado"] <- "active"
part4$ten_status[part4$ten_status=="Descartado"] <- "cancelled"
part4$ten_status[part4$ten_status=="Finalizado el plazo para manifestaciones de inter茅s"] <- "withdrawn"
part4$ten_status[part4$ten_status=="Lista Corta"] <- "planning"
part4$ten_status[part4$ten_status=="Terminado Anormalmente despu茅s de Convocado"] <- "cancelled"
part4$ten_status[part4$ten_status=="Borrador"] <- "planning"
part4$ten_status[part4$ten_status=="Convocado"] <- "planned"
part4$ten_status[part4$ten_status=="Expresi贸n de Inter茅s"] <- "planning"
part4$ten_status[part4$ten_status=="Liquidado"] <- "complete"
part4$ten_status[part4$ten_status=="Publicaci贸n para manifestaciones de inter茅s"] <- "planning"
part4$ten_status[part4$ten_status=="Terminado sin Liquidar"] <- "unsuccessful"

#recode ten_proc_method
table(part4$ten_proc_method)
part4$ten_proc_method[part4$ten_proc_method=="Asociaci贸n P煤blico Privada"] <- "direct"
part4$ten_proc_method[part4$ten_proc_method=="Concurso de M茅ritos Abierto"] <- "selective"
part4$ten_proc_method[part4$ten_proc_method=="Concurso de M茅ritos con Lista Corta"] <- "selective"
part4$ten_proc_method[part4$ten_proc_method=="Contrataci贸n Directa (Ley 1150 de 2007)"] <- "direct"
part4$ten_proc_method[part4$ten_proc_method=="Iniciativa Privada sin recursos p煤blicos"] <- "direct"
part4$ten_proc_method[part4$ten_proc_method=="Licitaci贸n P煤blica"] <- "open"
part4$ten_proc_method[part4$ten_proc_method=="R茅gimen Especial"] <- "selective"
part4$ten_proc_method[part4$ten_proc_method=="Selecci贸n Abreviada servicios de Salud"] <- "selective"
part4$ten_proc_method[part4$ten_proc_method=="Subasta"] <- "open"

part4$ten_proc_method1 <- part4$ten_proc_method
part4$ten_proc_method1[grepl("*nima Cuant*", part4$ten_proc_method1)] <- "limited"
part4$ten_proc_method1[grepl("*Abreviada de Menor*", part4$ten_proc_method1)] <- "limited"
part4$ten_proc_method1[grepl("*Abreviada del literal h del*", part4$ten_proc_method1)] <- "limited"

part4$ten_proc_method <- part4$ten_proc_method1
part4$ten_proc_method1 <- NULL

table(part4$ten_proc_method)



#recode ten_proc_cat
table(part4$ten_proc_cat)
part4$ten_proc_cat[part4$ten_proc_cat=="Acuerdo Marco"] <- "service"
part4$ten_proc_cat[part4$ten_proc_cat=="Compraventa"] <- "goods"
part4$ten_proc_cat[part4$ten_proc_cat=="Fiducia"] <- "goods"
part4$ten_proc_cat[part4$ten_proc_cat=="Otro Tipo de Contrato"] <- "service"
part4$ten_proc_cat[part4$ten_proc_cat=="Agregaci贸n de Demanda"] <- "goods"
part4$ten_proc_cat[part4$ten_proc_cat=="Concesi贸n"] <- "service"
part4$ten_proc_cat[part4$ten_proc_cat=="Interventor胊"] <- "service"
part4$ten_proc_cat[part4$ten_proc_cat=="Prestaci贸n de Servicios"] <- "service"
part4$ten_proc_cat[part4$ten_proc_cat=="Arrendamiento"] <- "service"
part4$ten_proc_cat[part4$ten_proc_cat=="Consultor胊"] <- "service"
part4$ten_proc_cat[part4$ten_proc_cat=="Suministro"] <- "goods"
part4$ten_proc_cat[part4$ten_proc_cat=="Comodato"] <- "service"
part4$ten_proc_cat[part4$ten_proc_cat=="Cr茅dito"] <- "service"
part4$ten_proc_cat[part4$ten_proc_cat=="Obra"] <- "work"

part4$ten_proc_cat1 <- part4$ten_proc_cat
part4$ten_proc_cat1[grepl("*Interventor*", part4$ten_proc_cat1)] <- "service"
part4$ten_proc_cat1[grepl("*Consultor*", part4$ten_proc_cat1)] <- "service"
table(part4$ten_proc_cat1)

part4$ten_proc_cat <- part4$ten_proc_cat1
table(part4$ten_proc_cat)

part4$ten_proc_cat1 <- NULL



#ten_item_descr_l1
table(part4$ten_item_descr_l1)
part4$ten_item_descr_l1[grepl("*Material Vivo Animal y Vegetal*", part4$ten_item_descr_l1)] <- "goods"
part4$ten_item_descr_l1[grepl("*Maquinaria, Herramientas, Equipo Industrial*", part4$ten_item_descr_l1)] <- "goods"
part4$ten_item_descr_l1[grepl("*Productos de Uso Final*", part4$ten_item_descr_l1)] <- "goods"
part4$ten_item_descr_l1[grepl("*Terrenos, Edificios, Estructuras*", part4$ten_item_descr_l1)] <- "work"
part4$ten_item_descr_l1[grepl("*Materias Primas*", part4$ten_item_descr_l1)] <- "goods"
part4$ten_item_descr_l1[grepl("*Componentes y Suministros*", part4$ten_item_descr_l1)] <- "goods"
part4$ten_item_descr_l1[grepl("*Servicios*", part4$ten_item_descr_l1)] <- "services"



#check missing values
sapply(part4, function(part4) sum(is.na(part4)))
mean(is.na(part4$buyer_level))
#close to 0
mean(is.na(part4$buyer_id))
#18.7%
mean(is.na(part4$ca_id))
#10.2%
mean(is.na(part4$ten_vamount))
#0%
mean(is.na(part4$aw_id))
#10.2%
mean(is.na(part4$aw_sup_ident_scheme)) 
#10.2%
mean(is.na(part4$aw_sup_ident_id))
#10.2%
mean(is.na(part4$aw_sup_name))
#10.2%
mean(is.na(part4$ca_dateSigned_x))
#10.2%
mean(is.na(part4$ca_startDate_x))
#10.2%
mean(is.na(part4$ca_durInDays))
#10.2%
mean(is.na(part4$ca_endDate_x))
#10.2%
mean(is.na(part4$ca_vamount))
#0%
mean(is.na(part4$ca_vcurr))
#10.2%

mean(is.na(part4$plan_budg_projID))
#drop
mean(is.na(part4$plan_budg_proj))
#drop

part4$plan_budg_projID <- NULL
part4$plan_budg_proj <- NULL


#remove outliers in contract vamount
summary(part4$ca_vamount)
length(which(part4$ca_vamount == 0))
#108755
part4$ca_vamount[part4$ca_vamount < 1055406] <- NA
part4$ca_vamount[part4$ca_vamount > 35165654708313] <- NA

sum(is.na(part4$ca_vamount))
#151356
mean(is.na(part4$ca_vamount))
#15.1%

#ten_vamount
summary(part4$ten_vamount)
length(which(part4$ten_vamount == 0))
#7363
part4$ten_vamount[part4$ten_vamount < 1055406] <- NA
part4$ten_vamount[part4$ten_vamount > 35165654708313] <- NA
sum(is.na(part4$ten_vamount))
#56403
mean(is.na(part4$ten_vamount))
#5.6%

#date outliers (if contract start date is later than end date)

part4$ca_startDate_x[part4$ca_startDate_x > part4$ca_endDate_x] <- NA
sum(is.na(part4$ca_startDate_x))
#101780 
mean(is.na(part4$ca_startDate_x))
#10.2%

#check unique ids
length(unique(part4$UID))
#unique

part4<- part4[c("UID", "aw_id", "aw_sup_ident_id", "aw_sup_ident_scheme", "aw_sup_name", "buyer_addid", "buyer_id", "buyer_level", "buyer_name", "ca_dateSigned_x", "ca_durInDays", "ca_startDate_x", "ca_endDate_x", "ca_id", "ca_vamount", "ca_vcurr", "ocid", "record_nr", "ten_item_class_descr", "ten_item_class_id", "ten_item_descr_l1", "ten_proc_cat", "ten_proc_method", "ten_status", "ten_vamount")]


#save files
write.csv(part4, file = "secopI_part4_c.csv", row.names = FALSE)
##################################

#secop I chunk5 (3.2e6-4.2e6)

huge_file <- 'C:/Users/Bertold/Downloads/columbia-extra/SECOP_I-real.csv'

#detect a data model for the file:
model <- detect_dm_csv(huge_file, sep=",", header=F, fill = T)
df.laf <- laf_open(model)
goto(df.laf, 3.2e6)
part5 <- next_block(df.laf,nrows=1e6)


names(part5) <- c('UID', 'drop1', 'drop2', 'drop3', 'buyer_level', 'buyer_name', 'buyer_id', 'buyer_addid', 'drop4', 'ten_proc_method', 'ten_status', 'drop5', 'drop6', 'drop7', 'ten_item_class_id', 'drop8', 'ten_item_class_descr', 'ten_proc_cat', 'drop9', 'drop10', 'drop11', 'drop12', 'record_nr', 'ocid', 'ca_id', 'ten_vamount', 'drop13', 'ten_item_descr_l1', 'drop14', 'drop15', 'drop16', 'drop17', 'aw_id', 'aw_sup_ident_scheme', 'aw_sup_ident_id', 'aw_sup_name', 'drop18', 'drop19', 'drop20', 'drop21', 'ca_dateSigned', 'ca_startDate', 'ca_durInDays', 'drop22', 'drop23', 'drop24', 'ca_endDate', 'drop25', 'ca_vamount', 'drop26', 'drop27', 'drop28', 'plan_budg_projID', 'plan_budg_proj', 'drop29', 'drop30', 'drop31', 'drop32', 'drop33', 'ca_vcurr', 'drop34', 'drop35')


#drop unnecessary variables
part5$drop1 <- NULL
part5$drop2 <- NULL
part5$drop3 <- NULL
part5$drop4 <- NULL
part5$drop5 <- NULL
part5$drop6 <- NULL
part5$drop7 <- NULL
part5$drop8 <- NULL
part5$drop9 <- NULL
part5$drop10 <- NULL
part5$drop11 <- NULL
part5$drop12 <- NULL
part5$drop13 <- NULL
part5$drop14 <- NULL
part5$drop15 <- NULL
part5$drop16 <- NULL
part5$drop17 <- NULL
part5$drop18 <- NULL
part5$drop19 <- NULL
part5$drop20 <- NULL
part5$drop21 <- NULL
part5$drop22 <- NULL
part5$drop23 <- NULL
part5$drop24 <- NULL
part5$drop25 <- NULL
part5$drop26 <- NULL
part5$drop27 <- NULL
part5$drop28 <- NULL
part5$drop29 <- NULL
part5$drop30 <- NULL
part5$drop31 <- NULL
part5$drop32 <- NULL
part5$drop33 <- NULL
part5$drop34 <- NULL
part5$drop35 <- NULL

####(remove duplicates in final file) delete first two rows (as they are the last two rows of the previous part)
part5 <- part5[-(1:2),]

#change 0, 'No registra', 'No definido' and empty cells to NA
part5[part5 == "No registra"] <- NA
part5[part5 == "No Definido"] <- NA
part5[part5 == "No definido"] <- NA
part5[part5 == "No Definida"] <- NA
part5[part5 == 0] <- NA
part5[part5 == ""] <- NA


#check and set types of variables 
sapply(part5, class)
#it read all non-string data as factor 

#correct types
cols <- c(2,6,7,8,10,11,15,17,25,26,27)
part5[,cols] = lapply(part5[,cols], as.character)


cols <- c(14,24)
part5[,cols] = lapply(part5[,cols], as.numeric)

part5$ca_durInDays <- as.integer(part5$ca_durInDays)


#dates are converted to factor, and loses many values with simple as-Date function
#ca_startDate
part5$ca_startDate_x <- part5$ca_startDate

part5$ca_startDate_x <- as.character(part5$ca_startDate_x)
part5$ca_startDate_x <- as.Date(part5$ca_startDate_x, format = "%d/%m/%Y")

class(part5$ca_startDate_x)
#now it looks good
sum(is.na(part5$ca_startDate_x))

part5$ca_startDate <-NULL



#ca_endDate
part5$ca_endDate_x <- part5$ca_endDate

part5$ca_endDate_x <- as.character(part5$ca_endDate_x)
part5$ca_endDate_x <- as.Date(part5$ca_endDate_x, format = "%d/%m/%Y")

class(part5$ca_endDate_x)
#now it looks good
sum(is.na(part5$ca_endDate_x))

part5$ca_endDate <-part5$ca_endDate_X




#ca_dateSigned
part5$ca_dateSigned_x <- part5$ca_dateSigned
part5$ca_dateSigned_x <- as.character(part5$ca_dateSigned_x)
part5$ca_dateSigned_x <- as.Date(part5$ca_dateSigned_x, format = "%d/%m/%Y")

class(part5$ca_dateSigned_x)
#now it looks good
sum(is.na(part5$ca_dateSigned_x))

part5$ca_dateSigned <-part5$ca_dateSigned_X



#recode ten_status
table(part5$ten_status)
part5$ten_status[part5$ten_status=="Adjudicado"] <- "planned"
part5$ten_status[part5$ten_status=="Celebrado"] <- "active"
part5$ten_status[part5$ten_status=="Descartado"] <- "cancelled"
part5$ten_status[part5$ten_status=="Finalizado el plazo para manifestaciones de inter茅s"] <- "withdrawn"
part5$ten_status[part5$ten_status=="Lista Corta"] <- "planning"
part5$ten_status[part5$ten_status=="Terminado Anormalmente despu茅s de Convocado"] <- "cancelled"
part5$ten_status[part5$ten_status=="Borrador"] <- "planning"
part5$ten_status[part5$ten_status=="Convocado"] <- "planned"
part5$ten_status[part5$ten_status=="Expresi贸n de Inter茅s"] <- "planning"
part5$ten_status[part5$ten_status=="Liquidado"] <- "complete"
part5$ten_status[part5$ten_status=="Publicaci贸n para manifestaciones de inter茅s"] <- "planning"
part5$ten_status[part5$ten_status=="Terminado sin Liquidar"] <- "unsuccessful"

#recode ten_proc_method
table(part5$ten_proc_method)
part5$ten_proc_method[part5$ten_proc_method=="Asociaci贸n P煤blico Privada"] <- "direct"
part5$ten_proc_method[part5$ten_proc_method=="Concurso de M茅ritos Abierto"] <- "selective"
part5$ten_proc_method[part5$ten_proc_method=="Concurso de M茅ritos con Lista Corta"] <- "selective"
part5$ten_proc_method[part5$ten_proc_method=="Contrataci贸n Directa (Ley 1150 de 2007)"] <- "direct"
part5$ten_proc_method[part5$ten_proc_method=="Iniciativa Privada sin recursos p煤blicos"] <- "direct"
part5$ten_proc_method[part5$ten_proc_method=="Licitaci贸n P煤blica"] <- "open"
part5$ten_proc_method[part5$ten_proc_method=="Licitaci贸n obra p煤blica"] <- "open"
part5$ten_proc_method[part5$ten_proc_method=="R茅gimen Especial"] <- "selective"
part5$ten_proc_method[part5$ten_proc_method=="Selecci贸n Abreviada servicios de Salud"] <- "selective"
part5$ten_proc_method[part5$ten_proc_method=="Subasta"] <- "open"

part5$ten_proc_method1 <- part5$ten_proc_method

part5$ten_proc_method1[grepl("*nima Cuant*", part5$ten_proc_method1)] <- "limited"
part5$ten_proc_method1[grepl("*Contratos y convenios con*", part5$ten_proc_method1)] <- "limited"
part5$ten_proc_method1[grepl("*Abreviada de Menor*", part5$ten_proc_method1)] <- "limited"
part5$ten_proc_method1[grepl("*Abreviada del literal h del*", part5$ten_proc_method1)] <- "limited"

part5$ten_proc_method <- part5$ten_proc_method1

table(part5$ten_proc_method)

part5$ten_proc_method1 <- NULL


#recode ten_proc_cat
table(part5$ten_proc_cat)
part5$ten_proc_cat[part5$ten_proc_cat=="Acuerdo Marco"] <- "service"
part5$ten_proc_cat[part5$ten_proc_cat=="Compraventa"] <- "goods"
part5$ten_proc_cat[part5$ten_proc_cat=="Fiducia"] <- "goods"
part5$ten_proc_cat[part5$ten_proc_cat=="Otro Tipo de Contrato"] <- "service"
part5$ten_proc_cat[part5$ten_proc_cat=="Agregaci贸n de Demanda"] <- "goods"
part5$ten_proc_cat[part5$ten_proc_cat=="Concesi贸n"] <- "service"
part5$ten_proc_cat[part5$ten_proc_cat=="Interventor胊"] <- "service"
part5$ten_proc_cat[part5$ten_proc_cat=="Prestaci贸n de Servicios"] <- "service"
part5$ten_proc_cat[part5$ten_proc_cat=="Arrendamiento"] <- "service"
part5$ten_proc_cat[part5$ten_proc_cat=="Consultor胊"] <- "service"
part5$ten_proc_cat[part5$ten_proc_cat=="Suministro"] <- "goods"
part5$ten_proc_cat[part5$ten_proc_cat=="Comodato"] <- "service"
part5$ten_proc_cat[part5$ten_proc_cat=="Cr茅dito"] <- "service"
part5$ten_proc_cat[part5$ten_proc_cat=="Obra"] <- "work"

part5$ten_proc_cat1 <- part5$ten_proc_cat
part5$ten_proc_cat1[grepl("*Interventor*", part5$ten_proc_cat1)] <- "service"
part5$ten_proc_cat1[grepl("*Consultor*", part5$ten_proc_cat1)] <- "service"

part5$ten_proc_cat <- part5$ten_proc_cat1
table(part5$ten_proc_cat)

part5$ten_proc_cat1 <- NULL


#ten_item_descr_l1
table(part5$ten_item_descr_l1)
part5$ten_item_descr_l1[grepl("*Material Vivo Animal y Vegetal*", part5$ten_item_descr_l1)] <- "goods"
part5$ten_item_descr_l1[grepl("*Maquinaria, Herramientas, Equipo Industrial*", part5$ten_item_descr_l1)] <- "goods"
part5$ten_item_descr_l1[grepl("*Productos de Uso Final*", part5$ten_item_descr_l1)] <- "goods"
part5$ten_item_descr_l1[grepl("*Terrenos, Edificios, Estructuras*", part5$ten_item_descr_l1)] <- "work"
part5$ten_item_descr_l1[grepl("*Materias Primas*", part5$ten_item_descr_l1)] <- "goods"
part5$ten_item_descr_l1[grepl("*Componentes y Suministros*", part5$ten_item_descr_l1)] <- "goods"
part5$ten_item_descr_l1[grepl("*Servicios*", part5$ten_item_descr_l1)] <- "services"


#check missing values
sapply(part5, function(part5) sum(is.na(part5)))
mean(is.na(part5$buyer_level))
#0.2%
mean(is.na(part5$buyer_id))
#16.4%
mean(is.na(part5$ca_id))
#10.6%
mean(is.na(part5$ten_vamount))
#0% -> many values are 0
mean(is.na(part5$aw_id))
#10.6%
mean(is.na(part5$aw_sup_ident_scheme)) 
#10.6%
mean(is.na(part5$aw_sup_ident_id))
#10.6%
mean(is.na(part5$aw_sup_name))
#10.6%
mean(is.na(part5$ca_dateSigned_x))
#10.6%
mean(is.na(part5$ca_startDate_x))
#10.6%
mean(is.na(part5$ca_durInDays))
#10.6%
mean(is.na(part5$ca_endDate_x))
#10.6
mean(is.na(part5$ca_vamount))
#0% -> many values are 0
mean(is.na(part5$ca_vcurr))
#10.6%

mean(is.na(part5$plan_budg_projID))
#73%, not so important, drop
mean(is.na(part5$plan_budg_proj))
#73%, not so important drop

part5$plan_budg_projID <- NULL
part5$plan_budg_proj <- NULL


#remove outliers in contract vamount
summary(part5$ca_vamount)
#111538 missings
part5$ca_vamount[part5$ca_vamount < 1055406] <- NA
part5$ca_vamount[part5$ca_vamount > 35165654708313] <- NA

sum(is.na(part5$ca_vamount))
#183073
mean(is.na(part5$ca_vamount))
#18%

#ten_vamount
summary(part5$ten_vamount)
part5$ten_vamount[part5$ten_vamount < 1055406] <- NA
part5$ten_vamount[part5$ten_vamount > 35165654708313] <- NA
sum(is.na(part5$ten_vamount))
#91789
mean(is.na(part5$ten_vamount))
#9.2%

#date outliers (if contract start date is later than end date)

part5$ca_startDate_x[part5$ca_startDate_x > part5$ca_endDate_x] <- NA
sum(is.na(part5$ca_startDate_x))
#106286 
mean(is.na(part5$ca_startDate_x))
#10.6%

#check unique ids
length(unique(part5$UID))
#unique

part5<- part5[c("UID", "aw_id", "aw_sup_ident_id", "aw_sup_ident_scheme", "aw_sup_name", "buyer_addid", "buyer_id", "buyer_level", "buyer_name", "ca_dateSigned_x", "ca_durInDays", "ca_startDate_x", "ca_endDate_x", "ca_id", "ca_vamount", "ca_vcurr", "ocid", "record_nr", "ten_item_class_descr", "ten_item_class_id", "ten_item_descr_l1", "ten_proc_cat", "ten_proc_method", "ten_status", "ten_vamount")]


#save files
write.csv(part5, file = "secopI_part5_c.csv", row.names = FALSE)
#######################################
#secop I chunk6 (4.2e6-5.2e6)


huge_file <- 'C:/Users/Bertold/Downloads/columbia-extra/SECOP_I-real.csv'

#detect a data model for the file:
model <- detect_dm_csv(huge_file, sep=",", header=F, fill = T)
df.laf <- laf_open(model)
goto(df.laf, 4.2e6)
part6 <- next_block(df.laf,nrows=1e6)


names(part6) <- c('UID', 'drop1', 'drop2', 'drop3', 'buyer_level', 'buyer_name', 'buyer_id', 'buyer_addid', 'drop4', 'ten_proc_method', 'ten_status', 'drop5', 'drop6', 'drop7', 'ten_item_class_id', 'drop8', 'ten_item_class_descr', 'ten_proc_cat', 'drop9', 'drop10', 'drop11', 'drop12', 'record_nr', 'ocid', 'ca_id', 'ten_vamount', 'drop13', 'ten_item_descr_l1', 'drop14', 'drop15', 'drop16', 'drop17', 'aw_id', 'aw_sup_ident_scheme', 'aw_sup_ident_id', 'aw_sup_name', 'drop18', 'drop19', 'drop20', 'drop21', 'ca_dateSigned', 'ca_startDate', 'ca_durInDays', 'drop22', 'drop23', 'drop24', 'ca_endDate', 'drop25', 'ca_vamount', 'drop26', 'drop27', 'drop28', 'plan_budg_projID', 'plan_budg_proj', 'drop29', 'drop30', 'drop31', 'drop32', 'drop33', 'ca_vcurr', 'drop34', 'drop35')


#drop unnecessary variables
part6$drop1 <- NULL
part6$drop2 <- NULL
part6$drop3 <- NULL
part6$drop4 <- NULL
part6$drop5 <- NULL
part6$drop6 <- NULL
part6$drop7 <- NULL
part6$drop8 <- NULL
part6$drop9 <- NULL
part6$drop10 <- NULL
part6$drop11 <- NULL
part6$drop12 <- NULL
part6$drop13 <- NULL
part6$drop14 <- NULL
part6$drop15 <- NULL
part6$drop16 <- NULL
part6$drop17 <- NULL
part6$drop18 <- NULL
part6$drop19 <- NULL
part6$drop20 <- NULL
part6$drop21 <- NULL
part6$drop22 <- NULL
part6$drop23 <- NULL
part6$drop24 <- NULL
part6$drop25 <- NULL
part6$drop26 <- NULL
part6$drop27 <- NULL
part6$drop28 <- NULL
part6$drop29 <- NULL
part6$drop30 <- NULL
part6$drop31 <- NULL
part6$drop32 <- NULL
part6$drop33 <- NULL
part6$drop34 <- NULL
part6$drop35 <- NULL


###(remove duplicates in final file) delete first two rows (as they are the last two rows of the previous part)
part6 <- part6[-(1:2),]


#change 0, 'No registra', 'No definido' and empty cells to NA
part6[part6 == "No registra"] <- NA
part6[part6 == "No Definido"] <- NA
part6[part6 == "No definido"] <- NA
part6[part6 == "No Definida"] <- NA
part6[part6 == 0] <- NA
part6[part6 == ""] <- NA


#check and set types of variables 
sapply(part6, class)
#it read all non-string data as factor 

#correct types
cols <- c(2,6,7,8,10,11,15,17,25,26,27)
part6[,cols] = lapply(part6[,cols], as.character)


cols <- c(14,24)
part6[,cols] = lapply(part6[,cols], as.numeric)

part6$ca_durInDays <- as.integer(part6$ca_durInDays)

#dates are converted to factor, and loses many values with simple as-Date function
#ca_startDate
part6$ca_startDate_x <- part6$ca_startDate

part6$ca_startDate_x <- as.character(part6$ca_startDate_x)
part6$ca_startDate_x <- as.Date(part6$ca_startDate_x, format = "%d/%m/%Y")

class(part6$ca_startDate_x)
#now it looks good
sum(is.na(part6$ca_startDate_x))

part6$ca_startDate <-part6$ca_startDate_X

part6$ca_startDate <- NULL


#ca_endDate
part6$ca_endDate_x <- part6$ca_endDate

part6$ca_endDate_x <- as.character(part6$ca_endDate_x)
part6$ca_endDate_x <- as.Date(part6$ca_endDate_x, format = "%d/%m/%Y")

class(part6$ca_endDate_x)
#now it looks good
sum(is.na(part6$ca_endDate_x))

part6$ca_endDate <-part6$ca_endDate_X


#ca_dateSigned
part6$ca_dateSigned_x <- part6$ca_dateSigned
part6$ca_dateSigned_x <- as.character(part6$ca_dateSigned_x)
part6$ca_dateSigned_x <- as.Date(part6$ca_dateSigned_x, format = "%d/%m/%Y")

class(part6$ca_dateSigned_x)
#now it looks good
sum(is.na(part6$ca_dateSigned_x))

part6$ca_dateSigned <-part6$ca_dateSigned_X



#recode ten_status
table(part6$ten_status)
part6$ten_status[part6$ten_status=="Adjudicado"] <- "planned"
part6$ten_status[part6$ten_status=="Celebrado"] <- "active"
part6$ten_status[part6$ten_status=="Descartado"] <- "cancelled"
part6$ten_status[part6$ten_status=="Finalizado el plazo para manifestaciones de inter茅s"] <- "withdrawn"
part6$ten_status[part6$ten_status=="Lista Corta"] <- "planning"
part6$ten_status[part6$ten_status=="Terminado Anormalmente despu茅s de Convocado"] <- "cancelled"
part6$ten_status[part6$ten_status=="Borrador"] <- "planning"
part6$ten_status[part6$ten_status=="Convocado"] <- "planned"
part6$ten_status[part6$ten_status=="Expresi贸n de Inter茅s"] <- "planning"
part6$ten_status[part6$ten_status=="Liquidado"] <- "complete"
part6$ten_status[part6$ten_status=="Publicaci贸n para manifestaciones de inter茅s"] <- "planning"
part6$ten_status[part6$ten_status=="Terminado sin Liquidar"] <- "unsuccessful"


#recode ten_proc_method
table(part6$ten_proc_method)
part6$ten_proc_method[part6$ten_proc_method=="Asociaci贸n P煤blico Privada"] <- "direct"
part6$ten_proc_method[part6$ten_proc_method=="Concurso de M茅ritos Abierto"] <- "selective"
part6$ten_proc_method[part6$ten_proc_method=="Concurso de M茅ritos con Lista Corta"] <- "selective"
part6$ten_proc_method[part6$ten_proc_method=="Contrataci贸n Directa (Ley 1150 de 2007)"] <- "direct"
part6$ten_proc_method[part6$ten_proc_method=="Iniciativa Privada sin recursos p煤blicos"] <- "direct"
part6$ten_proc_method[part6$ten_proc_method=="Licitaci贸n P煤blica"] <- "open"
part6$ten_proc_method[part6$ten_proc_method=="R茅gimen Especial"] <- "selective"
part6$ten_proc_method[part6$ten_proc_method=="Selecci贸n Abreviada servicios de Salud"] <- "selective"
part6$ten_proc_method[part6$ten_proc_method=="Subasta"] <- "open"

part6$ten_proc_method1 <- part6$ten_proc_method
part6$ten_proc_method1[grepl("*nima Cuant*", part6$ten_proc_method1)] <- "limited"
part6$ten_proc_method1[grepl("*Abreviada de Menor*", part6$ten_proc_method1)] <- "limited"
part6$ten_proc_method1[grepl("*Abreviada del literal h del*", part6$ten_proc_method1)] <- "limited"

part6$ten_proc_method <- part6$ten_proc_method1

table(part6$ten_proc_method)

part6$ten_proc_method1 <- NULL

#recode ten_proc_cat
table(part6$ten_proc_cat)
part6$ten_proc_cat[part6$ten_proc_cat=="Acuerdo Marco"] <- "service"
part6$ten_proc_cat[part6$ten_proc_cat=="Compraventa"] <- "goods"
part6$ten_proc_cat[part6$ten_proc_cat=="Fiducia"] <- "goods"
part6$ten_proc_cat[part6$ten_proc_cat=="Otro Tipo de Contrato"] <- "service"
part6$ten_proc_cat[part6$ten_proc_cat=="Agregaci贸n de Demanda"] <- "goods"
part6$ten_proc_cat[part6$ten_proc_cat=="Concesi贸n"] <- "service"
part6$ten_proc_cat[part6$ten_proc_cat=="Interventor胊"] <- "service"
part6$ten_proc_cat[part6$ten_proc_cat=="Prestaci贸n de Servicios"] <- "service"
part6$ten_proc_cat[part6$ten_proc_cat=="Arrendamiento"] <- "service"
part6$ten_proc_cat[part6$ten_proc_cat=="Consultor胊"] <- "service"
part6$ten_proc_cat[part6$ten_proc_cat=="Suministro"] <- "goods"
part6$ten_proc_cat[part6$ten_proc_cat=="Comodato"] <- "service"
part6$ten_proc_cat[part6$ten_proc_cat=="Cr茅dito"] <- "service"
part6$ten_proc_cat[part6$ten_proc_cat=="Obra"] <- "work"

part6$ten_proc_cat1 <- part6$ten_proc_cat
part6$ten_proc_cat1[grepl("*Interventor*", part6$ten_proc_cat1)] <- "service"
part6$ten_proc_cat1[grepl("*Consultor*", part6$ten_proc_cat1)] <- "service"
table(part6$ten_proc_cat1)

part6$ten_proc_cat <- part6$ten_proc_cat1

part6$ten_proc_cat1 <- NULL


#ten_item_descr_l1
table(part6$ten_item_descr_l1)
part6$ten_item_descr_l1[grepl("*Material Vivo Animal y Vegetal*", part6$ten_item_descr_l1)] <- "goods"
part6$ten_item_descr_l1[grepl("*Maquinaria, Herramientas, Equipo Industrial*", part6$ten_item_descr_l1)] <- "goods"
part6$ten_item_descr_l1[grepl("*Productos de Uso Final*", part6$ten_item_descr_l1)] <- "goods"
part6$ten_item_descr_l1[grepl("*Terrenos, Edificios, Estructuras*", part6$ten_item_descr_l1)] <- "work"
part6$ten_item_descr_l1[grepl("*Materias Primas*", part6$ten_item_descr_l1)] <- "goods"
part6$ten_item_descr_l1[grepl("*Componentes y Suministros*", part6$ten_item_descr_l1)] <- "goods"
part6$ten_item_descr_l1[grepl("*Servicios*", part6$ten_item_descr_l1)] <- "services"



#check missing values
sapply(part6, function(part6) sum(is.na(part6)))
mean(is.na(part6$buyer_level))
#0.1%
mean(is.na(part6$buyer_id))
#16%
mean(is.na(part6$ca_id))
#8.8%
mean(is.na(part6$ten_vamount))
#8.8%
mean(is.na(part6$aw_id))
#8.8%
mean(is.na(part6$aw_sup_ident_scheme)) 
#8.8%
mean(is.na(part6$aw_sup_ident_id))
#8.8%
mean(is.na(part6$aw_sup_name))
#8.8%
mean(is.na(part6$ca_dateSigned_x))
#8.8%
mean(is.na(part6$ca_startDate_x))
#8.8%
mean(is.na(part6$ca_durInDays))
#9%
mean(is.na(part6$ca_endDate_x))
#8.8%
mean(is.na(part6$ca_vamount))
#17%
mean(is.na(part6$ca_vcurr))
#8.9%

mean(is.na(part6$plan_budg_projID))
#drop
mean(is.na(part6$plan_budg_proj))
#drop

part6$plan_budg_projID <- NULL
part6$plan_budg_proj <- NULL


#remove outliers in contract vamount
summary(part6$ca_vamount)
#111538 missings
part6$ca_vamount[part6$ca_vamount < 1055406] <- NA
part6$ca_vamount[part6$ca_vamount > 35165654708313] <- NA

sum(is.na(part6$ca_vamount))
#166119
mean(is.na(part6$ca_vamount))
#16.6%

#ten_vamount
summary(part6$ten_vamount)
part6$ten_vamount[part6$ten_vamount < 1055406] <- NA
part6$ten_vamount[part6$ten_vamount > 35165654708313] <- NA
sum(is.na(part6$ten_vamount))
#87876
mean(is.na(part6$ten_vamount))
#8.8%

#date outliers (if contract start date is later than end date)

part6$ca_startDate_x[part6$ca_startDate_x > part6$ca_endDate_x] <- NA
sum(is.na(part6$ca_startDate_x))
#88704
mean(is.na(part6$ca_startDate_x))
#8.8%

#check unique ids
length(unique(part6$UID))
#unique

part6<- part6[c("UID", "aw_id", "aw_sup_ident_id", "aw_sup_ident_scheme", "aw_sup_name", "buyer_addid", "buyer_id", "buyer_level", "buyer_name", "ca_dateSigned_x", "ca_durInDays", "ca_startDate_x", "ca_endDate_x", "ca_id", "ca_vamount", "ca_vcurr", "ocid", "record_nr", "ten_item_class_descr", "ten_item_class_id", "ten_item_descr_l1", "ten_proc_cat", "ten_proc_method", "ten_status", "ten_vamount")]


#save files
write.csv(part6, file = "secopI_part6_c.csv", row.names = FALSE)
######################################
#secop I chunk7 (5.2e6-6.2e6)


huge_file <- 'C:/Users/Bertold/Downloads/columbia-extra/SECOP_I-real.csv'

#detect a data model for the file:
model <- detect_dm_csv(huge_file, sep=",", header=F, fill = T)
df.laf <- laf_open(model)
goto(df.laf, 5.2e6)
part7 <- next_block(df.laf,nrows=1e6)


names(part7) <- c('UID', 'drop1', 'drop2', 'drop3', 'buyer_level', 'buyer_name', 'buyer_id', 'buyer_addid', 'drop4', 'ten_proc_method', 'ten_status', 'drop5', 'drop6', 'drop7', 'ten_item_class_id', 'drop8', 'ten_item_class_descr', 'ten_proc_cat', 'drop9', 'drop10', 'drop11', 'drop12', 'record_nr', 'ocid', 'ca_id', 'ten_vamount', 'drop13', 'ten_item_descr_l1', 'drop14', 'drop15', 'drop16', 'drop17', 'aw_id', 'aw_sup_ident_scheme', 'aw_sup_ident_id', 'aw_sup_name', 'drop18', 'drop19', 'drop20', 'drop21', 'ca_dateSigned', 'ca_startDate', 'ca_durInDays', 'drop22', 'drop23', 'drop24', 'ca_endDate', 'drop25', 'ca_vamount', 'drop26', 'drop27', 'drop28', 'plan_budg_projID', 'plan_budg_proj', 'drop29', 'drop30', 'drop31', 'drop32', 'drop33', 'ca_vcurr', 'drop34', 'drop35')


#drop unnecessary variables
part7$drop1 <- NULL
part7$drop2 <- NULL
part7$drop3 <- NULL
part7$drop4 <- NULL
part7$drop5 <- NULL
part7$drop6 <- NULL
part7$drop7 <- NULL
part7$drop8 <- NULL
part7$drop9 <- NULL
part7$drop10 <- NULL
part7$drop11 <- NULL
part7$drop12 <- NULL
part7$drop13 <- NULL
part7$drop14 <- NULL
part7$drop15 <- NULL
part7$drop16 <- NULL
part7$drop17 <- NULL
part7$drop18 <- NULL
part7$drop19 <- NULL
part7$drop20 <- NULL
part7$drop21 <- NULL
part7$drop22 <- NULL
part7$drop23 <- NULL
part7$drop24 <- NULL
part7$drop25 <- NULL
part7$drop26 <- NULL
part7$drop27 <- NULL
part7$drop28 <- NULL
part7$drop29 <- NULL
part7$drop30 <- NULL
part7$drop31 <- NULL
part7$drop32 <- NULL
part7$drop33 <- NULL
part7$drop34 <- NULL
part7$drop35 <- NULL

####(remove duplicates in final file) delete first two rows (as they are the last two rows of the previous part)
part7 <- part7[-(1:2),]

#change 0, 'No registra', 'No definido' and empty cells to NA
part7[part7 == "No registra"] <- NA
part7[part7 == "No Definido"] <- NA
part7[part7 == "No definido"] <- NA
part7[part7 == "No Definida"] <- NA
part7[part7 == 0] <- NA
part7[part7 == ""] <- NA


#check and set types of variables 
sapply(part7, class)
#it read all non-string data as factor 

#correct types
cols <- c(2,6,7,8,10,11,15,17,25,26,27)
part7[,cols] = lapply(part7[,cols], as.character)


cols <- c(14,24)
part7[,cols] = lapply(part7[,cols], as.numeric)

part7$ca_durInDays <- as.integer(part7$ca_durInDays)


#dates are converted to factor, and loses many values with simple as-Date function
#ca_startDate
part7$ca_startDate_x <- part7$ca_startDate

part7$ca_startDate_x <- as.character(part7$ca_startDate_x)
part7$ca_startDate_x <- as.Date(part7$ca_startDate_x, format = "%d/%m/%Y")

class(part7$ca_startDate_x)
#now it looks good
sum(is.na(part7$ca_startDate_x))

part7$ca_startDate <-part7$ca_startDate_X
part7$ca_startDate <- NULL



#ca_endDate
part7$ca_endDate_x <- part7$ca_endDate

part7$ca_endDate_x <- as.character(part7$ca_endDate_x)
part7$ca_endDate_x <- as.Date(part7$ca_endDate_x, format = "%d/%m/%Y")

class(part7$ca_endDate_x)
#now it looks good
sum(is.na(part7$ca_endDate_x))

part7$ca_endDate <-part7$ca_endDate_X




#ca_dateSigned
part7$ca_dateSigned_x <- part7$ca_dateSigned

part7$ca_dateSigned_x <- as.character(part7$ca_dateSigned_x)
part7$ca_dateSigned_x <- as.Date(part7$ca_dateSigned_x, format = "%d/%m/%Y")

class(part7$ca_dateSigned_x)
#now it looks good
sum(is.na(part7$ca_dateSigned_x))

part7$ca_dateSigned <-part7$ca_dateSigned_X



#recode ten_status
table(part7$ten_status)
part7$ten_status[part7$ten_status=="Adjudicado"] <- "planned"
part7$ten_status[part7$ten_status=="Celebrado"] <- "active"
part7$ten_status[part7$ten_status=="Descartado"] <- "cancelled"
part7$ten_status[part7$ten_status=="Finalizado el plazo para manifestaciones de inter茅s"] <- "withdrawn"
part7$ten_status[part7$ten_status=="Lista Corta"] <- "planning"
part7$ten_status[part7$ten_status=="Terminado Anormalmente despu茅s de Convocado"] <- "cancelled"
part7$ten_status[part7$ten_status=="Borrador"] <- "planning"
part7$ten_status[part7$ten_status=="Convocado"] <- "planned"
part7$ten_status[part7$ten_status=="Expresi贸n de Inter茅s"] <- "planning"
part7$ten_status[part7$ten_status=="Liquidado"] <- "complete"
part7$ten_status[part7$ten_status=="Publicaci贸n para manifestaciones de inter茅s"] <- "planning"
part7$ten_status[part7$ten_status=="Terminado sin Liquidar"] <- "unsuccessful"

#recode ten_proc_method
table(part7$ten_proc_method)
part7$ten_proc_method[part7$ten_proc_method=="Asociaci贸n P煤blico Privada"] <- "direct"
part7$ten_proc_method[part7$ten_proc_method=="Concurso de M茅ritos Abierto"] <- "selective"
part7$ten_proc_method[part7$ten_proc_method=="Concurso de M茅ritos con Lista Corta"] <- "selective"
part7$ten_proc_method[part7$ten_proc_method=="Contrataci贸n Directa (Ley 1150 de 2007)"] <- "direct"
part7$ten_proc_method[part7$ten_proc_method=="Iniciativa Privada sin recursos p煤blicos"] <- "direct"
part7$ten_proc_method[part7$ten_proc_method=="Licitaci贸n P煤blica"] <- "open"
part7$ten_proc_method[part7$ten_proc_method=="R茅gimen Especial"] <- "selective"
part7$ten_proc_method[part7$ten_proc_method=="Selecci贸n Abreviada servicios de Salud"] <- "selective"
part7$ten_proc_method[part7$ten_proc_method=="Subasta"] <- "open"

part7$ten_proc_method1 <- part7$ten_proc_method
part7$ten_proc_method1[grepl("*nima Cuant*", part7$ten_proc_method1)] <- "limited"
part7$ten_proc_method1[grepl("*Abreviada de Menor*", part7$ten_proc_method1)] <- "limited"
part7$ten_proc_method1[grepl("*Abreviada del literal h del*", part7$ten_proc_method1)] <- "limited"

part7$ten_proc_method <- part7$ten_proc_method1

table(part7$ten_proc_method)

part7$ten_proc_method1 <- NULL

#recode ten_proc_cat
table(part7$ten_proc_cat)
part7$ten_proc_cat[part7$ten_proc_cat=="Acuerdo Marco"] <- "service"
part7$ten_proc_cat[part7$ten_proc_cat=="Compraventa"] <- "goods"
part7$ten_proc_cat[part7$ten_proc_cat=="Fiducia"] <- "goods"
part7$ten_proc_cat[part7$ten_proc_cat=="Otro Tipo de Contrato"] <- "service"
part7$ten_proc_cat[part7$ten_proc_cat=="Agregaci贸n de Demanda"] <- "goods"
part7$ten_proc_cat[part7$ten_proc_cat=="Concesi贸n"] <- "service"
part7$ten_proc_cat[part7$ten_proc_cat=="Interventor胊"] <- "service"
part7$ten_proc_cat[part7$ten_proc_cat=="Prestaci贸n de Servicios"] <- "service"
part7$ten_proc_cat[part7$ten_proc_cat=="Arrendamiento"] <- "service"
part7$ten_proc_cat[part7$ten_proc_cat=="Consultor胊"] <- "service"
part7$ten_proc_cat[part7$ten_proc_cat=="Suministro"] <- "goods"
part7$ten_proc_cat[part7$ten_proc_cat=="Comodato"] <- "service"
part7$ten_proc_cat[part7$ten_proc_cat=="Cr茅dito"] <- "service"
part7$ten_proc_cat[part7$ten_proc_cat=="Obra"] <- "work"

part7$ten_proc_cat1 <- part7$ten_proc_cat
part7$ten_proc_cat1[grepl("*Interventor*", part7$ten_proc_cat1)] <- "service"
part7$ten_proc_cat1[grepl("*Consultor*", part7$ten_proc_cat1)] <- "service"
table(part7$ten_proc_cat1)

part7$ten_proc_cat <- part7$ten_proc_cat1

part7$ten_proc_cat1 <- NULL

#ten_item_descr_l1
table(part7$ten_item_descr_l1)
part7$ten_item_descr_l1[grepl("*Material Vivo Animal y Vegetal*", part7$ten_item_descr_l1)] <- "goods"
part7$ten_item_descr_l1[grepl("*Maquinaria, Herramientas, Equipo Industrial*", part7$ten_item_descr_l1)] <- "goods"
part7$ten_item_descr_l1[grepl("*Productos de Uso Final*", part7$ten_item_descr_l1)] <- "goods"
part7$ten_item_descr_l1[grepl("*Terrenos, Edificios, Estructuras*", part7$ten_item_descr_l1)] <- "work"
part7$ten_item_descr_l1[grepl("*Materias Primas*", part7$ten_item_descr_l1)] <- "goods"
part7$ten_item_descr_l1[grepl("*Componentes y Suministros*", part7$ten_item_descr_l1)] <- "goods"
part7$ten_item_descr_l1[grepl("*Servicios*", part7$ten_item_descr_l1)] <- "services"


#check missing values
sapply(part7, function(part7) sum(is.na(part7)))
mean(is.na(part7$buyer_level))
#0.2%
mean(is.na(part7$buyer_id))
#14.6%
mean(is.na(part7$ca_id))
#9%
mean(is.na(part7$ten_vamount))
#9%
mean(is.na(part7$aw_id))
#9%
mean(is.na(part7$aw_sup_ident_scheme)) 
#9%
mean(is.na(part7$aw_sup_ident_id))
#17.5%
mean(is.na(part7$aw_sup_name))
#9%
mean(is.na(part7$ca_dateSigned_x))
#9%
mean(is.na(part7$ca_startDate_x))
#9%
mean(is.na(part7$ca_durInDays))
#9%
mean(is.na(part7$ca_endDate_x))
#9%
mean(is.na(part7$ca_vamount))
#17%
mean(is.na(part7$ca_vcurr))
#9%

mean(is.na(part7$plan_budg_projID))
#drop
mean(is.na(part7$plan_budg_proj))
#drop

part7$plan_budg_projID <- NULL
part7$plan_budg_proj <- NULL


#remove outliers in contract vamount
summary(part7$ca_vamount)
#111538 missings
part7$ca_vamount[part7$ca_vamount < 1055406] <- NA
part7$ca_vamount[part7$ca_vamount > 35165654708313] <- NA

sum(is.na(part7$ca_vamount))
#169588
mean(is.na(part7$ca_vamount))
#17%

#ten_vamount
summary(part7$ten_vamount)
part7$ten_vamount[part7$ten_vamount < 1055406] <- NA
part7$ten_vamount[part7$ten_vamount > 35165654708313] <- NA
sum(is.na(part7$ten_vamount))
#88111
mean(is.na(part7$ten_vamount))
#8.8%

#date outliers (if contract start date is later than end date)

part7$ca_startDate_x[part7$ca_startDate_x > part7$ca_endDate_x] <- NA
sum(is.na(part7$ca_startDate_x))
#89588 
mean(is.na(part7$ca_startDate_x))
#9%

#check unique ids
length(unique(part7$UID))
#unique


part7<- part7[c("UID", "aw_id", "aw_sup_ident_id", "aw_sup_ident_scheme", "aw_sup_name", "buyer_addid", "buyer_id", "buyer_level", "buyer_name", "ca_dateSigned_x", "ca_durInDays", "ca_startDate_x", "ca_endDate_x", "ca_id", "ca_vamount", "ca_vcurr", "ocid", "record_nr", "ten_item_class_descr", "ten_item_class_id", "ten_item_descr_l1", "ten_proc_cat", "ten_proc_method", "ten_status", "ten_vamount")]


#save files
write.csv(part7, file = "secopI_part7_c.csv", row.names = FALSE)
##############################
#secop I chunk8 (6.2e6-final)


huge_file <- 'C:/Users/Bertold/Downloads/columbia-extra/SECOP_I-real.csv'

#detect a data model for the file:
model <- detect_dm_csv(huge_file, sep=",", header=F, fill = T)
df.laf <- laf_open(model)
goto(df.laf, 6.2e6)
part8 <- next_block(df.laf, nrows=4e5)


names(part8) <- c('UID', 'drop1', 'drop2', 'drop3', 'buyer_level', 'buyer_name', 'buyer_id', 'buyer_addid', 'drop4', 'ten_proc_method', 'ten_status', 'drop5', 'drop6', 'drop7', 'ten_item_class_id', 'drop8', 'ten_item_class_descr', 'ten_proc_cat', 'drop9', 'drop10', 'drop11', 'drop12', 'record_nr', 'ocid', 'ca_id', 'ten_vamount', 'drop13', 'ten_item_descr_l1', 'drop14', 'drop15', 'drop16', 'drop17', 'aw_id', 'aw_sup_ident_scheme', 'aw_sup_ident_id', 'aw_sup_name', 'drop18', 'drop19', 'drop20', 'drop21', 'ca_dateSigned', 'ca_startDate', 'ca_durInDays', 'drop22', 'drop23', 'drop24', 'ca_endDate', 'drop25', 'ca_vamount', 'drop26', 'drop27', 'drop28', 'plan_budg_projID', 'plan_budg_proj', 'drop29', 'drop30', 'drop31', 'drop32', 'drop33', 'ca_vcurr', 'drop34', 'drop35')


#drop unnecessary variables
part8$drop1 <- NULL
part8$drop2 <- NULL
part8$drop3 <- NULL
part8$drop4 <- NULL
part8$drop5 <- NULL
part8$drop6 <- NULL
part8$drop7 <- NULL
part8$drop8 <- NULL
part8$drop9 <- NULL
part8$drop10 <- NULL
part8$drop11 <- NULL
part8$drop12 <- NULL
part8$drop13 <- NULL
part8$drop14 <- NULL
part8$drop15 <- NULL
part8$drop16 <- NULL
part8$drop17 <- NULL
part8$drop18 <- NULL
part8$drop19 <- NULL
part8$drop20 <- NULL
part8$drop21 <- NULL
part8$drop22 <- NULL
part8$drop23 <- NULL
part8$drop24 <- NULL
part8$drop25 <- NULL
part8$drop26 <- NULL
part8$drop27 <- NULL
part8$drop28 <- NULL
part8$drop29 <- NULL
part8$drop30 <- NULL
part8$drop31 <- NULL
part8$drop32 <- NULL
part8$drop33 <- NULL
part8$drop34 <- NULL
part8$drop35 <- NULL

####(remove duplicates in final file) delete first two rows (as they are the last two rows of the previous part)
part8 <- part8[-(1:2),]

#change 0, 'No registra', 'No definido' and empty cells to NA
part8[part8 == "No registra"] <- NA
part8[part8 == "No Definido"] <- NA
part8[part8 == "No definido"] <- NA
part8[part8 == "No Definida"] <- NA
part8[part8 == 0] <- NA
part8[part8 == ""] <- NA


#check and set types of variables 
sapply(part8, class)
#it read all non-string data as factor 

#correct types
cols <- c(2,6,7,8,10,11,15,17,25,26,27)
part8[,cols] = lapply(part8[,cols], as.character)


cols <- c(14,24)
part8[,cols] = lapply(part8[,cols], as.numeric)

part8$ca_durInDays <- as.integer(part8$ca_durInDays)


#dates are converted to factor, and loses many values with simple as-Date function
#ca_startDate
part8$ca_startDate_x <- part8$ca_startDate

part8$ca_startDate_x <- as.character(part8$ca_startDate_x)
part8$ca_startDate_x <- as.Date(part8$ca_startDate_x, format = "%d/%m/%Y")

class(part8$ca_startDate_x)
#now it looks good
sum(is.na(part8$ca_startDate_x))

part8$ca_startDate <-part8$ca_startDate_X




#ca_endDate
part8$ca_endDate_x <- part8$ca_endDate

part8$ca_endDate_x <- as.character(part8$ca_endDate_x)
part8$ca_endDate_x <- as.Date(part8$ca_endDate_x, format = "%d/%m/%Y")

class(part8$ca_endDate_x)
#now it looks good
sum(is.na(part8$ca_endDate_x))

part8$ca_endDate <-part8$ca_endDate_X




#ca_dateSigned
part8$ca_dateSigned_x <- part8$ca_dateSigned

part8$ca_dateSigned_x <- as.character(part8$ca_dateSigned_x)
part8$ca_dateSigned_x <- as.Date(part8$ca_dateSigned_x, format = "%d/%m/%Y")

class(part8$ca_dateSigned_x)
#now it looks good
sum(is.na(part8$ca_dateSigned_x))

part8$ca_dateSigned <-part8$ca_dateSigned_X



#recode ten_status
table(part8$ten_status)
part8$ten_status[part8$ten_status=="Adjudicado"] <- "planned"
part8$ten_status[part8$ten_status=="Celebrado"] <- "active"
part8$ten_status[part8$ten_status=="Descartado"] <- "cancelled"
part8$ten_status[part8$ten_status=="Finalizado el plazo para manifestaciones de inter茅s"] <- "withdrawn"
part8$ten_status[part8$ten_status=="Lista Corta"] <- "planning"
part8$ten_status[part8$ten_status=="Terminado Anormalmente despu茅s de Convocado"] <- "cancelled"
part8$ten_status[part8$ten_status=="Borrador"] <- "planning"
part8$ten_status[part8$ten_status=="Convocado"] <- "planned"
part8$ten_status[part8$ten_status=="Expresi贸n de Inter茅s"] <- "planning"
part8$ten_status[part8$ten_status=="Liquidado"] <- "complete"
part8$ten_status[part8$ten_status=="Publicaci贸n para manifestaciones de inter茅s"] <- "planning"
part8$ten_status[part8$ten_status=="Terminado sin Liquidar"] <- "unsuccessful"


#recode ten_proc_method
table(part8$ten_proc_method)
part8$ten_proc_method[part8$ten_proc_method=="Asociaci贸n P煤blico Privada"] <- "direct"
part8$ten_proc_method[part8$ten_proc_method=="Concurso de M茅ritos Abierto"] <- "selective"
part8$ten_proc_method[part8$ten_proc_method=="Concurso de M茅ritos con Lista Corta"] <- "selective"
part8$ten_proc_method[part8$ten_proc_method=="Contrataci贸n Directa (Ley 1150 de 2007)"] <- "direct"
part8$ten_proc_method[part8$ten_proc_method=="Iniciativa Privada sin recursos p煤blicos"] <- "direct"
part8$ten_proc_method[part8$ten_proc_method=="Licitaci贸n P煤blica"] <- "open"
part8$ten_proc_method[part8$ten_proc_method=="R茅gimen Especial"] <- "selective"
part8$ten_proc_method[part8$ten_proc_method=="Selecci贸n Abreviada servicios de Salud"] <- "selective"
part8$ten_proc_method[part8$ten_proc_method=="Subasta"] <- "open"

part8$ten_proc_method1 <- part8$ten_proc_method
part8$ten_proc_method1[grepl("*nima Cuant*", part8$ten_proc_method1)] <- "limited"
part8$ten_proc_method1[grepl("*Abreviada de Menor*", part8$ten_proc_method1)] <- "limited"
part8$ten_proc_method1[grepl("*Abreviada del literal h del*", part8$ten_proc_method1)] <- "limited"
part8$ten_proc_method1[grepl("*Concurso de M*", part8$ten_proc_method1)] <- "selective"
part8$ten_proc_method1[grepl("*Contratos y convenios con*", part8$ten_proc_method1)] <- "limited"
part8$ten_proc_method1[grepl("*Licitaci*", part8$ten_proc_method1)] <- "open"
part8$ten_proc_method <- part8$ten_proc_method1

table(part8$ten_proc_method)

part8$ten_proc_method1 <- NULL

#recode ten_proc_cat
table(part8$ten_proc_cat)
part8$ten_proc_cat[part8$ten_proc_cat=="Acuerdo Marco"] <- "service"
part8$ten_proc_cat[part8$ten_proc_cat=="Compraventa"] <- "goods"
part8$ten_proc_cat[part8$ten_proc_cat=="Fiducia"] <- "goods"
part8$ten_proc_cat[part8$ten_proc_cat=="Otro Tipo de Contrato"] <- "service"
part8$ten_proc_cat[part8$ten_proc_cat=="Agregaci贸n de Demanda"] <- "goods"
part8$ten_proc_cat[part8$ten_proc_cat=="Concesi贸n"] <- "service"
part8$ten_proc_cat[part8$ten_proc_cat=="Interventor胊"] <- "service"
part8$ten_proc_cat[part8$ten_proc_cat=="Prestaci贸n de Servicios"] <- "service"
part8$ten_proc_cat[part8$ten_proc_cat=="Arrendamiento"] <- "service"
part8$ten_proc_cat[part8$ten_proc_cat=="Consultor胊"] <- "service"
part8$ten_proc_cat[part8$ten_proc_cat=="Suministro"] <- "goods"
part8$ten_proc_cat[part8$ten_proc_cat=="Comodato"] <- "service"
part8$ten_proc_cat[part8$ten_proc_cat=="Cr茅dito"] <- "service"
part8$ten_proc_cat[part8$ten_proc_cat=="Obra"] <- "work"

part8$ten_proc_cat1 <- part8$ten_proc_cat
part8$ten_proc_cat1[grepl("*Interventor*", part8$ten_proc_cat1)] <- "service"
part8$ten_proc_cat1[grepl("*Consultor*", part8$ten_proc_cat1)] <- "service"
table(part8$ten_proc_cat)

part8$ten_proc_cat <- part8$ten_proc_cat1

part8$ten_proc_cat1 <- NULL

#ten_item_descr_l1
table(part8$ten_item_descr_l1)
part8$ten_item_descr_l1[grepl("*Material Vivo Animal y Vegetal*", part8$ten_item_descr_l1)] <- "goods"
part8$ten_item_descr_l1[grepl("*Maquinaria, Herramientas, Equipo Industrial*", part8$ten_item_descr_l1)] <- "goods"
part8$ten_item_descr_l1[grepl("*Productos de Uso Final*", part8$ten_item_descr_l1)] <- "goods"
part8$ten_item_descr_l1[grepl("*Terrenos, Edificios, Estructuras*", part8$ten_item_descr_l1)] <- "work"
part8$ten_item_descr_l1[grepl("*Materias Primas*", part8$ten_item_descr_l1)] <- "goods"
part8$ten_item_descr_l1[grepl("*Componentes y Suministros*", part8$ten_item_descr_l1)] <- "goods"
part8$ten_item_descr_l1[grepl("*Servicios*", part8$ten_item_descr_l1)] <- "services"


#check missing values
sapply(part8, function(part8) sum(is.na(part8)))
mean(is.na(part8$buyer_level))
#0.3%
mean(is.na(part8$buyer_id))
#14.2%
mean(is.na(part8$ca_id))
#21%
mean(is.na(part8$ten_vamount))
#0%
mean(is.na(part8$aw_id))
#21%
mean(is.na(part8$aw_sup_ident_scheme)) 
#21%
mean(is.na(part8$aw_sup_ident_id))
#21%
mean(is.na(part8$aw_sup_name))
#21%
mean(is.na(part8$ca_dateSigned_x))
#21%
mean(is.na(part8$ca_startDate_x))
#21%
mean(is.na(part8$ca_durInDays))
#21%
mean(is.na(part8$ca_endDate_x))
#21%
mean(is.na(part8$ca_vamount))
#0%
mean(is.na(part8$ca_vcurr))
#21%

mean(is.na(part8$plan_budg_projID))
#81% -> drop
mean(is.na(part8$plan_budg_proj))
#81% -> drop

part8$plan_budg_projID <- NULL
part8$plan_budg_proj <- NULL


#remove outliers in contract vamount
summary(part8$ca_vamount)
#111538 missings
part8$ca_vamount[part8$ca_vamount < 1055406] <- NA
part8$ca_vamount[part8$ca_vamount > 35165654708313] <- NA

sum(is.na(part8$ca_vamount))
#97847
mean(is.na(part8$ca_vamount))
#32%

#ten_vamount
summary(part8$ten_vamount)
part8$ten_vamount[part8$ten_vamount < 1055406] <- NA
part8$ten_vamount[part8$ten_vamount > 35165654708313] <- NA
sum(is.na(part8$ten_vamount))
#39772
mean(is.na(part8$ten_vamount))
#13%

#date outliers (if contract start date is later than end date)

part8$ca_startDate[part8$ca_startDate > part8$ca_endDate] <- NA
sum(is.na(part8$ca_startDate))
#64454, no change 
mean(is.na(part8$ca_startDate))
#21%

#check unique ids
length(unique(part8$UID))
#unique

part8 <- part8[c("UID", "aw_id", "aw_sup_ident_id", "aw_sup_ident_scheme", "aw_sup_name", "buyer_addid", "buyer_id", "buyer_level", "buyer_name", "ca_dateSigned_x", "ca_durInDays", "ca_startDate_x", "ca_endDate_x", "ca_id", "ca_vamount", "ca_vcurr", "ocid", "record_nr", "ten_item_class_descr", "ten_item_class_id", "ten_item_descr_l1", "ten_proc_cat", "ten_proc_method", "ten_status", "ten_vamount")]

#save files
write.csv(part8, file = "secopI_part8_c.csv", row.names = FALSE)
############################
#join SECOP I chunk files together  

#set directory
setwd("C:/Users/Bertold/Downloads/columbia-extra/merge")
getwd()


#checking current memory limit
memory.limit()
#setting a bigger one
memory.limit(30000)

install.packages("data.table")
library(data.table)

install.packages("plyr")
library(plyr)

#create a separate folder that contains only the files that need to be merged
files <- list.files("C:/Users/Bertold/Downloads/columbia-extra/merge", full.names = TRUE)
import.list <- llply(files, read.csv)

df_secopI_all <- rbindlist(lapply(files, function(x){read.csv(x, stringsAsFactors = F, sep = ',')}))  
View(df_secopI_all)

write.csv(df_secopI_all, file = "df_secopI_all.csv", row.names = FALSE)

##################################
#merge secopI and II


#check if ca_id is unique
length(unique(secopII_c$ca_id))
#yes, it's unique


#package for handling large data sets
install.packages("ff")
library(ff)

df_secopI_all_ff <- read.csv.ffdf(file = "C:/Users/Bertold/Downloads/columbia-extra/merge/df_secopI_all.csv")
secopII_s <- read.csv.ffdf(file = "C:/Users/Bertold/Downloads/columbia-extra/merge/secopII_s.csv")

install.packages("ffbase")
library(ffbase)

lapply(physical(df_secopI_all_ff), FUN=function(df_secopI_all_ff) sum(is.na(df_secopI_all_ff)))

#add to SECOP I only the non-overlapping variables
secopII_s <- secopII_final[c("ten_endDate_x", "ocid", "ca_id", "aw_startDate_x", "aw_vamount", "nr_ofTenderers", "ca_status")]


#take a random sample of the same size az secop II, so that missing rates are not disproportional
secopI_sample <- df_secopI_all_ff[sample(nrow(df_secopI_all_ff), 185000), ]

#secopI_II_sample <- merge(secopI_sample, secopII_s, by = "ca_id", all.x=T)
#although the two file contains the same variable, contract id, they do not belong to the same contract, they do not match


#check missing rates
sapply(secopI_II_sample, function(secopI_II_sample) sum(is.na(secopI_II_sample)))
mean(is.na(secopI_II_sample$ten_endDate_x))
mean(is.na(secopI_II_sample$ten_id))
mean(is.na(secopI_II_sample$ten_startDate_x))
mean(is.na(secopI_II_sample$aw_sup_id))
mean(is.na(secopI_II_sample$aw_sup_name))
mean(is.na(secopI_II_sample$ca_id))
mean(is.na(secopI_II_sample$aw_id))
mean(is.na(secopI_II_sample$aw_vamount))

write.csv(secopII_s, file = "secopII_s.csv", row.names = FALSE)
write.csv(secopI_sample, file = "secopI_sample.csv", row.names = FALSE)
write.csv(secopI_II, file = "secopI_II.csv", row.names = FALSE)

##############################################################
#tienda_virtual

df0 <- read_csv('Tienda_Virtual_del_Estado_Colombiano_-_Consolidado.csv')
names(df0) <- c('year', 'buyer_id', 'buyer_type', 'buyer_sector', 'buyer_name', 'buyer_conpoi_name', 'date', 'sup_name', 'ca_status','request_nr','ten_descr','ten_vamount', 'aggregation', 'city', 'obl_entity', 'post_confl')

#basic information
dim(df0)
class(df0)

#check and set types of variables 
sapply(df0, class)

#change type of date variable
df0$date <- gsub(df0$date, pattern = " 12:00:00 AM", replacement = "")
df0$date <- gsub(df0$date, pattern = "/", replacement = "-")
df0$date <- as.Date.factor(df0$date, "%m-%d-%Y")
typeof(df0$date)
df0$date <- as.Date(df0$date)
class(df0$date)


#check missing values
sapply(df0, function(df0) sum(is.na(df0)))
#very few missings, all rates are 0 or close to 0

#remove outliers
summary(df0$ten_vamount)

df0$ten_vamount[df0$ten_vamount < 1055406] <- NA
df0$ten_vamount[df0$ten_vamount > 35165654708313] <- NA

sum(is.na(df0$ten_vamount))
#3981 (before it was 0)
mean(is.na(df0$ten_vamount))
#13.2%

summary(df0$year)
#2013-2018

#rename ca_status
table(df0$ca_status)

df0$ca_status[df0$ca_status=="Cerrada"] <- "closed"
df0$ca_status[df0$ca_status=="CerradaxError"] <- "closed"
df0$ca_status[df0$ca_status=="Emitida"] <- "terminated"

#save .csv
write.csv(df0, file = "tienda_c.csv", row.names = FALSE)
###########################
#OCDS files

setwd("C:/Users/Bertold/Downloads/columbia_ocds_csv")
getwd()

#rel_awa_suppliers.csv
df3 <- read_csv('rel_awa_suppliers.csv')
View(df3)

#basic information on the database
class(df3)
dim(df3)
str(df3)

#rename important variables (id, sup_id, sup_name, ident_id)
names(df3) <- gsub(x = names(df3), pattern = "releases/0/awards/0/", replacement = "rel_aw_")
names(df3) <- gsub(x = names(df3), pattern = "suppliers/0/", replacement = "sup_")
names(df3) <- gsub(x = names(df3), pattern = "identifier/", replacement = "ident_")
names(df3) <- gsub(x = names(df3), pattern = "additionalIdentifiers", replacement = "addid")

names(df3)


#delete rows with only one variable filled (ocid)
df3$rel_aw_id[df3$rel_aw_id == 0] <- NA
library(tidyr)
df3 = df3 %>% drop_na(rel_aw_id)

#number and ratio of missing values
sapply(df3, function(df3) sum(is.na(df3)))

mean(is.na(df3$ocid))
mean(is.na(df3$rel_aw_id))
mean(is.na(df3$rel_aw_sup_ident_id))
mean(is.na(df3$rel_aw_sup_name))
#no, or very few missing (<915)

#drop addid and scheme variables
df3$rel_aw_sup_addid <- NULL
df3$rel_aw_sup_ident_scheme <- NULL


#check and set types of variables 
sapply(df3, class)
#types are ok


#check unique values
length(unique(df3$ocid))
#not unique

length(unique(df3$rel_aw_id))
#unique

length(unique(df3$rel_aw_sup_ident_id))
#not unique

#save .csv
write.csv(df3, file = "rel_awa_suppliers_c.csv", row.names = FALSE)

################################
#rel_awards
df4 <- read_csv('rel_awards.csv')
View(df4)

#basic information on the database
class(df4)
dim(df4)
str(df4)

#rename important variables (id, sup_id, sup_name, ident_id)
names(df4) <- gsub(x = names(df4), pattern = "releases/0/awards/0/", replacement = "rel_aw_")

names(df4)

#change 0 value to NA in rel_aw_id
df4$rel_aw_id[df4$rel_aw_id == 0] <- NA

#drop rel_aw_doc and items as they are completely missing
df4$rel_aw_documents <- NULL
df4$rel_aw_items <- NULL

#delete rows with only one variable filled (ocid)
library(tidyr)
df4 = df4 %>% drop_na(rel_aw_id)

#number and ratio of missing values
sapply(df4, function(df4) sum(is.na(df4)))
#only very missing in ocid variable

#same number of observations as rel_awa_suppliers.csv
#check if the rel_aw_ids are the same

df3 <- rel_awa_suppliers_c

install.packages("pracma")
library(pracma)

df3$rel_aw_id <- as.character(df3$rel_aw_id)
df4$rel_aw_id <- as.character(df4$rel_aw_id)
#TRUE, they are identical, no need for rel_awards

################################
#rel_rel_buy_additionalIdentifiers

df5 <- read_csv('rel_buy_additionalIdentifiers.csv')
View(df5)

#basic information on the database
class(df5)
dim(df5)

#rename important variables (id, sup_id, sup_name, ident_id)
names(df5) <- gsub(x = names(df5), pattern = "releases/0/buyer/", replacement = "rel_buyer_")
names(df5) <- gsub(x = names(df5), pattern = "additionalIdentifiers/0/", replacement = "addid_")

names(df5)

str(df5)

#check and set types of variables 
sapply(df5, class)
#types are ok

#number and ratio of missing values
sapply(df5, function(df5) sum(is.na(df5)))
#only few missings in ocid

#check unique values
length(unique(df5$ocid))
#not unique

n_occur <- data.frame(table(df5$ocid))
n_occur[n_occur$Freq > 1,]

#remove completely duplicated rows
df5 <- df5[!duplicated(df5),]

#save .csv
write.csv(df5, file = "rel_buy_additionalIdentifiers_c.csv", row.names = FALSE)

###########################
#rel_contracts
df6 <- read_csv('rel_contracts.csv')
View(df6)

#basic information on the database
class(df6)
dim(df6)

#rename important variables
names(df6) <- gsub(x = names(df6), pattern = "releases/0/contracts/0/", replacement = "rel_ca_")
names(df6) <- gsub(x = names(df6), pattern = "period/", replacement = "")
names(df6) <- gsub(x = names(df6), pattern = "/", replacement = "")
names(df6) <- gsub(x = names(df6), pattern = "value", replacement = "v")

names(df6)

str(df6)

#check types of variables 
sapply(df6, class)

#set types of variables
df6$rel_ca_startDate <- as.Date(df6$rel_ca_startDate, tz = "UTC","%Y-%m-%d")
df6$rel_ca_endDate <- as.Date(df6$rel_ca_endDate, tz = "UTC", "%Y-%m-%d")
df6$rel_ca_dateSigned <- as.Date(df6$rel_ca_dateSigned, tz = "UTC", "%Y-%m-%d")

#change 0, No definido values to NAs
df6$rel_ca_awardID[df6$rel_ca_awardID == 0] <- NA
df6$rel_ca_id[df6$rel_ca_id == "No definido"] <- NA

#number and ratio of missing values
sapply(df6, function(df6) sum(is.na(df6)))
mean(is.na(df6$rel_ca_id))
#11%
mean(is.na(df6$rel_ca_startDate))
#21.6%
mean(is.na(df6$rel_ca_endDate))
#21.7%
mean(is.na(df6$rel_ca_durationInDays))
#21.7%
mean(is.na(df6$rel_ca_vamount))
#12.5%
mean(is.na(df6$rel_ca_vcurrency))
#13.5%
mean(is.na(df6$rel_ca_awardID))
#21.6%
mean(is.na(df6$rel_ca_dateSigned))
#13%

#remove outliers in value amount
summary(df6$rel_ca_vamount)
#828029 missings
df6$rel_ca_vamount[df6$rel_ca_vamount < 1055406] <- NA
df6$rel_ca_vamount[df6$rel_ca_vamount > 35165654708313] <- NA

sum(is.na(df6$rel_ca_vamount))
#1821249 (before it was 0)
mean(is.na(df6$rel_ca_vamount))
#27.5% -> double

#removing completely duplicated rows
df6 <- df6[!duplicated(df6),]

#it seems where rel_ca_id is missing, the other variables are also missing, check it
#select in a separate data frame those lines where rel_ca_id is missing
check1 <- df6[is.na(df6$rel_ca_id ),]
#735862 rows (as we checked previously the missings)
sapply(check1, function(check1) sum(is.na(check1)))
#the other variables are not completely empty, around 200 elements are available
table(check1$rel_ca_startDate)

library(tidyr)

#fill the gaps from check1
nm <- c('rec_com_ca_durInDays', 'rec_com_ca_caType')
df8[nm] <- lapply(nm, function(x) check1[[x]][match(df8$rec_com_ca_aw_id, check1$rec_com_ca_aw_id)])



#removing empty rows
check1=check1[rowSums(is.na(check1)) != ncol(check1),]
#after removing empty ones we have 1056537
#removing the ones that have only one value
check1= check1 %>% drop_na(records.0.compiledRelease.contracts.0.id)
#now 1056331

#check for unique variables
length(unique(df6$ocid))

length(unique(df6$rel_ca_id))

#########################
#rel_ten_documents
df7 <- read_csv('rel_ten_documents.csv')
View(df7)

#only contains ocid and tender/document/url, not needed for indicators

################################

#rel_ten_items
df8 <- read.csv("C:/Users/Bertold/Downloads/columbia_ocds_csv/rel_ten_items.csv", header = TRUE, sep=",", colClasses = 'character')
View(df8)

#only contains ocid and tender/items/classification/id, not needed for indicators
