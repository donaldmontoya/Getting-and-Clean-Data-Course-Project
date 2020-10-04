#Dado el conjunto de datos UCI HAR Dataset
#Los datos se descargaron y descomprimieron en la carpeta data ubicada en
#Mis documentos.
#Tomando en cuenta lo anterior se realiza la extraccion de los datos

#extraer los datos de X_train.txt
trainx <- read.table("./data/UCI HAR Dataset/train/X_train.txt",header = FALSE)
head(trainx) #observar los valores extraidos

#extraer los datos de Y_train.txt que contiene los datos de las 6 actividades
trainy <- read.table("./data/UCI HAR Dataset/train/y_train.txt",header = FALSE)

#extraer datos de subject_train.txt que contiene los codigos de los 30 participantes
subjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt",
header = FALSE)

#extraer los datos de Xtest.txt
testx <- read.table("./data/UCI HAR Dataset/test/X_test.txt",header = FALSE)

#extraer los datos de Y_train.txt
testy <- read.table("./data/UCI HAR Dataset/test/y_test.txt",header = FALSE)

#extraer datos de subject_test.txt
subjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt",
header = FALSE)

#Extrayendo los nombres de las variables del archivo features.txt
features <- read.table("./data/UCI HAR Dataset/features.txt",
header = FALSE)
#Extrayendo las actividades
activities <- read.table("./data/UCI HAR Dataset/activity_labels.txt",
header = FALSE)

#La Variable features columna 2 tiene los nombres de las variables, aqui se adelante el paso 4
colnames(trainx) <- features[,2]
colnames(testx) <- features[,2]
str(trainx) #verificando los nombres de las variables

head(subjectTrain)
tail(subjectTrain)
head(subjectTest)
tail(subjectTest)
#Dando nombre a la columna de subjectTrain y subjecTest
colnames(subjectTrain) <- "code_subject"
colnames(subjectTest) <- "code_subject"

#Dando nombre a la columna de trainy testy
colnames(trainy) <- "code_activity"
colnames(testy) <- "code_activity"

#Dando nombre a las columnas de tabla activities que contiene los nombres de las actividades
colnames(activities) <- c("code_activity","name_activity")

#Uniendo los datos de entrenamiento
combinacionTrain <- cbind(subjectTrain,trainx,trainy)

#Uniendo los datos de prueba
combinacionTest <- cbind(subjectTest,testx,testy)

#Uniendo los datos de entrenamiento y los datos de prueba
todos_train_test <- rbind(combinacionTrain,combinacionTest)

#PASO 2 2. Extrae sólo las mediciones
#en la media y la desviación estándar para cada medición

name_columnas <- names(todos_train_test)
media_std <- (grepl("code_activity",name_columnas)|
grepl("code_subject",name_columnas) |
grepl("mean..",name_columnas) |
grepl("std..",name_columnas))
#Extraer columnas con las medias y desviacion estandar
todos_mean_std <- todos_train_test[,media_std==TRUE]
ncol(todos_mean_std)

#Agregado el nombre descriptivo a cada una de las actividades
todos_datos_actividades <- merge(todos_mean_std,activities,by="code_activity")
View(todos_datos_actividades)
todos_datos_actividades$name_activity

#Segundo conjunto de datos ordenado con promedio de cada variable
#por actividad y sujeto
resumen_activity_subject <- aggregate(. ~code_subject + code_activity+name_activity,
todos_datos_actividades,FUN=mean)
resumen_activity_subject <- resumen_activity_subject[order(resumen_activity_subject$code_activity,
resumen_activity_subject$code_subject),]
#Guardando el segundo conjunto de datos
write.table(resumen_activity_subject,
"./data/UCI HAR Dataset/resumen_activity_subject.txt",
row.names = FALSE)
