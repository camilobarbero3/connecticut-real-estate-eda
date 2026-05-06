source('librerias.R')
source('funciones.R')

# Carga de los datos
df = read.csv('Connecticut_realstate_tp.csv')

# DEFINICION DE VARIABLES

    # Date: La fecha de cuando ocurrió la transacción.
    # Locality: La localidad o área a la que pertenece la propiedad.
    # Estimated Value: El precio estimado de la propiedad.
    # Sale Price: El precio real de venta de la propiedad.
    # Property: El tipo de propiedad (e.g., Familiar).
    # Residential: Indica si la propiedad es residencial o no. 
    # Num_rooms: La cantidad de cuartos de la propiedad.
    # Num_bathrooms: La cantidad de baños de la propiedad.
    # Carpet Area: Superficie alfombrada (que puede ser utilizada).
    # Property Tax Rate: La tasa impositiva aplicable a la propiedad.
    # Face: La dirección a la cual apunta la propiedad (Norte, Sur, Este u Oeste)

# Analisis exploratorio de datos
eda = eda(df)

# OBS:
# - Podemos observar que todas las variables tienen NAs, por lo que habra que ocuparse de imputar los missings mas adelante
# - Tambien se puede observar como la variable Year tiene media 2130,79 lo cual es ilogico,
#     por lo que podria haber valores absurdos por error de tipeo.

# creo un subconjunto solo con las variables numericas
idx = which(sapply(df, class) %in% c("numeric","integer"))
df_num = subset(df, select = idx)

ggpairs(df_num)

# OBS:
#   - Podemos observar una asimetria extrema en todas las vairables numericas
#   - En casos como Year son outliers y no algo de la asimetria natural de la varable, ya que hay años de 5 digitos o años mayores al año actual
#   - Aunque en variables como estimated.value, sale.price, num_rooms, num_bathrooms podria deberse a la asimetris de la variable por existir propiedades muy costosas y extravagantes
#   - En property tax podria haber outliers que no tienen que ver con la asimetria de la variable. Debido a que son todas propiedades listadas en connecticut, no seria logico que haya una basta diferencia entre tasas impositivas sobre las propiedades
#       - Ej: Casas con tasa del 1% de impuestos y otras del 100%, debe quiza haber errores de tipeo

# ANALISIS DE OUTLIERS Y MISSINGS
    
    # OUTLIERS
    
      # BOXPLOTS PARA OBSERVAR OUTLIERS

      boxplots_vnum(df_num)
      
      # ACLARACION: Los valores que son outliers se los obtiene por el metodo de IQR
  
      # Caso: Year
      i_outliers_Year = encontrar_outliers(df$Year)
      df$Year[i_outliers_Year]
      # Al haber pocos outliers (63) y como parece un error de tipeo donde se agregó un 0 de mas al final, convendria imputarlos eliminando ese 0 adicional,
      # en vez de usar un metodo mas complejo ya que seria innecesario
      Year_old = df$Year
      
          # IMPUTACION outliers de Year:
          df$Year[i_outliers_Year] = as.numeric(substring(df$Year[i_outliers_Year], 1, nchar(df$Year[i_outliers_Year]) - 1))
      
      densidad_new_old(df$Year, Year_old, 'Year')
      # Al eliminar los outliers, tiene mas sentido la distribucion que es bastante uniforme
      
      # Caso: property_tax_rate
      i_outliers_ptr = encontrar_outliers(df$property_tax_rate)
      df$property_tax_rate[i_outliers_ptr]
      # Al haber pocos outliers (65) y como parece un error de tipeo donde la coma decimal esta mal posicionada en comparacion a otros valores no atipicos
      # en vez de usar un metodo mas complejo ya que seria innecesario
      property_tax_rate_old = df$property_tax_rate
      
          # IMPUTACION outliers de property_tax_rate:
          df$property_tax_rate[i_outliers_ptr] = df$property_tax_rate[i_outliers_ptr] / 10
          
      densidad_new_old(df$property_tax_rate, property_tax_rate_old, 'property_tax_rate')
      # Al eliminar los outliers, tiene mas sentido la distribucion y se elimina la asimetria
      
      # Caso: num_rooms
      # En este caso pareciera que los outliers fueran un error de tipeo, donde para se ha agregado un 0 de mas al final de num_rooms
      # permitiendo la aparicion de casas para familias con 30, 40 e incluso 60 cuartos.
      boxplot(df$num_rooms)
      
      i_outliers_nroom2 = encontrar_outliers(df$num_rooms) 
      # Con el metodo de IQR, hay 1752 outliers
      df$num_rooms[i_outliers_nroom2] # Aca te dice que la fila 9 es un outlier por tener 4 cuartos, pero no imputamos ese tipo de outliers 
                                      # porque son una caracteristica propia de la propiedad, no nos parece apropiado
      # Esto se puede observar mas en:
      df[i_outliers_nroom2,c('Estimated.Value','Sale.Price','num_rooms','num_bathrooms','Property')] # se puede observar algo interesante. las propiedades que tienen mas de 10 cuartos como 30 cuartos, tienen 2 baños y valen ridículamente poco, lo cual no es realista
      # Mientras tanto las otras no parecen muy descabelladas a pesar de ser consideradas outliers por el criterio de IQR
      
      i_outliers_nroom = which(df$num_rooms >= 10) # Con este criterio si encontramos extremos absurdos
      df[i_outliers_nroom,c('Estimated.Value','Sale.Price','num_rooms','num_bathrooms','Property')] # donde las propiedades que tienen 30 cuartos, tienen 2 baños y valen ridículamente poco, lo cual no es realista
      
      num_rooms_old = df$num_rooms
      
          # IMPUTACION outliers de num_rooms:
          df$num_rooms[i_outliers_nroom] = df$num_rooms[i_outliers_nroom] / 10
      
      densidad_new_old(df$num_rooms, num_rooms_old, 'num_rooms')
      # Ahora los valores tienen sentido
      
      # Caso: num_bathrooms
      # En este caso tambien pareciera que los outliers fueran un error de tipeo, donde para se ha agregado un 0 de mas al final de num_bathrooms
      # permitiendo la aparicion de casas para familias con 10, 20, 30, 40 e incluso 60 baños.
      
      i_outliers_nbathroom2 = encontrar_outliers(df$num_bathrooms) 
      df[i_outliers_nbathroom2,c('Estimated.Value','Sale.Price','num_rooms','num_bathrooms','Property')] # Dentro de estos outliers hay algunos que si parecen apropiados dejar
      
      boxplot(df$num_bathrooms)
      
      i_outliers_nbathroom = which(df$num_bathrooms >= 10)
      df[i_outliers_nbathroom,c('Estimated.Value','Sale.Price','num_rooms','num_bathrooms','Property')]
      
      num_bathrooms_old = df$num_bathrooms
      
          # IMPUTACION outliers de num_bathrooms:
          df$num_bathrooms[i_outliers_nbathroom] = df$num_bathrooms[i_outliers_nbathroom] / 10
      
      densidad_new_old(df$num_bathrooms, num_bathrooms_old, 'num_bathrooms')
      # Ahora los valores tienen sentido
      
      # Caso: Property
      unique(df$Property) # Los "?" los trasnformamos a NAs para luego hacer la imputacion con el resto
      df$Property = ifelse(df$Property == '?', NA, df$Property)
      
      # Caso: Estimated.Value
      df[df$Estimated.Value == 0 & !is.na(df$Estimated.Value),] # hay 4 registros con estiamted value = 0 lo cual es un error
      # Lo convertimos como en Property a NA
      df$Estimated.Value = ifelse(df$Estimated.Value == 0 & !is.na(df$Estimated.Value), NA, df$Estimated.Value)
    
    # MISSINGS / NAs
      
      # Luego de la limpieza de los datos evaluo si los datos faltantes de la base son completely missing at random planteando un test de Little's MCAR
      
      # H0: Los datos faltantes están distribuidos completamente al azar
      # H1: Los datos faltantes no están distribuidos completamente al azar
      mcar_test(df)
      
      # Se puede interpretar al haber obtenido el p-valor = 0.998 estableciendo un nivel de signifcancia de 0.05 que no habria que rechazar la hipotesis nula. 
      # Por lo tanto, es correcto no es correcto afirmar que los datos no estan distribuidos completamente al azar
      
      # A pesar que que nos confirma que es MCAR (faltanes completamente al azar) con alta seguridad (p-valor=0.999), nos da la calma
      # de que la eleccion del metodo de imputacion que se elegia no introducira un sesgo significante o modificara la distribucion de los datos gravemente.
      
      # Sin embargo, para no depender de un test y elegir correctamente los metodos de imputacion en vez de un unico metodo generalizado para todas las vairbales,
      # es importante analizar relaciones entre variables para mantener la integridad de los datos y utilizar tecnicas de imputacion mas exactas
      
      # Esto se puede hacer de diversas maneras:
      
      # creo un subconjunto solo con las variables numericas
      idx = which(sapply(df, class) %in% c("numeric","integer"))
      df_num = subset(df, select = idx)
      
      # 1) Analizo correlacion entre numericas
          # Correlation matrix
          cor_matrix_spearman = cor(df_num, use = "pairwise.complete.obs", method = c("spearman"))
          cor_matrix_pearson = cor(df_num, use = "pairwise.complete.obs", method = c("pearson"))
    
          corrplot(cor_matrix_spearman)
          corrplot(cor_matrix_pearson)
      
      # Vemos que: las variables en general estan casi que nada relacionadas.
      
        # La matriz de correlacion de Pearson nos indica que las variables tienen una relacion lineal nula o baja entre si.
              # - Aunque se observa una fuerte corrlacion positiva entre sale.price y estimated.value. Osea que a grandes rasgos, el precio estimado al subir, sube el precio de venta
              # - Tambien una relacion aun mas grande entre num_bathrooms y num_rooms de aprox >0.8 que tiene sentido ya que lo pudimos observar
              # a la hora de imputar los outliers de las mismas
          
        # La matriz de correlacion de Spearman, la cual nos indica si existe o no algun tipo de relacion, sea la que sea, 
        # muestra una relacion debil o nula entre ellas excepto por la cantidad de baños, de habitaciones y superficie cubierta
        # las cuales tienen una relacion moderada entre si lo cual resulta logico debido a que en propiedades al haber mas cuartos habra mas baños y se 
        # necsitara mayor superficie y viceversa
          
      # A pesar de decir que es MCAR y haber baja correlacion en general debido a la relacion entre ALGUNAS variables que podrian llegar a influir en el mecanismo de missings, conviene estudiarlas mas a fondo.
  
      # IMPUTACION de NAs
          
          # Imputo los NA de Sale.Price y Estimated.Value por separado ya que presentaban colinealidad multivariante. <- chequear
              df_subset1 = df[, c("Sale.Price", "Estimated.Value")]
              data_imputada1 = mice(df_subset1, m = 5, method = 'pmm', seed = 500)
              
              # Visualizar los métodos usados para la imputación
              data_imputada1$method
              
              summary(data_imputada1)
              
              df_imputado1 = complete(data_imputada1)
              
              na_count1 = sapply(df_imputado1, function(x) sum(is.na(x)))
              na_count1
              
              df[ , colnames(df_subset1)] = df_imputado1
              
              sapply(df, function(x) sum(is.na(x)))
          
          # Imputo los NA de num_rooms y num_bathrooms por separado que tienen una correlacion fuerte 
              df_subset1 =  df[, c("num_rooms", "num_bathrooms")]
              data_imputada1 = mice(df_subset1, m = 5, method = 'pmm', seed = 500)
              
              # Visualizar los métodos usados para la imputación
              data_imputada1$method
              
              summary(data_imputada1)
              
              df_imputado1 = complete(data_imputada1)
              
              na_count1 = sapply(df_imputado1, function(x) sum(is.na(x)))
              na_count1
              
              df[ , colnames(df_subset1)] = df_imputado1
              
              sapply(df, function(x) sum(is.na(x)))
          
          # Imputo los NAs de Date e Year
              
              # Al ser una variable que tiene un order lineal constante, donde cada lisitng de propeidad esta ordenado en el tiempo cronologicamente.
              # Se puede aplicar el metodo de interpolacion lineal
              
              # Convierto las fechas de char a Date
              df$Date = as.Date(df$Date, format = "%Y-%m-%d")

              # Convertir fecha a numerico para interpolacion
              numeric_dates = as.numeric(df$Date)
              
              # Interpolacion lineal
              interpolated_dates = na.approx(numeric_dates, na.rm = FALSE, rule=2)
              
              # Convierto fecha y modifico df original
              df$Date = as.Date(interpolated_dates, origin = "1970-01-01")
              
              # IMPUTO Year
              
              df$Year = format(df$Date, "%Y")
              df$Year[is.na(df$Year)] = format(df$Date[is.na(df$Year)], "%Y")
              df$Year = as.integer(df$Year)
              
              sapply(df, function(x) sum(is.na(x)))
              
          # Analizo la relacion entre las categoricas
              
              # Test de comparacion categoricas con chicuadrado
              # H0: variable1 y variable2 son independientes, H1: variable1 y variable2 no son independientes
              idx = which(!sapply(df, class) %in% c("numeric", "integer", 'Date'))
              df_cat = subset(df, select = idx)
              
              mati_cor_cat = matriz_cramers_v(df_cat)
              mati_cor_cat
              
              # Podemos observar que Property y Residential tienen una associacion perfecta porque lo que tiene sentido imputarlos entre si por separado
              tabla_contigencia_P_R <- matrix(
                data = table(df$Property, df$Residential),
                nrow = length(rownames(table(df$Property, df$Residential))),
                ncol = length(colnames(table(df$Property, df$Residential))),
                dimnames = list(rownames(table(df$Property, df$Residential)), colnames(table(df$Property, df$Residential)))
              )
              
              tabla_contigencia_P_R # Vemos que cada tipo de Property tiene asociado un unico tipo de hogar residencial
              
              # IMPUTACION de Property y Residential
              df = df %>% mutate(
                Property = ifelse(is.na(Property),
                                  case_when(
                                    Residential == "Detached House" ~ "Single Family",
                                    Residential == "Duplex" ~ "Two Family",
                                    Residential == "Fourplex" ~ "Four Family",
                                    Residential == "Triplex" ~ "Three Family",
                                    TRUE ~ Property
                                  ),Property),
                Residential = ifelse(is.na(Residential),case_when(
                  Property == "Four Family" ~ "Fourplex",
                  Property == "Single Family" ~ "Detached House",
                  Property == "Three Family" ~ "Triplex",
                  Property == "Two Family" ~ "Duplex",
                  TRUE ~ Residential
                ), Residential)
                )
              
              sapply(df, function(x) sum(is.na(x))) 
              df[is.na(df$Property),]
              # quedan igual 9 missings en ambas columnas debido a que tienen las 2 NA,
              # Estas se imputaran con el resto de las variables
              
          # Imputo los NA de "carpet_area", "Locality", "Face", 'property_tax_rate', 'Property', 'Residential'
              
              # Paso las variables categoricas a factor:
              idx = which(!sapply(df, class) %in% c("numeric", "integer", 'Date'))
              df[, idx] = lapply(df[,idx], as.factor)
              
              # Definimos la subdata que imputaremos con mice
              df_subset2 <- df[, c("carpet_area", "property_tax_rate","Locality", "Property", "Residential", "Face")]
              
              # Imputación múltiple utilizando el método por defecto (pmm: Predictive Mean Matching)
              data_imputada2 <- mice(df_subset2, m = 5, method = 'pmm', seed = 500)
              
              # Visualizar los métodos usados para la imputación
              data_imputada2$method
              
              summary(data_imputada2)
              
              df_imputado2 = complete(data_imputada2)
              
              na_count2 <- sapply(df_imputado2, function(x) sum(is.na(x)))
              na_count2
              
              df[ , colnames(df_subset2)] <- df_imputado2
              
              sapply(df, function(x) sum(is.na(x)))
          
        #------------------------------------------------------------
              
  # ----- K-medias  ------ 
              
        # 1) Estandarizo los datos para poder interpretar
              # creo un subconjunto solo con las variables numericas
              idx = which(sapply(df, class) %in% c("numeric","integer"))
              df_num = subset(df, select = idx)
              df_scaled = scale(df_num)
        
        # 2) Determino la k cantidad de clusters optima visulizando grafico del codo: Within-Cluster-Sum of Squares (WSS)
              
              set.seed(123)
              wss = c()
              for (i in 1:15){
                kms = kmeans(scale(datasets::USArrests),i,nstart = 10) #nstart varias veces para evitar tener un sesgo respecto a la inicializacion de los clusters
                wss = c(wss, kms$tot.withinss)
              }
              wss %>% enframe() %>% ggplot(aes(name, value)) + geom_point() + geom_line() 
        
        # 3) Aplico algoritmo de kmeans 
        set.seed(123)
        kmeans_result = kmeans(df_scaled, centers = 2, nstart=10, iter.max = 50)
        
        # 4) Visualizo los datos clusterizados de forma optima
        fviz_cluster(kmeans_result, data = df_scaled,
                     ellipse.type = "norm",
                     palette = "jco",
                     ggtheme = theme_minimal())
        
        df_num['cluster'] = kmeans_result$cluster
        df_num %>% group_by(cluster) %>% summarise_all(median)
        
        summary(lm(cluster ~ ., data = df_num))
          
   # ----- MCA  ------
          
          idx = which(!sapply(df, class) %in% c("numeric", "integer", 'Date'))
          df_cat = subset(df, select = idx)
  
          
          #tenemos un K y un j que no debe ser muy grande para que MCA me sea util
          funModeling::df_status(df_cat)
          K = 7 + 4 + 4 + 4
          j = ncol(df_cat)
          
          RelacionKj = K/j
          #Nos dio un numero razonable para realizar MCA (4.75)
          
          #Mostramos el DataFrame con las columnas categoricas
          print(df_cat)
          
          #Nos quedamos con las categoricas a utilizar
          df[, idx] = lapply(df[,idx], as.factor)
          
          #Se hace mca
          mca = FactoMineR::MCA(df[-8], quanti.sup = 1:7)
          #Se observa que el precio de venta y valor estimado es descrito por el primer componente, mientras que 
          #carpet_area, num_rooms y num_bathrooms se explican tanto por el primero como el segundo. Sale.Price y Estimated.Value
          #van juntos, y carpet_area, num_rooms y num_bathrooms van juntos
          
          fviz_mca_biplot(mca)
          #tomando en consideracion los vectores vistos recien en el grafico de las variables suplementarias en 
          #los primeros dos componentes principales y el MCA factor map, podemos formular los siguientes pensamientos:
          #Los Property "Four Family" y "Three Family" junto con los Residential "Fourplex" y "Triplex", son los que mas cantidad 
          #de num_room, carpet_area y num_bathroom tienen. Esto es completamente logico y observarlo en el analisis MCA da 
          #seguridad de que devuelve resultados coherentes. 
          #Siguiendo el hilo, pareciera ser que Waterbury es la Locality con casas mas grandes (juzgando por el numero de baños,
          #cuartos y carpet area).
          
          #Vemos cuanto van acumulando los componentes principales de MCA
          fviz_screeplot(mca, addlabels = T)
          #Los 8 primeros componentes principales visualizados acumulan un 75.3% de la variabilidad total
          #Los 10 componentes principales visualizados acumulan un 88.3% de la variabilidad total 
          
          #Agregamos los primeros dos componentes principales al df
          df_con_MC1MC2 = cbind(df, mca$svd$U[,1:2])
          View(df_con_MC1MC2)
          
          colnames(df_con_MC1MC2)[12] = 'MC1'
          colnames(df_con_MC1MC2)[13] = 'MC2'
          
          #Esto es para contestar a la pregunta tipo: Cuanto del segundo componente es explicado por
          #la variable Locality? Te fijas el R2 para contestar. 
          corratioLocalMC2 = lm(MC2 ~ Locality, df_con_MC1MC2) %>% summary()
          
  # ----- PCA ---------
          
          #Definimos el df numerico
          idx = which(sapply(df, class) %in% c("numeric","integer"))
          df_num = subset(df, select = idx)
          
          #No hay nulos por la imputacion
          sapply(df, function(x) sum(is.na(x)))
          
          #Miramos las correlaciones
          cor(df_num)
          cor_matrix_spearman = cor(df_num, use = "pairwise.complete.obs", method = c("spearman"))
          cor_matrix_pearson = cor(df_num, use = "pairwise.complete.obs", method = c("pearson"))
          
          corrplot(cor_matrix_spearman)
          corrplot(cor_matrix_pearson)
          
          #Se presentan algunas correlaciones por lo que el analisis PCA puede llegar a ser util.
          
          #Primero utilizamos PCA de FactoMineR para ver como se descomponen los vectores en las dimensiones 
          #de los 2 primeros PC.
          pca = PCA(df_num, scale.unit = T)
          #Se observa que carpet_area, num_bathrooms y num_rooms se explican mas por el PC1 que por el PC2, mientras que 
          #Estimated.Value y Sale.Price se explica mas por PC2 que PC1.
          
          
          #Se utiliza pcomp para poder trabajar con la matriz de rotacion
          pca = prcomp(df_num, scale = T)
          pca$rotation
          #En la matriz podemos ver como PC1 influye bastante sobre num_bathrooms, carpet_area y num_rooms. Luego tiene mas leve contribucion
          #sobre Sale.Price y Estimated.Value. Para Year y property_tax_rate casi no contribuye. 
          #PC2 Hace lo contrario. Podemos ver que contribuye bastante a Sale.Price y Estimated.Value, mientras que tiene leve
          #influencia sobre num_bathrooms, carpet_area y num_rooms. Nuevamente, para Year y property_tax_rate casi no contribuye.
          #La contribucion para Year y property_tax_rate viene dada en gran parte por PC3, donde vemos que las demas no contribuyen. En 
          #este caso, cuando PC3 crece, crecen Year y property_tax_rate.
          
          #Vemos en el Biplot como se distribuyen los puntos en los cuadrantes
          fviz_pca_biplot(pca)
          
          #Para trabajar primero nos vamos a quedar con los componentes principales que 
          #tengan un desvio estandar > 1, es decir, que me explican mas variacion que los datos
          #originales. 
          pca$sdev
          #Con esto concluimos que los PC con los que trabajaremos son el PC1, PC2 y PC3.
          #Ademas, visualicemos la variabilidad acumulada de estos tres componentes principales:
          fviz_screeplot(pca, addlabel = T)
          #Con los 3 primeros explico un 65,1% de la variabilidad total de mis datos
          
          #En terminos de las variables originales por lo que vimos en el primero grafico
          #(cuando corrimos PCA(df_num, scale.unit = TRUE)) y viendo la matriz 
          #de rotacion podemos decir que PC1 nos va a mostrar que, en nuestros datos cuando
          #tengamos un PC1 muy bajo, tendremos un carpet_area grande y muchos cuartos y baños, mientras que 
          #si tenemos un PC1 muy alto, significaria lo contrario.
          #Por otro lado, si tenemos un PC2 alto, significara que el Sale.Price y el Estimated.Value seran altos y 
          #si es bajo, tendremos valores bajos para estas variables.
          #Por ultimo, cuando tengamos Pc3 elevado, tenderemos a ver años y tasas impositivas a las propiedades mas altas,
          #lo mismo aplica al revés.
          
          #El hecho de que 3 componentes principales me expliquen el 65,1% de la variabilidad hace pensar que este
          #PCA no es el mas optimo, lo cual era de esperarse al no ver tanta correlacion en la matriz de correlaciones.
          #Igualmente si queremos hacer una reduccion de dimensionalidad puede venir bien. Agregaremos los primeros
          #tres PC al dataframe. 
          
          df_con_PC1PC2PC3 = cbind(df, pca$x[,1:3])
          View(df_con_PC1PC2PC3)
          
          colnames(df_con_PC1PC2PC3)[12] = 'PC1'
          colnames(df_con_PC1PC2PC3)[13] = 'PC2'
          colnames(df_con_PC1PC2PC3)[14] = 'PC3'
          View(df_con_PC1PC2PC3)
          
          
          #Observemos los tipo de propiedades (una/dos/tres/cuatro Familias)
          fviz_pca_biplot(pca, col.ind = df$Property, geom = "point", pointsize = 2, pointshape = 16)
          #Se puede visualizar como se ordenan las casas en el eje x (PC1) para revelar que las casas de 4
          #son las que mas cuartos, baños y area de alfombra tienen. Seguidos por las casas de 3, las de 2 y las de 1.
          #Hay algunos outliers pero son la minoría.
          
          
          #Podemos juntar ambos analisis MCA y PCA y poder utilizarlos para reducir la dimension de nuestro dataset e ir 
          #sacando conclusiones sobre los datos en base a las observaciones ya realizadas
          
          #PCA y MCA con el resto de los datos
          df_total_mix = cbind(df, pca$x[,1:3], mca$svd$U[,1:2])
          
          colnames(df_total_mix)[15] = 'MC1'
          colnames(df_total_mix)[16] = 'MC2'
          
          View(df_total_mix)
          
          
          #PCA y MCA para la reduccion de dimensiones
          df_mix = cbind(pca$x[,1:3], mca$svd$U[,1:2])
          
          colnames(df_mix)[4] = 'MC1'
          colnames(df_mix)[5] = 'MC2'
          
          View(df_mix)
          
          # Se concluye que los resultados obtenidos de PCA y MCA no son favorables para responder las preguntas e interrogantes planteados
          
          
  # ------------------ ANALISIS EN RELACION A LAS PREGUNTAS -----------------
          
        df$Spread = (df$Sale.Price/df$Estimated.Value) - 1
          
          # ------ Las ciudades más solicitadas para vivir se van a vender a mayor precio del estimado o su diferencia será cercana a nula? ------
          
        # Resumen del Spread por Localidad
        locality_summary <- df %>%
          group_by(Locality) %>%
          summarise(
            Mean_Spread = mean(Spread, na.rm = TRUE),
            Median_Spread = median(Spread, na.rm = TRUE),
            SD_Spread = sd(Spread, na.rm = TRUE),
            Count = n()
          ) %>%
          arrange(desc(Mean_Spread))
        
        locality_summary
        
        # OBS: La localidad de Greenich tiene el Spread medio mayor, esto indica que este barrio tiene mayor rentabilidad entre los otros
        # Esto es debido a que el valor de venta es en promedio mucho mayor que el estimado en comapracion a otro barrios.
        # Esto llevaria a creer que conviene este barrio
        
        # Sin embargo, podemos ver que tiene el mayor desvio del spread indicando que podra ser el barrio que tenga mayor renatbilidad,
        # Pero es el mas volatil ya que si uno se pone a vender una propiedad en ese barrio corre el riesgo de no conseguir la rentabildiad esperada,
        # Podria ser mayor o menor
        
        # Por esta razon, debido al posible sesgo en la media, y la volatilidad que tendria definir deciciones por la misma,
        # hay que tomar en cuenta tambien la mediana ya que West Hartford tiene la mayor mediana de Spread
        # Esto indica que consistentemente que hay aprox un 50% de probabildiad de conseguir ese spread si se decide vender una propeidad en esa localidad
        # La media y la mediana en este barrio estan bastante cercanas, lo cual permitiria sin mucho riesgo suponer que se podria suponer obtener la reantabilidad media de West Hartford.
          
        # Refiriendonos directamente con la pregunta obsservamos que aquellos barrios mas ofertados (osea los que tienen mayores listings) son los que tienen menor spread
        # Esto tiene sentido ya que un menor spread indica menos diferencia entre el valor estimado de la propiedad y el valor de venta
        # En ese caso habra exceso de oferta que va de la mano con la cantidad de listings que hay total para esa localidad.
        
        # Relizando un test de anova para comparar medias entre grupos
          
          # H0: no hay diferencia significativa en el Spread entre las localidades
          # H1: Al menos una de las medias del Spread es diferente de las otras. 
        
        summary(aov(Spread ~ Locality, data = df))
        
        # Podemos inferir que por tener un p-valor ~= 0 hay que evidencia estadistica signficamente para afirmar que al menos una de las medias del Spread es diferente de las otras
        
        
        # ------ ¿Es mejor una casa mala en un barrio bueno o una casa buena en un barrio malo? -------
        
            # Análisis por Calidad de Propiedad y Barrio
            # Dividir en "buena" y "mala" según ciertas condiciones
                # ACLARACIONES: 
                #     - se considera un barrio bueno o malo a partir del spread, osea sera bueno aquel con spread alto
                #     - se considera una casa buena o mala aquella que tenga num_bathrooms / num_rooms >= 1 asi se garantiza
                #     que cada cuarto tenga al menos 1 baño privado
    
            df$Calidad_Propiedad = ifelse(df$num_bathrooms/df$num_rooms >= 1, "Buena", "Mala")
          
        # Calcular métricas por Localidad y Calidad de Propiedad
        summary_by_quality_locality <- df %>%
          group_by(Locality, Calidad_Propiedad) %>%
          summarise(
            Mean_Spread = mean(Spread, na.rm = TRUE),
            Median_Spread = median(Spread, na.rm = TRUE),
            SD_Spread = sd(Spread, na.rm = TRUE),
            Count = n()
          ) %>%
          arrange(Calidad_Propiedad, desc(Mean_Spread))
        
        # Basandonos en la media observamos que una casa mala en Greenwich (barrio bueno establecido en el punto anterior),
        # es mucho mas conveniente en terminos de maximizar la rentabilidad, que una casa buena en un barrio malo como Bridgepot
        # Spread medio de 0.3508740 vs. 0.7249742
        
        # Esto podria indicar que la rentabilidad se ve mas afectada por la localidad que por las caracteristicas propias de la casa
        # Auqnue para esto convendria hacer un modelo de regresion:
        
        library(caret)
        
        # Definir el conjunto de datos y la fórmula del modelo
        data <- df
        formula <- Spread ~ Locality + num_rooms + num_bathrooms + carpet_area
        
        # Configurar el control de entrenamiento y realizar el ajuste del modelo
        control <- trainControl(method = "cv", number = 10)  # Validación cruzada con 10 folds
        model <- train(formula, data = data, method = "lm", trControl = control)
        
        # Ver el resumen del modelo
        print(model)
        
        # Analizo la importancia relativa de cada variable sobre el Spread
        importancia_var = varImp(model)$importance
        print(varImp(model))
        
        # Podemos ver que que la propiedad sea de la localidad de Greewich tiene una improtancia relativa del 100%,
        # Y el resto de los barrios presentan su improtancia en forma descenete. Esto se asemeja a lo que vimos previamente de la media del Spread por barrio
        # donde el Spread medio de Greenwich era el mas alto, ahora podemos ver que esto es mas debido a caracteristicas propias del barrio que de la prpiedad que lo consistuye
        # La cantidad de cuartos importa bastante alrededor de un 20% de importancia sobre el Spread pero en comparacion la cantidad de baños no asi como la superifice cubierta.
        
        # --------- Analisis de series de tiempo
        
        library(tseries)
        library(forecast)
        
        df$Date = as.Date(df$Date, format="%Y-%m-%d")
        
        #Extraer YearMonth para aggregation
        df$YearMonth = format(df$Date, "%Y-%m")
        
        #Calcular el spread mensual promedio
        spread_mensual = aggregate(Spread ~ YearMonth, df, mean)
        
        #Parsear YearMonth a formato Date
        spread_mensual$YearMonth = as.Date(paste0(spread_mensual$YearMonth, "-01"))
        
        #Plot de la serie de tiempo
        ggplot(spread_mensual, aes(x=YearMonth, y=Spread)) +
          geom_line() +
          labs(title="Spread Mensual Promedio por el tiempo",
               x="Fecha",
               y="Spread Promedio")
        
        spread_ts = ts(spread_mensual$Spread, start=c(2009, 1), frequency=12)
        
        spread_decompuesta = decompose(spread_ts)
        
        #Plot de los componentes descompuestos
        plot(spread_decompuesta)
        
        adf_test = adf.test(spread_ts)
        print(adf_test)
        
        
        #El pvalor da 2,5%, con un nivel de significancia del 5% seria correcto afirmar que es estacionaria,
        #pero nosotros observamos en el grafico que si hay una tendencia, y conociendo el contexto socio-economico
        #de la epoca, en medio y posteriores de la crisis inmobiliaria del 2008, sabemos que el spread de los inmuebles
        #aumento mucho, es decir, que los precios se habian inflado mucho por la incertidumbre de la epoca. Posterior 
        #a la crisis, vino la recesion y el spread comenzo a disminuir hasta el año 2013, donde a partir de ahi podemos 
        #ver que el spread comienza a aumentar hasta el final de la serie. Esto es porque el mercado inmobiliario ha estado
        #creciendo los ultimos años, por lo que tiene sentido observar esta tendencia. Frente a este conocimiento, y el hecho
        #de estar seguros de que existio verdaderamente una tendencia en el spread, seremos mas tajantes a la hora de definir
        #nuestro error de tipo I, y definiremos a alfa como 1%. Frente a este valor, seria correcto no descartar el hecho 
        #de que la serie de tiempo observada sea no estacionaria, es decir que haya presencia de una raíz unitaria.
        
        #Hacemos ARIMA para poder hacer un forecast
        spread_arima = auto.arima(spread_ts)
        print(summary(spread_arima))
        
        
        #Forecast de los valores futuros
        spread_forecast = forecast(spread_arima, h=24)
        plot(spread_forecast)
        
        
        
        
        #Ahora queremos ver tendencias por region (Locality)
        
        #Calcular el spread mensual promedio para cada localidad
        spread_mensual_locality = df %>%
          group_by(YearMonth, Locality) %>%
          summarize(Spread = mean(Spread))
        
        #Parsear YearMonth a formato Date
        spread_mensual_locality$YearMonth = as.Date(paste0(spread_mensual_locality$YearMonth, "-01"))
        
        
        
        #Plot de la serie de tiempo para cada localidad
        ggplot(spread_mensual_locality, aes(x=YearMonth, y=Spread, color=Locality)) +
          geom_line() +
          facet_wrap(~ Locality, scales = "free_y") +
          labs(title="Spread Mensual Promedio por el tiempo por Localidad",
               x="Fecha",
               y="Spread Promedio") +
          theme(legend.position="none")
        
        
        #Convertir la data a serie de tiempo para cada localidad y analizar
        localities = unique(spread_mensual_locality$Locality)
        
        
        for (locality in localities) {
          locality_data = spread_mensual_locality %>% filter(Locality == locality)
          spread_ts = ts(locality_data$Spread, start=c(2009, 1), frequency=12)
          
          #Decomponemos la serie de tiempo
          spread_decomposed = decompose(spread_ts)
          print(paste("Decomposition for locality:", locality))
          print(spread_decomposed)
          
          #Hacemos Dickey Fuller para ver si es estacionaria
          adf_test = adf.test(spread_ts)
          print(paste("ADF test for locality:", locality))
          print(adf_test)
          
          #Hacemos un ARIMA para poder predecir valores futuros
          spread_arima = auto.arima(spread_ts)
          print(paste("ARIMA model for locality:", locality))
          print(summary(spread_arima))
          
          #Hacemos el forecast
          spread_forecast <- forecast(spread_arima, h=24)
          plot(spread_forecast, main=paste("Forecast for locality:", locality))
        }
        
        
        
        #Se puede visualizar que el spread para las diferentes regiones no fue el mismo a lo largo de los años.
        #Realizando el test de Dickey-Fuller Aumentado para cada localidad, podemos ver que la unica localidad que no tuvo 
        #una serie estacionaria desde el 2009 hasta el 2022 fue Bridgeport, las demas presentan un p-valor menor al 1%, lo cual
        #tambien se sostiene con el grafico de la serie temporal del spread promedio a lo largo de los meses. Esto se puede deber a
        #que algunas regiones no fueron tan afectadas por la crisis del 2008, o lo fueron pero la recesion de los precios ocurrio
        #en el año 2008, del cual no tenemos datos. Otra suposicion de por que podria ser asi es que en estos barrios no hay
        #tendencias porque la inversion inmobiliaria es mas consistente mientras que en Bridgeport, la tendencia del aumento del 
        #spread podria deberse a que es un barrio que ha estado creciendo en atraccion inversionista y en proceso de gentrificación
        #los ultimos años. Esa gentrificacion pudo haberse visto en hiatus por la crisis inmobiliaria, para retomar su tendencia alcista
        #a partir del 2013.
        
        #Para saber que mes del año es el mas adecuado para comprar un inmueble, obtendremos el menor spread de los meses 
        #en el año sacandolo del componente estacional, y posteriormente visualizandolo en el grafico de tendencia estacional
        #para corroborar el resultado obtenido.
        
        componente_estacional = spread_decompuesta$seasonal
        min_componente_estacional = which.min(componente_estacional)
        meses = month.abb
        mejor_mes_compra = meses[min_componente_estacional]
        
        #Efectivamente se obtiene que el mes donde conviene comprar un inmueble, es febrero. No solo tiene el menor spread,
        #sino que tambien es negativo, significando un valor de compra menor que el valor estimado para la venta del mismo (-11,92%).
        
        #Para saber que mes del año es el mas adecuado para vender un inmueble, obtendremos el mayor spread de los meses 
        #en el año sacandolo del componente estacional, y posteriormente visualizandolo en el grafico de tendencia estacional
        #para corroborar el resultado obtenido.
        
        max_componente_estacional = which.max(componente_estacional)
        mejor_mes_venta = meses[max_componente_estacional]
        
        #Se obtuvo que el mes con el mayor spread, por ende, la mayor sobrepreciacion del inmueble es el mes de Julio 
        #con un valor de venta del 14,11% por encima del valor estimado. 
        #Esto puede visualizar en el grafico de la componente estacional, teniendo un pico a mitad de año.
            
        
      
