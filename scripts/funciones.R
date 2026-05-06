# Funciones

moda <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) {
    return("Todos NA")
  }
  freq <- table(x)
  
  moda_valor <- names(freq)[which.max(freq)]
  
  return(moda_valor)
}

encontrar_outliers = function(variable) {
  Q1 = quantile(variable, 0.25, type=1,na.rm=T)
  Q3 = quantile(variable, 0.75, type=1,na.rm=T)
  IQR = IQR(variable,na.rm=T)
  
  extremo_inf = Q1 - 1.5 * IQR
  extremo_sup = Q3 + 1.5 * IQR
  
  outliers = which((variable < extremo_inf | variable > extremo_sup) & !is.na(variable))
  
  return(outliers)
}

eda=function(df){
  ub = which(is.na(df),arr.ind=TRUE)
  coll = unique(ub[,2])
  mat = matrix(colnames(df),dim(df)[2],16,byrow = FALSE)
  for (i in coll){
    mat[i,13]=sum(ub[,2]==i)
  }
  
  for (i in 1:dim(mat)[1]){
    if (mat[i,1]==mat[i,2])
      mat[i,2]=0
  }
  
  for (i in 1:dim(mat)[1]){
    
    if (is.numeric(df[[i]])){
      (mat[i,2]=round(mean(df[[i]],na.rm=T),digits = 2))
    }else{mat[i,2]="Texto"}
    
    if (is.numeric(df[[i]])){
      (mat[i,3]=quantile(df[[i]], 0.25,type=1,na.rm=T))
    }else{mat[i,3]="Texto"}
    
    if (is.numeric(df[[i]])){
      (mat[i,4]=round(median(df[[i]],na.rm=T),digits = 2))
    }else{mat[i,4]="Texto"}
    
    if (is.numeric(df[[i]])){
      (mat[i,5]=quantile(df[[i]], 0.75,type=1,na.rm=T))
    }else{mat[i,5]="Texto"}
    
    mat[i,6]=moda(df[i])
    
    if (is.numeric(df[[i]])){
      (mat[i,7]=round(min(df[[i]],na.rm=T),digits = 2))
    }else{mat[i,7]="Texto"}  
    
    if (is.numeric(df[[i]])){
      (mat[i,8]=round(max(df[[i]],na.rm=T),digits = 2))
    }else{mat[i,8]="Texto"}
    
    if (is.numeric(df[[i]])){
      (mat[i,9]=round(sd(df[[i]],na.rm=T),digits = 2))
    }else{mat[i,9]="Texto"}
    
    if (is.numeric(df[[i]])){
      (mat[i,10]=round(sd(df[[i]],na.rm=T)/mean(df[[i]],na.rm=T),digits = 2))
    }else{mat[i,10]="Texto"}
    
    if (is.numeric(df[[i]])){
      (mat[i,11]=round(mean(((df[[i]]-mean(df[[i]],na.rm=T))/sd(df[[i]],na.rm=T))^3,na.rm=T), digits=2))
    }else{mat[i,11]="Texto"}
    
    if (is.numeric(df[[i]])){
      (mat[i,12]=round(mean(((df[[i]]-mean(df[[i]],na.rm=T))/sd(df[[i]],na.rm=T))^4,na.rm=T), digits=2))
    }else{mat[i,12]="Texto"}
    
  }
  
  for (i in 1:dim(mat)[1]){
    mat[i,14]=length(unique(df[[i]]))
    mat[i,15]=round(as.numeric(mat[i,14])/dim(df[1])[1]*100,digits = 2)
  }
  
  for (i in 1:dim(mat)[1]){
    if(is.numeric(df[[i]])){
      mat[i,16]= length(encontrar_outliers(df[[i]]))
    }else{mat[i,16]="TEXTO"}
  }
  
  matdf=data.frame(mat)
  cn=c('Columna','Media','Q1','Mediana','Q3','Moda','Min','Max','SD','Coef_Var','Coef_Asim','Coef_Curt','Cant_NA','Uniq',"% Uniq",'Extremos')
  colnames(matdf)=cn
  return(matdf)  
}

matriz_cramers_v = function(df_cat){
  vars = names(df_cat)
  cramerv_mat = data.frame(matrix(ncol = length(vars), nrow = length(vars)))
  rownames(cramerv_mat) <- colnames(cramerv_mat) <- vars
  
  for (i in 1:length(vars)) {
    for (j in 1:length(vars)) {
      if (i != j) {
        cramerv_mat[i, j] = assocstats(table(df_cat[[vars[i]]], df_cat[[vars[j]]]))$cramer
      } else {
        cramerv_mat[i, j] = NA
      }
    }
  }
  return(cramerv_mat)
}

boxplots_vnum = function(df_num){
  plot_list <- list()
  for (col in names(df_num)) {
    df_plot <- data.frame(value = df_num[[col]], variable = col)
    p <- ggplot(df_plot, aes(x = variable, y = value)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = col) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5)) 
    
    plot_list[[col]] <- p
  }
  
  # Que la función devuelva el objeto gráfico ensamblado
  return(plot_grid(plotlist = plot_list, nrow = 1))
}

densidad_new_old <- function(var_new, var_old, nombre) {
  plot1 <- ggplot(data = data.frame(var1 = var_old), aes(x = var1)) +
    geom_density() +
    labs(title = paste(nombre, '(viejo)'), x = nombre, y = "Densidad")
  plot2 <- ggplot(data = data.frame(var2 = var_new), aes(x = var2)) +
    geom_density() +
    labs(title = paste(nombre, '(nuevo)'), x = nombre) +
    theme(axis.title.y = element_blank())
  grid.arrange(plot1, plot2, nrow = 1)
}
