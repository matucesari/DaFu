###################################################################################################
###                                                                                                            
### FUNCION PARA CARACTERIZACION DE VARIABLES EN CLUSTER  - INGLES                                             
### ultima revision: 04-01-2017
### Modificado por Campo Elias Pardo, se agrega parametro para reportar solo caracterizaciones
### positivas#
### Elaborado por: Pedro Cesar del Campo Neira     
###    Revisado por: Campo Elias Pardo                                                                         
###    Traduccion de mensajes al ingles Campo Elias Pardo
###    Correcciones: Mauricio Sadinle                                                               
###    Universidad Nacional de Colombia                                                                        
###                                                                                                            
### cluster.carac( tabla  := objeto data.frame con variables a caracterizar de un solo tipo                    
###                (continuas o discretas)                                                                     
###        class  := vector que determina la particion de la tabla ( as.factor(class)=TRUE )                   
###        tipo.v := tipo de variables (continuas o discretas)                                                 
###        sign   := Nivel de significancia para la prueba estadistica... para eliminar V.test's               
###      )                                                                                                     
###                                                                                                            
###################################################################################################

cluster.carac<-function(tabla,class,tipo.v="d",v.lim=2,dn=3,dm=3,neg=TRUE){
  if (!inherits(tabla, "data.frame")) 
      stop("The first argument must be an object of class data.frame") # control de objeto
  metodo <- pmatch(tipo.v , c("categoricas","nominales","discretas","continuas","frecuencia") )
  if(is.na(metodo)==TRUE)
      {return(cat("Undefined type of variables\n\n"))}
  if(metodo==1 || metodo==2 || metodo==3 ){
  #  VARIABLES CATEGORICAS , NOMINALES O DISCRETAS
    nj      <- apply ( acm.disjonctif(tabla) , 2 ,sum ) ##   cantidad de individuos mod_j
    n       <- dim(tabla)[1]                            ##   cantidad de individuos
    # --------------------------------------------------------------------------------------------------
    # funcion interno
    interno <- function(c.tabla)
      {##  Funcion para procesar en un solo cluster_k
      if(is.factor(c.tabla)) c.tabla <- as.data.frame(c.tabla)  # Mod. Mauricio                        
      disy <- acm.disjonctif(c.tabla) ##  Tabla disyuntiva completa en cluster_k
      nk <- dim(disy)[1]              ##  cantidad de individuos cluster_k   
      njk <- apply(disy,2,sum)        ##  cantidad de individuos mod_j cluster_k
      #  Frecuencias
      clas.mod <- 100*njk/nj                   ##  % de cluster_k en mod_j
      mod.clas <- 100*njk/nk                   ##  % de mod_j en cluster_k 
      Global   <- 100*nj/n                     ##  % de mod_j en n 
      ##  probabilidad hipergeometrica y valor test
      # programado por CEPT julio 2015
      prob <- matrix(NA, length(nj), 1) # 0.5 -> NA CEPT
      prob <- phyper(njk, nj, n - nj, nk)
      prob[mod.clas>=Global]<-phyper(njk-1,nj,n-nj,nk,lower.tail=FALSE)[mod.clas>=Global]
      #valores test
      V.test <- qnorm(prob/2) 
      # (28jul2017) se cambio  V.test[mod.clas>Global] por: 
      V.test[mod.clas>=Global]<-qnorm(prob/2,lower.tail=FALSE)[mod.clas>=Global] 
      SALIDA <- data.frame(Test.Value=round(V.test,dn), 
                p.Value=round(prob,dn),    
                Class.Cat=round(clas.mod,1), Cat.Class=round(mod.clas,1),                           
                Global=round(Global,1), Weight = nj)                                                    
      rownames(SALIDA) <- rownames(data.frame(nj))                              
      SALIDA <- subset(SALIDA, abs(V.test) >= v.lim) 
      if (neg==FALSE) subset(SALIDA, V.test >= v.lim)
      SALIDA <- SALIDA[order(SALIDA$Test.Value, decreasing = TRUE),]  
      return(SALIDA)
    } # termina funcion salida
    # chi cuadrado, phi, valores test por variables
    
  return(by(tabla, class, interno))
    }
# Fin variables discretas 
       
 if(metodo==4){

  #  VARIABLES CONTINUAS
       
  n      <- dim(tabla)[1]                          # No. individuos
  tabla  <- data.frame(numeric(n),tabla)           # variable falsa
#  mean.X <- mean(tabla)                            # media general
  mean.X <- sapply(tabla,mean)                     # mean is deprecated for data.frame 
  S2.X   <- diag(var(tabla))                       # varianzas generales
   
  interno <- function(c.tabla){     ##  Funcion para procesar en un solo cluster_k

             nk       <- dim(c.tabla)[1]                       ## individuos en cluster_k
            # mean.Xk  <- mean(c.tabla)                         ## media cluster_k
             mean.Xk  <- sapply(c.tabla,mean) 
             S2.Xk    <- (n-nk)* S2.X / ( n*nk )               ## varianzas cluster_k

             V.test   <- ( mean.Xk - mean.X ) / sqrt(S2.Xk)    ## valores.test cluster_k
             
             ##-------------------------------
             SALIDA   <- data.frame(Test.Value=round(V.test,dn),Class.Mean=round(mean.Xk,dm),Frequency=nk,Global.Mean=round(mean.X,dm))      
  
                        ## SALIDA
 
             rownames(SALIDA)<-names(c.tabla)                   ## etiquetas variables 
             SALIDA<-SALIDA[-1,]                                ## Eliminacion variable falsa
             SALIDA <- subset(SALIDA, abs(SALIDA$Test.Value) >= v.lim )    
             if(neg==FALSE) SALIDA <- subset(SALIDA, SALIDA$Test.Value >= v.lim )    
             
                                                                ## salida de no representativos
             
             SALIDA <- SALIDA[order(SALIDA$Test.Value , decreasing = TRUE),]   ## ordena por V.test
 
             # SALIDA <- round(SALIDA,3)
              
               return(SALIDA)
            }

  return( by(tabla,class,interno) ) # orden "by" salida para todos los cluster

 }# Fin variables continuas    

# modificacion de Campo Elias Pardo

 if(metodo==5 ){
 
  #  VARIABLES DE TIPO FRECUENCIA O CONTEO
  
  nj      <- colSums(tabla)                           ##   cantidad de individuos mod_j
  n       <- sum(tabla)                               ##   frecuencia total

  interno <- function(c.tabla){     ##  Funcion para procesar en un solo cluster_k
             nk       <- sum(c.tabla)                ##  scantidad de individuos cluster_k   
             njk      <- colSums(data.frame(c.tabla))            ##  cantidad de individuos mod_j cluster_k

             clas.mod <- 100*njk/nj                   ##  % de cluster_k en mod_j
             mod.clas <- 100*njk/nk                   ##  % de mod_j en cluster_k 
             Global   <- 100*nj/n                     ##  % de mod_j en n 
             
             # se coloca la de variables categoricas CEPT
             
             ##  probabilidad hipergeometrica y valor test
             # programado por CEPT julio 2015
             p.lim<-pnorm(-v.lim)
             prob <- matrix(NA, length(nj), 1) # 0.5 -> NA CEPT
             
             
             p.lim=rep(pnorm(-v.lim),length(nj)) # v.lim -> -vlim CEPT
             prob <- phyper(njk, nj, n - nj, nk)
             prob[mod.clas>Global]<-phyper(njk-1,nj,n-nj,nk,lower.tail=FALSE)[mod.clas>Global]
             #valores test
             V.test <- qnorm(prob/2) 
             V.test[mod.clas>Global]<-qnorm(prob/2,lower.tail=FALSE)[mod.clas>Global] 
             
             SALIDA <- data.frame(Test.Value=round(V.test,dn), p.Value=round(prob,dn), 
             Class.Cat=round(clas.mod,1), Cat.Class=round(mod.clas,1),                           
             Global=round(Global,1), Weight = nj)                                                    
             rownames(SALIDA) <- rownames(data.frame(nj))                              
             SALIDA <- subset(SALIDA,  abs(SALIDA$Test.Value) >= v.lim) 
             if (neg==FALSE) SALIDA <- subset(SALIDA,  SALIDA$Test.Value >= v.lim)                                     
             
             SALIDA <- SALIDA[order(SALIDA$Test.Value, decreasing = TRUE),]                
             return(SALIDA)                                                                        
             }

   return( by(tabla,class,interno) ) # orden "by" salida para todos los cluster 

 }# End of counting or frequency variables 
}
# END OF THE FUNCTION #
