shinyServer(function(input, output) {

  #fechas
  #Svensson
  output$p1<-renderPrint({paste(substr(input$n1,9,10),substr(input$n1,6,7),substr(input$n1,1,4),sep = "/")})

  #Diebold-Li
  output$p3<-renderPrint({paste(substr(input$n3,9,10),substr(input$n3,6,7),substr(input$n3,1,4),sep = "/")})
  #Splines
  output$p4<-renderPrint({paste(substr(input$n4,9,10),substr(input$n4,6,7),substr(input$n4,1,4),sep = "/")})
  #Comparativo
  output$p5<-renderPrint({paste(substr(input$n5,9,10),substr(input$n5,6,7),substr(input$n5,1,4),sep = "/")})
  
  #titulos
  #tif
  output$q1<-renderPrint({c(input$t1,input$t2,input$t3,input$t4)})
  output$q1_dl <- renderPrint({c(input$t1_dl,input$t2_dl,input$t3_dl,input$t4_dl)})
  output$q1_sp <- renderPrint({c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp)})
  output$q1_comp <- renderPrint({c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp)})
  
  #vebonos
  output$q2<-renderPrint({c(input$v1,input$v2,input$v3,input$v4)})
  output$q2_ns<-renderPrint({c(input$v1_ns,input$v2_ns,input$v3_ns,input$v4_ns)})
  output$q2_dl<-renderPrint({c(input$v1_dl,input$v2_dl,input$v3_dl,input$v4_dl)})
  output$q2_sp<-renderPrint({c(input$v1_sp,input$v2_sp,input$v3_sp,input$v4_sp)})
  output$q2_comp<-renderPrint({c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp)})
  
  #precios
  #tf <- reactive({pos(c(input$t1,input$t2,input$t3,input$t4),0)})
  tf <- reactive({pos1(c(input$t1,input$t2,input$t3,input$t4),0)})
  
 #tf_dl <- reactive({pos(c(input$t1_dl,input$t2_dl,input$t3_dl,input$t4_dl),0)})
  tf_dl <- reactive({pos1(c(input$t1_dl,input$t2_dl,input$t3_dl,input$t4_dl),0)})
  
  #tf_sp <- reactive({pos(c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),0)})
  tf_sp <- reactive({pos1(c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),0)})
  
  #tv <- reactive({pos(c(input$v1,input$v2,input$v3,input$v4),1)})
  tv <- reactive({pos1(c(input$v1,input$v2,input$v3,input$v4),1)})
  
  #tv_ns <- reactive({pos(c(input$v1_ns,input$v2_ns,input$v3_ns,input$v4_ns),1)})
  tv_ns <- reactive({pos1(c(input$v1_ns,input$v2_ns,input$v3_ns,input$v4_ns),1)})
  
  #tv_dl <- reactive({pos(c(input$v1_dl,input$v2_dl,input$v3_dl,input$v4_dl),1)})
  tv_dl <- reactive({pos1(c(input$v1_dl,input$v2_dl,input$v3_dl,input$v4_dl),1)})
  
  #tv_sp <- reactive({pos(c(input$v1_sp,input$v2_sp,input$v3_sp,input$v4_sp),1)})
  tv_sp <- reactive({pos1(c(input$v1_sp,input$v2_sp,input$v3_sp,input$v4_sp),1)})
  
  output$pre1 <-renderPrint({tf()})
  output$pre2 <-renderPrint({tv()})
  output$pre2_ns <-renderPrint({tv_ns()})
  
  output$pre1_dl <-renderPrint({tf_dl()})
  output$pre1_sp <-renderPrint({tf_sp()})
  
  output$pre2_dl <-renderPrint({tv_dl()})
  output$pre2_sp <-renderPrint({tv_sp()})
  
  #comparativo
  #Nelson y Siegel - Svensson
  tf_comp <- reactive({pos1(c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),0)})
  tv_comp <- reactive({pos1(c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),1)})
  
  output$pre_comp_tif_ns <- renderPrint({tf_comp()})
  output$pre_comp_tif_sven <- renderPrint({tf_comp()})
  output$pre_comp_veb_ns <- renderPrint({tv_comp()})
  output$pre_comp_veb_sven <- renderPrint({tv_comp()})
  
  #output$pre1_ns_comp <-renderPrint({tf_ns_comp()})
  #output$pre2_ns_comp <-renderPrint({tv_ns_comp()})
  
  #Svensson
  #tf_comp <- reactive({pos(c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),0)})
  #tv_comp <- reactive({pos(c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),1)})
  
  
  #output$pre1_comp <-renderPrint({tf_comp()})
  #output$pre2_comp <-renderPrint({tv_comp()})
  
  #parametros
  #output$pa_tif <- renderPrint({print(paste0("$$\\beta_{0}$$"))})
  output$pa_tif <- renderPrint({pa_sven})
  # output$formula <- renderPrint({
  #   return(paste0("Use this formula: $$\beta_{0}", 1,"$$"))
  # })
  # output$ex1 <- renderPrint({
  #  #withMathJax(helpText('$$\\beta_{0} \\quad \\beta_{1}  \\quad \\beta_{2} \\quad \\beta_{3} \\quad \\tau_{1} \\quad\\tau_{2} $$'))
  #   withMathJax(print(pa_ns))
  #   })

  output$pa_tif_ns_el <- renderPrint({(pa_ns)})
  output$pa_veb <- renderPrint({pa1_sven})
  output$pa_veb_ns <- renderPrint({pa1_ns})
  
  #parametros elegir
  #NELSON Y SIEGEL
  #tif
  #individuales
  output$num_ns_b0_tif<-renderPrint({input$ns_b0_tif})
  output$num_ns_b1_tif<-renderPrint({input$ns_b1_tif})
  output$num_ns_b2_tif<-renderPrint({input$ns_b2_tif})
  output$num_ns_t_tif<-renderPrint({input$ns_t_tif})
  
  #conjunto
  output$new_ns_tif <- renderPrint({(data.frame('B0'=input$ns_b0_tif,'B1'=input$ns_b1_tif,'B2'=input$ns_b2_tif,'T'=input$ns_t_tif,row.names = " " ))})
  
  #verificacion
  output$ver_ns_tif <- renderPrint({data.frame('Condición_1'=input$ns_b0_tif>0,'Condición_2'=input$ns_b0_tif+input$ns_b1_tif>0,'Condición_3'=input$ns_t_tif>0,row.names = " " )})
  
  #Comparacion
  output$num_ns_b0_tif_comp <-renderPrint({input$ns_b0_tif_comp})
  output$num_ns_b1_tif_comp <-renderPrint({input$ns_b1_tif_comp})
  output$num_ns_b2_tif_comp <-renderPrint({input$ns_b2_tif_comp})
  output$num_ns_t_tif_comp <-renderPrint({input$ns_t_tif_comp})
  
  #conjunto
  output$new_ns_tif_comp <- renderPrint({(data.frame('B0'=input$ns_b0_tif_comp,'B1'=input$ns_b1_tif_comp,'B2'=input$ns_b2_tif_comp,'T'=input$ns_t_tif_comp,row.names = " " ))})
  
  #verificacion
  output$ver_ns_tif_comp <- renderPrint({data.frame('Condición_1'=input$ns_b0_tif_comp>0,'Condición_2'=input$ns_b0_tif_comp+input$ns_b1_tif_comp>0,'Condición_3'=input$ns_t_tif_comp>0,row.names = " " )})
  
  
  #veb
  #individuales
  output$num_ns_b0_veb<-renderPrint({input$ns_b0_veb})
  output$num_ns_b1_veb<-renderPrint({input$ns_b1_veb})
  output$num_ns_b2_veb<-renderPrint({input$ns_b2_veb})
  output$num_ns_t_veb<-renderPrint({input$ns_t_veb})
  
  #conjunto
  output$new_ns_veb <- renderPrint({(data.frame('B0'=input$ns_b0_veb,'B1'=input$ns_b1_veb,'B2'=input$ns_b2_veb,'T'=input$ns_t_veb,row.names = " " ))})
  
  #verificacion
  output$ver_ns_veb <- renderPrint({data.frame('Condición_1'=input$ns_b0_veb>0,'Condición_2'=input$ns_b0_veb+input$ns_b1_veb>0,'Condición_3'=input$ns_t_veb>0,row.names = " " )})
  
  #comparativo
  output$num_ns_b0_veb_comp<-renderPrint({input$ns_b0_veb_comp})
  output$num_ns_b1_veb_comp<-renderPrint({input$ns_b1_veb_comp})
  output$num_ns_b2_veb_comp<-renderPrint({input$ns_b2_veb_comp})
  output$num_ns_t_veb_comp<-renderPrint({input$ns_t_veb_comp})
  
  #conjunto
  output$new_ns_veb_comp <- renderPrint({(data.frame('B0'=input$ns_b0_veb_comp,'B1'=input$ns_b1_veb_comp,'B2'=input$ns_b2_veb_comp,'T'=input$ns_t_veb_comp,row.names = " " ))})
  
  #verificacion
  output$ver_ns_veb_comp <- renderPrint({data.frame('Condición_1'=input$ns_b0_veb_comp>0,'Condición_2'=input$ns_b0_veb_comp+input$ns_b1_veb_comp>0,'Condición_3'=input$ns_t_veb_comp>0,row.names = " " )})
  
  
  #SVENSSON
  #tif
  #individuales
  output$num_sven_b0_tif<-renderPrint({input$sven_b0_tif})
  output$num_sven_b1_tif<-renderPrint({input$sven_b1_tif})
  output$num_sven_b2_tif<-renderPrint({input$sven_b2_tif})
  output$num_sven_b3_tif<-renderPrint({input$sven_b3_tif})
  output$num_sven_t1_tif<-renderPrint({input$sven_t1_tif})
  output$num_sven_t2_tif<-renderPrint({input$sven_t2_tif})
  
  #conjunto
  output$new_sven_tif <- renderPrint({(data.frame('B0'=input$sven_b0_tif,'B1'=input$sven_b1_tif,'B2'=input$sven_b2_tif,'B3'=input$sven_b3_tif,'T1'=input$sven_t1_tif,'T2'=input$sven_t2_tif,row.names = " " ))})
  
  #verificacion
  output$ver_sven_tif <- renderPrint({data.frame('Condición_1'=input$sven_b0_tif>0,'Condición_2'=input$sven_b0_tif+input$sven_b1_tif>0,'Condición_3'=input$sven_t1_tif>0,'Condición_4'=input$sven_t2_tif>0,row.names = " " )})
  
  #comparativo
  output$num_sven_b0_tif_comp<-renderPrint({input$sven_b0_tif_comp})
  output$num_sven_b1_tif_comp<-renderPrint({input$sven_b1_tif_comp})
  output$num_sven_b2_tif_comp<-renderPrint({input$sven_b2_tif_comp})
  output$num_sven_b3_tif_comp<-renderPrint({input$sven_b3_tif_comp})
  output$num_sven_t1_tif_comp<-renderPrint({input$sven_t1_tif_comp})
  output$num_sven_t2_tif_comp<-renderPrint({input$sven_t2_tif_comp})
  
  #conjunto
  output$new_sven_tif_comp <- renderPrint({(data.frame('B0'=input$sven_b0_tif_comp,'B1'=input$sven_b1_tif_comp,'B2'=input$sven_b2_tif_comp,'B3'=input$sven_b3_tif_comp,'T1'=input$sven_t1_tif_comp,'T2'=input$sven_t2_tif_comp,row.names = " " ))})
  
  #verificacion
  output$ver_sven_tif_comp <- renderPrint({data.frame('Condición_1'=input$sven_b0_tif_comp>0,'Condición_2'=input$sven_b0_tif_comp+input$sven_b1_tif_comp>0,'Condición_3'=input$sven_t1_tif_comp>0,'Condición_4'=input$sven_t2_tif_comp>0,row.names = " " )})
  
  
  
  
  #veb
  output$num_sven_b0_veb<-renderPrint({input$sven_b0_veb})
  output$num_sven_b1_veb<-renderPrint({input$sven_b1_veb})
  output$num_sven_b2_veb<-renderPrint({input$sven_b2_veb})
  output$num_sven_b3_veb<-renderPrint({input$sven_b3_veb})
  output$num_sven_t1_veb<-renderPrint({input$sven_t1_veb})
  output$num_sven_t2_veb<-renderPrint({input$sven_t2_veb})
  
  #conjunto
  output$new_sven_veb <- renderPrint({(data.frame('B0'=input$sven_b0_veb,'B1'=input$sven_b1_veb,'B2'=input$sven_b2_veb,'B3'=input$sven_b3_veb,'T1'=input$sven_t1_veb,'T2'=input$sven_t2_veb,row.names = " " ))})
  
  #verificacion
  output$ver_sven_veb <- renderPrint({data.frame('Condición_1'=input$sven_b0_veb>0,'Condición_2'=input$sven_b0_veb+input$sven_b1_veb>0,'Condición_3'=input$sven_t1_veb>0,'Condición_4'=input$sven_t2_veb>0,row.names = " " )})
  
  #comparativo
  output$num_sven_b0_veb_comp<-renderPrint({input$sven_b0_veb_comp})
  output$num_sven_b1_veb_comp<-renderPrint({input$sven_b1_veb_comp})
  output$num_sven_b2_veb_comp<-renderPrint({input$sven_b2_veb_comp})
  output$num_sven_b3_veb_comp<-renderPrint({input$sven_b3_veb_comp})
  output$num_sven_t1_veb_comp<-renderPrint({input$sven_t1_veb_comp})
  output$num_sven_t2_veb_comp<-renderPrint({input$sven_t2_veb_comp})
  
  #conjunto
  output$new_sven_veb_comp <- renderPrint({(data.frame('B0'=input$sven_b0_veb_comp,'B1'=input$sven_b1_veb_comp,'B2'=input$sven_b2_veb_comp,'B3'=input$sven_b3_veb_comp,'T1'=input$sven_t1_veb_comp,'T2'=input$sven_t2_veb_comp,row.names = " " ))})
  
  #verificacion
  output$ver_sven_veb_comp <- renderPrint({data.frame('Condición_1'=input$sven_b0_veb_comp>0,'Condición_2'=input$sven_b0_veb_comp+input$sven_b1_veb_comp>0,'Condición_3'=input$sven_t1_veb_comp>0,'Condición_4'=input$sven_t2_veb_comp>0,row.names = " " )})
  
  
  #muestro caracteristicas
  #output$Ca <- renderDataTable({C})
  output$Ca <- renderDataTable({
    ca <- try(Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")))
    if(class(ca)=="try-error"){
      v <- print("El archivo no se encuentra, descargar y recargar página!")
      return(as.data.frame(v))
    }else{
      return(ca)
    }
  })
  

  output$Ca_dl <- renderDataTable({
    ca <- try(Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")))
    if(class(ca)=="try-error"){
      v <- print("El archivo no se encuentra, descargar y recargar página!")
      return(as.data.frame(v))
    }else{
      return(ca)
    }
  })
  #output$Ca_sp <- renderDataTable({C})
  output$Ca_sp <- renderDataTable({
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    })
  
  output$Ca1 <- renderDataTable({
    ca <- try(Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")))
    if(class(ca)=="try-error"){
      v <- print("El archivo no se encuentra, descargar y recargar página!")
      return(as.data.frame(v))
    }else{
      return(ca)
    }
  })
  
  
  output$Ca1_ns <- renderDataTable({
    ca <- try(Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")))
    if(class(ca)=="try-error"){
      v <- print("El archivo no se encuentra, descargar y recargar página!")
      return(as.data.frame(v))
    }else{
      return(ca)
    }
    })
  output$Ca1_dl <- renderDataTable({
    ca <- try(Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")))
    if(class(ca)=="try-error"){
      v <- print("El archivo no se encuentra, descargar y recargar página!")
      return(as.data.frame(v))
    }else{
      return(ca)
    }
  })
  output$Ca1_sp <- renderDataTable({
    ca <- try(Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")))
    if(class(ca)=="try-error"){
      v <- print("El archivo no se encuentra, descargar y recargar página!")
      return(as.data.frame(v))
    }else{
      return(ca)
    }
  })
  output$Ca_comp <- renderDataTable({
    ca <- try(Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")))
    if(class(ca)=="try-error"){
      v <- print("El archivo no se encuentra, descargar y recargar página!")
      return(as.data.frame(v))
    }else{
      return(ca)
    }
  })
  output$Ca1_comp <- renderDataTable({
    ca <- try(Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")))
    if(class(ca)=="try-error"){
      v <- print("El archivo no se encuentra, descargar y recargar página!")
      return(as.data.frame(v))
    }else{
      return(ca)
    }
  })
  

  
  #precios estimados iniciales
  output$p_est_tif <- renderDataTable({Tabla.sven(fv = input$n1 ,tit = c(input$t1,input$t2,input$t3,input$t4),pr =tf() ,pa = pa_sven,ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=0,fe3=0)[[1]] })
  #output$p_est_tif_ns <- renderDataTable({Tabla.ns(fv = input$n2 ,tit = c(input$t1_ns,input$t2_ns,input$t3_ns,input$t4_ns),pr =tf_ns() ,pa = pa_ns,ind = 0,C = C,fe2=0,fe3=0)[[1]] })
  output$p_est_tif_ns <- renderDataTable({Tabla.ns(fv = input$n2 ,tit = c(input$t1_ns,input$t2_ns,input$t3_ns,input$t4_ns),pr =tf_ns() ,pa = pa_ns,ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")) ,fe2=0,fe3=0)[[1]] })
  #output$p_est_tif_ns_el <- renderDataTable({Tabla.ns(fv = input$n2 ,tit = c(input$t1_ns,input$t2_ns,input$t3_ns,input$t4_ns),pr =tf_ns() ,pa = c(input$ns_b0_tif,input$ns_b1_tif,input$ns_b2_tif,input$ns_t_tif),ind = 0,C = C,fe2=0,fe3=0)[[1]] })
  output$p_est_tif_ns_el <- renderDataTable({Tabla.ns(fv = input$n2 ,tit = c(input$t1_ns,input$t2_ns,input$t3_ns,input$t4_ns),pr =tf_ns() ,pa = c(input$ns_b0_tif,input$ns_b1_tif,input$ns_b2_tif,input$ns_t_tif),ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=0,fe3=0)[[1]] })
  
  
  #comparativo
  output$p_est_tif_ns_el_comp <- renderDataTable({Tabla.ns(fv = input$n5 ,tit = c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),pr =tf_comp() ,pa = c(input$ns_b0_tif_comp,input$ns_b1_tif_comp,input$ns_b2_tif_comp,input$ns_t_tif_comp),ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=0,fe3=0)[[1]] })
  
  
  output$p_est_veb <- renderDataTable({Tabla.sven(fv = input$n1 ,tit = c(input$v1,input$v2,input$v3,input$v4),pr =tv() ,pa = pa1_sven,ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=0,fe3=0)[[1]] })
  output$p_est_veb_ns <- renderDataTable({Tabla.ns(fv = input$n2 ,tit = c(input$v1_ns,input$v2_ns,input$v3_ns,input$v4_ns),pr =tv_ns() ,pa = pa1_ns,ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=0,fe3=0)[[1]] })
  output$p_est_veb_ns_el <- renderDataTable({Tabla.ns(fv = input$n2 ,tit = c(input$v1_ns,input$v2_ns,input$v3_ns,input$v4_ns),pr =tv_ns() ,pa =c(input$ns_b0_veb,input$ns_b1_veb,input$ns_b2_veb,input$ns_t_veb) ,ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=0,fe3=0)[[1]] })
  
  #comparativo
  output$p_est_veb_ns_el_comp <- renderDataTable({Tabla.ns(fv = input$n5 ,tit = c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),pr =tv_comp() ,pa =c(input$ns_b0_veb_comp,input$ns_b1_veb_comp,input$ns_b2_veb_comp,input$ns_t_veb_comp) ,ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=0,fe3=0)[[1]] })
  
  
  #precios estimados optimizados
  #paquete alabama
  output$p_est_tif_opt <- renderDataTable({
    if(input$opt_tif_sven==1){
    withProgress(message = 'Calculando parámetros optimizados', value = 0, {
      incProgress(1/2, detail = "Realizando iteraciones")
    Tabla.sven(fv = input$n1 ,tit = c(input$t1,input$t2,input$t3,input$t4),pr =tf() ,pa = c(1,1,1,1,1,1),ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_tif_sven,fe3=0)[[1]] 
    })
    }else{
      Aviso <- "No se optimizará, revisar los precios de la sección parámetros iniciales"
      return(as.data.frame(Aviso))
    }
    })
  
  output$p_est_tif_opt_ns <- renderDataTable({
    if(input$opt_tif_ns==1){
    withProgress(message = 'Calculando precios teóricos...', value = 0, {
      incProgress(1/2, detail = "Realizando iteraciones")
    #Tabla.ns(fv = input$n2 ,tit = c(input$t1_ns,input$t2_ns,input$t3_ns,input$t4_ns),pr =tf_ns() ,pa = c(1,1,1,1),ind = 0,C = C,fe2=input$opt_tif_ns,fe3=0)[[1]] 
    Tabla.ns(fv = input$n2 ,tit = c(input$t1_ns,input$t2_ns,input$t3_ns,input$t4_ns),pr =tf_ns() ,pa = c(1,1,1,1),ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")) ,fe2=input$opt_tif_ns,fe3=0)[[1]] 
    
    })
    }else{
      Aviso <- "No se optimizará, revisar precios sección parámetros iniciales"
      return(as.data.frame(Aviso))
    }
    })
  
  output$p_est_tif_opt_sven_el <- renderDataTable({Tabla.sven(fv = input$n1 ,tit = c(input$t1,input$t2,input$t3,input$t4),pr =tf() ,pa = c(input$sven_b0_tif,input$sven_b1_tif,input$sven_b2_tif,input$sven_b3_tif,input$sven_t1_tif,input$sven_t2_tif),ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=0,fe3=0)[[1]] })
  
  #comparacion
  output$p_est_tif_opt_comp <- renderDataTable({
    if(input$opt_tif_sven_comp==1){
    withProgress(message = 'Calculando parámetros optimizados', value = 0, {
      incProgress(1/2, detail = "Realizando iteraciones")
    Tabla.sven(fv = input$n5 ,tit = c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),pr =tf_comp() ,pa = c(1,1,1,1,1,1),ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_tif_sven_comp,fe3=0)[[1]]
    #incProgress(1/2, detail = "Fin")
     })
    }else{
      Aviso <- "No se optimizará, revisar los precios de la sección parámetros iniciales"
      return(as.data.frame(Aviso))
    }
    
    })
  
  output$p_est_tif_opt_ns_comp <- renderDataTable({
    if(input$opt_tif_ns_comp==1){
    withProgress(message = 'Calculando parámetros optimizados', value = 0, {
      incProgress(1/2, detail = "Realizando iteraciones")
    Tabla.ns(fv = input$n5 ,tit = c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),pr =tf_comp() ,pa = c(1,1,1,1),ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_tif_ns_comp,fe3=0)[[1]] 
    #incProgress(1/2, detail = "Fin")
    })
    }else{
      Aviso <- "No se optimizará, revisar los precios de la sección parámetros iniciales"
      return(as.data.frame(Aviso))
    }
  
 
    })
  output$p_est_tif_opt_sven_el_comp <- renderDataTable({Tabla.sven(fv = input$n5 ,tit = c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),pr =tf_comp() ,pa = c(input$sven_b0_tif_comp,input$sven_b1_tif_comp,input$sven_b2_tif_comp,input$sven_b3_tif_comp,input$sven_t1_tif_comp,input$sven_t2_tif_comp),ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=0,fe3=0)[[1]] })
  
  
  output$p_est_veb_opt <- renderDataTable({
    if(input$opt_veb_sven==1){
    withProgress(message = 'Calculando parámetros optimizados', value = 0, {
      incProgress(1/2, detail = "Realizando iteraciones")
    Tabla.sven(fv = input$n1 ,tit = c(input$v1,input$v2,input$v3,input$v4),pr =tv() ,pa = c(1,1,1,1,1,1),ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_veb_sven,fe3=0)[[1]]
    })
    }else{
      Aviso <- "No se optimizará, revisar los precios de la sección parámetros iniciales"
      return(as.data.frame(Aviso))
      }
    })
  
  output$p_est_veb_opt_ns <- renderDataTable({
    if(input$opt_veb_ns==1){
    withProgress(message = 'Calculando parámetros optimizados', value = 0, {
      incProgress(1/2, detail = "Realizando iteraciones")
    Tabla.ns(fv = input$n2 ,tit = c(input$v1_ns,input$v2_ns,input$v3_ns,input$v4_ns),pr =tv_ns() ,pa = c(1,1,1,1),ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_veb_ns,fe3=0)[[1]] 
    })
    }else{
      Aviso <- "No se optimizará, revisar los precios de la sección parámetros iniciales"
      return(as.data.frame(Aviso))
    }
    })
  
  output$p_est_veb_opt_sven_el <- renderDataTable({Tabla.sven(fv = input$n1 ,tit = c(input$v1,input$v2,input$v3,input$v4),pr =tv() ,pa = c(input$sven_b0_veb,input$sven_b1_veb,input$sven_b2_veb,input$sven_b3_veb,input$sven_t1_veb,input$sven_t2_veb),ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=0,fe3=0)[[1]]})
  
  #comparacion
  output$p_est_veb_opt_comp <- renderDataTable({
    if(input$opt_veb_sven_comp==1){
    withProgress(message = 'Calculando precios teóricos...', value = 0, {
      incProgress(1/2, detail = "Realizando iteraciones")
      Tabla.sven(fv = input$n5 ,tit = c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),pr =tv_comp() ,pa = c(1,1,1,1,1,1),ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_veb_sven_comp,fe3=0)[[1]]
    })
    }else{
      Aviso <- "No se optimizará, revisar los precios de la sección parámetros iniciales"
      return(as.data.frame(Aviso))
    }
    })
  
  output$p_est_veb_opt_sven_el_comp <- renderDataTable({Tabla.sven(fv = input$n5 ,tit = c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),pr =tv_comp() ,pa = c(input$sven_b0_veb_comp,input$sven_b1_veb_comp,input$sven_b2_veb_comp,input$sven_b3_veb_comp,input$sven_t1_veb_comp,input$sven_t2_veb_comp),ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=0,fe3=0)[[1]]})
  output$p_est_veb_opt_ns_comp <- renderDataTable({
    if(input$opt_veb_ns_comp==1){
    withProgress(message = 'Calculando precios...', value = 0, {
      incProgress(1/2, detail = "Realizando iteraciones")
      Tabla.ns(fv = input$n5 ,tit = c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),pr =tv_comp() ,pa = c(1,1,1,1),ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_veb_ns_comp,fe3=0)[[1]] 
      #incProgress(1/2, detail = "Fin")
      })
    }else{
    Aviso <- "No se optimizará, revisar los precios de la sección parámetros iniciales"
    return(as.data.frame(Aviso))
    }
      
    })
  
  
  #precios DL
  #extraigo spline
  #parametro de suavizamiento de splines para DL
  #TIF
  output$spar_tif_dl <- renderPrint({input$parametro_tif_dl})
  
  #defino funcion auxliliar que hace el llamado a la funcion tabla.Splines
  #una sola vez
  tabla_sp_dl_tif <- reactive({
    dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    Tabla.splines(data = dat,tipo = "TIF",fe=input$n3,num =40,par = input$parametro_tif_dl,tit=c(input$t1_dl,input$t2_dl,input$t3_dl,input$t4_dl),car,pr=tf_dl())
    })
  
  dl_spline_tif <- reactive({
    withProgress(message = 'Calculando spline a utilizar', value = 0, {
      # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
      # dat[,3] <- as.Date(as.character(dat[,3]))
      # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
      tabla_sp_dl_tif()[[4]] 
    })
    })
  
  output$spline_tif <- renderPrint({dl_spline_tif()})
  
  output$p_est_dl_tif <- renderDataTable({
    withProgress(message = 'Calculando precios teóricos', value = 0, {
      incProgress(1/2, detail = "Realizando iteraciones")
      car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    precio.dl(tit = c(input$t1_dl,input$t2_dl,input$t3_dl,input$t4_dl),fv = input$n3 ,C = car,pa = c(1,1,1,1),spline1 = dl_spline_tif(),pr=tf_dl())[[1]]
    })
    })
  
  #comparativo
  output$spar_tif_dl_comp <- renderPrint({input$parametro_tif_dl_comp})
  
  #creo funcion para acelerar calculo en seccion comparativo DL 
  tabla_sp_dl_tif_comp  <- reactive({
    dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    Tabla.splines(data = dat,tipo = "TIF",fe=input$n5,num =40,par = input$parametro_tif_dl_comp,tit=c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),car,pr=tf_comp())
  })
  
  dl_spline_tif_comp <- reactive({
    withProgress(message = 'Calculando spline a utilizar...', value = 0, {
      # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
      # dat[,3] <- as.Date(as.character(dat[,3]))
      # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
      # Tabla.splines(data = dat,tipo = "TIF",fe=input$n5,num =40,par = input$parametro_tif_dl_comp,tit=c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),car,pr=tf_comp())[[4]] 
      tabla_sp_dl_tif_comp()[[4]] 
      #incProgress(1/2, detail = "Fin")
    })
    })
  
  output$spline_tif_comp_out <- renderPrint({dl_spline_tif_comp()})
  
  output$p_est_dl_tif_comp <- renderDataTable({
    withProgress(message = 'Calculando precios teóricos...', value = 0, {
      incProgress(1/2, detail = "Realizando iteraciones")
      precio.dl(tit = c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),fv = input$n5 ,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")) ,pa = c(1,1,1,1),spline1 = dl_spline_tif_comp(),pr=tf_comp())[[1]]
      })
    })
  
  
  #Vebonos
  output$spar_veb_dl <- renderPrint({input$parametro_veb_dl})
  
  #defino funcion auxliliar que hace el llamado a la funcion tabla.Splines
  #una sola vez
  tabla_sp_dl_veb <- reactive({
    dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n3,num =40,par = input$parametro_veb_dl,tit=c(input$v1_dl,input$v2_dl,input$v3_dl,input$v4_dl),car,pr=tv_dl())
  })
  
  
  dl_spline_veb <- reactive({
    withProgress(message = 'Calculando spline a utilizar...', value = 0, {
      # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
      # dat[,3] <- as.Date(as.character(dat[,3]))
      # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
      # Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n3,num =40,par = input$parametro_veb_dl,tit=c(input$v1_dl,input$v2_dl,input$v3_dl,input$v4_dl),car,pr=tv_dl())[[4]] 
      tabla_sp_dl_veb()[[4]] 
    })
    })
  
  output$spline_veb <- renderPrint({dl_spline_veb()})
  
  output$p_est_dl_veb <- renderDataTable({
    withProgress(message = 'Calculando precios teóricos...', value = 0, {
      incProgress(1/2, detail = "Realizando iteraciones")
      car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    precio.dl(tit = c(input$v1_dl,input$v2_dl,input$v3_dl,input$v4_dl),fv = input$n3 ,C = car ,pa = c(1,1,1,1),spline1 = dl_spline_veb(),pr=tv_dl())[[1]]
    })
    })
  
  #Comparativo
  output$spar_veb_dl_comp <- renderPrint({input$parametro_veb_dl_comp})
  
  #
  #creo funcion que me calcula una sola llamada de la funcion de splines
  tabla_sp_dl_veb_comp <- reactive({
    dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n5,num =40,par = input$parametro_veb_dl_comp,tit=c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),car,pr=tv_comp())
    
  })
  
  
  
  dl_spline_veb_comp <- reactive({
    withProgress(message = 'Calculando spline a utilizar...', value = 0, {
    #   dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    #   dat[,3] <- as.Date(as.character(dat[,3]))
    #   car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n5,num =40,par = input$parametro_veb_dl_comp,tit=c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),car,pr=tv_comp())[[4]] 
      tabla_sp_dl_veb_comp()[[4]]
    })
      })
  
  output$spline_veb_comp_out <- renderPrint({dl_spline_veb_comp()})
  
  output$p_est_dl_veb_comp <- renderDataTable({
    withProgress(message = 'Calculando precios teóricos', value = 0, {
      incProgress(1/2, detail = "Realizando iteraciones")
      precio.dl(tit = c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),fv = input$n5 ,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")) ,pa = c(1,1,1,1),spline1 = dl_spline_veb_comp(),pr=tv_comp())[[1]]
      })
    })
  
  
  #grafico 
  #extraigo puntos con los q se hace la curva, para DL
  #tif
  pto_sp_tif_dl <- reactive({
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n3,num = 40,par = input$parametro_tif_dl,tit=c(input$t1_dl,input$t2_dl,input$t3_dl,input$t4_dl),car,pr=tf_dl())[[2]]
    a <- tabla_sp_dl_tif()[[2]]
    
    # a1 <- cbind.data.frame(a$Plazo,a$Rendimiento)
    # names(a1) <- c("Plazo","Rendimiento")
    return(a)
  })
  
  #comparativo
  pto_sp_tif_dl_comp <- reactive({
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # 
    # a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n5,num = 40,par = input$parametro_tif_dl_comp,tit=c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),car,pr=tf_comp())[[2]]
    a <- tabla_sp_dl_tif_comp()[[2]]
    # a1 <- cbind.data.frame(a$Plazo,a$Rendimiento)
    # names(a1) <- c("Plazo","Rendimiento")
    return(a)
  })
  
  
  #veb
  pto_sp_veb_dl <- reactive({
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # a <- Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n3,num = 40,par = input$parametro_veb_dl,tit=c(input$v1_dl,input$v2_dl,input$v3_dl,input$v4_dl),car,pr=tv_dl())[[2]]
    a <- tabla_sp_dl_veb()[[2]]
    
    
    # a1 <- cbind.data.frame(a$Plazo,a$Rendimiento)
    # names(a1) <- c("Plazo","Rendimiento")
    return(a)
  })
  
  #comparativo
  pto_sp_veb_dl_comp <- reactive({
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # a <- Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n5,num = 40,par = input$parametro_veb_dl_comp,tit=c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),car,pr=tv_comp())[[2]]
    a <- tabla_sp_dl_veb_comp()[[2]]
    # a1 <- cbind.data.frame(a$Plazo,a$Rendimiento)
    # names(a1) <- c("Plazo","Rendimiento")
    return(a)
  })
  
  
  #Splines para Diebold-Li
  #tif
  output$c_tif_splines_dl <- renderRbokeh({
    withProgress(message = 'Graficando curva de rendimiento...', value = 0, {
      incProgress(1/2, detail = "Calculando alturas")
      # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
      # dat[,3] <- as.Date(as.character(dat[,3]))
      # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
      #y <-predict(Tabla.splines(data = dat,tipo = "TIF",fe=input$n3,num = 40,par = input$parametro_tif_dl,tit=c(input$t1_dl,input$t2_dl,input$t3_dl,input$t4_dl),car,pr=tf_dl())[[4]],seq(0.1,20,0.1)*365)$y
      y <-predict(tabla_sp_dl_tif()[[4]],seq(0.1,20,0.1)*365)$y
      
      incProgress(1/2, detail = "Ajustando spline")
    
    figure(width = 1000,height = 400) %>%
      ly_points(pto_sp_tif_dl()[,4],pto_sp_tif_dl()[,7],pto_sp_tif_dl(),hover=list("Nombre"=pto_sp_tif_dl()[,1],"Fecha de operación"=pto_sp_tif_dl()[,2])) %>%
      ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="green",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
      # theme_title(text_color="green",text_align="center",text_font_style="italic")%>%
      x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)") 
    })
  })
  
  #comparativo
  output$c_tif_splines_dl_comp <- renderRbokeh({
    withProgress(message = 'Graficando curva de rendimiento...', value = 0, {
      incProgress(1/2, detail = "Calculando alturas")
      # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
      # dat[,3] <- as.Date(as.character(dat[,3]))
      # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    #y <-predict(Tabla.splines(data = dat,tipo = "TIF",fe=input$n5,num = 40,par = input$parametro_tif_dl_comp,tit=c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),car,pr=tf_comp())[[4]],seq(0.1,20,0.1)*365)$y
      y <-predict(tabla_sp_dl_tif_comp()[[4]],seq(0.1,20,0.1)*365)$y
      incProgress(1/2, detail = "ajustando spline")
    figure(width = 1000,height = 400) %>%
      ly_points(pto_sp_tif_dl_comp()[,4],pto_sp_tif_dl_comp()[,7],pto_sp_tif_dl_comp(),hover=list("Nombre"=pto_sp_tif_dl_comp()[,1],"Fecha de operación"=pto_sp_tif_dl_comp()[,2])) %>%
      ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="green",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
      # theme_title(text_color="green",text_align="center",text_font_style="italic")%>%
      x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)") 
    })
  })
  
  #vebonos
  output$c_veb_splines_dl <- renderRbokeh({
    withProgress(message = 'Graficando curva de rendimiento...', value = 0, {
      incProgress(1/2, detail = "Calculando alturas")
    #   dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    #   dat[,3] <- as.Date(as.character(dat[,3]))
    #   car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # y <-predict(Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n3,num = 40,par = input$parametro_veb_dl,tit=c(input$v1_dl,input$v2_dl,input$v3_dl,input$v4_dl),car,pr=tv_dl())[[4]],seq(0.1,20,0.1)*365)$y
      y <-predict(tabla_sp_dl_veb()[[4]],seq(0.1,20,0.1)*365)$y
      
      incProgress(1/2, detail = "Ajustando spline")
    
    figure(width = 1000,height = 400) %>%
      ly_points(pto_sp_veb_dl()[,4],pto_sp_veb_dl()[,7],pto_sp_veb_dl(),hover=list("Nombre"=pto_sp_veb_dl()[,1],"Fecha de operación"=pto_sp_veb_dl()[,2])) %>%
      ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="brown",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
      # theme_title(text_color="green",text_align="center",text_font_style="italic")%>%
      x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)") 
    })
  })
  
  #comparativo
  output$c_veb_splines_dl_comp <- renderRbokeh({
    withProgress(message = 'Graficando curva de rendimiento...', value = 0, {
      incProgress(1/2, detail = "Calculando alturas")
    #   dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    #   dat[,3] <- as.Date(as.character(dat[,3]))
    #   car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # y <-predict(Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n5,num = 40,par = input$parametro_veb_dl_comp,tit=c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),car,pr=tv_comp())[[4]],seq(0.1,20,0.1)*365)$y
    y <-predict(tabla_sp_dl_veb_comp()[[4]],seq(0.1,20,0.1)*365)$y
    incProgress(1/2, detail = "ajustando spline")
    figure(width = 1000,height = 400) %>%
      ly_points(pto_sp_veb_dl_comp()[,4],pto_sp_veb_dl_comp()[,7],pto_sp_veb_dl_comp(),hover=list("Nombre"=pto_sp_veb_dl_comp()[,1],"Fecha de operación"=pto_sp_veb_dl_comp()[,2])) %>%
      ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="brown",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
      # theme_title(text_color="green",text_align="center",text_font_style="italic")%>%
      x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)") 
    })
  })
  
  #Grafico 3D!
  #tif
  output$curva_tif_dl <- renderPlotly({ 
    #defino eje maduracion
    X1 <- seq(1,20,0.1)
    
    #defino tiempos
    Y1 <- seq(1,50,1)
    
    #
    var_par <- as.data.frame(matrix(0,length(Y1),4))
    
    #guardo parametros segun cada tiempo
    for(i in 1:length(Y1)){
      #var_par[i,] <- par_dl(t[i],spline1,pa=c(1,1,1,1))
      var_par[i,] <- par_dl(Y1,dl_spline_tif(),pa=c(1,1,1,1))
      
    }
    
    #calculo nuevos rendimientos a partir de los nuevos parametros
    new_rend <- as.data.frame(matrix(0,length(X1),length(Y1)))
    
    
    for(i in 1:length(Y1)){
      
      new_rend[,i] <- diebold_li(as.numeric(var_par[i,]),X1)
      #new_rend[,i] <- nelson_siegel(as.numeric(var_par[i,]),X)
    }
    
    Z1 <- as.matrix(new_rend*100)
    row.names(Z1) <- X1
    colnames(Z1) <- Y1
    
    #defino configuracion de los ejes
    # Create lists for axis properties
    # f1 <- list(
    #   family = "Arial, sans-serif",
    #   size = 18,
    #   color = "blue")
    # 
    # f2 <- list(
    #   family = "Old Standard TT, serif",
    #   size = 14,
    #   color = "green")
    # 
    # axis <- list(
    #   titlefont = f1,
    #   tickfont = f2
    #   #showgrid = F
    # )
    
    scene = list(
      xaxis = list(domain = c(0, 50),
                   title = "Tiempo"),
      yaxis = list(domain = c(0, 200),
                   title = "Maduración (años)"),
      zaxis = list(domain = c(0, 0.12),
                   title = "Rendimiento (%)"),
      camera = list(eye = list(x = -1.25, y = 1.25, z = 1.25))
    )
    
    
    plot_ly(z = Z1,  type = "surface") %>%
      layout(title = "Curva de Rendimientos metodología Diebold-Li",scene = scene)
    
    })
  
  #comparativo
  output$curva_tif_dl_comp <- renderPlotly({ 
    #defino eje maduracion
    X1 <- seq(1,20,0.1)
    
    #defino tiempos
    Y1 <- seq(1,50,1)
    
    #
    var_par <- as.data.frame(matrix(0,length(Y1),4))
    
    #guardo parametros segun cada tiempo
    for(i in 1:length(Y1)){
      #var_par[i,] <- par_dl(t[i],spline1,pa=c(1,1,1,1))
      var_par[i,] <- par_dl(Y1,dl_spline_tif_comp(),pa=c(1,1,1,1))
      
    }
    
    #calculo nuevos rendimientos a partir de los nuevos parametros
    new_rend <- as.data.frame(matrix(0,length(X1),length(Y1)))
    
    
    for(i in 1:length(Y1)){
      
      new_rend[,i] <- diebold_li(as.numeric(var_par[i,]),X1)
      #new_rend[,i] <- nelson_siegel(as.numeric(var_par[i,]),X)
    }
    
    Z1 <- as.matrix(new_rend*100)
    row.names(Z1) <- X1
    colnames(Z1) <- Y1
    
    #defino configuracion de los ejes
    # Create lists for axis properties
    # f1 <- list(
    #   family = "Arial, sans-serif",
    #   size = 18,
    #   color = "blue")
    # 
    # f2 <- list(
    #   family = "Old Standard TT, serif",
    #   size = 14,
    #   color = "green")
    # 
    # axis <- list(
    #   titlefont = f1,
    #   tickfont = f2
    #   #showgrid = F
    # )
    
    scene = list(
      xaxis = list(domain = c(0, 50),
                   title = "Tiempo"),
      yaxis = list(domain = c(0, 200),
                   title = "Maduración (años)"),
      zaxis = list(domain = c(0, 0.12),
                   title = "Rendimiento (%)"),
      camera = list(eye = list(x = -1.25, y = 1.25, z = 1.25))
    )
    
    
    plot_ly(z = Z1,  type = "surface") %>%
      layout(title = "Curva de Rendimientos metodología Diebold-Li",scene = scene)
    
  })
  
  #vebonos
  output$curva_veb_dl <- renderPlotly({ 
    #defino eje maduracion
    X1 <- seq(1,20,0.1)
    
    #defino tiempos
    Y1 <- seq(1,50,1)
    
    #
    var_par <- as.data.frame(matrix(0,length(Y1),4))
    
    #guardo parametros segun cada tiempo
    for(i in 1:length(Y1)){
      #var_par[i,] <- par_dl(t[i],spline1,pa=c(1,1,1,1))
      var_par[i,] <- par_dl(Y1,dl_spline_veb(),pa=c(1,1,1,1))
      
    }
    
    #calculo nuevos rendimientos a partir de los nuevos parametros
    new_rend <- as.data.frame(matrix(0,length(X1),length(Y1)))
    
    
    for(i in 1:length(Y1)){
      
      new_rend[,i] <- diebold_li(as.numeric(var_par[i,]),X1)
      #new_rend[,i] <- nelson_siegel(as.numeric(var_par[i,]),X)
    }
    
    Z1 <- as.matrix(new_rend*100)
    row.names(Z1) <- X1
    colnames(Z1) <- Y1
    
    #defino configuracion de los ejes
    # Create lists for axis properties
    # f1 <- list(
    #   family = "Arial, sans-serif",
    #   size = 18,
    #   color = "blue")
    # 
    # f2 <- list(
    #   family = "Old Standard TT, serif",
    #   size = 14,
    #   color = "green")
    # 
    # axis <- list(
    #   titlefont = f1,
    #   tickfont = f2
    #   #showgrid = F
    # )
    
    scene = list(
      xaxis = list(domain = c(0, 50),
                   title = "Tiempo"),
      yaxis = list(domain = c(0, 200),
                   title = "Maduración (años)"),
      zaxis = list(domain = c(0, 0.12),
                   title = "Rendimiento (%)"),
      camera = list(eye = list(x = -1.25, y = 1.25, z = 1.25))
    )
    
    
    plot_ly(z = Z1,  type = "surface") %>%
      layout(title = "Curva de Rendimientos metodología Diebold-Li",scene = scene)
    
  })
  
  #Comparativo
  output$curva_veb_dl_comp <- renderPlotly({ 
    #defino eje maduracion
    X1 <- seq(1,20,0.1)
    
    #defino tiempos
    Y1 <- seq(1,50,1)
    
    #
    var_par <- as.data.frame(matrix(0,length(Y1),4))
    
    #guardo parametros segun cada tiempo
    for(i in 1:length(Y1)){
      #var_par[i,] <- par_dl(t[i],spline1,pa=c(1,1,1,1))
      var_par[i,] <- par_dl(Y1,dl_spline_veb_comp(),pa=c(1,1,1,1))
      
    }
    
    #calculo nuevos rendimientos a partir de los nuevos parametros
    new_rend <- as.data.frame(matrix(0,length(X1),length(Y1)))
    
    
    for(i in 1:length(Y1)){
      
      new_rend[,i] <- diebold_li(as.numeric(var_par[i,]),X1)
      #new_rend[,i] <- nelson_siegel(as.numeric(var_par[i,]),X)
    }
    
    Z1 <- as.matrix(new_rend*100)
    row.names(Z1) <- X1
    colnames(Z1) <- Y1
    
    #defino configuracion de los ejes
    # Create lists for axis properties
    # f1 <- list(
    #   family = "Arial, sans-serif",
    #   size = 18,
    #   color = "blue")
    # 
    # f2 <- list(
    #   family = "Old Standard TT, serif",
    #   size = 14,
    #   color = "green")
    # 
    # axis <- list(
    #   titlefont = f1,
    #   tickfont = f2
    #   #showgrid = F
    # )
    
    scene = list(
      xaxis = list(domain = c(0, 50),
                   title = "Tiempo"),
      yaxis = list(domain = c(0, 200),
                   title = "Maduración (años)"),
      zaxis = list(domain = c(0, 0.12),
                   title = "Rendimiento (%)"),
      camera = list(eye = list(x = -1.25, y = 1.25, z = 1.25))
    )
    
    
    plot_ly(z = Z1,  type = "surface") %>%
      layout(title = "Curva de Rendimientos metodología Diebold-Li",scene = scene)
    
  })
  
  #parametros optimizados
  # output$par_tif_ns_op <- renderPrint({a <- Tabla.ns(fv = input$n2 ,tit = c(input$t1_ns,input$t2_ns,input$t3_ns,input$t4_ns),pr =tf_ns() ,pa = pa_ns,ind = 0,C = C,fe2=input$opt_tif_ns,fe3=0)
  # a[[2]]
  # })
  
  #graficos
  #output$c_tif_ns <- renderPlot({plot(seq(1,20,1),nelson_siegel(t=seq(1,20,1),pa=pa_ns)*100,type = "l",col="blue",xlab = "Maduración (años)",ylab="Rendimiento (%)",main = "Curva de redimientos Nelson y Siegel Parametros Iniciales")})
  #NELSON Y SIEGEL
  #TIF 
   output$c_tif_ns <- renderPlot({
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=nelson_siegel(t=seq(0.1,20,0.1),pa=pa_ns)*100),aes(x=x,y=y))+
      geom_line(color="blue")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de rendimiento Nelson y Siegel Parámetros Iniciales TIF")+
      theme(plot.title = element_text(hjust = 0.5))
    
  })
   #
   output$c_tif_ns1_new <- renderPlot({
     ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=nelson_siegel(t=seq(0.1,20,0.1),pa=c(input$ns_b0_tif,input$ns_b1_tif,input$ns_b2_tif,input$ns_t_tif))*100),aes(x=x,y=y))+
       geom_line(color="brown")+xlab("Maduración (años)")+
       ylab("Rendimiento (%)")+theme_gray()+
       ggtitle("Curva de rendimiento Nelson y Siegel Parámetros elegidos TIF")+
       theme(plot.title = element_text(hjust = 0.5))
   })
   #comparativo
   output$c_tif_ns1_new_comp <- renderPlot({
     ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=nelson_siegel(t=seq(0.1,20,0.1),pa=c(input$ns_b0_tif_comp,input$ns_b1_tif_comp,input$ns_b2_tif_comp,input$ns_t_tif_comp))*100),aes(x=x,y=y))+
       geom_line(color="red")+xlab("Maduración (años)")+
       ylab("Rendimiento (%)")+theme_gray()+
       ggtitle("Curva de rendimiento Nelson y Siegel Parámetros elegidos TIF")+
       theme(plot.title = element_text(hjust = 0.5))
   })
   
   
   #
   output$c_veb_ns1_new <- renderPlot({
     ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=nelson_siegel(t=seq(0.1,20,0.1),pa=c(input$ns_b0_veb,input$ns_b1_veb,input$ns_b2_veb,input$ns_t_veb))*100),aes(x=x,y=y))+
       geom_line(color="brown")+xlab("Maduración (años)")+
       ylab("Rendimiento (%)")+theme_gray()+
       ggtitle("Curva de rendimiento Nelson y Siegel Parámetros elegidos VEBONO")+
       theme(plot.title = element_text(hjust = 0.5))
   })
   
   #comparativo
   output$c_veb_ns1_new_comp <- renderPlot({
     ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=nelson_siegel(t=seq(0.1,20,0.1),pa=c(input$ns_b0_veb_comp,input$ns_b1_veb_comp,input$ns_b2_veb_comp,input$ns_t_veb_comp))*100),aes(x=x,y=y))+
       geom_line(color="brown")+xlab("Maduración (años)")+
       ylab("Rendimiento (%)")+theme_gray()+
       ggtitle("Curva de rendimiento Nelson y Siegel Parámetros elegidos VEBONO")+
       theme(plot.title = element_text(hjust = 0.5))
   })
   
   
  #
  output$c_veb_ns <- renderPlot({
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=nelson_siegel(t=seq(0.1,20,0.1),pa=pa1_ns)*100),aes(x=x,y=y))+
      geom_line(color="blue")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de rendimiento Nelson y Siegel Parámetros Iniciales VEBONOS")+
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  #SVENSSON
  output$c_tif_sven <- renderPlot({
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=sven(t=seq(0.1,20,0.1),pa=pa_sven)*100),aes(x=x,y=y))+
      geom_line(color="blue")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de rendimiento Svensson Parámetros Iniciales TIF")+
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  #
  output$c_veb_sven <- renderPlot({
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=sven(t=seq(0.1,20,0.1),pa=pa1_sven)*100),aes(x=x,y=y))+
      geom_line(color="blue")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de rendimiento Svensson Parámetros Iniciales VEBONOS")+
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  #
  output$c_tif_sven_new <- renderPlot({
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=sven(t=seq(0.1,20,0.1),pa=c(input$sven_b0_tif,input$sven_b1_tif,input$sven_b2_tif,input$sven_b3_tif,input$sven_t1_tif,input$sven_t2_tif))*100),aes(x=x,y=y))+
      geom_line(color="brown")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de rendimiento Nelson y Siegel Parámetros elegidos TIF")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  #comparativo
  output$c_tif_sven_new_comp <- renderPlot({
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=sven(t=seq(0.1,20,0.1),pa=c(input$sven_b0_tif_comp,input$sven_b1_tif_comp,input$sven_b2_tif_comp,input$sven_b3_tif_comp,input$sven_t1_tif_comp,input$sven_t2_tif_comp))*100),aes(x=x,y=y))+
      geom_line(color="brown")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de rendimiento Nelson y Siegel Parámetros elegidos TIF")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  #
  output$c_veb_sven_new <- renderPlot({
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=sven(t=seq(0.1,20,0.1),pa=c(input$sven_b0_veb,input$sven_b1_veb,input$sven_b2_veb,input$sven_b3_veb,input$sven_t1_veb,input$sven_t2_veb))*100),aes(x=x,y=y))+
      geom_line(color="brown")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de rendimiento Svensson Parámetros elegidos VEBONOS")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  #comparativo
  output$c_veb_sven_new_comp <- renderPlot({
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=sven(t=seq(0.1,20,0.1),pa=c(input$sven_b0_veb_comp,input$sven_b1_veb_comp,input$sven_b2_veb_comp,input$sven_b3_veb_comp,input$sven_t1_veb_comp,input$sven_t2_veb_comp))*100),aes(x=x,y=y))+
      geom_line(color="brown")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de rendimiento Svensson Parámetros elegidos VEBONOS")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  #caso Nelson y Siegel
  #gra_tif_ns <- reactive({Tabla.ns(fv = input$n2 ,tit = c(input$t1_ns,input$t2_ns,input$t3_ns,input$t4_ns),pr =tf_ns() ,pa = c(1,1,1,1),ind = 0,C = C,fe2=input$opt_tif_ns,fe3=0)[[2]] })
  gra_tif_ns <- reactive({Tabla.ns(fv = input$n2 ,tit = c(input$t1_ns,input$t2_ns,input$t3_ns,input$t4_ns),pr =tf_ns() ,pa = c(1,1,1,1),ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_tif_ns,fe3=0)[[2]] })
  
  gra_veb_ns <- reactive({Tabla.ns(fv = input$n2 ,tit = c(input$v1_ns,input$v2_ns,input$v3_ns,input$v4_ns),pr =tv_ns() ,pa = c(1,1,1,1),ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_veb_ns,fe3=0)[[2]] })
  

  output$par_tif_ns_op<-renderPrint({if(input$opt_tif_ns==1){gra_tif_ns()
  }else{}})
  
  #comparativo
  gra_tif_ns_comp <- reactive({Tabla.ns(fv = input$n5 ,tit = c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),pr =tf_comp() ,pa = c(1,1,1,1),ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_tif_ns_comp,fe3=0)[[2]] })
  
  
  output$par_tif_ns_op_comp<-renderPrint({if(input$opt_tif_ns_comp==1){gra_tif_ns_comp()
  }else{}})
  
  
  output$c_tif_ns_op <- renderPlot({if(input$opt_tif_ns==1){
    #plot(seq(1,20,1),nelson_siegel(t=seq(1,20,1),pa=gra())*100,type = "l",col="blue",xlab = "Maduración (años)",ylab="Rendimiento (%)",main = "Curva de redimientos Nelson y Siegel Parametros Optimizados TIF")
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=nelson_siegel(t=seq(0.1,20,0.1),pa=gra_tif_ns())*100),aes(x=x,y=y))+
      geom_line(color="green")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de redimientos Nelson y Siegel Parametros Optimizados TIF")+
      theme(plot.title = element_text(hjust = 0.5))
    }else{}})
  
  #comparativo
  output$c_tif_ns_op_comp <- renderPlot({if(input$opt_tif_ns_comp==1){
    #plot(seq(1,20,1),nelson_siegel(t=seq(1,20,1),pa=gra())*100,type = "l",col="blue",xlab = "Maduración (años)",ylab="Rendimiento (%)",main = "Curva de redimientos Nelson y Siegel Parametros Optimizados TIF")
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=nelson_siegel(t=seq(0.1,20,0.1),pa=gra_tif_ns_comp())*100),aes(x=x,y=y))+
      geom_line(color="blue")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de redimientos Nelson y Siegel Parametros Optimizados TIF")+
      theme(plot.title = element_text(hjust = 0.5))
  }else{}})
  
  
  #
  output$par_veb_ns_op<-renderPrint({if(input$opt_veb_ns==1){gra_veb_ns()
  }else{}})

  output$c_veb_ns_op <- renderPlot({if(input$opt_veb_ns==1){
    #plot(seq(1,20,1),nelson_siegel(t=seq(1,20,1),pa=gra())*100,type = "l",col="blue",xlab = "Maduración (años)",ylab="Rendimiento (%)",main = "Curva de redimientos Nelson y Siegel Parametros Optimizados TIF")
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=nelson_siegel(t=seq(0.1,20,0.1),pa=gra_veb_ns())*100),aes(x=x,y=y))+
      geom_line(color="green")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de redimientos Nelson y Siegel Parametros Optimizados VEBONOS")+
      theme(plot.title = element_text(hjust = 0.5))
  }else{}})
    
  #comparativo
  gra_veb_ns_comp <- reactive({Tabla.ns(fv = input$n5 ,tit = c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),pr =tv_comp() ,pa = c(1,1,1,1),ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_veb_ns_comp,fe3=0)[[2]] })
  
  
  output$par_veb_ns_op_comp<-renderPrint({if(input$opt_veb_ns_comp==1){gra_veb_ns_comp()
  }else{}})
  
  output$c_veb_ns_op_comp <- renderPlot({if(input$opt_veb_ns_comp==1){
    #plot(seq(1,20,1),nelson_siegel(t=seq(1,20,1),pa=gra())*100,type = "l",col="blue",xlab = "Maduración (años)",ylab="Rendimiento (%)",main = "Curva de redimientos Nelson y Siegel Parametros Optimizados TIF")
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=nelson_siegel(t=seq(0.1,20,0.1),pa=gra_veb_ns_comp())*100),aes(x=x,y=y))+
      geom_line(color="green")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de redimientos Nelson y Siegel Parametros Optimizados VEBONOS")+
      theme(plot.title = element_text(hjust = 0.5))
  }else{}})
  
  #caso Svensson
  gra_tif_sven <- reactive({Tabla.sven(fv = input$n1 ,tit = c(input$t1,input$t2,input$t3,input$t4),pr =tf() ,pa = c(1,1,1,1,1,1),ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_tif_sven,fe3=0)[[2]] })
  gra_veb_sven <- reactive({Tabla.sven(fv = input$n1 ,tit = c(input$v1,input$v2,input$v3,input$v4),pr =tv() ,pa = c(1,1,1,1,1,1),ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_veb_sven,fe3=0)[[2]] })
  
  #tif
  output$par_tif_sven_op<-renderPrint({if(input$opt_tif_sven==1){gra_tif_sven()
  }else{}})
  
  #
  output$c_tif_sven_op <- renderPlot({if(input$opt_tif_sven==1){
    #plot(seq(1,20,1),nelson_siegel(t=seq(1,20,1),pa=gra())*100,type = "l",col="blue",xlab = "Maduración (años)",ylab="Rendimiento (%)",main = "Curva de redimientos Nelson y Siegel Parametros Optimizados TIF")
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=sven(t=seq(0.1,20,0.1),pa=gra_tif_sven())*100),aes(x=x,y=y))+
      geom_line(color="green")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de redimientos Svensson Parametros Optimizados TIF")+
      theme(plot.title = element_text(hjust = 0.5))
  }else{}})
  
  #comparativo
  gra_tif_sven_comp <- reactive({Tabla.sven(fv = input$n5 ,tit = c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),pr =tf_comp() ,pa = c(1,1,1,1,1,1),ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_tif_sven_comp,fe3=0)[[2]] })
  gra_veb_sven_comp <- reactive({Tabla.sven(fv = input$n5 ,tit = c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),pr =tv_comp() ,pa = c(1,1,1,1,1,1),ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=input$opt_veb_sven_comp,fe3=0)[[2]] })
  
  #tif
  output$par_tif_sven_op_comp<-renderPrint({if(input$opt_tif_sven_comp==1){gra_tif_sven_comp()
  }else{}})
  
  #
  output$c_tif_sven_op_comp <- renderPlot({if(input$opt_tif_sven_comp==1){
    #plot(seq(1,20,1),nelson_siegel(t=seq(1,20,1),pa=gra())*100,type = "l",col="blue",xlab = "Maduración (años)",ylab="Rendimiento (%)",main = "Curva de redimientos Nelson y Siegel Parametros Optimizados TIF")
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=sven(t=seq(0.1,20,0.1),pa=gra_tif_sven_comp())*100),aes(x=x,y=y))+
      geom_line(color="green")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de redimientos Svensson Parametros Optimizados TIF")+
      theme(plot.title = element_text(hjust = 0.5))
  }else{}})
  
  #veb
  output$par_veb_sven_op<-renderPrint({if(input$opt_veb_sven==1){gra_veb_sven()
  }else{}})
  
  #
  output$c_veb_sven_op <- renderPlot({if(input$opt_veb_sven==1){
    #plot(seq(1,20,1),nelson_siegel(t=seq(1,20,1),pa=gra())*100,type = "l",col="blue",xlab = "Maduración (años)",ylab="Rendimiento (%)",main = "Curva de redimientos Nelson y Siegel Parametros Optimizados TIF")
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=sven(t=seq(0.1,20,0.1),pa=gra_veb_sven())*100),aes(x=x,y=y))+
      geom_line(color="green")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de redimientos Svensson Parametros Optimizados VEBONOS")+
      theme(plot.title = element_text(hjust = 0.5))
  }else{}})
  
  #comparativo
  output$par_veb_sven_op_comp<-renderPrint({if(input$opt_veb_sven_comp==1){gra_veb_sven_comp()
  }else{}})
  
  #
  output$c_veb_sven_op_comp <- renderPlot({if(input$opt_veb_sven_comp==1){
    #plot(seq(1,20,1),nelson_siegel(t=seq(1,20,1),pa=gra())*100,type = "l",col="blue",xlab = "Maduración (años)",ylab="Rendimiento (%)",main = "Curva de redimientos Nelson y Siegel Parametros Optimizados TIF")
    ggplot(cbind.data.frame(x=seq(0.1,20,0.1),y=sven(t=seq(0.1,20,0.1),pa=gra_veb_sven_comp())*100),aes(x=x,y=y))+
      geom_line(color="green")+xlab("Maduración (años)")+
      ylab("Rendimiento (%)")+theme_gray()+
      ggtitle("Curva de redimientos Svensson Parametros Optimizados VEBONOS")+
      theme(plot.title = element_text(hjust = 0.5))
  }else{}})
  
  #SPLINES
  #tif
  output$dias_tif <- renderPrint({input$d_tif})
  output$spar_tif <- renderPrint({input$parametro_tif})
  
  #comparativo
  output$dias_tif_comp <- renderPrint({input$d_tif_comp})
  output$spar_tif_comp <- renderPrint({input$parametro_tif_comp})
  
  #veb
  output$dias_veb <- renderPrint({input$d_veb})
  output$spar_veb <- renderPrint({input$parametro_veb})
  
  #comparativo
  output$dias_veb_comp <- renderPrint({input$d_veb_comp})
  output$spar_veb_comp <- renderPrint({input$parametro_veb_comp})
  
  
  #precios
  #output$pre_sp_tif <- renderDataTable({Tabla.splines(data = data_splines,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),C_splines,pr=tf_sp())[[5]] })
  output$pre_sp_tif <- renderDataTable({
    #Tabla.splines(data = data_splines,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),C_splines,pr=tf_sp())[[5]] 
    #  dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    #  dat[,3] <- as.Date(as.character(dat[,3]))
    #  car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # # #print(str(data_splines))
    # #print(str(dat))
     #Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[5]]
    a <- tabla_sp_tif()
    a[[5]]
     # 
    
    })
  
  #output$pre_sp <- renderPrint({precio_diario_sp(fe=input$n4,num=input$d_tif,par =input$parametro_tif ,datatif =datatif ,tit =tf_sp() ,C=C_splines,letra=c(97,1.34)) })
  output$pre_sp_veb <- renderDataTable({
    #Tabla.splines(data = data_splines,tipo = "VEBONO",fe=input$n4,num = input$d_veb,par = input$parametro_veb,tit=c(input$v1_sp,input$v2_sp,input$v3_sp,input$v4_sp),C_splines,pr=tv_sp())[[5]] 
     # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
     # dat[,3] <- as.Date(as.character(dat[,3]))
     # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
     # 
     # Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n4,num = input$d_veb,par = input$parametro_veb,tit=c(input$v1_sp,input$v2_sp,input$v3_sp,input$v4_sp),car,pr=tv_sp())[[5]] 
     # 
    a <- tabla_sp_veb()
    a[[5]]
    
    })
  
  #comparativo
  #tif
  output$pre_sp_tif_comp <- renderDataTable({
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # Tabla.splines(data = dat,tipo = "TIF",fe=input$n5,num = input$d_tif_comp,par = input$parametro_tif_comp,tit=c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),car,pr=tf_comp())[[5]] 
    tabla_sp_tif_comp()[[5]]
    })

  #veb
  output$pre_sp_veb_comp <- renderDataTable({
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n5,num = input$d_veb_comp,par = input$parametro_veb_comp,tit=c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),car,pr=tv_comp())[[5]] 
    tabla_sp_veb_comp()[[5]]
    })
  
  
  
  #curva de rendimiento
  #tif
  output$c_tif_splines <- renderRbokeh({
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # 
    
    
    # y <-predict(Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[4]],seq(0.1,20,0.1)*365)$y
    a <- tabla_sp_tif()
    y <-predict(a[[4]],seq(0.1,20,0.1)*365)$y
    
    # f <- ggplot(cbind.data.frame(x=seq(1,20,0.1)*365,y),aes(x=x,y=y))+
    #   geom_line(color="black")+
    #   geom_point(data = pto_sp_tif(),aes(x=pto_sp_tif()[,1],y=pto_sp_tif()[,2]),color="blue",size=3)+
    #   xlab("Maduración (días)")+
    #   ylab("Rendimiento (%)")+theme_gray()+
    #   ggtitle("Curva de redimientos Splines TIF ")+
    #   theme(plot.title = element_text(hjust = 0.5))
    #   
    # 
    # ggplotly(f) 
    
    #letra <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[3]]
    letra <- a[[3]]
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    
    # names(letra1)=names(pto_sp_tif())
    # 
    # figure(width = 1000,height = 400) %>%
    #   ly_points(c(letra[,7],pto_sp_tif()[,4]),c(letra[,15],pto_sp_tif()[,7]),rbind.data.frame(letra1,pto_sp_tif()),hover=list("Nombre"=c(as.character(letra[,2]),as.character(pto_sp_tif()[,1])),"Fecha de operación"=c(letra[,3],pto_sp_tif()[,2]))) %>%
     
      names(letra1)=names(a[[2]])
     
      figure(width = 1000,height = 400) %>%
      ly_points(c(letra[,7],a[[2]][,4]),c(letra[,15],a[[2]][,7]),rbind.data.frame(letra1,a[[2]]),hover=list("Nombre"=c(as.character(letra[,2]),as.character(a[[2]][,1])),"Fecha de operación"=c(letra[,3],a[[2]][,2]))) %>%
      ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="blue",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
      # theme_title(text_color="green",text_align="center",text_font_style="italic")%>%
      x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)") 
  
  })

  #comparativo
  output$c_tif_splines_comp <- renderRbokeh({
    #y <-predict(tabla_sp_tif_comp()[[4]],seq(0.1,20,0.1)*365)$y
    
    
    #figure(width = 1000,height = 400) %>%
     # ly_points(pto_sp_tif_comp()[,4],pto_sp_tif_comp()[,7],pto_sp_tif_comp(),hover=list("Nombre"=pto_sp_tif_comp()[,1],"Fecha de operación"=pto_sp_tif_comp()[,2])) %>%
      #ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="blue",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
      # theme_title(text_color="green",text_align="center",text_font_style="italic")%>%
      #x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)") 
    
    a <- tabla_sp_tif_comp()
    y <-predict(a[[4]],seq(0.1,20,0.1)*365)$y
    
    
    #letra <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[3]]
    letra <- a[[3]]
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    
    # names(letra1)=names(pto_sp_tif())
    # 
    # figure(width = 1000,height = 400) %>%
    #   ly_points(c(letra[,7],pto_sp_tif()[,4]),c(letra[,15],pto_sp_tif()[,7]),rbind.data.frame(letra1,pto_sp_tif()),hover=list("Nombre"=c(as.character(letra[,2]),as.character(pto_sp_tif()[,1])),"Fecha de operación"=c(letra[,3],pto_sp_tif()[,2]))) %>%
    
    names(letra1)=names(a[[2]])
    
    figure(width = 1000,height = 400) %>%
      ly_points(c(letra[,7],a[[2]][,4]),c(letra[,15],a[[2]][,7]),rbind.data.frame(letra1,a[[2]]),hover=list("Nombre"=c(as.character(letra[,2]),as.character(a[[2]][,1])),"Fecha de operación"=c(letra[,3],a[[2]][,2]))) %>%
      ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="blue",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
      # theme_title(text_color="green",text_align="center",text_font_style="italic")%>%
      x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)") 
    
  })
  
  #veb
  output$c_veb_splines <- renderRbokeh({
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # 
    # y <-predict(Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n4,num = input$d_veb,par = input$parametro_veb,tit=c(input$v1_sp,input$v2_sp,input$v3_sp,input$v4_sp),car,pr=tv_sp())[[4]],seq(0.1,20,0.1)*365)$y
    a <- tabla_sp_veb()
    y <-predict(a[[4]],seq(0.1,20,0.1)*365)$y
    
    
    # f <- ggplot(cbind.data.frame(x=seq(1,20,0.1)*365,y),aes(x=x,y=y))+
    #   geom_line(color="black")+
    #   geom_point(data = pto_sp_veb(),aes(x=pto_sp_veb()[,1],y=pto_sp_veb()[,2]),color="blue",size=3)+
    #   xlab("Maduración (días)")+
    #   ylab("Rendimiento (%)")+theme_gray()+
    #   ggtitle("Curva de redimientos Splines VEBONO ")+
    #   theme(plot.title = element_text(hjust = 0.5))
    # 
    # 
    # ggplotly(f) 
    # %>%
    #   add_markers(text= ~  pto_sp_tif()[,1],hoverinfo = "text")
    # figure(width = 1000,height = 400) %>%
    #   ly_points(pto_sp_veb()[,4],pto_sp_veb()[,7],pto_sp_veb(),hover=list("Nombre"=pto_sp_veb()[,1],"Fecha de operación"=pto_sp_veb()[,2])) %>%
    #   ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="blue",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
    #   # theme_title(text_color="green",text_align="center",text_font_style="italic")%>%
    #   x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)") 
    # 
    
    letra <- a[[3]]
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    
    # names(letra1)=names(pto_sp_tif())
    # 
    # figure(width = 1000,height = 400) %>%
    #   ly_points(c(letra[,7],pto_sp_tif()[,4]),c(letra[,15],pto_sp_tif()[,7]),rbind.data.frame(letra1,pto_sp_tif()),hover=list("Nombre"=c(as.character(letra[,2]),as.character(pto_sp_tif()[,1])),"Fecha de operación"=c(letra[,3],pto_sp_tif()[,2]))) %>%
    
    names(letra1)=names(a[[2]])
    
    figure(width = 1000,height = 400) %>%
      ly_points(c(letra[,7],a[[2]][,4]),c(letra[,15],a[[2]][,7]),rbind.data.frame(letra1,a[[2]]),hover=list("Nombre"=c(as.character(letra[,2]),as.character(a[[2]][,1])),"Fecha de operación"=c(letra[,3],a[[2]][,2]))) %>%
      ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="blue",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
      # theme_title(text_color="green",text_align="center",text_font_style="italic")%>%
      x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)") 
    
  })
  
  #comparativo
  output$c_veb_splines_comp <- renderRbokeh({
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # y <-predict(Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n5,num = input$d_veb_comp,par = input$parametro_veb_comp,tit=c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),car,pr=tv_comp())[[4]],seq(0.1,20,0.1)*365)$y
    a <- tabla_sp_veb_comp()
    y <-predict(a[[4]],seq(0.1,20,0.1)*365)$y
    
    
    #letra <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[3]]
    letra <- a[[3]]
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    
    # names(letra1)=names(pto_sp_tif())
    # 
    # figure(width = 1000,height = 400) %>%
    #   ly_points(c(letra[,7],pto_sp_tif()[,4]),c(letra[,15],pto_sp_tif()[,7]),rbind.data.frame(letra1,pto_sp_tif()),hover=list("Nombre"=c(as.character(letra[,2]),as.character(pto_sp_tif()[,1])),"Fecha de operación"=c(letra[,3],pto_sp_tif()[,2]))) %>%
    
    names(letra1)=names(a[[2]])
    
    figure(width = 1000,height = 400) %>%
      ly_points(c(letra[,7],a[[2]][,4]),c(letra[,15],a[[2]][,7]),rbind.data.frame(letra1,a[[2]]),hover=list("Nombre"=c(as.character(letra[,2]),as.character(a[[2]][,1])),"Fecha de operación"=c(letra[,3],a[[2]][,2]))) %>%
      ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="blue",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
      # theme_title(text_color="green",text_align="center",text_font_style="italic")%>%
      x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)")  
    
    
  })
  
  #titulos candidatos
  #funcion que calcula una sola vez valores de funcion "Tabla Spline"
  #TIF
  tabla_sp_tif <- reactive({
    dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())
    return(a)
  })
  
  #VEBONOS
  tabla_sp_veb <- reactive({
    dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    a <- Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n4,num = input$d_veb,par = input$parametro_veb,tit=c(input$v1_sp,input$v2_sp,input$v3_sp,input$v4_sp),car,pr=tv_sp())
    return(a)
  })
  
  
  #tif
  output$tit_cand_tif <- renderDataTable({
    #Tabla.splines(data = data_splines,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),C_splines,pr=tf_sp())[[2]]
     # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
     # dat[,3] <- as.Date(as.character(dat[,3]))
     # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    #return(car)
     # #print(str(data_splines))
    # #print(str(dat))
     #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())
     a <- tabla_sp_tif()
     letra <- a[[3]]
     letra[,6] <- as.Date(letra[,6])
     letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
     cand <- a[[2]]
     
     
     names(letra1)=names(cand)
     
     a1 <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
     return(a1)
     
     #Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[2]]
    
     })
  
  #comparativo
  #creo funcion para llamar una sola vez a la funcion splines
  tabla_sp_tif_comp <- reactive({
    dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    Tabla.splines(data = dat,tipo = "TIF",fe=input$n5,num = input$d_tif_comp,par = input$parametro_tif_comp,tit=c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),car,pr=tf_comp())
  })
  
  
  output$tit_cand_tif_comp <- renderDataTable({
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # Tabla.splines(data = dat,tipo = "TIF",fe=input$n5,num = input$d_tif_comp,par = input$parametro_tif_comp,tit=c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),car,pr=tf_comp())[[2]] 
    a <- tabla_sp_tif_comp()
    letra <- a[[3]]
    letra[,6] <- as.Date(letra[,6])
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    cand <- a[[2]]
    
    
    names(letra1)=names(cand)
    
    a1 <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
    return(a1)
    
  
    })
  
  
  #veb
  output$tit_cand_veb <- renderDataTable({
    #Tabla.splines(data = data_splines,tipo = "VEBONO",fe=input$n4,num = input$d_veb,par = input$parametro_veb,tit=c(input$v1_sp,input$v2_sp,input$v3_sp,input$v4_sp),C_splines,pr=tv_sp())[[2]] 
     # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
     # dat[,3] <- as.Date(as.character(dat[,3]))
     # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
     # 
     # Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n4,num = input$d_veb,par = input$parametro_veb,tit=c(input$v1_sp,input$v2_sp,input$v3_sp,input$v4_sp),car,pr=tv_sp())[[2]] 
     # 
    a <- tabla_sp_veb()
    letra <- a[[3]]
    letra[,6] <- as.Date(letra[,6])
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    cand <- a[[2]]
    
    
    names(letra1)=names(cand)
    
    a1 <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
    return(a1)
    
    
    })
  
  #comparativo
  #creo funcion auxiliar para llamar una sola vez a funcion splines
  tabla_sp_veb_comp <- reactive({
    dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n5,num = input$d_veb_comp,par = input$parametro_veb_comp,tit=c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),car,pr=tv_comp()) 
  })
  
  output$tit_cand_veb_comp <- renderDataTable({
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n5,num = input$d_veb_comp,par = input$parametro_veb_comp,tit=c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),car,pr=tv_comp())[[2]] 
    #tabla_sp_veb_comp()[[2]]
    
    a <- tabla_sp_veb_comp()
    letra <- a[[3]]
    letra[,6] <- as.Date(letra[,6])
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    cand <- a[[2]]
    
    
    names(letra1)=names(cand)
    
    a1 <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
    return(a1)
    
    })
  
  
  
  #extraigo puntos con los q se hace la curva
  #tif
  pto_sp_tif <- reactive({
    dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    
    a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[2]]
    
    #a <- Tabla.splines(data = data_splines,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),C_splines,pr=tf_sp())[[2]]
    # a1 <- cbind.data.frame(a$Plazo,a$Rendimiento)
    # names(a1) <- c("Plazo","Rendimiento")
    return(a)
    })
  
  #comparativo
  pto_sp_tif_comp <- reactive({
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n5,num = input$d_tif_comp,par = input$parametro_tif_comp,tit=c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),car,pr=tf_comp())[[2]]
    a <- tabla_sp_tif_comp()[[2]]
    # a1 <- cbind.data.frame(a$Plazo,a$Rendimiento)
    # names(a1) <- c("Plazo","Rendimiento")
    return(a)
  })
  
  #veb
  pto_sp_veb <- reactive({
    dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    a <- Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n4,num = input$d_veb,par = input$parametro_veb,tit=c(input$v1_sp,input$v2_sp,input$v3_sp,input$v4_sp),car,pr=tv_sp())[[2]]
    # a1 <- cbind.data.frame(a$Plazo,a$Rendimiento)
    # names(a1) <- c("Plazo","Rendimiento")
    return(a)
  })
  
  #comparativo
  pto_sp_veb_comp <- reactive({
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # a <- Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n5,num = input$d_veb_comp,par = input$parametro_veb_comp,tit=c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),car,pr=tv_comp())[[2]]
    a <- tabla_sp_veb_comp()[[2]]
    # a1 <- cbind.data.frame(a$Plazo,a$Rendimiento)
    # names(a1) <- c("Plazo","Rendimiento")
    return(a)
  })
  
  
  #output$datos <- renderPrint({pto_sp_tif()})
  
  #COMPARATIVO DE PRECIOS
  #tif
  gra_tif_ns_comp_i <- reactive({Tabla.ns(fv = input$n5 ,tit = c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),pr =tf_comp() ,pa = c(1,1,1,1),ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=1,fe3=0)[[2]] })
  
  gra_tif_sven_comp_i <- reactive({Tabla.sven(fv = input$n5 ,tit = c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),pr =tf_comp() ,pa = c(1,1,1,1,1,1),ind = 0,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=1,fe3=0)[[2]] })
  
  
  precios_tif <- reactive({
    #ojo con los dos primeros
    withProgress(message = 'Calculando precios...', value = 0, {
    #   incProgress(1/5, detail = "Metodología Nelson y Siegel")
       dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
       dat[,3] <- as.Date(as.character(dat[,3]))
       car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # a <-   Tabla.ns(fv = input$n5 ,tit = c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),pr =tf_comp() ,pa = gra_tif_ns_comp_i(),ind = 0,C = car,fe2=0,fe3=0)[[3]]
    incProgress(1/5, detail = "Metodología Svensson")
    b <-   Tabla.sven(fv = input$n5 ,tit = c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),pr =tf_comp() ,pa = gra_tif_sven_comp_i() ,ind = 0,C = car,fe2=0,fe3=0)[[3]]
    #
    # incProgress(1/5, detail = "Metodología Diebold-Li")
    # c <-   precio.dl(tit = c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),fv = input$n5 ,C = car ,pa = c(1,1,1,1),spline1 = dl_spline_tif_comp(),pr=tf_comp())[[2]]
    incProgress(1/5, detail = "Metodología Splines")
    d <-   Tabla.splines(data = dat,tipo = "TIF",fe=input$n5,num = input$d_tif_comp,par = input$parametro_tif_comp,tit=c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),car,pr=tf_comp())[[1]]
    
    })
    
    #f <- cbind.data.frame(c(tf_comp(),0),a[,2],b[,2],c[,2],d[,2])
    f <- cbind.data.frame(c(tf_comp(),0),b[,2],d[,2])
    
    #names(f) <- c("Precio Promedio","Nelson y Siegel","Svensson","Diebold-Li","Splines")
    names(f) <- c("Precio Promedio","Svensson","Splines")
    
    #row.names(f)[length(f[,1])] <- "SRC"
    row.names(f)[dim(f)[1]] <- "SRC"
    return(f)
    
  })
  
  output$comparativo_precios_tif <- renderDataTable({precios_tif()})
  
  
  #veb
  gra_veb_ns_comp_i <- reactive({Tabla.ns(fv = input$n5 ,tit = c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),pr =tv_comp() ,pa = c(1,1,1,1),ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=1,fe3=0)[[2]] })
  
  gra_veb_sven_comp_i <- reactive({Tabla.sven(fv = input$n5 ,tit = c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),pr =tv_comp() ,pa = c(1,1,1,1,1,1),ind = 1,C = Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")),fe2=1,fe3=0)[[2]] })
  
  
  precios_veb <- reactive({
    #ojo con los dos primeros
    withProgress(message = 'Calculando precios...', value = 0, {
      incProgress(1/5, detail = "Metodología Nelson y Siegel")
      dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
      dat[,3] <- as.Date(as.character(dat[,3]))
      car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
     a <-   Tabla.ns(fv = input$n5 ,tit = c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),pr =tv_comp() ,pa = gra_veb_ns_comp_i(),ind = 1,C = car,fe2=0,fe3=0)[[3]]
     incProgress(1/5, detail = "Metodología Svensson")
     b <-   Tabla.sven(fv = input$n5 ,tit = c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),pr =tv_comp() ,pa = gra_veb_sven_comp_i() ,ind = 1,C = car,fe2=0,fe3=0)[[3]]
    #
     incProgress(1/5, detail = "Metodología Diebold-Li")
     c <-   precio.dl(tit = c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),fv = input$n5 ,C = car ,pa = c(1,1,1,1),spline1 = dl_spline_veb_comp(),pr=tv_comp())[[2]]
     incProgress(1/5, detail = "Metodología Splines")
      d <-   Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n5,num = input$d_veb_comp,par = input$parametro_veb_comp,tit=c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),car,pr=tv_comp())[[1]]
    
    })
     
     f <- cbind.data.frame(c(tv_comp(),0),a[,2],b[,2],c[,2],d[,2])
     names(f) <- c("Precio Promedio","Nelson y Siegel","Svensson","Diebold-Li","Splines")
     #row.names(f)[length(f[,1])] <- "SRC"
     row.names(f)[dim(f)[1]] <- "SRC"
     return(f)
    
  })
  
  output$comparativo_precios_veb <- renderDataTable({precios_veb()})
  
  #Graficas comparativos
  #tif
  spline_tif_comp <- reactive({
    dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    Tabla.splines(data = dat,tipo = "TIF",fe=input$n5,num =input$d_tif_comp,par = input$parametro_tif_comp,tit=c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),car,pr=tf_comp())[[4]] 
    })
  
  dl_spline_tif_comp_i <- reactive({
    dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    Tabla.splines(data = dat,tipo = "TIF",fe=input$n5,num =40,par = input$parametro_tif_dl_comp,tit=c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),car,pr=tf_comp())[[4]] 
    })
  
  

  dl_tif <- reactive({
  #defino eje maduracion
  X1 <- seq(0.1,20,0.1)
  
  #defino tiempos
  Y1 <- seq(1,50,1)
  
  #
  var_par <- as.data.frame(matrix(0,length(Y1),4))
  
  #guardo parametros segun cada tiempo
  for(i in 1:length(Y1)){
    #var_par[i,] <- par_dl(t[i],spline1,pa=c(1,1,1,1))
    var_par[i,] <- par_dl(Y1,dl_spline_tif_comp_i(),pa=c(1,1,1,1))
    
  }
  
  return(var_par[dim(var_par)[1],])
  })
  
  
  #TIF
  output$curva_comp_tif <- renderPlotly({ 
    #data
    x <- seq(1,20,0.1)
    withProgress(message = 'Calculando alturas...', value = 0, {
    #y_ns <- c(0.0097,nelson_siegel(x[2:length(x)],pa=gra_tif_ns_comp_i()))*100
    incProgress(1/5, detail = "Metodología Nelson y Siegel")
    y_ns <- nelson_siegel(x,pa=gra_tif_ns_comp_i())*100
    incProgress(1/5, detail = "Metodología Svensson")
    y_sven <- sven(x,pa=gra_tif_sven_comp_i())*100
    incProgress(1/5, detail = "Metodología Diebold-Li")
    y_dl <- diebold_li(pa=as.numeric(dl_tif()),x)*100
    incProgress(1/5, detail = "Metodología Splines")
    y_sp <- predict(spline_tif_comp(),x*365)$y
  
    })
    
    #dataframe
    data <- data.frame(x,y_ns,y_sven,y_dl,y_sp)
    
    #curva 
    #defino nombres de ejes
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x1 <- list(
      title = "Maduración (días)",
      titlefont = f
    )
    y1 <- list(
      title = "Rendimientos (%)",
      titlefont = f
    )
    
    #
    plot_ly(data, x = ~x, y = ~y_ns, name = 'Nelson y Siegel', type = 'scatter', mode = 'lines') %>% 
    add_trace(y = ~y_sven, name = 'Svensson', mode = 'lines') %>% 
    add_trace(y = ~y_dl, name = 'Diebold-Li', mode = 'lines') %>% 
    add_trace(y = ~y_sp, name = 'Splines', mode = 'lines') %>%
     layout(title = "Curvas de Rendimientos TIF",xaxis = x1, yaxis = y1)
      
    })
  
  #VEBONOS
  
  spline_veb_comp <- reactive({
    dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n5,num =40,par = input$parametro_veb_comp,tit=c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),car,pr=tv_comp())[[4]]
    #tabla_sp_dl_veb_comp()[[4]]
    })
  
  dl_spline_veb_comp_i <- reactive({
    dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    Tabla.splines(data = dat,tipo = "VEBONO",fe=input$n5,num =40,par = input$parametro_veb_dl_comp,tit=c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),car,pr=tv_comp())[[4]]
    })
  
  
  
  dl_veb <- reactive({
    #defino eje maduracion
    X1 <- seq(0.1,20,0.1)
    
    #defino tiempos
    Y1 <- seq(1,50,1)
    
    #
    var_par <- as.data.frame(matrix(0,length(Y1),4))
    
    #guardo parametros segun cada tiempo
    for(i in 1:length(Y1)){
      #var_par[i,] <- par_dl(t[i],spline1,pa=c(1,1,1,1))
      var_par[i,] <- par_dl(Y1,dl_spline_veb_comp_i(),pa=c(1,1,1,1))
      
    }
    
    return(var_par[dim(var_par)[1],])
  })
  
  
  #TIF
  output$curva_comp_veb <- renderPlotly({ 
    #data
    x <- seq(1,20,0.1)
    withProgress(message = 'Calculando alturas...', value = 0, {
      incProgress(1/5, detail = "Metodología Nelson y Siegel")  
    y_ns <- nelson_siegel(x,pa=gra_veb_ns_comp_i())*100
    incProgress(1/5, detail = "Metodología Svensson")
    y_sven <- sven(x,pa=gra_veb_sven_comp_i())*100
    incProgress(1/5, detail = "Metodología Diebold-Li")
    y_dl <- diebold_li(pa=as.numeric(dl_veb()),x)*100
    incProgress(1/5, detail = "Metodología Splines")
    y_sp <- predict(spline_veb_comp(),x*365)$y
    })
    
    #dataframe
    data <- data.frame(x,y_ns,y_sven,y_dl,y_sp)
    
    #curva 
    #defino nombres de ejes
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x1 <- list(
      title = "Maduración (días)",
      titlefont = f
    )
    y1 <- list(
      title = "Rendimientos (%)",
      titlefont = f
    )
    
    #
    plot_ly(data, x = ~x, y = ~y_ns, name = 'Nelson y Siegel', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~y_sven, name = 'Svensson', mode = 'lines') %>% 
      add_trace(y = ~y_dl, name = 'Diebold-Li', mode = 'lines') %>% 
      add_trace(y = ~y_sp, name = 'Splines', mode = 'lines') %>%
      layout(title = "Curvas de Rendimientos VEBONO",xaxis = x1, yaxis = y1)
    
  })
  
  # Reporte código genérico
  
  output$report <- downloadHandler(
    filename = "reporte1.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "reporte1.Rmd")
      #tempReport <- file.path(getwd(), "reporte1.Rmd")
      
      file.copy("reporte1.Rmd", tempReport, overwrite = TRUE)
      
      # Configuración de parámetros pasados a documento Rmd
      params <- list(titulos = c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),
                    pre_prom = tf_comp(),
                    par_ns_t = gra_tif_ns_comp_i(),
                    par_sven_t= gra_tif_sven_comp_i(),
                    par_dl_t=dl_tif(),
                    par_sp_t=spline_tif_comp(),
                    comp_pre_t=precios_tif()
                    
                    
                     # data2 = data()$corriente,
                     # data3 = data()$corriente.rem,
                     # dist1 = input$distVarA,
                     # dist2 = input$distVarC,
                     # dist3 = input$distVarCR,
                     # pconf = input$porVar,
                     # reali = input$reali,
                     # revi = input$revi
                     )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    })
  
  #reporte vebonos
  output$report1 <- downloadHandler(
    filename = "reporte2.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "reporte2.Rmd")
      #tempReport <- file.path(getwd(), "reporte1.Rmd")
      
      file.copy("reporte2.Rmd", tempReport, overwrite = TRUE)
      
      # Configuración de parámetros pasados a documento Rmd
      params <- list(titulos = c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),
                     pre_prom = tv_comp(),
                     par_ns_v = gra_veb_ns_comp_i(),
                     par_sven_v= gra_veb_sven_comp_i(),
                     par_dl_v=dl_veb(),
                     par_sp_v=spline_veb_comp(),
                     comp_pre_v=precios_veb()
                     
                     
                     # data2 = data()$corriente,
                     # data3 = data()$corriente.rem,
                     # dist1 = input$distVarA,
                     # dist2 = input$distVarC,
                     # dist3 = input$distVarCR,
                     # pconf = input$porVar,
                     # reali = input$reali,
                     # revi = input$revi
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    })
  
  #descarga BCV
  datasetInput <- reactive({
    switch(input$dataset,
           "0-22" = ruta_bcv("0-22"),
           "Caracteristicas" = ruta_bcv("caracteristicas"))
  })
  
  output$desc <- renderPrint({ input$dataset })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xls", sep = "")
    },
    content = function(file) {
      file<-paste(getwd(),"data",paste(input$dataset, ".xls", sep = ""),sep="/")
      #antes en method estaba libcurl
      download.file(url=datasetInput(),destfile=file,method = "internal",mode="wb")
      #write.xlsx(datasetInput(), file, row.names = FALSE)
    }
  )
  
  #leo caracteristica guardada en carpeta data
  output$Ca_leida <- renderDataTable({
    ca <- try(Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")))
    if(class(ca)=="try-error"){
      v <- print("El archivo no se encuentra, descargar y recargar página!")
      return(as.data.frame(v))
    }else{
    return(ca)
    }
    })
  
  #leo documento 022
  output$docbcv <- renderDataTable({
    ca <- try(Preciosbcv(paste(getwd(),"data","0-22.xls",sep = "/")))
    ca1 <- try(Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/")))
    
    if(class(ca)=="try-error" | class(ca1)=="try-error" ){
      v <- print("El archivo no se encuentra, descargar y recargar página!")
      return(as.data.frame(v))
    }else{
      ca2 <- formatop(ca1,ca)
      #convierto fecha de op y venc en fechas
      ca2$`Fecha op` <- as.Date(as.character(ca2$`Fecha op`),format="%d/%m/%Y")
      ca2$F.Vencimiento <- as.Date(as.character(ca2$F.Vencimiento),format="%d/%m/%Y")

      #este data frame es el que utiliza la metodologia Spline para los calculos
      ca3 <- dplyr::arrange(ca2,(`Fecha op`))

      #guardo historico_actualizado
      hist <- read.csv(paste(getwd(),"data","Historico.txt",sep = "/"),sep="")
      hist[,3] <- as.Date(as.character(hist[,3]))
      hist[,6] <- as.Date(as.character(hist[,6]))


      names(ca3)=names(hist)
     #print(str(ca3))
     #print(str(hist))

      hist_act <- rbind.data.frame(hist,ca3)


      write.table(hist_act,paste(getwd(),"data","Historico_act.txt",sep = "/"),row.names = FALSE)

      return(ca3)
    }
  })
  
  
  output$pre_prom_tif <- renderPrint({
            #leo el historico actualizado
            hist <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
            
            #como primer enfoque busco todos los tif y veb
            #luego se puede buscar solamente los tit seleccionados
            #no seria muy dificil este cambio
            hist_18 <- pre_prom(hist,"2018")
            hist_17 <- pre_prom(hist,"2017")
            hist_16 <- pre_prom(hist,"2016")
            
            #para buscar tif uso hist_18 u otro año y uso el segundo 
            #elemento de la lista
            #busco tif de mi cartera en historico 2018
            tif_18 <- comp(tit,hist_18[[2]])
            
            #los tif que no encuentro en 2018 los busco en 2017
            if(length(tif_18[[2]])!=0){
              tif_17 <- comp(tif_18[[2]],hist_17[[2]])
            }else{
              print("Todos los instrumentos estan")
            }
            
            #los tif que no encuentro en 2017 los busco en 2016
            if(length(tif_17[[2]])!=0){
              tif_16 <- comp(tif_17[[2]],hist_16[[2]])
            }else{
              print("Todos los instrumentos estan")
            }
            
            #precios promedio que salen
            TIF <- rbind.data.frame(tif_18[[1]],tif_17[[1]],tif_16[[1]])
            names(TIF) <- c("Títulos","Precio Promedio","Año")
            write.table(TIF,paste(getwd(),"data","Precio_prom_tif.txt",sep = "/"),row.names = FALSE)
            
            
            #titulos tif que no salen
            no_tif <- as.data.frame(tif_16[[2]])
            names(no_tif) <- "Títulos faltantes"
            write.table(no_tif,paste(getwd(),"data","Tif_faltantes.txt",sep = "/"),row.names = FALSE)
            
            
            #return(TIF)
            print("Titulos en historico")
            print(TIF)
            
            print("Titulos que no salen")
            print(no_tif)

            
            
                          })
    
    
  output$pre_prom_veb <- renderPrint({
    #leo el historico actualizado
    hist <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    
    #como primer enfoque busco todos los tif y veb
    #luego se puede buscar solamente los tit seleccionados
    #no seria muy dificil este cambio
    hist_18 <- pre_prom(hist,"2018")
    hist_17 <- pre_prom(hist,"2017")
    hist_16 <- pre_prom(hist,"2016")
    
    #para buscar tif uso hist_18 u otro año y uso el tercer 
    #elemento de la lista
    #busco veb de mi cartera en historico 2018
    veb_18 <- comp(tit1,hist_18[[3]])
    
    #los tif que no encuentro en 2018 los busco en 2017
    if(length(veb_18[[2]])!=0){
      veb_17 <- comp(veb_18[[2]],hist_17[[3]])
    }else{
      print("Todos los instrumentos estan")
    }
    
    #los tif que no encuentro en 2017 los busco en 2016
    if(length(veb_17[[2]])!=0){
      veb_16 <- comp(veb_17[[2]],hist_16[[3]])
    }else{
      print("Todos los instrumentos estan")
    }
    
    #precios promedio que salen
    VEB <- rbind.data.frame(veb_18[[1]],veb_17[[1]],veb_16[[1]])
    names(VEB) <- c("Títulos","Precio Promedio","Año")
    write.table(VEB,paste(getwd(),"data","Precio_prom_veb.txt",sep = "/"),row.names = FALSE)
    
    
    #titulos tif que no salen
    no_veb <- as.data.frame(veb_16[[2]])
    names(no_veb) <- "Títulos faltantes"
    write.table(no_veb,paste(getwd(),"data","Veb_faltantes.txt",sep = "/"),row.names = FALSE)
    
    
    #return(TIF)
    print("Titulos en historico")
    print(VEB)
    
    print("Titulos que no salen")
    print(no_veb)
    
    
    
  })
  
  #opcion eliminar SPLINES
  # output$tabla_elim_tif <- renderDataTable({
  #   if(input$elim_tif==1){
  #       
  # 
  #   }else{
  #     Aviso <- "No se eliminarán observaciones iniciales"
  #     return(as.data.frame(Aviso))
  #   }
  # })
  
  #calculo candidatos a elimnar
  #TIF
  obs_elim_tif <- reactive({
    #Tabla.splines(data = data_splines,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),C_splines,pr=tf_sp())[[2]]
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # #return(car)
    # #print(str(data_splines))
    # #print(str(dat))
    #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[2]]
    
     #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())
     a <- tabla_sp_tif()
     letra <- a[[3]]
     cand <- a[[2]]
     
     a1 <- c(as.character(letra[,2]),as.character(cand[,1]))
     return(as.character(a1))
    #return(as.character(a[,1]))
  })
  
  #TIF-COMP
  obs_elim_tif_comp <- reactive({
    #Tabla.splines(data = data_splines,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),C_splines,pr=tf_sp())[[2]]
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # #return(car)
    # #print(str(data_splines))
    # #print(str(dat))
    #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[2]]
    
    #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())
    a <- tabla_sp_tif_comp()
    letra <- a[[3]]
    cand <- a[[2]]
    
    a1 <- c(as.character(letra[,2]),as.character(cand[,1]))
    return(as.character(a1))
    #return(as.character(a[,1]))
  })
  
  #VEBONO
  obs_elim_veb <- reactive({
    a <- tabla_sp_veb()
    letra <- a[[3]]
    cand <- a[[2]]
    
    a1 <- c(as.character(letra[,2]),as.character(cand[,1]))
    return(as.character(a1))
  })

  #VEBONO-COMP
  obs_elim_veb_comp <- reactive({
    #Tabla.splines(data = data_splines,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),C_splines,pr=tf_sp())[[2]]
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # #return(car)
    # #print(str(data_splines))
    # #print(str(dat))
    #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[2]]
    
    #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())
    a <- tabla_sp_veb_comp()
    letra <- a[[3]]
    cand <- a[[2]]
    
    a1 <- c(as.character(letra[,2]),as.character(cand[,1]))
    return(as.character(a1))
    #return(as.character(a[,1]))
  })
  
  #hago el imputselect interactivo
  #TIF
  output$selectUI_tif <- renderUI({ 
    selectInput("obs_tif", "Seleccionar títulos", obs_elim_tif(),multiple = TRUE)
  })
  
  #TIF - COMPARATIVO
  output$selectUI_tif_comp <- renderUI({ 
    selectInput("obs_tif_comp", "Seleccionar títulos", obs_elim_tif_comp(),multiple = TRUE)
  })
  
  
  #VEBONO
  output$selectUI_veb <- renderUI({ 
    selectInput("obs_veb", "Seleccionar títulos", obs_elim_veb(),multiple = TRUE)
  })
  
  #VEBONO - COMPARATIVO
  output$selectUI_veb_comp <- renderUI({ 
    selectInput("obs_veb_comp", "Seleccionar títulos", obs_elim_veb_comp(),multiple = TRUE)
  })
  
  #imprimo titulos seleccionados a eliminar
  #TIF
  output$obs_elim_tif <- renderPrint(input$obs_tif)
  
  #TIF-COMP
  output$obs_elim_tif_comp <- renderPrint(input$obs_tif_comp)
  
  
  #VEBONO
  output$obs_elim_veb <- renderPrint(input$obs_veb)
  
  #VEBONO-COMP
  output$obs_elim_veb_comp <- renderPrint(input$obs_veb_comp)
  
  #muestro nuevos candidatos
  #TIF
  output$tit_cand_tif_new <- renderDataTable({
    #Tabla.splines(data = data_splines,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),C_splines,pr=tf_sp())[[2]]
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # #return(car)
    # #print(str(data_splines))
    # #print(str(dat))
    #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[2]]
    
    #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())
    a <- tabla_sp_tif()
    letra <- a[[3]]
    letra[,6] <- as.Date(letra[,6])
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    cand <- a[[2]]
    
    
    names(letra1)=names(cand)
    
    a <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
    
    if(length(input$obs_tif)==0){
      Aviso <- "No se ha seleccionado nada"
      return(as.data.frame(Aviso))
      
    }else{
    
    b <- c()
    for(i in 1:length(input$obs_tif)){
    b[i] <- which(input$obs_tif[i]==as.character(a[,1]))
    }
    a <- a[-b,]
    return(a)
    
    }
    
  })
  
  #TIF - COMP
  
  output$tit_cand_tif_new_comp <- renderDataTable({
    a <- tabla_sp_tif_comp()
    letra <- a[[3]]
    letra[,6] <- as.Date(letra[,6])
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    cand <- a[[2]]
    
    
    names(letra1)=names(cand)
    
    a <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
    
    if(length(input$obs_tif_comp)==0){
      Aviso <- "No se ha seleccionado nada"
      return(as.data.frame(Aviso))
      
    }else{
      
      b <- c()
      for(i in 1:length(input$obs_tif_comp)){
        b[i] <- which(input$obs_tif_comp[i]==as.character(a[,1]))
      }
      a <- a[-b,]
      return(a)
      
    }
    
  })
  
  
  #VEBONOS
  output$tit_cand_veb_new <- renderDataTable({
    a <- tabla_sp_veb()
    letra <- a[[3]]
    letra[,6] <- as.Date(letra[,6])
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    cand <- a[[2]]
    
    
    names(letra1)=names(cand)
    
    a <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
    
    if(length(input$obs_veb)==0){
      Aviso <- "No se ha seleccionado nada"
      return(as.data.frame(Aviso))
      
    }else{
      
      b <- c()
      for(i in 1:length(input$obs_veb)){
        b[i] <- which(input$obs_veb[i]==as.character(a[,1]))
      }
      a <- a[-b,]
      return(a)
      
    }
    
  })
  
  #VEBONOS-COMP
  output$tit_cand_veb_new_comp <- renderDataTable({
    a <- tabla_sp_veb_comp()
    letra <- a[[3]]
    letra[,6] <- as.Date(letra[,6])
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    cand <- a[[2]]
    
    
    names(letra1)=names(cand)
    
    a <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
    
    if(length(input$obs_veb_comp)==0){
      Aviso <- "No se ha seleccionado nada"
      return(as.data.frame(Aviso))
      
    }else{
      
      b <- c()
      for(i in 1:length(input$obs_veb_comp)){
        b[i] <- which(input$obs_veb_comp[i]==as.character(a[,1]))
      }
      a <- a[-b,]
      return(a)
      
    }
    
  })
  
  #calculo nuevos precios
  #TIF
  output$precios_tif_nuevos <- renderDataTable({
    #Tabla.splines(data = data_splines,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),C_splines,pr=tf_sp())[[2]]
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
     car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # #return(car)
    # #print(str(data_splines))
    # #print(str(dat))
    #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[2]]
    #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())
    a <- tabla_sp_tif()
    letra <- a[[3]]
    letra[,6] <- as.Date(letra[,6])
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    cand <- a[[2]]
    
    
    names(letra1)=names(cand)
    
    a <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
    
    if(length(input$obs_tif)==0){
      Aviso <- "No se ha eliminado nada, ver tabla anterior"
      return(as.data.frame(Aviso))
      
    }else{
      
      b <- c()
      for(i in 1:length(input$obs_tif)){
        b[i] <- which(input$obs_tif[i]==as.character(a[,1]))
      }
      a <- a[-b,]
      
      
      #calculo spline
      spline <- smooth.spline(a[,4],a[,7],spar = input$parametro_tif)
      
      #calculo precios
      pre <- precio(tit = c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),spline1 = spline,fv =input$n4 ,C = car)
      
      pre1 <- cbind.data.frame("Títulos"=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),"Precios"=pre)
      
      return(pre1)
    }
  
  })
  
  #TIF-COMP
  output$precios_tif_nuevos_comp <- renderDataTable({
    #Tabla.splines(data = data_splines,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),C_splines,pr=tf_sp())[[2]]
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # #return(car)
    # #print(str(data_splines))
    # #print(str(dat))
    #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[2]]
    #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())
    a <- tabla_sp_tif_comp()
    letra <- a[[3]]
    letra[,6] <- as.Date(letra[,6])
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    cand <- a[[2]]
    
    
    names(letra1)=names(cand)
    
    a <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
    
    if(length(input$obs_tif_comp)==0){
      Aviso <- "No se ha eliminado nada, ver tabla anterior"
      return(as.data.frame(Aviso))
      
    }else{
      
      b <- c()
      for(i in 1:length(input$obs_tif_comp)){
        b[i] <- which(input$obs_tif_comp[i]==as.character(a[,1]))
      }
      a <- a[-b,]
      
      
      #calculo spline
      spline <- smooth.spline(a[,4],a[,7],spar = input$parametro_tif_comp)
      
      #calculo precios
      pre <- precio(tit = c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),spline1 = spline,fv =input$n5 ,C = car)
      
      pre1 <- cbind.data.frame("Títulos"=c(input$t1_comp,input$t2_comp,input$t3_comp,input$t4_comp),"Precios"=pre)
      
      return(pre1)
    }
    
  })
  
  
  #VEBONOS
  output$precios_veb_nuevos <- renderDataTable({
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    a <- tabla_sp_veb()
    letra <- a[[3]]
    letra[,6] <- as.Date(letra[,6])
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    cand <- a[[2]]
    
    
    names(letra1)=names(cand)
    
    a <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
    
    if(length(input$obs_veb)==0){
      Aviso <- "No se ha eliminado nada, ver tabla anterior"
      return(as.data.frame(Aviso))
      
    }else{
      
      b <- c()
      for(i in 1:length(input$obs_veb)){
        b[i] <- which(input$obs_veb[i]==as.character(a[,1]))
      }
      a <- a[-b,]
      
      
      #calculo spline
      spline <- smooth.spline(a[,4],a[,7],spar = input$parametro_veb)
      
      #calculo precios
      pre <- precio(tit = c(input$v1_sp,input$v2_sp,input$v3_sp,input$v4_sp),spline1 = spline,fv =input$n4 ,C = car)
      
      pre1 <- cbind.data.frame("Títulos"=c(input$v1_sp,input$v2_sp,input$v3_sp,input$v4_sp),"Precios"=pre)
      
      return(pre1)
    }
    
  })
  
  #VEBONO-COMP
  output$precios_veb_nuevos_comp <- renderDataTable({
    #Tabla.splines(data = data_splines,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),C_splines,pr=tf_sp())[[2]]
    # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
    # dat[,3] <- as.Date(as.character(dat[,3]))
    car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
    # #return(car)
    # #print(str(data_splines))
    # #print(str(dat))
    #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[2]]
    #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())
    a <- tabla_sp_veb_comp()
    letra <- a[[3]]
    letra[,6] <- as.Date(letra[,6])
    letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
    cand <- a[[2]]
    
    
    names(letra1)=names(cand)
    
    a <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
    
    if(length(input$obs_veb_comp)==0){
      Aviso <- "No se ha eliminado nada, ver tabla anterior"
      return(as.data.frame(Aviso))
      
    }else{
      
      b <- c()
      for(i in 1:length(input$obs_veb_comp)){
        b[i] <- which(input$obs_veb_comp[i]==as.character(a[,1]))
      }
      a <- a[-b,]
      
      
      #calculo spline
      spline <- smooth.spline(a[,4],a[,7],spar = input$parametro_veb_comp)
      
      #calculo precios
      pre <- precio(tit = c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),spline1 = spline,fv =input$n5 ,C = car)
      
      pre1 <- cbind.data.frame("Títulos"=c(input$v1_comp,input$v2_comp,input$v3_comp,input$v4_comp),"Precios"=pre)
      
      return(pre1)
    }
    
  })
  
  
  #nueva curva TIF
  #renderRbokeh
  output$c_tif_splines_new <- renderRbokeh({
     withProgress(message = 'Graficando curva de rendimiento...', value = 0, {
       incProgress(1/2, detail = "Calculando alturas")
       # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
       # dat[,3] <- as.Date(as.character(dat[,3]))
       # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
       # 
       #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[2]]
       #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())
       a <- tabla_sp_tif()
       letra <- a[[3]]
       letra[,6] <- as.Date(letra[,6])
       letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
       cand <- a[[2]]
       
       
       names(letra1)=names(cand)
       
       a <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
       
       
       if(length(input$obs_tif)==0){
         figure(width = 1000,height = 400) 
       }else{
         
         b <- c()
         for(i in 1:length(input$obs_tif)){
           b[i] <- which(input$obs_tif[i]==as.character(a[,1]))
         }
         a <- a[-b,]
       }
         
         #calculo spline
         spline <- smooth.spline(a[,4],a[,7],spar = input$parametro_tif)
         
       y <-predict(spline,seq(0.1,20,0.1)*365)$y
       incProgress(1/2, detail = "Ajustando spline")
       
    #   plot(seq(0.1,20,0.1)*365,y)
    #   figure(width = 1000,height = 400) %>%
    #     ly_points(pto_sp_tif_dl()[,4],pto_sp_tif_dl()[,7],pto_sp_tif_dl(),hover=list("Nombre"=pto_sp_tif_dl()[,1],"Fecha de operación"=pto_sp_tif_dl()[,2])) %>%
    #     ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="green",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
    #     x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)") 
         figure(width = 1000,height = 400) %>%
           ly_points(a[,4],a[,7],a,hover=list("Nombre"=a[,1],"Fecha de operación"=a[,2])) %>%
           ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="green",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
           x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)")
       })
  })
  
  #TIF-COMP
  output$c_tif_splines_new_comp <- renderRbokeh({
    withProgress(message = 'Graficando curva de rendimiento...', value = 0, {
      incProgress(1/2, detail = "Calculando alturas")
      # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
      # dat[,3] <- as.Date(as.character(dat[,3]))
      # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
      # 
      #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[2]]
      #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())
      a <- tabla_sp_tif_comp()
      letra <- a[[3]]
      letra[,6] <- as.Date(letra[,6])
      letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
      cand <- a[[2]]
      
      
      names(letra1)=names(cand)
      
      a <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
      
      
      if(length(input$obs_tif_comp)==0){
        figure(width = 1000,height = 400) 
      }else{
        
        b <- c()
        for(i in 1:length(input$obs_tif_comp)){
          b[i] <- which(input$obs_tif_comp[i]==as.character(a[,1]))
        }
        a <- a[-b,]
      }
      
      #calculo spline
      spline <- smooth.spline(a[,4],a[,7],spar = input$parametro_tif_comp)
      
      y <-predict(spline,seq(0.1,20,0.1)*365)$y
      incProgress(1/2, detail = "Ajustando spline")
      
      #   plot(seq(0.1,20,0.1)*365,y)
      #   figure(width = 1000,height = 400) %>%
      #     ly_points(pto_sp_tif_dl()[,4],pto_sp_tif_dl()[,7],pto_sp_tif_dl(),hover=list("Nombre"=pto_sp_tif_dl()[,1],"Fecha de operación"=pto_sp_tif_dl()[,2])) %>%
      #     ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="green",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
      #     x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)") 
      figure(width = 1000,height = 400) %>%
        ly_points(a[,4],a[,7],a,hover=list("Nombre"=a[,1],"Fecha de operación"=a[,2])) %>%
        ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="green",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
        x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)")
    })
  })
  
  #VEBONOS
  output$c_veb_splines_new <- renderRbokeh({
    withProgress(message = 'Graficando curva de rendimiento...', value = 0, {
      incProgress(1/2, detail = "Calculando alturas")
      a <- tabla_sp_veb()
      letra <- a[[3]]
      letra[,6] <- as.Date(letra[,6])
      letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
      cand <- a[[2]]
      
      names(letra1)=names(cand)
      
      a <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
      
      
      if(length(input$obs_veb)==0){
        figure(width = 1000,height = 400) 
      }else{
        
        b <- c()
        for(i in 1:length(input$obs_veb)){
          b[i] <- which(input$obs_veb[i]==as.character(a[,1]))
        }
        a <- a[-b,]
      }
      
      #calculo spline
      spline <- smooth.spline(a[,4],a[,7],spar = input$parametro_veb)
      
      y <-predict(spline,seq(0.1,20,0.1)*365)$y
      incProgress(1/2, detail = "Ajustando spline")
      
      figure(width = 1000,height = 400) %>%
        ly_points(a[,4],a[,7],a,hover=list("Nombre"=a[,1],"Fecha de operación"=a[,2])) %>%
        ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="green",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
        x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)")
    })
  })
  
  #VEBONO_COMP
  
  output$c_veb_splines_new_comp <- renderRbokeh({
    withProgress(message = 'Graficando curva de rendimiento...', value = 0, {
      incProgress(1/2, detail = "Calculando alturas")
      # dat <- read.csv(paste(getwd(),"data","Historico_act.txt",sep = "/"),sep="")
      # dat[,3] <- as.Date(as.character(dat[,3]))
      # car <- Carac(paste(getwd(),"data","Caracteristicas.xls",sep = "/"))
      # 
      #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())[[2]]
      #a <- Tabla.splines(data = dat,tipo = "TIF",fe=input$n4,num = input$d_tif,par = input$parametro_tif,tit=c(input$t1_sp,input$t2_sp,input$t3_sp,input$t4_sp),car,pr=tf_sp())
      a <- tabla_sp_veb_comp()
      letra <- a[[3]]
      letra[,6] <- as.Date(letra[,6])
      letra1 <- data.frame(letra[,c(2,3,6,7,12,13,15)],"Corto Plazo","C1")
      cand <- a[[2]]
      
      
      names(letra1)=names(cand)
      
      a <- rbind.data.frame(letra1,cand,make.row.names = FALSE)
      
      
      if(length(input$obs_veb_comp)==0){
        figure(width = 1000,height = 400) 
      }else{
        
        b <- c()
        for(i in 1:length(input$obs_veb_comp)){
          b[i] <- which(input$obs_veb_comp[i]==as.character(a[,1]))
        }
        a <- a[-b,]
      }
      
      #calculo spline
      spline <- smooth.spline(a[,4],a[,7],spar = input$parametro_veb_comp)
      
      y <-predict(spline,seq(0.1,20,0.1)*365)$y
      incProgress(1/2, detail = "Ajustando spline")
      
      #   plot(seq(0.1,20,0.1)*365,y)
      #   figure(width = 1000,height = 400) %>%
      #     ly_points(pto_sp_tif_dl()[,4],pto_sp_tif_dl()[,7],pto_sp_tif_dl(),hover=list("Nombre"=pto_sp_tif_dl()[,1],"Fecha de operación"=pto_sp_tif_dl()[,2])) %>%
      #     ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="green",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
      #     x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)") 
      figure(width = 1000,height = 400) %>%
        ly_points(a[,4],a[,7],a,hover=list("Nombre"=a[,1],"Fecha de operación"=a[,2])) %>%
        ly_points(x=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],y=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2],color="green",hover=list("Plazo"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,1],"Rendimiento"=cbind.data.frame(x=seq(0.1,20,0.1)*365,y)[,2]),size=4) %>%
        x_axis("Plazo (días)") %>% y_axis("Rendimiento (%)")
    })
  })
  
  
  
 
})
