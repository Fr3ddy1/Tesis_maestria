#funcion ruta bcv
ruta_bcv <- function(file){
  if(file=="0-22"){
    webpage <- read_html("http://www.bcv.org.ve/sistemas-de-pago/sicet/estadisticas")
    # Extract records info
    results <- webpage %>% html_nodes(".file")
    
    # Building the dataset
    records <- vector("list", length = length(results))
    
    for (i in seq_along(results)) {
      url <- results[i] %>% html_nodes("a") %>% html_attr("href")
      records[[i]] <- data_frame(url = url)
    }
    
    df <- bind_rows(records)
    return(as.character(df[1,1]))
  }#final if 0-22
  else if(file=="caracteristicas" | file=="tasas"){
    webpage <- read_html("http://www.bcv.org.ve/estadisticas/tasas-de-interes")
    # Extract records info
    results <- webpage %>% html_nodes(".file")
    
    # Building the dataset
    records <- vector("list", length = length(results))
    
    for (i in seq_along(results)) {
      url <- results[i] %>% html_nodes("a") %>% html_attr("href")
      records[[i]] <- data_frame(url = url)
    }
    
    df <- bind_rows(records)
    if(file=="tasas"){
      return(as.character(df[33,1]))
    }
    if(file=="caracteristicas"){
      return(as.character(df[37,1]))
    }
    
    
  }#final caracteristicas y tasas
  else if(file=="abs"){
    webpage <- read_html("http://www.bcv.org.ve/politica-monetaria/operaciones-absorcion-diaria")
    # Extract records info
    results <- webpage %>% html_nodes(".file")
    
    # Building the dataset
    records <- vector("list", length = length(results))
    
    for (i in seq_along(results)) {
      url <- results[i] %>% html_nodes("a") %>% html_attr("href")
      records[[i]] <- data_frame(url = url)
    }
    
    df <- bind_rows(records)
    return(as.character(df[1,1]))
  }#final abs
  else if(file=="iny"){
    webpage <- read_html("http://www.bcv.org.ve/politica-monetaria/operaciones-inyeccion-diaria")
    # Extract records info
    results <- webpage %>% html_nodes(".file")
    
    # Building the dataset
    records <- vector("list", length = length(results))
    
    for (i in seq_along(results)) {
      url <- results[i] %>% html_nodes("a") %>% html_attr("href")
      records[[i]] <- data_frame(url = url)
    }
    
    df <- bind_rows(records)
    return(as.character(df[1,1]))
  }#final iny
  else if(file=="letras"){
    webpage <- read_html("http://www.bcv.org.ve/politica-monetaria/letras-del-tesoro")
    # Extract records info
    results <- webpage %>% html_nodes(".file")
    
    # Building the dataset
    records <- vector("list", length = length(results))
    
    for (i in seq_along(results)) {
      url <- results[i] %>% html_nodes("a") %>% html_attr("href")
      records[[i]] <- data_frame(url = url)
    }
    
    df <- bind_rows(records)
    return(as.character(df[1,1]))
  }#final letras
  else if(file=="tif" | file=="veb" ){
    webpage <- read_html("http://www.bcv.org.ve/politica-monetaria/bonos-dpn")
    # Extract records info
    results <- webpage %>% html_nodes(".file")
    
    # Building the dataset
    records <- vector("list", length = length(results))
    
    for (i in seq_along(results)) {
      url <- results[i] %>% html_nodes("a") %>% html_attr("href")
      records[[i]] <- data_frame(url = url)
    }
    
    df <- bind_rows(records)
    if(file=="tif"){return(as.character(df[10,1]))}
    if(file=="veb"){return(as.character(df[6,1]))}
  }#final tif o veb
  else{
    print("Nombre inválido, revise eintente nuevamente")
  }
  
}#final funcion ruta_bcv
